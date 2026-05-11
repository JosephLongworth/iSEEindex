# Unified upload + convert modal. Accepts .h5ad / Seurat .rds / SCE .rds.
# Non-SCE inputs are converted inline to a SingleCellExperiment via anndataR
# (H5AD) or Seurat::as.SingleCellExperiment (Seurat). The resulting SCE is
# what gets handed to the iSEEindex add-for-session / commit-to-shared flow,
# so the on-disk format is always .rds containing an SCE.
#
# The "Download converted .rds" button lets users save the SCE locally so
# subsequent uploads skip the conversion step.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Input-ID constants matching the iSEEindex inline upload form. These IDs
# must stay in sync with R/constants.R in the iSEEindex fork so the existing
# `.create_upload_observers` picks them up.
.UP_IDS <- list(
  sce_file    = "iSEEindex_INTERNAL_upload_sce_file",
  title       = "iSEEindex_INTERNAL_upload_title",
  description = "iSEEindex_INTERNAL_upload_description",
  layout_file = "iSEEindex_INTERNAL_upload_layout_file",
  add_btn     = "iSEEindex_INTERNAL_upload_add",
  commit_btn  = "iSEEindex_INTERNAL_upload_commit",
  status      = "iSEEindex_INTERNAL_upload_status"
)

# Cross-modal pre-fill state. Populated by the layout builder when it produces
# an initial.R; read here on modal open.
LAST_RESULTS <- new.env(parent = emptyenv())
LAST_RESULTS$layout_R    <- NULL
LAST_RESULTS$layout_name <- NULL

# Server-side fallback consulted by the patched iSEEindex upload helpers.
# Our modal performs conversion server-side, then writes the resulting .rds
# path here. The patched `.handle_upload_add` / `.handle_upload_commit` use
# this when `input[[.ui_upload_sce_file]]` is NULL — which is always the case
# for files we converted, because `session$sendInputMessage` only updates the
# client-side fileInput state.
UPLOAD_FALLBACK <- new.env(parent = emptyenv())
UPLOAD_FALLBACK$datapath <- NULL
UPLOAD_FALLBACK$name     <- NULL

upload_modal_ui <- function() {
  modalDialog(
    title = tagList(icon("upload"), " Upload dataset"),
    size = "xl", easyClose = TRUE,
    footer = modalButton("Close"),

    pipeline_ui_styles(),

    tags$p(class = "text-muted",
      "Upload an ", tags$code(".h5ad"), ", Seurat ", tags$code(".rds"),
      ", or SingleCellExperiment ", tags$code(".rds"),
      ". Non-SCE files are converted automatically. Then click ",
      tags$strong("Add for this session"),
      " to use it now without persisting, or ",
      tags$strong("Commit to shared storage"),
      " to make it available on the curated landing page across sessions."),

    fluidRow(
      column(12,
        selectInput("up_prefill", "Pre-fill layout from",
          choices = c(
            "(none)"                      = "none",
            "Last layout-builder result"  = "layout"
          ),
          selected = "none", width = "100%"),
        tags$small(class = "text-muted", uiOutput("up_prefill_status"))
      )
    ),
    hr(),

    fluidRow(
      column(6,
        fileInput("up_input_file",
          label = "Dataset file (.h5ad, Seurat .rds, or SCE .rds)",
          accept = c(".h5ad", ".rds", ".RDS"),
          multiple = FALSE),
        textInput(.UP_IDS$title,
          label = "Dataset title",
          placeholder = "e.g. My scRNA-seq experiment"),
        textAreaInput(.UP_IDS$description,
          label = "Description (optional)",
          rows = 3L,
          placeholder = "Brief description of this dataset.")
      ),
      column(6,
        fileInput(.UP_IDS$layout_file,
          label = "Initial layout configuration (.R, optional)",
          accept = c(".r", ".R"),
          multiple = FALSE),
        br(),
        fluidRow(
          column(6,
            actionButton(.UP_IDS$add_btn,
              label = "Add for this session",
              style = "color:#ffffff; background-color:#4B7EA3; border-color:#2e6da4; width:100%")
          ),
          column(6,
            actionButton(.UP_IDS$commit_btn,
              label = "Commit to shared storage",
              style = "color:#ffffff; background-color:#6B1435; border-color:#4a0e25; width:100%")
          )
        ),
        br(),
        uiOutput(.UP_IDS$status)
      )
    ),

    hr(),
    div(class = "card",
      div(class = "card-header",
        icon("diagram-project"), " Conversion pipeline"),
      div(class = "card-body",
        browser_busy_banner(),
        uiOutput("up_pipeline_ui"),
        uiOutput("up_result_ui")
      )
    )
  )
}

upload_register <- function(input, output, session) {
  rv <- reactiveValues(
    proc_stage  = NULL,
    proc_source = NULL,
    sce_path    = NULL,   # path to converted/passed-through SCE .rds
    orig_name   = NULL,
    orig_size   = NULL,   # bytes, for time-hint
    meta        = NULL,
    needed_conv = FALSE   # TRUE if input was h5ad / Seurat
  )

  set_stage <- function(stage, source = NULL) {
    rv$proc_stage <- stage
    if (!is.null(source)) rv$proc_source <- source
  }
  run_next <- function(fn) {
    domain <- shiny::getDefaultReactiveDomain()
    later::later(function() {
      shiny::withReactiveDomain(domain, { isolate(fn()) })
    }, delay = 0.05)
  }

  observeEvent(input$open_upload, {
    showModal(upload_modal_ui())
  })

  output$up_prefill_status <- renderUI({
    have_layout <- !is.null(LAST_RESULTS$layout_R) &&
                    file.exists(LAST_RESULTS$layout_R)
    if (have_layout) {
      HTML(paste0("Layout script available: ",
                  basename(LAST_RESULTS$layout_R)))
    } else {
      HTML("No prior layout generated this session.")
    }
  })

  observeEvent(input$up_prefill, {
    sel <- input$up_prefill
    if (is.null(sel) || sel != "layout") return()
    p <- LAST_RESULTS$layout_R
    if (is.null(p) || !file.exists(p)) return()
    session$sendInputMessage(.UP_IDS$layout_file, list(
      name     = basename(p),
      size     = file.info(p)$size,
      type     = "",
      datapath = p
    ))
  }, ignoreInit = TRUE)

  # When a file is picked, convert if needed and write the resulting SCE to
  # a temp .rds, then surface that path to the iSEEindex upload helpers via
  # session$sendInputMessage on the sce_file input. The helpers expect a
  # fileInput-shaped list with name/size/type/datapath.
  observeEvent(input$up_input_file, {
    req(input$up_input_file)
    f <- input$up_input_file
    ext <- tolower(tools::file_ext(f$name))
    if (!ext %in% c("h5ad", "rds")) {
      showNotification("Unsupported file type. Use .h5ad or .rds",
                       type = "error")
      return()
    }

    rv$sce_path <- NULL
    rv$meta <- NULL
    rv$orig_name <- f$name
    rv$orig_size <- f$size
    rv$needed_conv <- ext != "rds"  # provisional; refined after readRDS

    # Helper: push a status update to the persistent browser-side banner.
    # The DNA spinner and shimmer keep running across all calls — only the
    # title/detail text changes. We hide the size pill after upload since
    # subsequent stages are about RAM/disk, not network.
    busy_set <- function(title, detail = "", hide_size = TRUE) {
      msg <- list(title = title, detail = detail)
      if (hide_size) msg$size <- FALSE
      session$sendCustomMessage("up_busy_set", msg)
    }

    # Stage text for an h5ad includes a coarse time hint based on file size.
    h5ad_hint <- estimate_h5ad_time(f$size)
    load_detail <- if (ext == "h5ad" && nzchar(h5ad_hint))
      paste0("Reading H5AD via anndataR - ", h5ad_hint, ".")
    else if (ext == "rds")
      "Reading .rds from disk..."
    else ""

    busy_set("Loading dataset into memory...", load_detail)

    ts <- format(Sys.time(), "%Y%m%d_%H%M%S_")
    base_name <- tools::file_path_sans_ext(f$name)
    dest <- file.path(tempdir(), paste0(ts, base_name, "_sce.rds"))

    run_next(function() {
      sce <- tryCatch(
        convert_to_sce(f$datapath, ext, on_stage = function(s, src = NULL) {
          set_stage(s, src)
          # on_stage runs inside the blocking convert call, so it can't push
          # to the browser in real time. The pre-call busy_set covers the
          # whole window.
        }),
        error = function(e) {
          showNotification(paste("Conversion failed:", conditionMessage(e)),
                           type = "error", duration = NULL)
          set_stage(NULL)
          session$sendCustomMessage("up_busy_hide", list())
          NULL
        })
      if (is.null(sce)) return()

      rv$needed_conv <- !identical(rv$proc_source, "rds")

      set_stage("convert", source = rv$proc_source)
      run_next(function() {
        m <- tryCatch(extract_sce_metadata(sce), error = function(e) list())
        run_next(function() {
          # For SCE input, copy the original so the upload helpers and the
          # download button both point at a stable path; for converted input,
          # save the new SCE.
          if (identical(rv$proc_source, "rds")) {
            file.copy(f$datapath, dest, overwrite = TRUE)
          } else {
            saveRDS(sce, dest)
          }
          rv$sce_path <- dest
          rv$meta <- m

          # Stash the converted path so the patched iSEEindex helpers find it.
          # We also send a client-side input message so the fileInput shows
          # the filename to the user, but that update does NOT make the value
          # readable on the server — the server-side fallback above is what
          # actually feeds the add/commit handlers.
          UPLOAD_FALLBACK$datapath <- dest
          UPLOAD_FALLBACK$name     <- paste0(base_name, "_sce.rds")
          session$sendInputMessage(.UP_IDS$sce_file, list(
            name     = paste0(base_name, "_sce.rds"),
            size     = file.info(dest)$size,
            type     = "",
            datapath = dest
          ))
          # Pre-fill title with the basename if empty.
          updateTextInput(session, .UP_IDS$title, value = base_name)

          set_stage("ready")
          session$sendCustomMessage("up_busy_hide", list())
        })
      })
    })
  })

  output$up_pipeline_ui <- renderUI({
    render_pipeline(rv$proc_stage, rv$proc_source,
                    sce_ready_fallback = !is.null(rv$sce_path),
                    input_size = rv$orig_size)
  })

  output$up_result_ui <- renderUI({
    if (is.null(rv$sce_path)) return(NULL)
    m <- rv$meta %||% list()
    summary_rows <- tagList(
      if (!is.null(m$n_cells)) tags$li(tags$strong("Cells: "),
        format(m$n_cells, big.mark = ",")),
      if (!is.null(m$n_features)) tags$li(tags$strong("Features: "),
        format(m$n_features, big.mark = ",")),
      if (length(m$assays))
        tags$li(tags$strong("Assays: "),
          paste(unlist(m$assays), collapse = ", ")),
      if (length(m$reducedDims))
        tags$li(tags$strong("Reduced dims: "),
          paste(unlist(m$reducedDims), collapse = ", ")),
      if (!is.null(m$n_cell_types))
        tags$li(tags$strong(m$cell_types_col, " types: "), m$n_cell_types)
    )
    tagList(
      h5("Dataset summary"),
      tags$ul(summary_rows),
      if (isTRUE(rv$needed_conv)) tagList(
        downloadButton("up_dl_rds", "Download converted .rds",
                       class = "btn-outline-secondary"),
        tags$p(class = "text-muted", style = "margin-top:8px;",
          tags$small("Save this SCE locally so future uploads skip the conversion step."))
      )
    )
  })

  output$up_dl_rds <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(rv$orig_name %||% "dataset")
      paste0(base, "_sce.rds")
    },
    content = function(file) {
      req(rv$sce_path)
      file.copy(rv$sce_path, file, overwrite = TRUE)
    }
  )

  invisible(NULL)
}
