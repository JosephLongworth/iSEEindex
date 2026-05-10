# Convert workflow as a modal dialog with directly-prefixed (non-modular) IDs.
# All inputs/outputs are prefixed `cv_` to avoid collisions with iSEEindex.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

convert_modal_ui <- function() {
  modalDialog(
    title = tagList(icon("right-left"), " Convert dataset to SingleCellExperiment"),
    size = "xl", easyClose = TRUE,
    footer = modalButton("Close"),

    pipeline_ui_styles(),

    fluidRow(
      column(4,
        h5("Upload"),
        fileInput("cv_upload", NULL,
          accept = c(".h5ad", ".rds", ".RDS"),
          placeholder = ".h5ad or .rds",
          buttonLabel = icon("folder-open")),
        tags$small(class = "text-muted",
          "H5AD and Seurat (.rds) files are converted to SingleCellExperiment.
           Plain SCE .rds files pass through unchanged.")
      ),
      column(8,
        div(class = "card",
          div(class = "card-header",
              icon("diagram-project"), " Processing Pipeline"),
          div(class = "card-body", uiOutput("cv_pipeline_ui"))
        )
      )
    ),
    hr(),
    uiOutput("cv_result_ui"),
    h6("Conversion log"),
    verbatimTextOutput("cv_log")
  )
}

# Register observers + outputs on the active iSEE session. Call once from the
# wrapped server in app.R. Uses reactiveValues local to that session.
convert_register <- function(input, output, session) {
  rv <- reactiveValues(
    proc_stage = NULL,
    proc_source = NULL,
    log = character(0),
    sce_path = NULL,
    meta_path = NULL,
    orig_name = NULL,
    meta = NULL
  )

  log_msg <- function(...) {
    rv$log <- c(rv$log,
      paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", paste0(...)))
  }
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

  observeEvent(input$open_convert, {
    showModal(convert_modal_ui())
  })

  observeEvent(input$cv_upload, {
    req(input$cv_upload)
    f <- input$cv_upload
    ext <- tolower(tools::file_ext(f$name))
    if (!ext %in% c("h5ad", "rds")) {
      showNotification("Unsupported file type. Use .h5ad or .rds",
                       type = "error")
      return()
    }

    rv$sce_path <- NULL
    rv$meta_path <- NULL
    rv$meta <- NULL
    rv$orig_name <- f$name

    ts <- format(Sys.time(), "%Y%m%d_%H%M%S_")
    dest <- file.path(tempdir(),
      paste0(ts, tools::file_path_sans_ext(f$name), ".rds"))

    log_msg("Starting conversion: ", f$name)
    run_next(function() {
      sce <- tryCatch(
        convert_to_sce(f$datapath, ext, on_stage = function(s, src = NULL) {
          set_stage(s, src)
          log_msg("Stage: ", s,
                  if (!is.null(src)) paste0(" (", src, ")") else "")
        }),
        error = function(e) {
          log_msg("ERROR: ", conditionMessage(e))
          showNotification(paste("Conversion failed:", conditionMessage(e)),
                           type = "error", duration = NULL)
          set_stage(NULL); NULL
        })
      if (is.null(sce)) return()

      set_stage("annotate", source = rv$proc_source)
      log_msg("Stage: annotating - extracting metadata...")
      run_next(function() {
        m <- tryCatch(extract_sce_metadata(sce), error = function(e) {
          log_msg("WARN: metadata extraction failed: ", conditionMessage(e))
          list()
        })
        set_stage("cache")
        log_msg("Stage: writing converted RDS to temp...")
        run_next(function() {
          saveRDS(sce, dest)
          meta_file <- paste0(dest, ".meta.json")
          jsonlite::write_json(m, meta_file,
                               pretty = TRUE, auto_unbox = TRUE)
          rv$sce_path <- dest
          rv$meta_path <- meta_file
          rv$meta <- m
          # Expose to the upload modal for pre-fill.
          if (exists("LAST_RESULTS", inherits = TRUE)) {
            LAST_RESULTS$convert_rds  <- dest
            LAST_RESULTS$convert_name <- f$name
          }
          set_stage("ready")
          log_msg("Ready: download the .rds below.")
          showNotification("Conversion complete.", type = "message")
        })
      })
    })
  })

  output$cv_pipeline_ui <- renderUI({
    render_pipeline(rv$proc_stage, rv$proc_source,
                    sce_ready_fallback = !is.null(rv$sce_path))
  })

  output$cv_log <- renderText({
    if (length(rv$log) == 0L) return("No activity yet.")
    paste(tail(rv$log, 80L), collapse = "\n")
  })

  output$cv_result_ui <- renderUI({
    if (is.null(rv$sce_path)) return(NULL)
    m <- rv$meta %||% list()
    summary_rows <- tagList(
      if (!is.null(m$n_cells)) tags$li(tags$strong("Cells: "),
        format(m$n_cells, big.mark = ",")),
      if (!is.null(m$n_features)) tags$li(tags$strong("Features: "),
        format(m$n_features, big.mark = ",")),
      if (length(m$assays))
        tags$li(tags$strong("Assays: "), paste(unlist(m$assays), collapse = ", ")),
      if (length(m$reducedDims))
        tags$li(tags$strong("Reduced dims: "),
          paste(unlist(m$reducedDims), collapse = ", ")),
      if (!is.null(m$n_cell_types))
        tags$li(tags$strong(m$cell_types_col, " types: "), m$n_cell_types)
    )
    tagList(
      h5("Converted dataset"),
      tags$ul(summary_rows),
      downloadButton("cv_dl_rds", "Download .rds",
                     class = "btn-primary"),
      downloadButton("cv_dl_meta", "Download metadata .json",
                     class = "btn-outline-secondary"),
      tags$p(class = "text-muted", style = "margin-top:8px;",
        tags$small("Download the .rds before closing — nothing is cached.
                    To use it, upload it via the curated index's session-upload
                    form, or commit it to data/datasets.yaml."))
    )
  })

  output$cv_dl_rds <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(rv$orig_name %||% "dataset")
      paste0(base, "_sce.rds")
    },
    content = function(file) {
      req(rv$sce_path)
      file.copy(rv$sce_path, file, overwrite = TRUE)
    }
  )
  output$cv_dl_meta <- downloadHandler(
    filename = function() {
      base <- tools::file_path_sans_ext(rv$orig_name %||% "dataset")
      paste0(base, "_meta.json")
    },
    content = function(file) {
      req(rv$meta_path)
      file.copy(rv$meta_path, file, overwrite = TRUE)
    }
  )

  invisible(NULL)
}
