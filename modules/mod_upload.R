# Upload-dataset modal. Re-uses the iSEEindex landing-page upload mechanism
# by rendering inputs with the same IDs as the inline form, so the existing
# `.create_upload_observers` (already attached by iSEEindex's landing page)
# handles add-for-session and commit-to-shared, including the live rerender
# of the dataset table — no F5 required.

# Input-ID constants matching the iSEEindex inline upload form, inlined so
# the module does not depend on package internals being exported. These
# strings must stay in sync with R/constants.R in the iSEEindex fork.
.UP_IDS <- list(
  sce_file    = "iSEEindex_INTERNAL_upload_sce_file",
  title       = "iSEEindex_INTERNAL_upload_title",
  description = "iSEEindex_INTERNAL_upload_description",
  layout_file = "iSEEindex_INTERNAL_upload_layout_file",
  add_btn     = "iSEEindex_INTERNAL_upload_add",
  commit_btn  = "iSEEindex_INTERNAL_upload_commit",
  status      = "iSEEindex_INTERNAL_upload_status"
)

# Cross-modal pre-fill state. Populated by the convert and layout modules
# whenever they finish producing a file; read by the upload modal on open.
LAST_RESULTS <- new.env(parent = emptyenv())
LAST_RESULTS$convert_rds  <- NULL  # path to most recent converted .rds
LAST_RESULTS$convert_name <- NULL  # original upload basename
LAST_RESULTS$layout_R     <- NULL  # path to most recent generated initial.R
LAST_RESULTS$layout_name  <- NULL  # dataset id used when generating

upload_modal_ui <- function() {
  modalDialog(
    title = tagList(icon("upload"), " Upload dataset"),
    size = "xl", easyClose = TRUE,
    footer = modalButton("Close"),

    tags$p(class = "text-muted",
      "Upload an SCE (.rds), optionally with an initial-layout R script.
       Click ", tags$strong("Add for this session"),
      " to use it now without persisting, or ",
      tags$strong("Commit to shared storage"),
      " to make it available on the curated landing page across sessions."),

    fluidRow(
      column(12,
        selectInput("up_prefill", "Pre-fill from",
          choices = c(
            "(none)"                      = "none",
            "Last conversion result"      = "convert",
            "Last layout-builder result"  = "layout",
            "Both convert + layout"       = "both"
          ),
          selected = "none", width = "100%"),
        tags$small(class = "text-muted",
          uiOutput("up_prefill_status"))
      )
    ),
    hr(),

    fluidRow(
      column(6,
        fileInput(.UP_IDS$sce_file,
          label = "SingleCellExperiment / SummarizedExperiment (.rds)",
          accept = c(".rds", ".RDS"),
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
    )
  )
}

upload_register <- function(input, output, session) {
  observeEvent(input$open_upload, {
    showModal(upload_modal_ui())
  })

  output$up_prefill_status <- renderUI({
    have_conv   <- !is.null(LAST_RESULTS$convert_rds) &&
                    file.exists(LAST_RESULTS$convert_rds)
    have_layout <- !is.null(LAST_RESULTS$layout_R) &&
                    file.exists(LAST_RESULTS$layout_R)
    bits <- c(
      if (have_conv)
        paste0("Converted RDS available: ",
               basename(LAST_RESULTS$convert_rds)),
      if (have_layout)
        paste0("Layout script available: ",
               basename(LAST_RESULTS$layout_R)),
      if (!have_conv && !have_layout)
        "No prior conversion or layout this session."
    )
    HTML(paste(bits, collapse = "<br>"))
  })

  # When the user picks a pre-fill source, copy file paths into the fileInput
  # state. Shiny's fileInput cannot be programmatically populated with a path,
  # so we instead surface the path to the iSEEindex upload helpers by writing
  # a synthetic input value via session$sendInputMessage. iSEEindex's helpers
  # read input[[.ui_upload_sce_file]] expecting a fileInput-shaped list with
  # `name` and `datapath`; we mimic that shape.
  observeEvent(input$up_prefill, {
    sel <- input$up_prefill
    if (is.null(sel) || sel == "none") return()

    set_file_input <- function(input_id, path) {
      if (is.null(path) || !file.exists(path)) return()
      session$sendInputMessage(input_id, list(
        name     = basename(path),
        size     = file.info(path)$size,
        type     = "",
        datapath = path
      ))
    }

    if (sel %in% c("convert", "both")) {
      set_file_input(.UP_IDS$sce_file, LAST_RESULTS$convert_rds)
      if (!is.null(LAST_RESULTS$convert_name)) {
        updateTextInput(session, .UP_IDS$title,
          value = tools::file_path_sans_ext(LAST_RESULTS$convert_name))
      }
    }
    if (sel %in% c("layout", "both")) {
      set_file_input(.UP_IDS$layout_file, LAST_RESULTS$layout_R)
    }
  }, ignoreInit = TRUE)

  invisible(NULL)
}
