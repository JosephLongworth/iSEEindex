# Layout-builder workflow as a modal dialog with directly-prefixed IDs (`lb_`).

layout_modal_ui <- function() {
  modalDialog(
    title = tagList(icon("sliders"), " Build initial.R for iSEEindex"),
    size = "xl", easyClose = TRUE,
    footer = modalButton("Close"),

    tags$p(class = "text-muted",
      "Generate a full ", tags$code("initial.R"),
      " script for an iSEEindex configuration. Pick an SCE, choose panels,
       download the script, then reference it in ",
      tags$code("data/datasets.yaml"), "."),

    fluidRow(
      column(5,
        h5("1. Choose data source"),
        radioButtons("lb_src_type", NULL,
          choices = c(
            "Curated dataset (datasets.yaml)" = "curated",
            "Upload SCE .rds (session)"      = "sce",
            "Upload + convert (.h5ad / Seurat .rds)" = "convert"
          ),
          selected = "curated"),
        uiOutput("lb_src_input_ui"),
        actionButton("lb_load", "Load dataset",
                     class = "btn-primary", icon = icon("download")),
        tags$div(style = "margin-top:8px;",
                 textOutput("lb_load_status", inline = TRUE))
      ),
      column(7,
        h5("2. Inspect"),
        uiOutput("lb_inspect_ui")
      )
    ),
    hr(),
    uiOutput("lb_panels_ui"),
    hr(),
    uiOutput("lb_download_ui")
  )
}

layout_register <- function(input, output, session, datasets_yaml_path) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

  rv <- reactiveValues(
    sce = NULL,
    meta = NULL,
    dataset_id = NULL,
    load_msg = ""
  )

  observeEvent(input$open_layout, {
    showModal(layout_modal_ui())
  })

  curated_entries <- reactive({
    if (!file.exists(datasets_yaml_path)) return(list())
    y <- tryCatch(yaml::read_yaml(datasets_yaml_path),
                  error = function(e) list())
    y$datasets %||% list()
  })

  output$lb_src_input_ui <- renderUI({
    req(input$lb_src_type)
    switch(input$lb_src_type,
      curated = {
        entries <- curated_entries()
        choices <- setNames(
          vapply(entries, function(e) e$id %||% "", character(1)),
          vapply(entries, function(e) {
            paste0(e$title %||% e$id %||% "(unnamed)",
                   " [", e$id %||% "?", "]")
          }, character(1))
        )
        choices <- choices[nzchar(choices)]
        if (!length(choices)) {
          return(tags$em("No datasets found in datasets.yaml."))
        }
        selectInput("lb_curated_id", "Curated dataset", choices = choices)
      },
      sce = fileInput("lb_sce_file", "SCE .rds",
                      accept = c(".rds", ".RDS"),
                      placeholder = ".rds containing SCE"),
      convert = fileInput("lb_conv_file",
                          "H5AD / Seurat / SCE file",
                          accept = c(".h5ad", ".rds", ".RDS"),
                          placeholder = ".h5ad or .rds")
    )
  })

  output$lb_load_status <- renderText({ rv$load_msg })

  observeEvent(input$lb_load, {
    rv$sce <- NULL; rv$meta <- NULL; rv$dataset_id <- NULL
    rv$load_msg <- "Loading..."

    tryCatch({
      if (input$lb_src_type == "curated") {
        req(input$lb_curated_id)
        entries <- curated_entries()
        entry <- Filter(function(e) identical(e$id, input$lb_curated_id),
                        entries)[[1]]
        uri <- entry$uri
        if (is.null(uri)) stop("Selected entry has no URI.")
        local_path <- .resolve_curated_uri(uri)
        rv$sce <- readRDS(local_path)
        rv$dataset_id <- entry$id
      } else if (input$lb_src_type == "sce") {
        req(input$lb_sce_file)
        obj <- readRDS(input$lb_sce_file$datapath)
        if (!inherits(obj, "SingleCellExperiment")) {
          stop("RDS does not contain a SingleCellExperiment.")
        }
        rv$sce <- obj
        rv$dataset_id <- tools::file_path_sans_ext(input$lb_sce_file$name)
      } else {
        req(input$lb_conv_file)
        ext <- tolower(tools::file_ext(input$lb_conv_file$name))
        rv$sce <- convert_to_sce(input$lb_conv_file$datapath, ext)
        rv$dataset_id <- tools::file_path_sans_ext(input$lb_conv_file$name)
      }
      rv$meta <- extract_sce_metadata(rv$sce)
      rv$load_msg <- paste0("Loaded: ", rv$dataset_id, " (",
        format(rv$meta$n_cells, big.mark = ","), " cells × ",
        format(rv$meta$n_features, big.mark = ","), " features)")
    }, error = function(e) {
      rv$load_msg <- paste0("ERROR: ", conditionMessage(e))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })

  output$lb_inspect_ui <- renderUI({
    m <- rv$meta
    if (is.null(m)) return(tags$em("No dataset loaded yet."))
    tags$ul(
      tags$li(tags$strong("Cells: "), format(m$n_cells, big.mark = ",")),
      tags$li(tags$strong("Features: "), format(m$n_features, big.mark = ",")),
      if (length(m$assays))
        tags$li(tags$strong("Assays: "),
          paste(unlist(m$assays), collapse = ", ")),
      if (length(m$reducedDims))
        tags$li(tags$strong("Reduced dims: "),
          paste(unlist(m$reducedDims), collapse = ", ")),
      if (length(m$colData_names))
        tags$li(tags$strong("colData: "),
          paste(unlist(m$colData_names), collapse = ", "))
    )
  })

  output$lb_panels_ui <- renderUI({
    m <- rv$meta
    if (is.null(m)) return(NULL)
    rd_choices <- unlist(m$reducedDims) %||% character(0)
    cd_choices <- unlist(m$colData_names) %||% character(0)
    if (!length(rd_choices) && !length(cd_choices)) {
      return(tags$em("This SCE has no reducedDims and no colData — no panels can be built."))
    }
    ct_col <- m$cell_types_col
    default_colour <- if (!is.null(ct_col) && ct_col %in% cd_choices) ct_col
                      else if (length(cd_choices)) cd_choices[1L] else NULL
    default_rdim <- if (length(rd_choices)) rd_choices[1L] else NULL
    first_gene <- if (nrow(rv$sce) > 0L) rownames(rv$sce)[1L] else ""

    tagList(
      h5("3. Configure panels"),
      fluidRow(
        column(3,
          selectInput("lb_rdim", "Reduced dim",
                      choices = rd_choices, selected = default_rdim)),
        column(3,
          selectInput("lb_umap_colour", "UMAP colour (metadata)",
                      choices = cd_choices, selected = default_colour)),
        column(3,
          selectInput("lb_fap_x", "Feature plot X",
                      choices = c("None", cd_choices), selected = "None")),
        column(3,
          selectInput("lb_fap_colour", "Feature plot colour",
                      choices = cd_choices, selected = default_colour))
      ),
      fluidRow(
        column(6,
          textInput("lb_gene", "Initial gene", value = first_gene)),
        column(6,
          checkboxGroupInput("lb_include_panels", "Include panels",
            choices = c(
              "ReducedDimensionPlot (by metadata)" = "rdp_meta",
              "FeatureAssayPlot"                   = "fap",
              "RowDataTable (gene selector)"       = "rdt",
              "ReducedDimensionPlot (by gene)"     = "rdp_gene",
              "ColumnDataTable"                    = "cdt"
            ),
            selected = c("rdp_meta", "fap", "rdt", "rdp_gene"))
        )
      )
    )
  })

  panels_spec <- reactive({
    req(rv$sce, rv$meta, input$lb_include_panels)
    rdim <- input$lb_rdim
    gene <- if (nzchar(input$lb_gene %||% "")) input$lb_gene
            else if (nrow(rv$sce) > 0L) rownames(rv$sce)[1L] else ""
    out <- list()
    if ("rdp_meta" %in% input$lb_include_panels)
      out <- c(out, list(panel_reduced_dim_by_metadata(rdim, input$lb_umap_colour)))
    if ("fap" %in% input$lb_include_panels)
      out <- c(out, list(panel_feature_assay(gene, input$lb_fap_x, input$lb_fap_colour)))
    if ("rdt" %in% input$lb_include_panels)
      out <- c(out, list(panel_row_data_table(gene)))
    if ("rdp_gene" %in% input$lb_include_panels)
      out <- c(out, list(panel_reduced_dim_by_gene(rdim, gene = gene)))
    if ("cdt" %in% input$lb_include_panels)
      out <- c(out, list(panel_column_data_table()))
    out
  })

  output$lb_download_ui <- renderUI({
    if (is.null(rv$sce)) return(NULL)
    tagList(
      h5("4. Preview & download"),
      tags$pre(
        style = "max-height: 280px; overflow:auto; background:#f5f5f5; padding:8px;",
        textOutput("lb_script_preview")
      ),
      downloadButton("lb_dl_initial", "Download initial.R",
                     class = "btn-primary")
    )
  })

  script_text <- reactive({
    ps <- panels_spec()
    if (!length(ps)) return("# No panels selected.\n")
    build_initial_script(ps,
      dataset_id = rv$dataset_id,
      generated_for = paste0("iSEEindex (", rv$dataset_id %||% "?", ")"))
  })

  output$lb_script_preview <- renderText({ script_text() })

  output$lb_dl_initial <- downloadHandler(
    filename = function() {
      base <- gsub("[^A-Za-z0-9_]+", "_", rv$dataset_id %||% "dataset")
      paste0(base, "_initial.R")
    },
    content = function(file) {
      writeLines(script_text(), file)
      # Also persist a copy to tempdir so the upload modal can pre-fill from it.
      if (exists("LAST_RESULTS", inherits = TRUE)) {
        keep <- tempfile(pattern = paste0(
          gsub("[^A-Za-z0-9_]+", "_", rv$dataset_id %||% "dataset"),
          "_initial_"), fileext = ".R")
        writeLines(script_text(), keep)
        LAST_RESULTS$layout_R    <- keep
        LAST_RESULTS$layout_name <- rv$dataset_id
      }
    }
  )

  invisible(NULL)
}

# Resolve a curated dataset URI to a local readable path.
.resolve_curated_uri <- function(uri) {
  if (grepl("^localhost://", uri)) {
    p <- sub("^localhost://", "", uri)
    if (substr(p, 1, 1) != "/" && substr(p, 2, 2) != ":") {
      p <- file.path(getwd(), p)
    }
    return(normalizePath(p, mustWork = TRUE))
  }
  if (grepl("^https?://", uri)) {
    dest <- tempfile(fileext = ".rds")
    utils::download.file(uri, dest, mode = "wb", quiet = TRUE)
    return(dest)
  }
  stop("Unsupported URI scheme for layout builder: ", uri)
}
