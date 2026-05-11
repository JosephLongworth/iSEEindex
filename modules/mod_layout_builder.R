# Layout-builder modal: prepares an LLM "skill" prompt (.md) that the user
# feeds to their LLM of choice. The LLM, given the skill, produces an
# `initial.R` that the user uploads to the app separately.
#
# The skill bundles:
#   1. A task description for the LLM (what initial.R is for iSEE / iSEEindex,
#      what panels are available, the slot conventions).
#   2. A "house-style" recommendation for default layouts (UMAP-by-celltype,
#      UMAP-by-condition, gene table, gene-coloured UMAP, violin) so the LLM
#      makes sensible choices.
#   3. A concrete dump of the selected dataset's structure: assays,
#      reducedDims, colData columns with example values, n_cells, n_features.
#   4. Conversation guidance so the LLM asks the user when a variable is
#      ambiguous but otherwise commits to its best guess.

layout_modal_ui <- function() {
  modalDialog(
    title = tagList(icon("wand-magic-sparkles"), " Prepare LLM skill for initial.R"),
    size = "xl", easyClose = TRUE,
    footer = modalButton("Close"),

    # Client-side handler for the post-commit page reload. The reload is what
    # makes the freshly written initial.R appear in the landing-page table
    # and dropdown - pObjects is built once per session, so we re-init.
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('lb_schedule_reload', function(msg) {",
      "  setTimeout(function(){ window.location.reload(); },",
      "             (msg && msg.delay_ms) || 2000);",
      "});"
    )),

    tags$div(class = "alert alert-warning",
      style = "margin-bottom:12px;",
      tags$strong(icon("triangle-exclamation"), " Experimental: "),
      "this layout builder hands the dataset structure to an LLM and trusts ",
      "the model to write a valid ", tags$code("initial.R"), ". Output ",
      "quality varies between models, prompts, and runs - always review the ",
      "generated file before committing, and expect to iterate."),

    tags$p(class = "text-muted",
      "Pick a dataset, download the ", tags$code(".md"), " skill file, ",
      "feed it to the LLM of your choice (Claude, ChatGPT, ...) to get an ",
      tags$code("initial.R"),
      ", then upload that file below to commit it as a shared layout for ",
      "the selected dataset."),

    fluidRow(
      column(5,
        h5("1. Pick a dataset"),
        uiOutput("lb_curated_select_ui"),
        actionButton("lb_load", "Inspect dataset",
                     class = "btn-primary", icon = icon("magnifying-glass")),
        tags$div(style = "margin-top:8px;",
                 textOutput("lb_load_status", inline = TRUE))
      ),
      column(7,
        h5("2. Dataset structure"),
        uiOutput("lb_inspect_ui")
      )
    ),
    hr(),
    uiOutput("lb_download_ui"),
    hr(),
    uiOutput("lb_commit_ui")
  )
}

layout_register <- function(input, output, session, datasets_yaml_path) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

  rv <- reactiveValues(
    sce = NULL,
    meta = NULL,
    dataset_id = NULL,
    dataset_title = NULL,
    load_msg = ""
  )

  observeEvent(input$open_layout, {
    showModal(layout_modal_ui())
  })

  # Entries from datasets.yaml, merged with any shared uploads written to
  # data/uploads/index.yaml (mirrors how dataset_fun assembles the landing
  # page list, so the skill builder offers the same datasets a user can
  # actually view in iSEE).
  curated_entries <- reactive({
    base <- list()
    if (file.exists(datasets_yaml_path)) {
      y <- tryCatch(yaml::read_yaml(datasets_yaml_path),
                    error = function(e) list())
      base <- y$datasets %||% list()
    }
    # Append shared uploads (data/uploads/index.yaml) - same logic as the
    # in-app .load_shared_uploads helper, kept local to avoid circular sourcing.
    idx <- file.path("data", "uploads", "index.yaml")
    if (file.exists(idx)) {
      shared <- tryCatch(yaml::read_yaml(idx), error = function(e) list())
      for (entry in shared) {
        rds_path <- gsub("^runr://readRDS\\(['\"]|['\"]\\)$", "", entry$uri)
        if (file.exists(rds_path)) {
          base <- c(base, list(list(
            id          = entry$id,
            title       = paste0(entry$title, " [shared]"),
            uri         = entry$uri,
            description = entry$description
          )))
        }
      }
    }
    base
  })

  output$lb_curated_select_ui <- renderUI({
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
      return(tags$em("No datasets found in datasets.yaml or shared uploads."))
    }
    selectInput("lb_curated_id", "Dataset", choices = choices,
                width = "100%")
  })

  output$lb_load_status <- renderText({ rv$load_msg })

  observeEvent(input$lb_load, {
    rv$sce <- NULL; rv$meta <- NULL
    rv$dataset_id <- NULL; rv$dataset_title <- NULL
    rv$load_msg <- "Loading..."

    tryCatch({
      req(input$lb_curated_id)
      entries <- curated_entries()
      entry <- Filter(function(e) identical(e$id, input$lb_curated_id),
                      entries)[[1]]
      uri <- entry$uri
      if (is.null(uri)) stop("Selected entry has no URI.")
      local_path <- .resolve_curated_uri(uri)
      sce <- readRDS(local_path)
      if (!inherits(sce, "SingleCellExperiment")) {
        stop("Dataset at ", local_path,
             " is not a SingleCellExperiment (class: ",
             paste(class(sce), collapse = "/"), ").")
      }
      rv$sce <- sce
      rv$dataset_id <- entry$id
      rv$dataset_title <- entry$title %||% entry$id
      rv$meta <- extract_sce_metadata(sce)
      rv$load_msg <- paste0(
        "Loaded: ", rv$dataset_title, " (",
        format(rv$meta$n_cells, big.mark = ","), " cells x ",
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
        tags$li(tags$strong("colData columns: "),
          paste(unlist(m$colData_names), collapse = ", "))
    )
  })

  skill_text <- reactive({
    req(rv$sce, rv$meta, rv$dataset_id)
    build_skill_md(rv$sce, rv$meta,
                   dataset_id    = rv$dataset_id,
                   dataset_title = rv$dataset_title)
  })

  output$lb_download_ui <- renderUI({
    if (is.null(rv$sce)) return(NULL)
    tagList(
      h5("3. Download the LLM skill"),
      tags$p(class = "text-muted",
        "Give this ", tags$code(".md"), " file to your LLM. It contains ",
        "the task description, iSEE panel reference, and the structure of ",
        "your dataset. Ask the LLM to produce an ",
        tags$code("initial.R"), "."),
      tags$pre(
        style = "max-height: 280px; overflow:auto; background:#f5f5f5; padding:8px; font-size:11px;",
        textOutput("lb_skill_preview")
      ),
      downloadButton("lb_dl_skill", "Download skill (.md)",
                     class = "btn-primary")
    )
  })

  output$lb_skill_preview <- renderText({
    txt <- skill_text()
    # Cap preview to keep the modal responsive on large datasets.
    if (nchar(txt) > 6000) {
      paste0(substr(txt, 1, 6000),
             "\n\n... [", nchar(txt) - 6000,
             " more characters in the downloaded file]")
    } else {
      txt
    }
  })

  output$lb_dl_skill <- downloadHandler(
    filename = function() {
      base <- gsub("[^A-Za-z0-9_]+", "_", rv$dataset_id %||% "dataset")
      paste0(base, "_iSEE_initial_skill.md")
    },
    content = function(file) {
      writeLines(skill_text(), file)
    }
  )

  # --- Step 4: upload + commit the LLM-produced initial.R --------------------
  output$lb_commit_ui <- renderUI({
    if (is.null(rv$sce)) return(NULL)
    tagList(
      h5("4. Upload and commit the produced initial.R"),
      tags$p(class = "text-muted",
        "Once the LLM has produced an ",
        tags$code("initial.R"),
        ", upload it here and commit it. Committing writes the file to ",
        "shared storage and registers it against the selected dataset so it ",
        "appears in the layout dropdown on the landing page for everyone on ",
        "the next page load."),
      fluidRow(
        column(7,
          fileInput("lb_initial_file",
            label = "initial.R produced by the LLM",
            accept = c(".r", ".R"), multiple = FALSE)
        ),
        column(5,
          textInput("lb_layout_title", "Layout title (shown in dropdown)",
                    placeholder = "e.g. UMAP + gene table")
        )
      ),
      textAreaInput("lb_layout_desc", "Description (optional)", rows = 2L,
                    placeholder = "Short summary of what this layout shows.",
                    width = "100%"),
      actionButton("lb_commit", "Commit layout to shared storage",
        icon = icon("share-from-square"),
        style = "color:#ffffff; background-color:#6B1435; border-color:#4a0e25;"),
      tags$div(style = "margin-top:10px;",
        uiOutput("lb_commit_status"))
    )
  })

  rv_commit <- reactiveValues(msg_ui = NULL)
  output$lb_commit_status <- renderUI({ rv_commit$msg_ui })

  observeEvent(input$lb_commit, {
    rv_commit$msg_ui <- NULL
    f <- input$lb_initial_file
    if (is.null(f)) {
      rv_commit$msg_ui <- tags$p(style = "color:#D64D38; font-weight:bold;",
        "Pick an initial.R file before committing.")
      return()
    }
    if (is.null(rv$dataset_id)) {
      rv_commit$msg_ui <- tags$p(style = "color:#D64D38; font-weight:bold;",
        "Inspect a dataset first.")
      return()
    }
    title_val <- trimws(input$lb_layout_title %||% "")
    if (!nzchar(title_val)) {
      title_val <- paste0(rv$dataset_title %||% rv$dataset_id, " layout")
    }
    desc_val <- trimws(input$lb_layout_desc %||% "")
    if (!nzchar(desc_val)) {
      desc_val <- sprintf("Shared layout for %s.", rv$dataset_id)
    }

    res <- tryCatch(
      .commit_layout(
        dataset_id = rv$dataset_id,
        initial_src_path = f$datapath,
        title = title_val,
        description = desc_val,
        datasets_yaml_path = datasets_yaml_path),
      error = function(e) list(error = conditionMessage(e)))

    if (!is.null(res$error)) {
      rv_commit$msg_ui <- tags$p(style = "color:#D64D38; font-weight:bold;",
        paste0("ERROR: ", res$error))
    } else {
      rv_commit$msg_ui <- tagList(
        tags$p(style = "color:#4B7EA3; font-weight:bold;", res$message),
        tags$p(class = "text-muted", tags$small(
          "Reloading the page in 2 seconds so the new layout appears in",
          " the dropdown..."))
      )
      showNotification(res$message, type = "message", duration = 8)
      # The landing-page Available Data Sets table and initial-config
      # dropdown are built from pObjects at session start; the only reliable
      # way to surface a freshly committed entry is a session reload.
      session$sendCustomMessage("lb_schedule_reload", list(delay_ms = 2000))
    }
  })

  invisible(NULL)
}

# Persist a user-uploaded initial.R as a shared layout for `dataset_id`.
#
# - If `dataset_id` matches an entry in `data/uploads/index.yaml` (a shared
#   uploaded dataset), updates that entry's `layout_uri` so the landing page
#   picks it up via `.load_shared_upload_initial`.
# - Otherwise (curated dataset from `datasets.yaml`), appends a new entry to
#   the `initial:` section of `datasets.yaml` scoped to `dataset_id`.
#
# Files are written under `data/uploads/layouts/`. Returns list(message=...)
# on success or list(error=...) on failure (callers translate to UI text).
.commit_layout <- function(dataset_id, initial_src_path, title, description,
                           datasets_yaml_path) {
  if (!file.exists(initial_src_path)) {
    return(list(error = "Uploaded initial.R was not found on disk."))
  }
  # Quick sanity check: the file must define `initial`. iSEEindex sources the
  # file and reads `initial` by name - catching this here gives a much better
  # error than the cryptic launch-time failure. Parent the probe env to the
  # global env so attached packages (iSEE, etc.) are visible, and make sure
  # iSEE is loaded in case the app process hasn't attached it yet.
  if (!requireNamespace("iSEE", quietly = TRUE)) {
    return(list(error = "iSEE package is not installed."))
  }
  suppressPackageStartupMessages(requireNamespace("iSEE"))
  if (!"package:iSEE" %in% search()) {
    suppressPackageStartupMessages(
      attachNamespace("iSEE"))
  }
  probe_env <- new.env(parent = globalenv())
  src_ok <- tryCatch({
    source(initial_src_path, local = probe_env)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(src_ok)) {
    return(list(error = paste0(
      "initial.R failed to source: ", src_ok)))
  }
  if (!exists("initial", envir = probe_env, inherits = FALSE)) {
    return(list(error = paste0(
      "initial.R did not define an object called `initial`. The script must ",
      "assign `initial <- list(...)`.")))
  }

  layout_dir <- file.path("data", "uploads", "layouts")
  dir.create(layout_dir, showWarnings = FALSE, recursive = TRUE)

  layout_id <- paste0(dataset_id, "_layout_",
                      format(Sys.time(), "%Y%m%d%H%M%S"))
  dest <- file.path(layout_dir, paste0(layout_id, ".R"))
  ok <- file.copy(initial_src_path, dest, overwrite = TRUE)
  if (!isTRUE(ok)) {
    return(list(error = "Failed to copy initial.R into shared layout dir."))
  }
  abs_dest <- normalizePath(dest, winslash = "/")
  # Use localhost:// not rcall:// : the rcall scheme parses+evaluates whatever
  # follows the prefix as R code, which mangles Windows paths (l:/... fails
  # with "unexpected '/'"). The localhost scheme just strips the prefix and
  # passes the remainder to file.exists / BiocFileCache. Use the absolute
  # form "localhost:///abs/path" so it isn't treated as relative.
  layout_uri <- paste0("localhost://", abs_dest)

  # Branch on whether the dataset is a shared upload (set its layout_uri on
  # its index.yaml entry) or a curated dataset (append to datasets.yaml's
  # `initial:` list).
  shared_index_path <- file.path("data", "uploads", "index.yaml")
  shared_index <- if (file.exists(shared_index_path)) {
    tryCatch(yaml::read_yaml(shared_index_path), error = function(e) list())
  } else list()
  shared_ids <- vapply(shared_index, function(e) e$id %||% "",
                       character(1))

  if (dataset_id %in% shared_ids) {
    for (i in seq_along(shared_index)) {
      if (identical(shared_index[[i]]$id, dataset_id)) {
        shared_index[[i]]$layout_uri <- layout_uri
        break
      }
    }
    yaml::write_yaml(shared_index, shared_index_path)
    return(list(message = sprintf(
      "Layout committed for shared dataset '%s'.", dataset_id)))
  }

  # Curated dataset path: append to datasets.yaml's `initial:` list.
  if (!file.exists(datasets_yaml_path)) {
    return(list(error = sprintf("datasets.yaml not found at %s",
                                datasets_yaml_path)))
  }
  dy <- tryCatch(yaml::read_yaml(datasets_yaml_path),
                 error = function(e) NULL)
  if (is.null(dy)) {
    return(list(error = "Could not parse datasets.yaml."))
  }
  if (is.null(dy$initial)) dy$initial <- list()

  new_entry <- list(
    id          = layout_id,
    datasets    = list(dataset_id),
    title       = title,
    uri         = layout_uri,
    description = description
  )
  dy$initial[[length(dy$initial) + 1L]] <- new_entry
  yaml::write_yaml(dy, datasets_yaml_path)
  list(message = sprintf(
    "Layout '%s' committed for curated dataset '%s'.", title, dataset_id))
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
  if (grepl("^runr://readRDS\\(", uri)) {
    p <- gsub("^runr://readRDS\\(['\"]|['\"]\\)$", "", uri)
    return(normalizePath(p, mustWork = TRUE))
  }
  if (grepl("^https?://", uri)) {
    dest <- tempfile(fileext = ".rds")
    utils::download.file(uri, dest, mode = "wb", quiet = TRUE)
    return(dest)
  }
  stop("Unsupported URI scheme for layout builder: ", uri)
}
