options(shiny.autoload.r = FALSE)
options(shiny.maxRequestSize = 10 * 1024^3)  # 10 GB upload limit

library("iSEE")
library("iSEEindex")
library("BiocFileCache")
library("DT")
library("stringr")
library("rintrojs")
library("shinyjs")
library("urltools")
library("yaml")
library("jsonlite")
library("later")
library("shiny")
library("shinydashboard")

# ── Resolve app dir & source modules ──────────────────────────────────────────
APP_DIR <- tryCatch({
  this <- sys.frame(1)$ofile
  if (!is.null(this)) normalizePath(dirname(this), mustWork = FALSE) else getwd()
}, error = function(e) getwd())
if (!dir.exists(file.path(APP_DIR, "modules"))) APP_DIR <- getwd()

source(file.path(APP_DIR, "modules", "utils_convert.R"))
source(file.path(APP_DIR, "modules", "utils_layout_codegen.R"))
source(file.path(APP_DIR, "modules", "mod_branding.R"))
source(file.path(APP_DIR, "modules", "mod_pipeline_ui.R"))
source(file.path(APP_DIR, "modules", "mod_convert.R"))
source(file.path(APP_DIR, "modules", "mod_layout_builder.R"))
source(file.path(APP_DIR, "modules", "mod_upload.R"))

# ── iSEEindex setup (unchanged behaviour) ─────────────────────────────────────
bfc <- BiocFileCache(cache = tempdir())
upload_dir <- file.path("data", "uploads")
dir.create(upload_dir, showWarnings = FALSE, recursive = TRUE)

# Inline the shared-upload helpers from the local iSEEindex source so the
# app does not rely on the installed package version exposing them.
.load_shared_uploads <- function(base_list,
                                 upload_dir = file.path("data", "uploads")) {
  index_path <- file.path(upload_dir, "index.yaml")
  if (!file.exists(index_path)) return(base_list)
  index <- tryCatch(yaml::read_yaml(index_path), error = function(e) list())
  if (!length(index)) return(base_list)
  for (entry in index) {
    rds_path <- gsub("^runr://readRDS\\(['\"]|['\"]\\)$", "", entry$uri)
    if (!file.exists(rds_path)) next
    base_list <- c(base_list, list(list(
      id          = entry$id,
      title       = paste0(entry$title, " [shared]"),
      uri         = entry$uri,
      description = entry$description
    )))
  }
  base_list
}

.load_shared_upload_initial <- function(base_list,
                                        upload_dir = file.path("data", "uploads")) {
  index_path <- file.path(upload_dir, "index.yaml")
  if (!file.exists(index_path)) return(base_list)
  index <- tryCatch(yaml::read_yaml(index_path), error = function(e) list())
  if (!length(index)) return(base_list)
  for (entry in index) {
    layout_uri <- entry$layout_uri
    if (is.null(layout_uri) || is.na(layout_uri) || !nzchar(layout_uri)) next
    layout_path <- sub("^rcall://", "", layout_uri)
    if (!file.exists(layout_path)) next
    base_list <- c(base_list, list(list(
      id          = paste0(entry$id, "_layout"),
      datasets    = entry$id,
      title       = paste0(entry$title, " (shared layout)"),
      uri         = layout_uri,
      description = "Shared uploaded initial layout configuration."
    )))
  }
  base_list
}

dataset_fun <- function() {
  x <- yaml::read_yaml("data/datasets.yaml")
  .load_shared_uploads(x$datasets, upload_dir)
}
initial_fun <- function() {
  x <- yaml::read_yaml("data/datasets.yaml")
  .load_shared_upload_initial(x$initial, upload_dir)
}

header <- branding_header_ui()

# Build the iSEEindex shinyApp, then wrap its server to register our modal
# observers (Convert + Layout builder). All our IDs are prefixed cv_/lb_ to
# avoid clashing with iSEE's own inputs.
app <- iSEEindex(bfc, dataset_fun, initial_fun,
                 body.header = header,
                 show_upload_box = FALSE)

datasets_yaml_path <- file.path(APP_DIR, "data", "datasets.yaml")

# iSEE returns a shiny.appobj whose server is produced lazily via
# `serverFuncSource()`. Wrap that source so the produced server runs iSEE's
# logic AND registers our modal observers.
original_server_source <- app$serverFuncSource
app$serverFuncSource <- function() {
  inner <- original_server_source()
  function(input, output, session) {
    inner(input, output, session)
    convert_register(input, output, session)
    layout_register(input, output, session, datasets_yaml_path)
    upload_register(input, output, session)
  }
}

app
