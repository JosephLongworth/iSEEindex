# Conversion helpers shared by mod_convert and mod_layout_builder.
# Loads .h5ad / Seurat .rds / SCE .rds and returns a SingleCellExperiment.

extract_sce_metadata <- function(sce) {
  suppressPackageStartupMessages(library(SingleCellExperiment))
  info <- list(
    n_cells = ncol(sce),
    n_features = nrow(sce),
    assays = assayNames(sce),
    reducedDims = reducedDimNames(sce),
    colData_names = names(colData(sce))
  )
  cd <- as.data.frame(colData(sce))
  ct_cols <- grep(
    "cell.?type|cluster|leiden|louvain|annotation|label|celltype",
    names(cd), ignore.case = TRUE, value = TRUE
  )
  if (length(ct_cols)) {
    col <- ct_cols[1]
    vals <- sort(unique(as.character(cd[[col]])))
    info$cell_types_col <- col
    info$cell_types <- head(vals, 30)
    info$n_cell_types <- length(vals)
  }
  info
}

# Convert an uploaded file to an SCE. `ext` is one of "h5ad", "rds".
# `on_stage(stage, source)` is an optional callback for pipeline UI updates.
convert_to_sce <- function(datapath, ext, on_stage = function(stage, source = NULL) {}) {
  ext <- tolower(ext)
  if (ext == "h5ad") {
    on_stage("load", "h5ad")
    if (!requireNamespace("anndataR", quietly = TRUE)) {
      stop("anndataR not installed. Run: install.packages('anndataR', repos = c('https://scverse.r-universe.dev', 'https://cloud.r-project.org'))")
    }
    on_stage("convert", "h5ad")
    sce <- suppressPackageStartupMessages(
      anndataR::read_h5ad(datapath, as = "SingleCellExperiment")
    )
    return(sce)
  }
  if (ext == "rds") {
    on_stage("load", "rds")
    obj <- readRDS(datapath)
    if (inherits(obj, "Seurat")) {
      on_stage("convert", "seurat")
      if (!requireNamespace("Seurat", quietly = TRUE)) {
        stop("Seurat not installed. Run: install.packages('Seurat')")
      }
      sce <- suppressPackageStartupMessages(Seurat::as.SingleCellExperiment(obj))
      return(sce)
    }
    if (inherits(obj, "SingleCellExperiment")) {
      on_stage("convert", "rds")
      return(obj)
    }
    stop("RDS does not contain a Seurat or SingleCellExperiment object (class: ",
         paste(class(obj), collapse = "/"), ")")
  }
  stop("Unsupported file extension: ", ext)
}
