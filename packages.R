if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c(
    "SummarizedExperiment",
    "SingleCellExperiment",
    "BiocFileCache",
    "iSEE",
    "DT",
    "rintrojs",
    "shiny",
    "shinydashboard",
    "shinyjs",
    "stringr",
    "urltools",
    "paws.storage",
    "yaml",
    "jsonlite",
    "later",
    "Seurat"
), ask = FALSE, update = FALSE)

# anndataR: pure-R reader for .h5ad, replaces zellkonverter/Python path.
# Installed from Bioconductor (more robust mirror coverage than r-universe).
# Fail loudly if missing post-install - BiocManager::install only warns, which
# would otherwise silently produce an image that errors at first h5ad upload.
BiocManager::install("anndataR", ask = FALSE, update = FALSE)
if (!requireNamespace("anndataR", quietly = TRUE)) {
    stop("anndataR failed to install from Bioconductor.")
}

BiocManager::install("JosephLongworth/iSEEindex@merge-with-converter", ask = FALSE, force = TRUE)
if (!requireNamespace("iSEEindex", quietly = TRUE)) {
    stop("iSEEindex failed to install from the merge-with-converter branch.")
}
