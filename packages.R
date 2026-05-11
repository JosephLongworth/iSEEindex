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
install.packages("anndataR",
    repos = c("https://scverse.r-universe.dev", "https://cloud.r-project.org"))

BiocManager::install("JosephLongworth/iSEEindex@merge-with-converter", ask = FALSE)
