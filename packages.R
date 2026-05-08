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
    "zellkonverter",
    "Seurat"
), ask = FALSE, update = FALSE)

BiocManager::install("iSEE/iSEEindex", ask = FALSE)
