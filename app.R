options(shiny.autoload.r = FALSE)
options(shiny.maxRequestSize = 5 * 1024^3)  # 5 GB upload limit

library("iSEE")
library("iSEEindex")
library("BiocFileCache")
library("DT")
library("stringr")
library("rintrojs")
library("shinyjs")
library("urltools")
library("yaml")
library("shiny")
library("shinydashboard")

bfc <- BiocFileCache(cache = tempdir())

upload_dir <- file.path("data", "uploads")
dir.create(upload_dir, showWarnings = FALSE, recursive = TRUE)

dataset_fun <- function() {
  x <- yaml::read_yaml("data/datasets.yaml")
  .load_shared_uploads(x$datasets, upload_dir)
}

initial_fun <- function() {
  x <- yaml::read_yaml("data/datasets.yaml")
  .load_shared_upload_initial(x$initial, upload_dir)
}

header <- tagList(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = paste0("dii_brand.css?v=", as.integer(Sys.time()))
    )
  ),
  div(class = "dii-banner",
    div(class = "dii-banner-logos",
      img(src = "DII_logo_vertical_neg.png", class = "logo-dii", alt = "DII"),
      img(src = "Fox_hex.png",                class = "logo-fox", alt = "EMI")
    ),
    div(class = "dii-banner-text",
      p(class = "dii-banner-title",
        "iSEEindex"),
      p(class = "dii-banner-subtitle",
        "Select a dataset below to explore it interactively with iSEE.")
    ),
    div(class = "dii-attribution",
      div(class = "dev-name",   "Dr. Joseph Longworth"),
      div(class = "dept-line",  "Experimental Molecular Immunology"),
      div(class = "dept-line",  "Dept. of Infection & Immunity · LIH")
    )
  )
)

iSEEindex(bfc, dataset_fun, initial_fun, body.header = header)
