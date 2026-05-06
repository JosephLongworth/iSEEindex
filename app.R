options(shiny.autoload.r = FALSE)

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

dataset_fun <- function() {
  x <- yaml::read_yaml("data/datasets.yaml")
  x$datasets
}

initial_fun <- function() {
  x <- yaml::read_yaml("data/datasets.yaml")
  x$initial
}

header <- tagList(
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
  fluidRow(
    box(width = 12L,
      column(width = 10,
        div(class = "dii-header",
          p(class = "dii-header-title",
            "Department of Infection and Immunity — Interactive Data Explorer"),
          p(class = "dii-header-subtitle",
            "Select a dataset below to explore it interactively with iSEE.")
        )
      ),
      column(width = 2,
        img(src = "logo.png", height = "80px", style = "float:right; padding-top:8px;")
      )
    )
  )
)

iSEEindex(bfc, dataset_fun, initial_fun, body.header = header)
