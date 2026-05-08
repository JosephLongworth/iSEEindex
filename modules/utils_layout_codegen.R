# Generate a full initial.R script for iSEEindex from a list of panel choices.
# The output is a complete, runnable R file that returns list(<panels>) when
# sourced — i.e. the same shape iSEEindex's `initial_fun` consumes.

# Render a single value as R source.
.r_lit <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.numeric(x)) return(format(x))
  paste0('"', gsub('"', '\\\\"', as.character(x)), '"')
}

# Render `Panel(arg = value, ...)`.
.r_call <- function(fn, args) {
  parts <- character(0)
  for (k in names(args)) {
    v <- args[[k]]
    if (is.null(v)) next
    parts <- c(parts, paste0("    ", k, " = ", .r_lit(v)))
  }
  paste0(fn, "(\n", paste(parts, collapse = ",\n"), "\n  )")
}

# Build the panel list given panel specs (a list of list(type=..., args=...)).
build_initial_script <- function(panels, dataset_id = NULL,
                                 generated_for = NULL) {
  stopifnot(is.list(panels), length(panels) >= 1L)

  panel_calls <- vapply(panels, function(p) {
    .r_call(p$type, p$args)
  }, character(1))

  body <- paste0("  ", panel_calls, collapse = ",\n")

  header <- c(
    "# initial.R --- iSEE panel layout",
    "#",
    if (!is.null(dataset_id))
      paste0("# Dataset id : ", dataset_id) else NULL,
    if (!is.null(generated_for))
      paste0("# Generated  : ", generated_for) else NULL,
    paste0("# Created    : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "#",
    "# This file is consumed by iSEEindex via an `rcall://` or `https://` /",
    "# `localhost://` URI. Sourcing the file MUST return a list of iSEE Panel",
    "# objects (the value of the final expression).",
    "",
    "library(iSEE)",
    ""
  )

  paste0(
    paste(header, collapse = "\n"),
    "list(\n",
    body,
    "\n)\n"
  )
}

# Helpers that build panel specs from a few high-level choices.
panel_reduced_dim_by_metadata <- function(rdim, colour) {
  list(type = "ReducedDimensionPlot", args = list(
    Type = rdim,
    ColorBy = if (!is.null(colour) && nzchar(colour)) "Column data" else "None",
    ColorByColumnData = colour %||% "",
    PanelWidth = 6L
  ))
}
panel_reduced_dim_by_gene <- function(rdim, gene_source = "RowDataTable1",
                                       gene = "") {
  list(type = "ReducedDimensionPlot", args = list(
    Type = rdim,
    ColorBy = "Feature name",
    ColorByFeatureName = gene,
    ColorByFeatureSource = gene_source,
    PanelWidth = 6L
  ))
}
panel_feature_assay <- function(gene, fap_x, fap_colour,
                                 gene_source = "RowDataTable1") {
  list(type = "FeatureAssayPlot", args = list(
    YAxisFeatureName = gene,
    YAxisFeatureSource = gene_source,
    XAxis = if (!is.null(fap_x) && nzchar(fap_x) && fap_x != "None")
              "Column data" else "None",
    XAxisColumnData = if (!is.null(fap_x) && fap_x != "None") fap_x else "",
    ColorBy = if (!is.null(fap_colour) && nzchar(fap_colour))
                "Column data" else "None",
    ColorByColumnData = fap_colour %||% "",
    PanelWidth = 6L
  ))
}
panel_row_data_table <- function(gene) {
  list(type = "RowDataTable", args = list(
    Selected = gene,
    Search = "",
    PanelWidth = 6L
  ))
}
panel_column_data_table <- function() {
  list(type = "ColumnDataTable", args = list(PanelWidth = 6L))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
