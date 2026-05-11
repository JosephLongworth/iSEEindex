# Build the LLM "skill" markdown that the user feeds to an LLM to produce
# an `initial.R` for iSEEindex. The output is a self-contained .md: task
# description, iSEE panel reference, house-style layout recommendations, and
# a dump of the selected dataset's structure (assays, reducedDims, colData
# columns with example values).

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Probe a colData column to give the LLM a realistic sense of its values.
# - Categorical-looking (<= 30 unique values): list the levels.
# - Numeric: report range + a few summary stats.
# - Otherwise: report first 5 sample values.
.summarize_coldata_col <- function(sce, col) {
  v <- SummarizedExperiment::colData(sce)[[col]]
  if (is.null(v)) return("(NULL)")
  if (is.factor(v) || is.character(v) || is.logical(v)) {
    u <- unique(as.character(v))
    if (length(u) <= 30L) {
      paste0("categorical, ", length(u), " unique: ",
             paste(shQuote(sort(u), type = "cmd"), collapse = ", "))
    } else {
      paste0("categorical, ", length(u), " unique (first 10): ",
             paste(shQuote(head(sort(u), 10), type = "cmd"), collapse = ", "))
    }
  } else if (is.numeric(v)) {
    rng <- suppressWarnings(range(v, na.rm = TRUE, finite = TRUE))
    if (!all(is.finite(rng))) return("numeric, all NA")
    qs <- suppressWarnings(stats::quantile(v, c(0.25, 0.5, 0.75),
                                           na.rm = TRUE))
    sprintf("numeric, range [%g, %g], median %g (q25=%g, q75=%g)",
            rng[1L], rng[2L], qs[2L], qs[1L], qs[3L])
  } else {
    paste0("class ", paste(class(v), collapse = "/"),
           ", first values: ",
           paste(utils::head(as.character(v), 5), collapse = ", "))
  }
}

# Build a dataset-structure block: assays, reducedDims, colData with summaries,
# example feature names, dimensions. This is what the LLM uses to decide which
# variables to plug into panel slots.
.dataset_structure_block <- function(sce, meta) {
  cd_names <- meta$colData_names %||% character(0)
  cd_lines <- if (length(cd_names)) {
    vapply(cd_names, function(n) {
      paste0("- `", n, "` - ", .summarize_coldata_col(sce, n))
    }, character(1))
  } else "(no colData columns)"

  feat_sample <- if (nrow(sce) > 0L) {
    paste(utils::head(rownames(sce), 15), collapse = ", ")
  } else "(none)"

  guess_ct <- if (!is.null(meta$cell_types_col)) meta$cell_types_col else NULL
  guess_cond <- {
    cd <- cd_names
    cand <- grep("condition|treatment|group|genotype|sample|stim|disease|status",
                 cd, ignore.case = TRUE, value = TRUE)
    if (length(cand)) cand[1L] else NULL
  }

  c(
    "## Dataset structure",
    "",
    paste0("- **Cells (columns):** ", format(meta$n_cells, big.mark = ",")),
    paste0("- **Features (rows / genes):** ",
           format(meta$n_features, big.mark = ",")),
    paste0("- **Assays:** ",
           if (length(meta$assays))
             paste(paste0("`", meta$assays, "`"), collapse = ", ")
           else "(none)"),
    paste0("- **Reduced dims:** ",
           if (length(meta$reducedDims))
             paste(paste0("`", meta$reducedDims, "`"), collapse = ", ")
           else "(none)"),
    "",
    "### colData columns (cell-level metadata)",
    "",
    cd_lines,
    "",
    "### Sample feature (row) names",
    "",
    paste0("First 15: ", feat_sample),
    "",
    "### LLM hints from auto-inspection",
    "",
    paste0("- Best guess for **cell-type column**: ",
           if (!is.null(guess_ct)) paste0("`", guess_ct, "`")
           else "(none detected - ask the user)"),
    paste0("- Best guess for **condition / treatment column**: ",
           if (!is.null(guess_cond)) paste0("`", guess_cond, "`")
           else "(none detected - ask the user)"),
    ""
  )
}

# Static iSEE reference (panels, slot conventions) and house-style guidance.
# Kept in one place so updates flow into every generated skill.
.skill_reference_block <- function() {
  c(
    "## How `initial.R` is consumed",
    "",
    "iSEEindex sources `initial.R` and then looks up an object named",
    "**`initial`** in the sourced environment. The script MUST assign a",
    "variable called `initial` whose value is a list of iSEE Panel objects",
    "(S4 instances). Not assigning `initial` causes iSEEindex to error with",
    "`No object named 'initial' was found`.",
    "",
    "Example shape:",
    "",
    "```r",
    "library(iSEE)",
    "initial <- list(",
    "  ReducedDimensionPlot(...),",
    "  ColumnDataPlot(...),",
    "  FeatureAssayPlot(...),",
    "  RowDataTable(...)",
    ")",
    "```",
    "",
    "## Panel classes you can use",
    "",
    "- `ReducedDimensionPlot()` - UMAP / PCA / t-SNE scatter, coloured by",
    "  metadata or by a gene's expression.",
    "- `ColumnDataPlot()` - violin / scatter of per-cell metadata.",
    "- `ColumnDataTable()` - browsable table of cell metadata.",
    "- `FeatureAssayPlot()` - expression of a feature across cells, with X",
    "  axis from metadata (violin per group) or another gene.",
    "- `RowDataPlot()` - per-feature metadata scatter.",
    "- `RowDataTable()` - browsable gene table; row clicks broadcast",
    "  `RowDataTable1` as a `*FeatureSource` to other panels.",
    "- `SampleAssayPlot()` - assay value across samples.",
    "- `ComplexHeatmapPlot()` - feature x cell heatmap.",
    "",
    "## Layout (12-unit Bootstrap grid)",
    "",
    "Set `PanelWidth = 6L` for half-width, `12L` for full-width, `4L` for",
    "thirds. Order in the list = order on the page (left to right, then top",
    "to bottom).",
    "",
    "## Slot conventions worth knowing",
    "",
    "**Reduced dim plot coloured by metadata:**",
    "",
    "```r",
    "ReducedDimensionPlot(",
    "  Type = \"UMAP\",",
    "  ColorBy = \"Column data\",",
    "  ColorByColumnData = \"<colData column>\",",
    "  PanelWidth = 6L",
    ")",
    "```",
    "",
    "**Reduced dim plot coloured by a gene (driven by a row-table click):**",
    "",
    "```r",
    "ReducedDimensionPlot(",
    "  Type = \"UMAP\",",
    "  ColorBy = \"Feature name\",",
    "  ColorByFeatureSource = \"RowDataTable1\",",
    "  ColorByFeatureName = \"<initial gene>\",",
    "  PanelWidth = 6L",
    ")",
    "```",
    "",
    "**Violin of one gene's expression stratified by condition, coloured by",
    "another grouping:**",
    "",
    "```r",
    "FeatureAssayPlot(",
    "  YAxisFeatureSource = \"RowDataTable1\",",
    "  YAxisFeatureName = \"<initial gene>\",",
    "  XAxis = \"Column data\",",
    "  XAxisColumnData = \"<condition column>\",",
    "  ColorBy = \"Column data\",",
    "  ColorByColumnData = \"<grouping column, e.g. genotype>\",",
    "  PanelWidth = 6L",
    ")",
    "```",
    "",
    "**Row data table that broadcasts gene selections:**",
    "",
    "```r",
    "RowDataTable(",
    "  Selected = \"<initial gene>\",",
    "  PanelWidth = 6L",
    ")",
    "```",
    "",
    "iSEE auto-numbers panels in their order of appearance, so the first",
    "`RowDataTable()` in the list is referenceable as `\"RowDataTable1\"`",
    "from other panels' `*Source` slots."
  )
}

.skill_house_style_block <- function() {
  c(
    "## House-style starting layout",
    "",
    "Unless the user asks otherwise, produce this two-row layout:",
    "",
    "**Row 1 - reduced-dimension overview (three panels, 4 units wide each):**",
    "",
    "1. UMAP coloured by **cell type** (fall back to PCA if no UMAP reduced",
    "   dim exists).",
    "2. Same reduced dim coloured by **condition / treatment** (fall back to",
    "   any obvious grouping column like genotype, sample, disease status).",
    "3. Same reduced dim coloured by the **currently selected gene** (driven",
    "   by `RowDataTable1`).",
    "",
    "**Row 2 - gene-driven exploration (two panels, 6 units wide each):**",
    "",
    "4. `RowDataTable` listing all genes. Clicking a row updates the gene-",
    "   coloured UMAP **and** the violin's Y axis.",
    "5. `FeatureAssayPlot` violin of the selected gene's expression, with",
    "   X axis stratified by the **condition column** and points coloured",
    "   by the cell-type / genotype column.",
    "",
    "If the dataset has no UMAP and no PCA, drop the reduced-dim row and",
    "explain why in a comment at the top of the file.",
    "",
    "## Decision rules for the LLM",
    "",
    "- The script MUST assign `initial <- list(...)` - iSEEindex looks up",
    "  the object by name. A bare `list(...)` at the end of the file will",
    "  fail.",
    "- Use the **Best guess** hints below as your default mappings.",
    "- If a slot is ambiguous (e.g. two equally good candidates for the",
    "  cell-type column), **ask the user** before generating - one short",
    "  question, then commit.",
    "- If a slot is unambiguous, do **not** ask - just pick and proceed.",
    "- Always set `PanelWidth` explicitly.",
    "- The first gene in `RowDataTable.Selected` and the `*FeatureName`",
    "  slots can be any gene from the dataset; pick one from the sample",
    "  feature names listed below, or ask the user for a favourite gene.",
    "- Deliver the result as a downloadable `initial.R` file. If your",
    "  interface cannot attach files, output the complete script inside one",
    "  fenced ```r code block and tell the user to save it as `initial.R`.",
    "- No commentary outside the code block / file unless you are asking a",
    "  clarifying question first.",
    "- The script must be runnable as-is: no `...` placeholders, no TODO",
    "  notes, no example values left in the slots."
  )
}

# Public entrypoint used by mod_layout_builder.R.
build_skill_md <- function(sce, meta, dataset_id, dataset_title = NULL) {
  title <- dataset_title %||% dataset_id
  header <- c(
    paste0("# iSEE `initial.R` skill - ", title),
    "",
    paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
           " for dataset `", dataset_id, "`."),
    "",
    "## Your task (read this first)",
    "",
    "You are helping a scientist configure a pre-made iSEE / iSEEindex",
    "viewer for a single-cell dataset. Produce an `initial.R` file that",
    "the iSEEindex app sources to lay out its panels at startup.",
    "",
    "The script MUST assign a variable called `initial` whose value is a",
    "list of iSEE Panel objects. iSEEindex looks up `initial` by name in",
    "the sourced environment; a bare `list(...)` at the end of the file",
    "will fail. Do not write anything that requires loading the data -",
    "iSEE wires up the SingleCellExperiment for you.",
    "",
    "**Deliverable:** the user expects to **download a file named",
    "`initial.R`** containing your script. Produce the file as a downloadable",
    "attachment if your interface supports it; otherwise output the complete",
    "script in a single fenced ```r code block and tell the user to save it",
    "as `initial.R`. Either way, the script must be runnable as-is - no",
    "placeholder text, no `...` ellipses, no commentary mixed into the R code.",
    ""
  )

  paste(c(
    header,
    .skill_reference_block(),
    "",
    .skill_house_style_block(),
    "",
    .dataset_structure_block(sce, meta)
  ), collapse = "\n")
}
