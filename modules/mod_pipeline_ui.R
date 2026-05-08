# Animated pipeline diagram showing conversion progress.
# Used by Tab 2 (mod_convert) and optionally Tab 3 (mod_layout_builder).

pipeline_ui_styles <- function() {
  tags$style(HTML(
    ".pipeline {
       display:flex; align-items:center; justify-content:center;
       gap:8px; flex-wrap:wrap; padding:6px 4px;
     }
     .pipe-stack { display:flex; flex-direction:column; gap:6px; }
     .pipe-box {
       border-radius:8px; padding:10px 14px; color:#fff; font-weight:600;
       text-align:center; min-width:110px;
       box-shadow:0 1px 3px rgba(0,0,0,0.15);
       border:2px solid transparent; transition:transform 0.2s, box-shadow 0.2s;
       font-size:0.9em;
     }
     .pipe-box small { display:block; font-weight:400; opacity:0.9; font-size:0.75em; }
     .pipe-h5ad   { background:#3776ab; }
     .pipe-seurat { background:#198754; }
     .pipe-sce    { background:#fd7e14; }
     .pipe-conv   { background:#6c757d; }
     .pipe-arrow  { color:#888; font-size:1.5em; font-weight:bold; }
     @keyframes pulseGlow {
       0%,100% { box-shadow:0 0 0 0 rgba(255,193,7,0.7); transform:scale(1); }
       50%     { box-shadow:0 0 0 10px rgba(255,193,7,0); transform:scale(1.05); }
     }
     .pulse { animation:pulseGlow 1.4s infinite; border-color:#ffc107 !important; }
     .pipe-desc { font-size:0.85em; color:#555; margin-top:8px; text-align:center; }
     #conv_log, #lb_log {
       background:#1e1e1e !important; color:#d4d4d4 !important;
       font-family:monospace; font-size:0.8em; border:none; margin:0; padding:8px;
     }"
  ))
}

# Render the pipeline diagram. `proc_stage` is one of NULL/load/convert/annotate/cache/ready;
# `proc_source` is one of NULL/h5ad/seurat/rds.
render_pipeline <- function(proc_stage, proc_source, sce_ready_fallback = FALSE) {
  pulse_if <- function(cond) if (isTRUE(cond)) "pulse" else ""
  box <- function(cls, title, sub = NULL) {
    tags$div(class = paste("pipe-box", cls), title,
             if (!is.null(sub)) tags$small(sub))
  }
  arrow <- tags$div(class = "pipe-arrow", HTML("&rarr;"))

  src_active <- function(which) {
    identical(proc_source, which) &&
      (!is.null(proc_stage) && proc_stage %in% c("load", "convert"))
  }
  conv_active     <- !is.null(proc_stage) && proc_stage == "convert"
  annotate_active <- !is.null(proc_stage) && proc_stage == "annotate"
  cache_active    <- !is.null(proc_stage) && proc_stage == "cache"
  sce_ready <- (!is.null(proc_stage) && proc_stage == "ready") ||
    (is.null(proc_stage) && isTRUE(sce_ready_fallback))

  stage_label <- if (!is.null(proc_stage)) {
    switch(proc_stage,
      load     = "Stage 1/4 - Loading file from disk...",
      convert  = "Stage 2/4 - Converting to SingleCellExperiment...",
      annotate = "Stage 3/4 - Extracting metadata...",
      cache    = "Stage 4/4 - Preparing download...",
      ready    = "Ready - download the converted .rds below.",
      ""
    )
  } else if (sce_ready) {
    "Dataset ready."
  } else {
    "Idle - upload an H5AD, Seurat .rds, or SingleCellExperiment .rds to begin."
  }

  detail <- if (!is.null(proc_stage) &&
                proc_stage %in% c("load", "convert", "annotate", "cache")) {
    "This can take several minutes for large datasets. The active step pulses yellow; the app has not frozen."
  } else ""

  tagList(
    tags$div(class = "pipeline",
      tags$div(class = "pipe-stack",
        box(paste("pipe-h5ad",   pulse_if(src_active("h5ad"))),   "H5AD",   ".h5ad (Python)"),
        box(paste("pipe-seurat", pulse_if(src_active("seurat"))), "Seurat", ".rds (R)"),
        box(paste("pipe-sce",    pulse_if(src_active("rds"))),    "SCE",    ".rds (R)")
      ),
      arrow,
      box(paste("pipe-conv", pulse_if(conv_active)), "Converter", "to SCE"),
      arrow,
      tags$div(class = "pipe-stack",
        box(paste("pipe-conv", pulse_if(annotate_active)), "Annotate", "metadata"),
        box(paste("pipe-conv", pulse_if(cache_active)),    "Package",  "for download")
      ),
      arrow,
      box(paste("pipe-sce", pulse_if(sce_ready)), "SingleCellExperiment", "ready")
    ),
    tags$div(class = "pipe-desc fw-semibold", stage_label),
    if (nzchar(detail)) tags$div(class = "pipe-desc", tags$small(detail))
  )
}
