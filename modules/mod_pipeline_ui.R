# Animated pipeline diagram showing conversion progress.
# Used by the unified upload modal and the layout builder.
#
# Key design point: anndataR::read_h5ad() blocks the R thread, so Shiny cannot
# push UI updates during the read. All "still working" feedback during that
# window must be CSS/JS-driven (animations on already-rendered DOM), not
# reactive re-renders. We also override Shiny's `.recalculating` greying on
# pipeline outputs so they don't dim while R is busy.

pipeline_ui_styles <- function() {
  tagList(
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
         border:2px solid transparent;
         transition:transform 0.2s, box-shadow 0.2s;
         font-size:0.9em;
       }
       .pipe-box small { display:block; font-weight:400; opacity:0.9; font-size:0.75em; }
       .pipe-h5ad   { background:#3776ab; }
       .pipe-seurat { background:#198754; }
       .pipe-sce    { background:#fd7e14; }
       .pipe-conv   { background:#6c757d; }
       .pipe-arrow  { color:#888; font-size:1.5em; font-weight:bold; }

       .pipe-desc { font-size:0.85em; color:#555; margin-top:8px; text-align:center; }

       /* Spinning DNA helix used during long blocking work. */
       @keyframes dnaSpin {
         from { transform:rotate(0deg); }
         to   { transform:rotate(360deg); }
       }
       @keyframes dnaPulse {
         0%,100% { opacity:0.9; }
         50%     { opacity:0.4; }
       }
       .dna-spinner {
         display:inline-block; font-size:2.4em; line-height:1;
         animation:dnaSpin 2.4s linear infinite, dnaPulse 1.2s ease-in-out infinite;
         color:#6B1435;
       }
       .busy-banner {
         display:flex; align-items:center; gap:14px;
         padding:10px 14px; margin:8px 0;
         background:linear-gradient(90deg,#fff7d6 0%,#ffeaa0 50%,#fff7d6 100%);
         background-size:200% 100%;
         animation:shimmer 2.2s linear infinite;
         border-radius:8px; border:1px solid #f0c948;
       }
       @keyframes shimmer {
         from { background-position:0% 0%; }
         to   { background-position:200% 0%; }
       }
       .busy-banner-text { font-weight:600; color:#553300; }
       .busy-banner-text small { display:block; font-weight:400; color:#7a5500; }

       /* Determinate-looking progress bar. Driven by a CSS keyframe, so it
          continues to animate even while R is blocked. Loops every 6s. */
       .indet-bar {
         height:6px; background:#eee; border-radius:3px; overflow:hidden;
         margin-top:6px;
       }
       .indet-bar > span {
         display:block; width:30%; height:100%;
         background:linear-gradient(90deg,#4B7EA3,#6B1435);
         animation:indetSlide 1.8s ease-in-out infinite;
       }
       @keyframes indetSlide {
         0%   { margin-left:-30%; width:30%; }
         50%  { margin-left:50%;  width:40%; }
         100% { margin-left:110%; width:30%; }
       }"
    )),
    # JS: the moment a file is chosen in the browser, light up the busy
    # banner — before Shiny has even started uploading it server-side. The
    # observer on the R side later replaces this with the real stage UI.
    tags$script(HTML("
      (function(){
        function attach() {
          var f = document.getElementById('up_input_file');
          if (!f) { return; }
          if (f.dataset.busyHook === '1') return;
          f.dataset.busyHook = '1';
          f.addEventListener('change', function(ev){
            var banner = document.getElementById('up_browser_busy');
            if (!banner) return;
            if (f.files && f.files.length) {
              var sz = f.files[0].size;
              var hr = (sz/1e9 >= 1)
                ? (sz/1e9).toFixed(2) + ' GB'
                : (sz/1e6).toFixed(1) + ' MB';
              banner.querySelector('.busy-size').textContent = hr;
              banner.querySelector('.busy-size-wrap').style.display = '';
              banner.querySelector('.busy-title').textContent =
                'Uploading file to server...';
              banner.querySelector('.busy-detail').textContent =
                'Keep this tab open.';
              banner.style.display = 'flex';
            } else {
              banner.style.display = 'none';
            }
          });
        }
        // The file input is inside a modal that is created lazily; poll
        // briefly until it exists, then attach once.
        var tries = 0;
        var iv = setInterval(function(){
          attach();
          if (++tries > 40 || document.getElementById('up_input_file')) {
            clearInterval(iv);
          }
        }, 250);
        document.addEventListener('shiny:connected', attach);

        // Server pushes status updates here. The banner stays visible (and
        // the DNA keeps spinning) across every stage — we just swap the text.
        if (window.Shiny && Shiny.addCustomMessageHandler) {
          Shiny.addCustomMessageHandler('up_busy_set', function(msg) {
            var banner = document.getElementById('up_browser_busy');
            if (!banner) return;
            if (msg.title)  banner.querySelector('.busy-title').textContent  = msg.title;
            if (msg.detail !== undefined) {
              banner.querySelector('.busy-detail').textContent = msg.detail || '';
            }
            if (msg.size === false) {
              banner.querySelector('.busy-size-wrap').style.display = 'none';
            }
            banner.style.display = 'flex';
          });
          Shiny.addCustomMessageHandler('up_busy_hide', function(_) {
            var banner = document.getElementById('up_browser_busy');
            if (banner) banner.style.display = 'none';
          });
        }
      })();
    "))
  )
}

# Translate a file size in bytes to a coarse, honest time-range hint.
estimate_h5ad_time <- function(size_bytes) {
  if (is.null(size_bytes) || !is.finite(size_bytes) || size_bytes <= 0) return("")
  gb <- size_bytes / 1e9
  if (gb < 0.1) return("typically 30 seconds to 2 minutes")
  if (gb < 0.5) return("typically 1-3 minutes")
  if (gb < 1.5) return("typically 2-5 minutes")
  if (gb < 4)   return("typically 5-10 minutes")
  "typically 10+ minutes"
}

# Always-on browser-side busy banner. Hidden by default; JS reveals it as
# soon as the user picks a file (before the server-side upload starts).
browser_busy_banner <- function() {
  tags$div(id = "up_browser_busy", class = "busy-banner",
    style = "display:none;",
    tags$span(class = "dna-spinner", HTML("&#129516;")),  # DNA emoji
    tags$div(class = "busy-banner-text",
      tags$span(class = "busy-title", "Uploading file to server..."),
      tags$small(
        tags$span(class = "busy-detail"),
        tags$span(class = "busy-size-wrap",
          " - ", tags$span(class = "busy-size"))
      ),
      tags$div(class = "indet-bar", tags$span())
    )
  )
}

# Render the pipeline diagram. `proc_stage` is one of NULL/load/convert/ready;
# `proc_source` is one of NULL/h5ad/seurat/rds. `input_size` is bytes (for time hint).
render_pipeline <- function(proc_stage, proc_source,
                            sce_ready_fallback = FALSE,
                            input_size = NULL) {
  box <- function(cls, title, sub = NULL) {
    tags$div(class = paste("pipe-box", cls), title,
             if (!is.null(sub)) tags$small(sub))
  }
  arrow <- tags$div(class = "pipe-arrow", HTML("&rarr;"))

  sce_ready <- (!is.null(proc_stage) && proc_stage == "ready") ||
    (is.null(proc_stage) && isTRUE(sce_ready_fallback))

  stage_label <- if (!is.null(proc_stage)) {
    switch(proc_stage,
      load    = "Loading dataset into memory...",
      convert = "Converting to SingleCellExperiment...",
      ready   = "Ready - dataset prepared.",
      ""
    )
  } else if (sce_ready) {
    "Dataset ready."
  } else {
    "Idle - upload an H5AD, Seurat .rds, or SingleCellExperiment .rds to begin."
  }

  tagList(
    tags$div(class = "pipeline",
      tags$div(class = "pipe-stack",
        box("pipe-h5ad",   "H5AD",   ".h5ad"),
        box("pipe-seurat", "Seurat", ".rds"),
        box("pipe-sce",    "SCE",    ".rds")
      ),
      arrow,
      box("pipe-conv", "Converter", "to SCE"),
      arrow,
      box("pipe-sce", "SingleCellExperiment", "ready")
    ),
    tags$div(class = "pipe-desc fw-semibold", stage_label)
  )
}
