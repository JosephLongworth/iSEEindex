# DII branded header. Includes two buttons that open the Convert and
# Layout-builder modals — these IDs are observed in app.R.

branding_header_ui <- function() {
  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = paste0("dii_brand.css?v=", as.integer(Sys.time()))
      ),
      tags$style(HTML(
        ".dii-banner-tools .btn { white-space: nowrap; }"
      ))
    ),
    div(class = "dii-banner",
      div(class = "dii-banner-logos",
        img(src = "DII_logo_vertical_neg.png", class = "logo-dii", alt = "DII"),
        img(src = "Fox_hex.png", class = "logo-fox", alt = "EMI")
      ),
      div(class = "dii-banner-text",
        p(class = "dii-banner-title", "iSEEindex"),
        p(class = "dii-banner-subtitle",
          "Select a dataset below to explore it interactively with iSEE.")
      ),
      div(class = "dii-banner-tools",
        style = "display:flex; gap:8px; align-items:center; padding-right:12px;",
        actionButton("open_upload", "Upload dataset",
                     icon = icon("upload"),
                     class = "btn btn-sm btn-light"),
        actionButton("open_convert", "Convert dataset",
                     icon = icon("right-left"),
                     class = "btn btn-sm btn-light"),
        actionButton("open_layout", "Build initial.R",
                     icon = icon("sliders"),
                     class = "btn btn-sm btn-light")
      ),
      div(class = "dii-attribution",
        div(class = "dev-name", "Dr. Joseph Longworth"),
        div(class = "dept-line", "Experimental Molecular Immunology"),
        div(class = "dept-line", "Dept. of Infection & Immunity · LIH")
      )
    )
  )
}
