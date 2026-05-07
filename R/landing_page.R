#' Landing page function
#'
#' @param bfc A [BiocFileCache()] object.
#' @param FUN.datasets A function that returns a `data.frame` of metadata for
#' available data sets.
#' @param FUN.initial A function that returns a `data.frame` of metadata for
#' initial configuration states.
#' @param default.add Logical scalar indicating whether a default
#' initial configuration should be added as a choice in the Shiny
#' `selectizeInput()`.
#' See [iSEEindex()].
#' @param default.position Character scalar indicating whether the default
#' initial configuration should be added as the `"first"` or `"last"` option
#' in the Shiny `selectizeInput()`.
#' @param body.header UI element to display \emph{above} the main landing page body.
#' @param body.footer UI element to display \emph{below} the main landing page body.
#'
#' @return A `function` that defines UI elements and observers for the
#' landing page of the app.
#'
#' @author Kevin Rue-Albrecht
#'
#' @importFrom shiny actionButton br column fileInput fluidRow reactiveValues
#' renderUI selectizeInput tabPanel tagList textAreaInput textInput uiOutput
#' @importFrom shinydashboard box tabBox
#' @importFrom DT DTOutput
#' @importFrom shinyjs disable
#'
#' @rdname INTERNAL_landing_page
.landing_page <- function(bfc,
                          FUN.datasets,
                          FUN.initial,
                          default.add = TRUE,
                          default.position = c("first", "last"),
                          body.header = NULL,
                          body.footer = NULL) {
    default.position <- match.arg(default.position)
    # datasets
    datasets_available_list <- FUN.datasets()
    .check_datasets_list(datasets_available_list)
    datasets_available_table <- .datasets_to_dataframe(datasets_available_list)
    # initial configurations
    initial_available_list <- FUN.initial()
    .check_initial_list(initial_available_list)
    initial_available_table <- .initial_to_dataframe(initial_available_list,
                                                     datasets_available_table[[.datasets_id]])
    # landing page function (return value)
    function (FUN, input, output, session) {
        # nocov start
        output$allPanels <- renderUI({
            tagList(
                body.header,
                fluidRow(
                    column(width = 7L,
                        shinydashboard::box(title = "Available Data Sets",
                            collapsible = FALSE, width = NULL,
                            selectizeInput(.ui_dataset_columns, label = "Show columns:",
                                choices = setdiff(colnames(datasets_available_table),
                                                  c(.datasets_id, .datasets_uri, .datasets_description)),
                                selected = c(.datasets_title),
                                multiple = TRUE,
                                options = list(plugins=list('remove_button', 'drag_drop'))),
                            DTOutput(.ui_dataset_table)
                    )),
                    column(width = 5L,
                        shinydashboard::tabBox(id = .ui_box_dataset, title = "Selected dataset",
                            side = "left",
                            width = NULL,
                            tabPanel("Info",
                                uiOutput(.ui_markdown_overview)),
                            tabPanel("Configure and launch",
                                fluidRow(
                                    column(width = 10L,
                                        selectizeInput(.ui_initial, label = "Initial settings:",
                                    choices = character(0))),
                                    column(width = 2L,
                                        br(),
                                        actionButton(.ui_launch_button, label="Launch!",
                                        style="color: #ffffff; background-color: #0092AC; border-color: #2e6da4"))
                                ),
                                uiOutput(.ui_initial_overview))
                            )
                        )
                    ),
                fluidRow(
                    column(width = 12L,
                        shinydashboard::box(
                            title = "Upload Your Own Data",
                            collapsible = TRUE, collapsed = TRUE,
                            width = NULL,
                            solidHeader = FALSE,
                            fluidRow(
                                column(width = 6L,
                                    fileInput(.ui_upload_sce_file,
                                        label = "SingleCellExperiment / SummarizedExperiment (.rds) — large files may take a moment to upload",
                                        accept = c(".rds", ".RDS"),
                                        multiple = FALSE),
                                    textInput(.ui_upload_title,
                                        label = "Dataset title",
                                        placeholder = "e.g. My scRNA-seq experiment"),
                                    textAreaInput(.ui_upload_description,
                                        label = "Description (optional)",
                                        rows = 3L,
                                        placeholder = "Brief description of this dataset.")
                                ),
                                column(width = 6L,
                                    fileInput(.ui_upload_layout_file,
                                        label = "Initial layout / configuration script (.R, optional)",
                                        accept = c(".r", ".R"),
                                        multiple = FALSE),
                                    br(),
                                    fluidRow(
                                        column(width = 6L,
                                            actionButton(.ui_upload_add_button,
                                                label = "Add for this session",
                                                style = "color:#ffffff; background-color:#4B7EA3; border-color:#2e6da4; width:100%")
                                        ),
                                        column(width = 6L,
                                            actionButton(.ui_upload_commit_button,
                                                label = "Commit to shared storage",
                                                style = "color:#ffffff; background-color:#6B1435; border-color:#4a0e25; width:100%")
                                        )
                                    ),
                                    br(),
                                    uiOutput(.ui_upload_status)
                                )
                            )
                        )
                    )
                ),
                body.footer
                ) # tagList
        }) # renderUI

        ## Disable navbar buttons that are not linked to any observer yet
        shinyjs::disable(iSEE:::.generalOrganizePanels) # organize panels
        shinyjs::disable(iSEE:::.generalLinkGraph) # link graph
        shinyjs::disable(iSEE:::.generalExportOutput) # export content
        shinyjs::disable(iSEE:::.generalCodeTracker) # tracked code
        shinyjs::disable(iSEE:::.generalPanelSettings) # panel settings
        shinyjs::disable(iSEE:::.generalVignetteOpen) # open vignette
        shinyjs::disable(iSEE:::.generalSessionInfo) # session info
        shinyjs::disable(iSEE:::.generalCitationInfo) # citation info

        pObjects <- .create_persistent_objects(
            datasets_available_table,
            initial_available_table)
        rObjects <- reactiveValues(
            rerender_datasets=1L,
            rerender_overview=1L,
            rerender_initial=1L,
            upload_status=NULL)

        .create_observers(input, session, pObjects, rObjects, FUN.initial, default.add, default.position)

        .create_launch_observers(FUN, bfc, input, session, pObjects)

        .create_upload_observers(input, output, session, pObjects, rObjects)

        .render_datasets_table(output, pObjects, rObjects)

        .render_markdown_overview(output, pObjects, rObjects)

        .render_initial_overview(output, pObjects, rObjects)

        invisible(NULL)
        # nocov end
    }
}


#' Create persistent objects
#'
#' @param datasets_table A `data.frame` of metadata for all available data sets.
#' @param initial_table A `data.frame` of metadata for all available initial
#' configuration scripts.
#'
#' @return An environment containing several global variables for use throughout
#' the application.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_create_persistent_objects
.create_persistent_objects <- function(datasets_table, initial_table) {
    pObjects <- new.env()
    pObjects$datasets_table <- datasets_table
    pObjects$initial_table <- initial_table
    pObjects
}
