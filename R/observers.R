#' Observers for iSEEindex
#'
#' @description
#'
#' `.create_observers()` initialises observers for the \pkg{iSEEindex} landing page.
#'
#' `.create_launch_observers()` initialises observers for launching the
#' \pkg{iSEE} main app.
#'
#' `.create_upload_observers()` initialises observers for the file-upload panel.
#'
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the
#' landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#' @param FUN.initial A function that returns available scripts for initial
#' configurations states for a given data set identifier.
#' @param default.add Logical scalar indicating whether a default
#' initial configuration should be added as a choice in the Shiny `selectizeInput()`.
#' See [iSEEindex()].
#' @param default.position Character scalar indicating whether the default
#' initial configuration should be added as the `"first"` or `"last"` option
#' in the Shiny `selectizeInput()`.
#'
#' @return
#' Those functions create observers in the server function in which they are called.
#' In all cases, a \code{NULL} value is invisibly returned.
#'
#' @author Kevin Rue-Albrecht
#'
#' @importFrom shiny isolate observeEvent renderUI updateSelectizeInput
#' @importFrom shiny markdown modalButton modalDialog p showModal showNotification
#' @importFrom rintrojs introjs
#'
#' @rdname INTERNAL_create_observers
.create_observers <- function(input,
                              session,
                              pObjects,
                              rObjects,
                              FUN.initial,
                              default.add,
                              default.position) {

    # nocov start
    observeEvent(input[[.dataset_selected_row]], {
        dataset_selected_id <- pObjects$datasets_table[[.datasets_id]][input[[.dataset_selected_row]]]
        pObjects[[.dataset_selected_id]] <- dataset_selected_id
        rObjects$rerender_overview <- iSEE:::.increment_counter(isolate(rObjects$rerender_overview))
        initial_choices <- .initial_choices(dataset_selected_id, pObjects$initial_table, default.add, default.position)
        updateSelectizeInput(session, .ui_initial, choices = initial_choices)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    # nocov end

    # nocov start
    observeEvent(input[[.ui_dataset_columns]], {
        pObjects[[.ui_dataset_columns]] <- input[[.ui_dataset_columns]]
        rObjects$rerender_datasets <- iSEE:::.increment_counter(isolate(rObjects$rerender_datasets))
    })
    # nocov end

    # nocov start
    observeEvent(input[[iSEE:::.generalTourSteps]], {
        introjs(session, options=list(steps=.landing_page_tour))
    }, ignoreInit=TRUE)
    # nocov end

    # nocov start
    observeEvent(input[[.ui_initial]], {
        pObjects[[.ui_initial]] <- input[[.ui_initial]]
        rObjects$rerender_initial <- iSEE:::.increment_counter(isolate(rObjects$rerender_initial))
    })
    # nocov end

    invisible(NULL)
}

#' @param FUN A function to initialize the \pkg{iSEE} observer
#' architecture. Refer to [iSEE::createLandingPage()] for more details.
#' @param bfc A [BiocFileCache()] object.
#' landing page.
#' @param input The Shiny input object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the
#' landing page.
#'
#' @importFrom shiny observeEvent
#'
#' @rdname INTERNAL_create_observers
.create_launch_observers <- function(FUN, bfc, input, session, pObjects) {

    # nocov start
    observeEvent(input[[.ui_launch_button]], {
        .launch_isee(FUN, bfc, session, pObjects)
    }, ignoreNULL=TRUE, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
}

#' @param upload_dir Path to the persistent shared upload directory.
#' @param max_persistent Integer. Maximum number of datasets to retain in the
#'   persistent upload directory. Oldest entries (by file mtime) are removed
#'   when this limit is exceeded.
#'
#' @rdname INTERNAL_create_observers
.create_upload_observers <- function(input,
                                     output,
                                     session,
                                     pObjects,
                                     rObjects,
                                     upload_dir = file.path("data", "uploads"),
                                     max_persistent = 10L) {
    # nocov start

    ## Status message output ----
    output[[.ui_upload_status]] <- renderUI({
        rObjects$upload_status
    })

    ## "Add for this session" button ----
    observeEvent(input[[.ui_upload_add_button]], {
        result <- .handle_upload_add(input, pObjects, rObjects)
        if (!is.null(result$error)) {
            rObjects$upload_status <- shiny::p(
                style = "color:#D64D38; font-weight:bold;",
                result$error)
        } else {
            rObjects$upload_status <- shiny::p(
                style = "color:#4B7EA3; font-weight:bold;",
                result$message)
            rObjects$rerender_datasets <- iSEE:::.increment_counter(
                isolate(rObjects$rerender_datasets))
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    ## "Commit to shared storage" button ----
    observeEvent(input[[.ui_upload_commit_button]], {
        sce_info <- input[[.ui_upload_sce_file]]
        if (is.null(sce_info)) {
            rObjects$upload_status <- shiny::p(
                style = "color:#D64D38; font-weight:bold;",
                "Please select an RDS file before committing.")
            return(invisible(NULL))
        }
        title_val <- trimws(input[[.ui_upload_title]])
        if (!nzchar(title_val)) title_val <- tools::file_path_sans_ext(sce_info$name)

        showModal(modalDialog(
            title = "Commit dataset to shared storage?",
            shiny::p(
                "This will save the uploaded dataset to the shared storage location.",
                "It will be visible to ALL users of this app the next time they load",
                "the landing page.",
                style = "margin-bottom:8px;"
            ),
            shiny::p(
                paste0("Only the ", max_persistent, " most recent datasets are kept.",
                       " Older shared datasets will be automatically removed."),
                style = "color:#6B1435;"
            ),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("iSEEindex_INTERNAL_confirm_commit", "Commit",
                    style = "color:#ffffff; background-color:#6B1435; border-color:#4a0e25;")
            ),
            easyClose = TRUE
        ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    ## Confirm commit ----
    observeEvent(input[["iSEEindex_INTERNAL_confirm_commit"]], {
        removeModal(session)
        result <- .handle_upload_commit(input, pObjects, rObjects,
                                        upload_dir, max_persistent)
        if (!is.null(result$error)) {
            rObjects$upload_status <- shiny::p(
                style = "color:#D64D38; font-weight:bold;",
                result$error)
        } else {
            rObjects$upload_status <- shiny::p(
                style = "color:#4B7EA3; font-weight:bold;",
                result$message)
            rObjects$rerender_datasets <- iSEE:::.increment_counter(
                isolate(rObjects$rerender_datasets))
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    invisible(NULL)
    # nocov end
}
