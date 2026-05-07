#' Handle "Add for this session" upload action
#'
#' Copies the uploaded RDS (and optional layout script) to a per-session
#' temporary directory, then registers the dataset in `pObjects$datasets_table`
#' and (if a layout was supplied) in `pObjects$initial_table`.  The dataset
#' exists only for the lifetime of this Shiny session.
#'
#' @param input The Shiny input object.
#' @param pObjects An environment containing global parameters generated in the
#'   landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#'
#' @return A list with either an `error` string or a `message` string.
#'
#' @author Joseph Longworth
#'
#' @importFrom tools file_path_sans_ext
#'
#' @rdname INTERNAL_handle_upload
.handle_upload_add <- function(input, pObjects, rObjects) {
    sce_info <- input[[.ui_upload_sce_file]]
    if (is.null(sce_info)) {
        return(list(error = "Please select an RDS file to upload."))
    }

    title_val <- trimws(input[[.ui_upload_title]])
    if (!nzchar(title_val)) {
        title_val <- tools::file_path_sans_ext(sce_info$name)
    }
    desc_val <- trimws(input[[.ui_upload_description]])
    if (!nzchar(desc_val)) {
        desc_val <- sprintf("Uploaded dataset: %s", title_val)
    }

    dataset_id <- paste0(.upload_id_prefix, gsub("[^A-Za-z0-9_]", "_", title_val),
                         "_", format(Sys.time(), "%Y%m%d%H%M%S"))

    if (dataset_id %in% pObjects$datasets_table[[.datasets_id]]) {
        return(list(error = "A dataset with this name is already loaded in this session."))
    }

    # Copy RDS to session-scoped temp location so the original tempfile persists.
    session_dir <- file.path(tempdir(), "iSEEindex_session_uploads")
    dir.create(session_dir, showWarnings = FALSE, recursive = TRUE)
    dest_rds <- file.path(session_dir, paste0(dataset_id, ".rds"))
    ok <- file.copy(sce_info$datapath, dest_rds, overwrite = TRUE)
    if (!ok) {
        return(list(error = "Failed to copy uploaded RDS file to temporary location."))
    }

    new_row <- data.frame(
        id          = dataset_id,
        title       = title_val,
        uri         = paste0("runr://readRDS('", dest_rds, "')"),
        description = desc_val,
        stringsAsFactors = FALSE
    )
    # Align columns with existing table (fill NA for any extra columns).
    for (col in setdiff(colnames(pObjects$datasets_table), colnames(new_row))) {
        new_row[[col]] <- NA_character_
    }
    pObjects$datasets_table <- rbind(
        pObjects$datasets_table,
        new_row[, colnames(pObjects$datasets_table), drop = FALSE]
    )

    layout_info <- input[[.ui_upload_layout_file]]
    if (!is.null(layout_info)) {
        dest_r <- file.path(session_dir, paste0(dataset_id, "_layout.R"))
        file.copy(layout_info$datapath, dest_r, overwrite = TRUE)
        layout_id <- paste0(dataset_id, "_layout")
        layout_row <- data.frame(
            id          = layout_id,
            datasets    = dataset_id,
            title       = paste0(title_val, " (uploaded layout)"),
            uri         = paste0("rcall://", dest_r),
            description = "User-uploaded initial layout configuration.",
            stringsAsFactors = FALSE
        )
        if (!is.null(pObjects$initial_table)) {
            for (col in setdiff(colnames(pObjects$initial_table), colnames(layout_row))) {
                layout_row[[col]] <- NA_character_
            }
            pObjects$initial_table <- rbind(
                pObjects$initial_table,
                layout_row[, colnames(pObjects$initial_table), drop = FALSE]
            )
        } else {
            pObjects$initial_table <- layout_row
        }
    }

    list(message = sprintf(
        "Dataset '%s' added for this session. Select it from the table above to launch.",
        title_val))
}


#' Handle "Commit to shared storage" upload action
#'
#' Copies the uploaded RDS (and optional layout script) to the persistent
#' shared upload directory.  The number of stored datasets is capped at
#' `max_persistent`; the oldest files (by mtime) are removed when the cap is
#' exceeded.  The committed dataset is also registered in
#' `pObjects$datasets_table` for immediate use.
#'
#' @param input The Shiny input object.
#' @param pObjects An environment containing global parameters generated in the
#'   landing page.
#' @param rObjects A reactive list of values generated in the landing page.
#' @param upload_dir Character scalar. Path to the persistent shared upload
#'   directory.
#' @param max_persistent Integer. Maximum number of datasets to retain.
#'
#' @return A list with either an `error` string or a `message` string.
#'
#' @author Joseph Longworth
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom yaml write_yaml read_yaml
#'
#' @rdname INTERNAL_handle_upload
.handle_upload_commit <- function(input, pObjects, rObjects,
                                  upload_dir, max_persistent) {
    sce_info <- input[[.ui_upload_sce_file]]
    if (is.null(sce_info)) {
        return(list(error = "Please select an RDS file to commit."))
    }

    title_val <- trimws(input[[.ui_upload_title]])
    if (!nzchar(title_val)) {
        title_val <- tools::file_path_sans_ext(sce_info$name)
    }
    desc_val <- trimws(input[[.ui_upload_description]])
    if (!nzchar(desc_val)) {
        desc_val <- sprintf("Shared uploaded dataset: %s", title_val)
    }

    dir.create(upload_dir, showWarnings = FALSE, recursive = TRUE)

    dataset_id <- paste0("shared_upload_",
                         gsub("[^A-Za-z0-9_]", "_", title_val), "_",
                         format(Sys.time(), "%Y%m%d%H%M%S"))

    dest_rds <- file.path(upload_dir, paste0(dataset_id, ".rds"))
    ok <- file.copy(sce_info$datapath, dest_rds, overwrite = TRUE)
    if (!ok) {
        return(list(error = "Failed to copy uploaded RDS file to shared storage."))
    }

    layout_uri <- NA_character_
    layout_info <- input[[.ui_upload_layout_file]]
    if (!is.null(layout_info)) {
        dest_r <- file.path(upload_dir, paste0(dataset_id, "_layout.R"))
        file.copy(layout_info$datapath, dest_r, overwrite = TRUE)
        layout_uri <- paste0("rcall://", normalizePath(dest_r, winslash = "/"))
    }

    abs_rds <- normalizePath(dest_rds, winslash = "/")

    # Persist metadata to an index YAML inside upload_dir.
    index_path <- file.path(upload_dir, "index.yaml")
    index <- if (file.exists(index_path)) yaml::read_yaml(index_path) else list()
    index[[length(index) + 1L]] <- list(
        id          = dataset_id,
        title       = title_val,
        uri         = paste0("runr://readRDS('", abs_rds, "')"),
        description = desc_val,
        layout_uri  = layout_uri,
        timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )

    # Enforce max_persistent: remove oldest entries by file mtime.
    rds_files <- list.files(upload_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(rds_files) > max_persistent) {
        mtimes <- file.info(rds_files)$mtime
        oldest <- rds_files[order(mtimes)][seq_len(length(rds_files) - max_persistent)]
        for (f in oldest) {
            old_id <- tools::file_path_sans_ext(basename(f))
            file.remove(f)
            layout_f <- file.path(upload_dir, paste0(old_id, "_layout.R"))
            if (file.exists(layout_f)) file.remove(layout_f)
            # Remove from index
            index <- Filter(function(e) e$id != old_id, index)
        }
    }

    yaml::write_yaml(index, index_path)

    # Register in pObjects for immediate use in this session.
    new_row <- data.frame(
        id          = dataset_id,
        title       = title_val,
        uri         = paste0("runr://readRDS('", abs_rds, "')"),
        description = desc_val,
        stringsAsFactors = FALSE
    )
    for (col in setdiff(colnames(pObjects$datasets_table), colnames(new_row))) {
        new_row[[col]] <- NA_character_
    }
    if (!dataset_id %in% pObjects$datasets_table[[.datasets_id]]) {
        pObjects$datasets_table <- rbind(
            pObjects$datasets_table,
            new_row[, colnames(pObjects$datasets_table), drop = FALSE]
        )
    }

    if (!is.na(layout_uri)) {
        layout_id  <- paste0(dataset_id, "_layout")
        layout_row <- data.frame(
            id          = layout_id,
            datasets    = dataset_id,
            title       = paste0(title_val, " (shared layout)"),
            uri         = layout_uri,
            description = "Shared uploaded initial layout configuration.",
            stringsAsFactors = FALSE
        )
        if (!is.null(pObjects$initial_table)) {
            for (col in setdiff(colnames(pObjects$initial_table), colnames(layout_row))) {
                layout_row[[col]] <- NA_character_
            }
            pObjects$initial_table <- rbind(
                pObjects$initial_table,
                layout_row[, colnames(pObjects$initial_table), drop = FALSE]
            )
        } else {
            pObjects$initial_table <- layout_row
        }
    }

    list(message = sprintf(
        "Dataset '%s' committed to shared storage and available immediately.",
        title_val))
}


#' Load Shared Uploads into the Dataset List
#'
#' Reads the persistent upload index YAML and appends any committed datasets
#' to the list returned by the user-supplied `FUN.datasets` callback.  Called
#' at app startup so that previously committed datasets appear on the landing
#' page automatically.
#'
#' @param base_list The list returned by `FUN.datasets()`.
#' @param upload_dir Path to the persistent shared upload directory.
#'
#' @return The (possibly extended) list of dataset metadata.
#'
#' @author Joseph Longworth
#'
#' @importFrom yaml read_yaml
#'
#' @rdname INTERNAL_load_shared_uploads
.load_shared_uploads <- function(base_list,
                                 upload_dir = file.path("data", "uploads")) {
    index_path <- file.path(upload_dir, "index.yaml")
    if (!file.exists(index_path)) return(base_list)

    index <- tryCatch(yaml::read_yaml(index_path), error = function(e) list())
    if (!length(index)) return(base_list)

    for (entry in index) {
        # URI is stored as runr://readRDS('/abs/path') — extract the path for existence check
        rds_path <- gsub("^runr://readRDS\\(['\"]|['\"]\\)$", "", entry$uri)
        if (!file.exists(rds_path)) next  # skip if file was removed
        item <- list(
            id          = entry$id,
            title       = paste0(entry$title, " [shared]"),
            uri         = entry$uri,
            description = entry$description
        )
        base_list <- c(base_list, list(item))
    }
    base_list
}


#' Load Shared Upload Initial Configurations
#'
#' Reads the persistent upload index YAML and returns initial configuration
#' entries for any committed datasets that also had a layout script.
#'
#' @param base_list The list returned by `FUN.initial()`.
#' @param upload_dir Path to the persistent shared upload directory.
#'
#' @return The (possibly extended) list of initial configuration metadata.
#'
#' @author Joseph Longworth
#'
#' @importFrom yaml read_yaml
#'
#' @rdname INTERNAL_load_shared_uploads
.load_shared_upload_initial <- function(base_list,
                                        upload_dir = file.path("data", "uploads")) {
    index_path <- file.path(upload_dir, "index.yaml")
    if (!file.exists(index_path)) return(base_list)

    index <- tryCatch(yaml::read_yaml(index_path), error = function(e) list())
    if (!length(index)) return(base_list)

    for (entry in index) {
        layout_uri <- entry$layout_uri
        if (is.null(layout_uri) || is.na(layout_uri) || !nzchar(layout_uri)) next
        # URI is stored as rcall:///abs/path — extract the path for existence check
        layout_path <- sub("^rcall://", "", layout_uri)
        if (!file.exists(layout_path)) next
        item <- list(
            id          = paste0(entry$id, "_layout"),
            datasets    = entry$id,
            title       = paste0(entry$title, " (shared layout)"),
            uri         = layout_uri,
            description = "Shared uploaded initial layout configuration."
        )
        base_list <- c(base_list, list(item))
    }
    base_list
}
