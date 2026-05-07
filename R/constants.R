# Shiny outputs ----

.ui_dataset_columns <- "iSEEindex_INTERNAL_datasets_columns"
.ui_dataset_table <- "iSEEindex_INTERNAL_datasets_table"
.ui_box_dataset <- "iSEEindex_INTERNAL_box_dataset"
.ui_markdown_overview <- "iSEEindex_INTERNAL_markdown_overview"
.ui_initial <- "iSEEindex_INTERNAL_initial"
.ui_launch_button <- "iSEEindex_INTERNAL_launch"
.ui_initial_overview <- "iSEEindex_INTERNAL_initial_overview"

# Shiny inputs ----

.dataset_selected_row <- paste0(.ui_dataset_table, "_rows_selected")

# Reactive object names ----

.dataset_selected_id <- paste0(.ui_dataset_table, "_id_selected")

# Default values ----

.initial_default_choice <- "(Default)"

# Data sets table fields ----

.datasets_id <- "id"
.datasets_uri <- "uri"
.datasets_title <- "title"
.datasets_description <- "description"
.dataset_region <- "region"

.initial_config_id <- "id"
.initial_datasets_id <- "datasets"
.initial_title <- "title"
.initial_uri <- "uri"
.initial_description <- "description"
.initial_region <- "region"

# Upload UI inputs/outputs ----

.ui_upload_sce_file     <- "iSEEindex_INTERNAL_upload_sce_file"
.ui_upload_title        <- "iSEEindex_INTERNAL_upload_title"
.ui_upload_description  <- "iSEEindex_INTERNAL_upload_description"
.ui_upload_layout_file  <- "iSEEindex_INTERNAL_upload_layout_file"
.ui_upload_add_button   <- "iSEEindex_INTERNAL_upload_add"
.ui_upload_commit_button <- "iSEEindex_INTERNAL_upload_commit"
.ui_upload_status       <- "iSEEindex_INTERNAL_upload_status"

# Prefix used to mark uploaded (session-only) dataset IDs ----
.upload_id_prefix <- "UPLOAD_SESSION_"
