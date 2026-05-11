# iSEE initial.R for Tcells_sce
# Dataset: UMAP_of_Tcells_sce — 14,970 cells, 32,383 features
# Cell-type column : Detailed_Cell_Type (8 T cell subtypes)
# Condition column : Patient_status (BE / Healthy / IM)
# UMAP             : X_umap_MinDist_0.1_N_Neighbors_15
# Initial gene     : ENSG00000177757 (CD3D)

library(iSEE)

initial <- list(

  # --- Row 1: three UMAP panels, 4 units each ---

  # 1. UMAP coloured by detailed cell type
  ReducedDimensionPlot(
    Type = "X_umap_MinDist_0.1_N_Neighbors_15",
    ColorBy = "Column data",
    ColorByColumnData = "Detailed_Cell_Type",
    PanelWidth = 4L
  ),

  # 2. UMAP coloured by patient status (condition)
  ReducedDimensionPlot(
    Type = "X_umap_MinDist_0.1_N_Neighbors_15",
    ColorBy = "Column data",
    ColorByColumnData = "Patient_status",
    PanelWidth = 4L
  ),

  # 3. UMAP coloured by currently selected gene (driven by RowDataTable1)
  ReducedDimensionPlot(
    Type = "X_umap_MinDist_0.1_N_Neighbors_15",
    ColorBy = "Feature name",
    ColorByFeatureSource = "RowDataTable1",
    ColorByFeatureName = "ENSG00000177757",
    PanelWidth = 4L
  ),

  # --- Row 2: gene-driven exploration, 6 units each ---

  # 4. Gene table — clicking a row updates UMAP #3 and the violin below
  RowDataTable(
    Selected = "ENSG00000177757",
    PanelWidth = 6L
  ),

  # 5. Violin of selected gene's expression, X = Patient_status, colour = Detailed_Cell_Type
  FeatureAssayPlot(
    YAxisFeatureSource = "RowDataTable1",
    YAxisFeatureName = "ENSG00000177757",
    XAxis = "Column data",
    XAxisColumnData = "Patient_status",
    ColorBy = "Column data",
    ColorByColumnData = "Detailed_Cell_Type",
    PanelWidth = 6L
  )

)
