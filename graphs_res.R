
#Figure 1

FYN_MYL9_FLNB_CYTOiPaclitaxel_ALL_C_index
FYN_MYL9_FLNB_BRCA_CIndex
TCF7L2_FYN_CYTOiPaclitaxel_ALL_C_index
FYN_TCF7L2_CIndex


plot_list_fig1 <- list(
  FYN_MYL9_FLNB_CYTOiPaclitaxel_ALL_C_index, 
  FYN_MYL9_FLNB_BRCA_CIndex,
  TCF7L2_FYN_CYTOiPaclitaxel_ALL_C_index,
  FYN_TCF7L2_CIndex
)

titles_fig1 <- c(
  'Paclitaxel Samples (C-Index: MYL9, FYN and FLNB)', 
  'Paclitaxel BRCA Samples (C-Index: MYL9, FYN and FLNB)', 
  'Paclitaxel Samples (C-Index: TCF7L2 and FYN)',
  'Paclitaxel BRCA Samples (C-Index: TCF7L2 and FYN)'  
)

Figure1 <- create_figure_2(
  plot_list = plot_list_fig1,
  list_of_titles = titles_fig1,
  ncol = 2,
  nrow = 2,
  labels = c("a", "b", "c", "d")
)

# print(Figure1) #  1368x768

#Figure 2
LDHB_TCF_HR
TCF7L2_LDHB_Paclitaxel_ALL_C_index


Figure_3_LDHB_TCF <- create_figure_3(
  hr_plot = LDHB_TCF_HR,
  cindex_plot = TCF7L2_LDHB_Paclitaxel_ALL_C_index,
  main_title = "Paclitaxel Samples (HRs and C-Indices of LDHB & TCF7L2)",
  layout = "horizontal" 
 
)

LDHB_TCF7L2_BRCA_Cindex
LDHB_TCF7L2_HR_BRCA


Figure_3_LDHB_TCF_BRCA <- create_figure_3(
  hr_plot = LDHB_TCF7L2_HR_BRCA,
  cindex_plot = LDHB_TCF7L2_BRCA_Cindex,
  main_title = "Paclitaxel BRCA Samples (HRs and C-Indices of LDHB & TCF7L2)",
  layout = "horizontal" 
  
)


FYN_TCF_HR
TCF7L2_FYN_Paclitaxel_ALL_C_index

Figure_3_FYN_TCF <- create_figure_3(
  hr_plot = FYN_TCF_HR,
  cindex_plot = TCF7L2_FYN_Paclitaxel_ALL_C_index,
  main_title = "Paclitaxel Samples (HRs and C-Indices of FYN & TCF7L2)",
  layout = "horizontal" 
 
)


FYN_TCF7L2_HR_BRCA
FYN_TCF7L2_BRCA_Cindex

Figure_3_FYN_TCF_BRCA <- create_figure_3(
  hr_plot = FYN_TCF7L2_HR_BRCA,
  cindex_plot = FYN_TCF7L2_BRCA_Cindex,
  main_title = "Paclitaxel BRCA Samples (HRs and C-Indices of FYN & TCF7L2)",
  layout = "horizontal"
  
)


FYN_CXCR4_HR
CXCR4_FYN_Paclitaxel_ALL_C_index


Figure_3_FYN_CXCR4 <- create_figure_3(
  hr_plot = FYN_CXCR4_HR,
  cindex_plot = CXCR4_FYN_Paclitaxel_ALL_C_index,
  main_title = "Paclitaxel Samples (HR & C-Index: FYN and CXCR4)",
  layout = "horizontal" 
  
)

FYN_CXCR4_BRCA_Cindex
FYN_CXCR4_HR_BRCA

Figure_3_FYN_CXCR4_BRCA <- create_figure_3(
  hr_plot = FYN_CXCR4_HR_BRCA,
  cindex_plot = FYN_CXCR4_BRCA_Cindex,
  main_title = "Paclitaxel BRCA Samples (HR & C-Index: FYN and CXCR4)",
  layout = "horizontal" 
  
)

FYN_CXCR4_HR
CXCR4_FYN_Paclitaxel_ALL_C_index

Figure_3_FYN_CXCR4 <- create_figure_3(
  hr_plot = FYN_CXCR4_HR,
  cindex_plot = CXCR4_FYN_Paclitaxel_ALL_C_index,
  main_title = "Paclitaxel Samples (HR & C-Index: FYN and CXCR4)",
  layout = "horizontal"
 
)

FYN_CXCR4_BRCA_Cindex
FYN_CXCR4_HR_BRCA

Figure_3_FYN_CXCR4_BRCA <- create_figure_3(
  hr_plot = FYN_CXCR4_HR_BRCA,
  cindex_plot = FYN_CXCR4_BRCA_Cindex,
  main_title = "Paclitaxel BRCA Samples (HR & C-Index: FYN and CXCR4)",
  layout = "horizontal"
)


FYN_CXCR4_TCF7L2_HR
CXCR4_FYN_TCF7L2_Paclitaxel_ALL_C_index

Figure_4_FYN_CXCR4_TCF7L2 <- create_figure_3(
  hr_plot = FYN_CXCR4_TCF7L2_HR,
  cindex_plot = CXCR4_FYN_TCF7L2_Paclitaxel_ALL_C_index,
  main_title = "Paclitaxel Samples (HR & C-Index: FYN, CXCR4 and TCF7L2)",
  layout = "horizontal"
  
)

FYN_CXCR4_TCF7L2_BRCA_HR
CXCR4_FYN_TCF7L2_Paclitaxel_BRCA_C_index

Figure_4_FYN_CXCR4_TCF7L2_BRCA <- create_figure_3(
  hr_plot = FYN_CXCR4_TCF7L2_BRCA_HR,
  cindex_plot = CXCR4_FYN_TCF7L2_Paclitaxel_BRCA_C_index,
  main_title = "Paclitaxel BRCA Samples (HR & C-Index: FYN, CXCR4 and TCF7L2)",
  layout = "horizontal" 
)


# 1200X 1300 vertical
#Figure3 
Figure2 <- plot_grid(
  Figure_3_LDHB_TCF,
  Figure_3_LDHB_TCF_BRCA,
  Figure_3_FYN_TCF, 
  Figure_3_FYN_TCF_BRCA,
  ncol = 2,
  nrow=2,
  labels = c("a", "b",'c','d') 
)

# 1000x600  vertical
Figure3 <- plot_grid(
  Figure_3_FYN_CXCR4,
  Figure_3_FYN_CXCR4_BRCA,
  ncol = 2,
  nrow=1,
  labels = c("a", "b") 
)

# 1000X 1100 horizontal
#Figure3 
Figure2_v2 <- plot_grid(
  Figure_3_LDHB_TCF,
  Figure_3_LDHB_TCF_BRCA,
  Figure_3_FYN_TCF, 
  Figure_3_FYN_TCF_BRCA,
  ncol = 1,
  nrow=4,
  labels = c("A", "B",'C','D') # 
)

# 1000x600  horizontal  ,new  1100X 1200
Figure3_v2 <- plot_grid(
  Figure_3_FYN_CXCR4,
  Figure_3_FYN_CXCR4_BRCA,
  Figure_4_FYN_CXCR4_TCF7L2,
  Figure_4_FYN_CXCR4_TCF7L2_BRCA,
  ncol = 1,
  nrow=4,
  labels = c("a", "b",'c','d') 
)

#Figure 4 ----------------------

all_km_plots <- list(
  MYL9_KM_ALL, 
  SDC4_KM_ALL, 
  SULF2_KM_ALL,
  MYL9_KM_BRCA,
  SDC4_KM_BRCA,
  SULF2_KM_BRCA
)

titles_list <- list(
  NULL, 
  NULL,
  NULL, 
  NULL,
  NULL, 
  NULL  
)

pvals_list <- list(
  'p=0.001',        
  'p=0.001',       
  'p=0.0001',       
  'p=0.003', 
  'p=0.003', 
  NULL 
)

# # 1180x 620
Figure4 <- create_figure_5(
  km_plot_list = all_km_plots,
  ncol = 3,
  nrow = 2,
  labels = c("a",'b','c'), # A, B, C, D, E, F
  
 
  list_of_titles = NULL, 
  list_of_custom_pvals = pvals_list,
  original_pval_layer_index = 4, 
  
  
  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)

#Figure 5 ----------------------


all_km_plots_figure5 <- list(
  CXCR4_KM_ALL, 
  CXCR4_CNA_KM_ALL, 
  CXCR4_KM_BRCA,
  CXCR4_KM_CNA_BRCA
)

titles_list <- list(
  NULL, 
  NULL, 
  NULL, 
  NULL
)

pvals_list <- list(
  NULL,        
  NULL,        
  'p = 0.002',      
  NULL
)
 # 720x 560
Figure5 <- create_figure_5(
  km_plot_list = all_km_plots_figure5,
  ncol = 2,
  nrow = 2,
  labels = c("a",'b'), # A, B, C, D, E, F
  
  
  list_of_titles = NULL, 
  list_of_custom_pvals = pvals_list, 
  original_pval_layer_index = 4, 
  
  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)

#-------Figure6----------
# Figure 6----------------
Figure6_Top_Row <- create_figure_7_top_row(
  plot_A_raw = LYN_MYO1F_PINK1_TNFSF13B_PREX1_Cindex_ALL,
  plot_B_raw = LYN_MYO1F_PINK1_TNFSF13B_PREX1_BRCA_Cindex,
  title_A = "5-FU Samples (C-Index)",
  title_B = "5-FU STAD Samples (C-Index)",
  labels = c("A", "B")
)

LYN_Cindex
TNFSF13B_Cindex

Plot_C <- create_figure_7_bottom_plot(
  plot_top = LYN_Cindex,
  plot_bottom = TNFSF13B_Cindex,
  common_title = "5FU Samples of LYN and TNFSF13B (C-Index)"
)

LYN_BRCA_Cindex
TNFSF13B_BRCA_Cindex
Plot_D <- create_figure_7_bottom_plot(
  plot_top = LYN_BRCA_Cindex,
  plot_bottom = TNFSF13B_BRCA_Cindex,
  common_title = "5FU STAD Samples of LYN and TNFSF13B (C-Index)"
)

#1400x 900
Figure6_Final <- plot_grid(
  Figure6_Top_Row, 
  plot_grid(Plot_C, Plot_D, ncol = 2, labels = c("C", "D")), 
  nrow = 2,
  ncol = 1,
  labels = c("A", ""), 
  rel_heights = c(1, 1.5),
  align = 'v'
)

#Figure7---------------------

all_km_plots_figure7 <- list(
  CCND1_KM_ALL, 
  FOSL1_KM_ALL, 
  CTTN_KM_ALL,
  CCND1_KM_STAD,
  FOSL1_KM_STAD,
  CTTN_KM_STAD
)

titles_list <- list(
  NULL, 
  NULL, 
  NULL, 
  NULL,
  NULL,
  NULL
)

pvals_list <- list(
  NULL,        
  NULL,       
  NULL,        
  'p = 0.001', 
  'p = 0.001',
  'p = 0.001'
)


#  1180x 620
Figure7 <- create_figure_5(
  km_plot_list = all_km_plots_figure7,
  ncol = 3,
  nrow = 2,
  labels = c("a",'b','c'), # A, B, C, D, E, F
  
  
  list_of_titles = NULL, 
  list_of_custom_pvals = pvals_list, 
  original_pval_layer_index = 4, 
  
  
  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)

#----------Figure 8

all_km_plots_figure8 <- list(
  YAP1_KM_ALL, 
  ASAP2_KM_ALL, 
  YAP1_KM_STAD,
  ASAP2_KM_STAD
)

titles_list <- list(
  NULL, 
  NULL, 
  NULL, 
  NULL
)

pvals_list <- list(
  'p = 0.0003',        
  'p = 0.002',        
  'p = 0.001',       
  'p = 0.01' 
)

 #   720x 560
Figure8 <- create_figure_5(
  km_plot_list = all_km_plots_figure8,
  ncol = 2,
  nrow = 2,
  labels = c("a",'b'), # A, B, C, D, E, F
  
  
  list_of_titles = NULL, 
  list_of_custom_pvals = pvals_list, 
  original_pval_layer_index = 4, 
  
  
  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)



#-------------Figure 9 

all_km_plots_figure9 <- list(
  TNC_KM_ALL,
  TNC_KM_STAD
)

titles_list <- list(
  NULL, 
  NULL 
)

pvals_list <- list(
  'p = 0.0001',       
  'p = 0.001',       
 
)
 #   780x 340
Figure9 <- create_figure_5(
  km_plot_list = all_km_plots_figure9,
  ncol = 2,
  nrow = 1,
  labels = c("a"), # A, B, C, D, E, F
  
  
  list_of_titles = NULL, 
  list_of_custom_pvals = pvals_list, 
  original_pval_layer_index = 4, 
  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)

#Figure 10-------------

MGST2_CDH1_BMP4_GPX8_ALL_Cindex
MGST2_CDH1_BMP4_GPX8_PAAD_Cindex

plot_list_fig10 <- list(
  MGST2_CDH1_BMP4_GPX8_ALL_Cindex, 
  MGST2_CDH1_BMP4_GPX8_PAAD_Cindex
)
titles_fig10 <- c(
  "Gemcitabine Samples (C-Index)",  
  "Gemcitabine PAAD Samples (C-Index)" 
)
# 4.#1000x600
Figure10 <- create_figure_2(
  plot_list = plot_list_fig10,
  list_of_titles = titles_fig10,
  ncol = 1,
  nrow = 2,
  labels = c("A", "B")
)

#Figure 11---------
all_km_plots_figure11 <- list(
  BAMBI_KM_ALL, 
  BAMBI_CNA_KM_ALL, 
  BAMBI_PAAD_KM_ALL,
  BAMBI_CNA_PAAD_KM_ALL
)

titles_list <- list(
  NULL, 
  NULL, 
  NULL, 
  NULL,
  NULL,
  NULL
)

pvals_list <- list(
  'p = 0.0003',        
  'p = 0.003',        
  'p = 0.0003',        
  'p = 0.01'
)
 #  720x 560
Figure11 <- create_figure_5(
  km_plot_list = all_km_plots_figure11,
  ncol = 2,
  nrow = 2,
  labels = c("a",'b'), # A, B, C, D, E, F

  list_of_titles = NULL, 
  list_of_custom_pvals = pvals_list, 
  original_pval_layer_index = 4, 
  
  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)
#-----Figure12

all_km_plots_figure12 <- list(
  COL18A1_KM_ALL, 
  DLC1_KM_ALL,
  BLM_KM_ALL,
  COL18A1_PAAD_KM_ALL,
  DLC1_PAAD_KM_ALL,
  BLM_PAAD_KM_ALL
)
titles_list <- list(
  NULL, 
  NULL, 
  NULL, 
  NULL,
  NULL,
  NULL,
  NULL,
  NULL
)

pvals_list <- list(
  'p = 0.02',      
  'p = 0.04',       
   NULL,        
  'p = 0.006', 
   NULL,
   NULL
)

 #  1180x 620
Figure12 <- create_figure_5(
  km_plot_list = all_km_plots_figure12,
  ncol = 3,
  nrow = 2,
  labels = c("a",'b','c','d'), # A, B, C, D, E, F
  
 
  list_of_titles = NULL,
  list_of_custom_pvals = pvals_list, 
  original_pval_layer_index = 4, 
  

  pval_x = 1000, 
  pval_y = 0.15,
  pval_size = 3
  
)


