#

FYN_MYL9_FLNB_CYTOiPaclitaxel_ALL_C_index
FYN_MYL9_FLNB_BRCA_CIndex
TCF7L2_FYN_CYTOiPaclitaxel_ALL_C_index
FYN_TCF7L2_CIndex


plot_list_fig2 <- list(
  FYN_MYL9_FLNB_CYTOiPaclitaxel_ALL_C_index, 
  FYN_MYL9_FLNB_BRCA_CIndex,
  TCF7L2_FYN_CYTOiPaclitaxel_ALL_C_index,
  FYN_TCF7L2_CIndex
)

titles_fig2 <- c(
  'Paclitaxel Samples (C-Index: MYL9, FYN and FLNB)', 
  'Paclitaxel BRCA Samples (C-Index: MYL9, FYN and FLNB)', 
  'Paclitaxel Samples (C-Index: TCF7L2 and FYN)',
  'Paclitaxel BRCA Samples (C-Index: TCF7L2 and FYN)'  
)

Figure2 <- create_figure_2(
  plot_list = plot_list_fig2,
  list_of_titles = titles_fig2,
  ncol = 2,
  nrow = 2,
  labels = c("a", "b", "c", "d")
)

# print(Figure2) #  1368x768


#Figure 3

create_figure_3 <- function(
    hr_plot,
    cindex_plot,
    main_title,
    layout = "horizontal",
    
 
    main_title_size = 9,
    main_title_face = "bold",
    
   
    hr_axis_title_y_size = 6,
    hr_axis_title_y_face = 'bold',
    hr_strip_text_size = 7,
    hr_strip_text_face = 'bold',
    hr_axis_text_x_size = 5,
    hr_axis_text_y_size = 6,
    hr_plot_margin = unit(c(-0.1, 0.1, 0.1, 0.1), "cm"),
    hr_label_size = 1.8,
    hr_label_vjust = -0.2,
    
  
    cindex_axis_title_y_size = 7,
    cindex_axis_title_y_face = 'bold',
    cindex_axis_text_y_size = 6,
    cindex_axis_text_x_size = 7,
    cindex_strip_text_size = 8,
    cindex_plot_margin = unit(c(-0.1, 0.1, 0.1, 0.1), "cm"),
    cindex_bar_width = 0.5,
    cindex_label_size = 2.4,
    cindex_label_vjust = -0.2
) {
  

  p_hr <- hr_plot
  

  p_hr$labels$title <- element_blank()
  

  p_hr$theme$axis.title.y$size <- hr_axis_title_y_size
  p_hr$theme$axis.title.y$face <- hr_axis_title_y_face
  p_hr$theme$axis.text.x$size <- hr_axis_text_x_size
  p_hr$theme$axis.text.y$size <- hr_axis_text_y_size
  p_hr$theme$plot.margin <- hr_plot_margin
  
  
  if (!is.null(p_hr$theme$strip.text)) {
    p_hr$theme$strip.text$size <- hr_strip_text_size
    p_hr$theme$strip.text$face <- hr_strip_text_face
  }
  
 
  if (length(p_hr$layers) >= 2 && !is.null(p_hr$layers[[2]]$aes_params$size)) {
    p_hr$layers[[2]]$aes_params$size <- hr_label_size
  }
  if (length(p_hr$layers) >= 2 && !is.null(p_hr$layers[[2]]$aes_params$vjust)) {
    p_hr$layers[[2]]$aes_params$vjust <- hr_label_vjust
  }

 
  p_cindex <- cindex_plot
  

  p_cindex$labels$title <- element_blank()
  

  p_cindex$theme$axis.title.y$size <- cindex_axis_title_y_size
  p_cindex$theme$axis.title.y$face <- cindex_axis_title_y_face
  p_cindex$theme$axis.text.y$size <- cindex_axis_text_y_size
  p_cindex$theme$axis.text.x$size <- cindex_axis_text_x_size
  p_cindex$theme$plot.margin <- cindex_plot_margin
  

  if (!is.null(p_cindex$theme$strip.text)) {
    p_cindex$theme$strip.text$size <- cindex_strip_text_size
  }
  

  if (length(p_cindex$layers) >= 1 && !is.null(p_cindex$layers[[1]]$geom_params$width)) {
    p_cindex$layers[[1]]$geom_params$width <- cindex_bar_width
  }
  if (length(p_cindex$layers) >= 2 && !is.null(p_cindex$layers[[2]]$aes_params$size)) {
    p_cindex$layers[[2]]$aes_params$size <- cindex_label_size
  }
  if (length(p_cindex$layers) >= 2 && !is.null(p_cindex$layers[[2]]$aes_params$vjust)) {
    p_cindex$layers[[2]]$aes_params$vjust <- cindex_label_vjust
  }
  

  title_grob <- textGrob(
    main_title, 
    gp = gpar(fontsize = main_title_size, fontface = main_title_face)
  )
  

  

  if (layout == "vertical") {

    final_plot <- grid.arrange(p_hr, p_cindex, nrow = 2, top = title_grob)
  } else {
    final_plot <- grid.arrange(p_hr, p_cindex, ncol = 2, top = title_grob)
  }
  
  return(final_plot)
}


#Figure 3
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
Figure3 <- plot_grid(
  Figure_3_LDHB_TCF,
  Figure_3_LDHB_TCF_BRCA,
  Figure_3_FYN_TCF, 
  Figure_3_FYN_TCF_BRCA,
  ncol = 2,
  nrow=2,
  labels = c("a", "b",'c','d') 
)

# 1000x600  vertical
Figure4 <- plot_grid(
  Figure_3_FYN_CXCR4,
  Figure_3_FYN_CXCR4_BRCA,
  ncol = 2,
  nrow=1,
  labels = c("a", "b") 
)

# 1000X 1100 horizontal
#Figure3 
Figure3_v2 <- plot_grid(
  Figure_3_LDHB_TCF,
  Figure_3_LDHB_TCF_BRCA,
  Figure_3_FYN_TCF, 
  Figure_3_FYN_TCF_BRCA,
  ncol = 1,
  nrow=4,
  labels = c("A", "B",'C','D') # 
)

# 1000x600  horizontal  ,new  1100X 1200
Figure4_v2 <- plot_grid(
  Figure_3_FYN_CXCR4,
  Figure_3_FYN_CXCR4_BRCA,
  Figure_4_FYN_CXCR4_TCF7L2,
  Figure_4_FYN_CXCR4_TCF7L2_BRCA,
  ncol = 1,
  nrow=4,
  labels = c("a", "b",'c','d') 
)

#Figure 5 ----------------------

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
Figure5 <- create_figure_5(
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

#Figure 6 ----------------------


all_km_plots_figure6 <- list(
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
Figure6 <- create_figure_5(
  km_plot_list = all_km_plots_figure6,
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

#-------Figure7----------
# Figure 7----------------
Figure7_Top_Row <- create_figure_7_top_row(
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
Figure7_Final <- plot_grid(
  Figure7_Top_Row, 
  plot_grid(Plot_C, Plot_D, ncol = 2, labels = c("C", "D")), 
  nrow = 2,
  ncol = 1,
  labels = c("A", ""), 
  rel_heights = c(1, 1.5),
  align = 'v'
)

#Figure8---------------------

all_km_plots_figure8 <- list(
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
Figure8 <- create_figure_5(
  km_plot_list = all_km_plots_figure8,
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

#----------Figure 9

all_km_plots_figure9 <- list(
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
Figure9 <- create_figure_5(
  km_plot_list = all_km_plots_figure9,
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



#-------------Figure 10 

all_km_plots_figure10 <- list(
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
Figure10 <- create_figure_5(
  km_plot_list = all_km_plots_figure10,
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

#Figure 11-------------

MGST2_CDH1_BMP4_GPX8_ALL_Cindex
MGST2_CDH1_BMP4_GPX8_PAAD_Cindex

plot_list_fig11 <- list(
  MGST2_CDH1_BMP4_GPX8_ALL_Cindex, 
  MGST2_CDH1_BMP4_GPX8_PAAD_Cindex
)
titles_fig11 <- c(
  "Gemcitabine Samples (C-Index)",  
  "Gemcitabine PAAD Samples (C-Index)" 
)
# 4.#1000x600
Figure11 <- create_figure_2(
  plot_list = plot_list_fig11,
  list_of_titles = titles_fig11,
  ncol = 1,
  nrow = 2,
  labels = c("A", "B")
)

#Figure 12---------
all_km_plots_figure12 <- list(
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
Figure12 <- create_figure_5(
  km_plot_list = all_km_plots_figure12,
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
#-----Figure13

all_km_plots_figure13 <- list(
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
Figure13 <- create_figure_5(
  km_plot_list = all_km_plots_figure13,
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


