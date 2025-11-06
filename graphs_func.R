
library(grid)
library(ggplot2)
library(gplots)
library(ggfortify)
library(ggpubr)
library(ggraph)
library(grid)
library(gridExtra)
library(survminer)
library(cowplot)

create_figure_2 <- function(
    plot_list,
    list_of_titles,
    ncol = 2,
    nrow = 2,
    labels = c("a", "b", "c", "d"),
    
    # Tema (Theme) Parametreleri
    title_size = 14,
    text_face = 'bold',
    axis_title_y_size = 9,
    axis_text_y_size = 9,
    axis_text_x_size = 9,
    axis_title_y_face = 'bold',
    strip_text_size = 9,
    
    # Katman (Layer) Parametreleri
    # UYARI: Bu ayarlar plot'taki layer sırasına bağımlıdır.
    # layer 1 = geom_bar, layer 2 = geom_text olduğunu varsayar.
    bar_width = 0.7,
    bar_label_size = 3,
    bar_label_vjust = -0.2
) {
  
  # Girdi kontrolü
  if (length(plot_list) != length(list_of_titles)) {
    stop("Hata: 'plot_list' ve 'list_of_titles' aynı sayıda elemana sahip olmalı.")
  }
  
  modified_plots <- list() # Değiştirilmiş plotları saklamak için boş liste
  
  # Tüm plotlar üzerinde döngü ile değişiklikleri yap
  for (i in 1:length(plot_list)) {
    
    p <- plot_list[[i]]
    
    # 1. Başlığı ata
    p$labels$title <- list_of_titles[[i]]
    
    # 2. Tema (Theme) ayarlarını uygula
    p$theme$plot.title$size <- title_size
    p$theme$text$face <- text_face
    p$theme$axis.title.y$size <- axis_title_y_size
    p$theme$axis.text.y$size <- axis_text_y_size
    p$theme$axis.text.x$size <- axis_text_x_size
    p$theme$axis.title.y$face <- axis_title_y_face
    
    # strip.text (facet başlığı) her plot'ta olmayabilir, NULL kontrolü
    if (!is.null(p$theme$strip.text)) {
      p$theme$strip.text$size <- strip_text_size
    }
    
    # 3. Katman (Layer) ayarlarını uygula
    # Bu kısım, plot'un yapısına (layer 1'in bar, 2'nin text olması)
    # bağımlı olduğu için hassastır.
    
    # Layer 1 (Bar genişliği)
    if (length(p$layers) >= 1 && !is.null(p$layers[[1]]$geom_params$width)) {
      p$layers[[1]]$geom_params$width <- bar_width
    }
    
    # Layer 2 (Yazı boyutu)
    if (length(p$layers) >= 2 && !is.null(p$layers[[2]]$aes_params$size)) {
      p$layers[[2]]$aes_params$size <- bar_label_size
    }
    
    # Layer 2 (Yazı konumu)
    if (length(p$layers) >= 2 && !is.null(p$layers[[2]]$aes_params$vjust)) {
      p$layers[[2]]$aes_params$vjust <- bar_label_vjust
    }
    
    # Değiştirilmiş plot'u listeye ekle
    modified_plots[[i]] <- p
  }
  
  # 4. Tüm değiştirilmiş plot'ları plot_grid ile birleştir
  final_figure <- plot_grid(
    plotlist = modified_plots,
    ncol = ncol,
    nrow = nrow,
    labels = labels
  )
  
  return(final_figure)
}

#Figure 3 


create_figure_3 <- function(
    hr_plot,
    cindex_plot,
    main_title,
    layout = "horizontal",
    
    # === YENİ PARAMETRELER ===
    relative_widths = c(2, 1),  # HR plot'a %66, C-Index'e %33 yer ver
    relative_heights = c(1, 1), # Dikeyde varsayılan eşit yükseklik
    # ========================
    
    # Ana Başlık Ayarları
    main_title_size = 11,
    main_title_face = "bold",
    
    # === HR Plot Parametreleri ===
    hr_axis_title_y_size = 9,
    hr_axis_title_y_face = 'bold',
    hr_strip_text_size = 9,
    hr_strip_text_face = 'bold',
    hr_axis_text_x_size = 9,
    hr_axis_text_y_size = 9,
    hr_plot_margin = unit(c(-0.1, 0.1, 0.1, 0.1), "cm"),
    hr_label_size = 3,
    hr_label_vjust = -0.2,
    
    # === C-Index Plot Parametreleri ===
    cindex_axis_title_y_size = 9,
    cindex_axis_title_y_face = 'bold',
    cindex_axis_text_y_size = 9,
    cindex_axis_text_x_size = 9,
    cindex_strip_text_size = 9,
    cindex_plot_margin = unit(c(-0.1, 0.1, 0.1, 0.1), "cm"),
    cindex_bar_width = 0.5,
    cindex_label_size = 3,
    cindex_label_vjust = -0.2
) {
  
  # --- 1. HR Plot'u Düzenle ---
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
  
  # --- 2. C-Index Plot'u Düzenle ---
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
  
  # --- 3. Ana Başlığı Oluştur ---
  title_grob <- textGrob(
    main_title, 
    gp = gpar(fontsize = main_title_size, fontface = main_title_face)
  )
  
  # --- 4. Grafikleri Birleştir (GÜNCELLENMİŞ BÖLÜM) ---
  
  if (layout == "vertical") {
    # Dikey (alt alta) - relative_heights kullan
    final_plot <- grid.arrange(
      p_hr, p_cindex, 
      nrow = 2, 
      top = title_grob, 
      heights = relative_heights # EKLENDİ
    )
  } else {
    # Yatay (yan yana) - relative_widths kullan
    final_plot <- grid.arrange(
      p_hr, p_cindex, 
      ncol = 2, 
      top = title_grob, 
      widths = relative_widths # EKLENDİ
    )
  }
  
  return(final_plot)
}


create_figure_5 <- function(
    km_plot_list,
    ncol = 3,
    nrow = 2,
    labels = "AUTO", # A, B, C...
    
    # Başlık ve P-value (Opsiyonel)
    list_of_titles = NULL,
    list_of_custom_pvals = NULL,
    original_pval_layer_index = 4, # ggsurvplot'un p-value katmanı (tahmin)
    pval_x = 500,
    pval_y = 0.2,
    pval_size = 3,
    pval_face = 'bold',
    
    # Genel Etiket ve Tema
    x_label = 'Days',
    line_size = 0.8,
    plot_title_size = 10,
    axis_title_y_size = 9,
    axis_title_x_size = 9,
    axis_text_y_size = 8,
    axis_text_x_size = 8,
    legend_text_size = 7,
    legend_ncol = 2,
    legend_nrow = 2
) {
  
  modified_plots <- list() # Değiştirilmiş plotları (ggplot objeleri) saklamak için
  
  for (i in 1:length(km_plot_list)) {
    
    # 1. Orijinal objeyi al
    g_surv <- km_plot_list[[i]]
    
    # ggsurvplot bir liste döner, bize $plot lazım
    if (!is.ggplot(g_surv) && is.list(g_surv) && "plot" %in% names(g_surv)) {
      p <- g_surv$plot
    } else if (is.ggplot(g_surv)) {
      p <- g_surv # Zaten bir ggplot objesi ise
    } else {
      stop(paste("Liste elemanı", i, "bir ggsurvplot objesi veya ggplot objesi değil."))
    }
    
    # 2. Özel P-value işle (Eğer istenmişse)
    # Bu KISIM, tema ayarlarından ÖNCE yapılmalı
    custom_pval <- NULL
    if (!is.null(list_of_custom_pvals) && i <= length(list_of_custom_pvals)) {
      custom_pval <- list_of_custom_pvals[[i]]
    }
    
    if (!is.null(custom_pval)) {
      # a) Orijinal p-value katmanını kaldır
      if (!is.null(original_pval_layer_index) && length(p$layers) >= original_pval_layer_index) {
        cat("Plot", i, ": Orijinal p-value katmanı (", original_pval_layer_index, ") kaldırılıyor.\n")
        p$layers <- p$layers[-original_pval_layer_index]
      }
      
      # b) Yeni p-value'yu 'annotate' ile ekle
      p <- p + annotate(
        "text", 
        x = pval_x, y = pval_y, 
        label = custom_pval, 
        size = pval_size,
        fontface = pval_face
      )
    }
    
    # 3. Katman (Layer) Ayarları
    # Layer 1 (Survival eğrisi kalınlığı)
    if (length(p$layers) >= 1 && !is.null(p$layers[[1]]$aes_params$size)) {
      p$layers[[1]]$aes_params$size <- line_size
    }
    
    # 4. Tema (Theme) Ayarları
    p <- p + 
      theme_survminer() + # Tutarlılık için baz temayı uygula
      theme(
        plot.title = element_text(size = plot_title_size, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = axis_title_y_size, face = 'bold'),
        axis.title.x = element_text(size = axis_title_x_size, face = 'bold'),
        axis.text.y = element_text(size = axis_text_y_size, face = 'bold'),
        axis.text.x = element_text(size = axis_text_x_size, face = 'bold'),
        legend.title = element_blank(),
        legend.text = element_text(size = legend_text_size),
        legend.position = "top"
        # 'tex' argümanı 'element_text' içinde geçerli değil, kaldırıldı.
      ) +
      guides(
        color = guide_legend(ncol = legend_ncol, nrow = legend_nrow),
        fill = guide_legend(ncol = legend_ncol, nrow = legend_nrow)
      )
    
    # 5. Başlık ve Etiketler (Opsiyonel)
    labs_to_add <- list()
    
    # Özel X ekseni etiketi
    if (!is.null(x_label)) {
      labs_to_add$x <- x_label
    }
    
    # Özel Başlık
    custom_title <- NULL
    if (!is.null(list_of_titles) && i <= length(list_of_titles)) {
      custom_title <- list_of_titles[[i]]
    }
    
    if (!is.null(custom_title)) {
      labs_to_add$title <- custom_title
    }
    
    # Toplu olarak 'labs' fonksiyonunu uygula
    if (length(labs_to_add) > 0) {
      p <- p + do.call(labs, labs_to_add)
    }
    
    # 6. Değiştirilmiş plot'u listeye ekle
    modified_plots[[i]] <- p
  }
  
  # 7. Tüm plot'ları cowplot ile birleştir
  final_figure <- plot_grid(
    plotlist = modified_plots,
    ncol = ncol,
    nrow = nrow,
    labels = labels
  )
  
  return(final_figure)
}




create_figure_7_top_row <- function(plot_A_raw, 
                                    plot_B_raw, 
                                    title_A, 
                                    title_B, 
                                    labels = c("A", "B")) {
  
  # İç (helper) fonksiyon: Plot'u güvenli şekilde stiller
  # Bu, senin manuel kodunu temel alır
  .style_fig7_cindex_plot <- function(p_raw, title_str) {
    
    p <- p_raw
    
    # Eğer plot $CNA listesi içindeyse, onu çıkar
    if (!is.ggplot(p) && is.list(p) && "CNA" %in% names(p)) {
      p <- p$CNA
    } else if (!is.ggplot(p)) {
      stop("Giriş objesi (", deparse(substitute(p_raw)), ") bir ggplot objesi değil.")
    }
    
    # 1. Başlık ve Güvenli Tema (Doğrudan $ ataması yerine + theme() ile)
    p <- p + 
      labs(title = title_str) +
      theme(
        plot.title = element_text(size = 12, face = "bold"), # Yüzü ekledim
        axis.title.y = element_text(size = 9, face = 'bold'),
        strip.text = element_text(size = 10, face = 'bold'),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )
    
    # 2. Güvenli Katman Ayarı (layer 2'nin geom_text olduğunu varsayar)
    try({
      p$layers[[2]]$aes_params$size <- 2.3
      p$layers[[2]]$aes_params$vjust <- -0.2
    }, silent = TRUE) # Hata verse bile devam et
    
    return(p)
  }
  
  # --- Ana Fonksiyon ---
  
  # 1. Plot A'yı Stille
  Plot_A_styled <- .style_fig7_cindex_plot(plot_A_raw, title_A)
  
  # 2. Plot B'yi Stille
  Plot_B_styled <- .style_fig7_cindex_plot(plot_B_raw, title_B)
  
  # 3. İkisini birleştir
  top_row_figure <- plot_grid(
    Plot_A_styled,
    Plot_B_styled,
    ncol = 2,
    nrow = 1,
    labels = labels
  )
  
  return(top_row_figure)
}


create_figure_7_bottom_plot <- function(plot_top, 
                                        plot_bottom, 
                                        common_title,
                                        title_size = 10,
                                        title_face = "bold") {
  
  # 1. Üst plot'u düzenle (Başlığı kaldır ve basit tema ayarı)
  p_top <- plot_top + 
    labs(title = element_blank()) +
    theme(
      # Resimden yola çıkarak bazı basit ayarlar
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 9),
      strip.text = element_text(size = 9, face = "bold")
    )
  
  # 2. Alt plot'u düzenle (Başlığı kaldır ve basit tema ayarı)
  p_bottom <- plot_bottom + 
    labs(title = element_blank()) +
    theme(
      axis.text.x = element_text(size = 7),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 9),
      strip.text = element_text(size = 9, face = "bold")
    )
  
  # 3. Ortak başlığı oluştur
  title_grob <- textGrob(
    common_title, 
    gp = gpar(fontsize = title_size, fontface = title_face)
  )
  
  # 4. grid.arrange ile dikey olarak birleştir
  combined_plot <- grid.arrange(
    p_top, 
    p_bottom,
    nrow = 2,
    top = title_grob
  )
  
  return(combined_plot)
}





