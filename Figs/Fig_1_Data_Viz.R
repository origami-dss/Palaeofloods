## extract_time_series_from_raw_data

# Libraries  ----
library(tidyverse)
library(readr)
library("ggsci")   # special color tables
library("cowplot")

COL_EXP=pal_uchicago("default")(7)

# Read Data Sets  ----

floods_per_year = read_tsv("Data/floods_per_year.csv", show_col_types = FALSE)
floods_per_decade = read_tsv("Data/floods_per_decade.csv", show_col_types = FALSE)
floods_per_century = read_tsv("Data/floods_per_century.csv", show_col_types = FALSE)

floods_per_year %>% 
  mutate(time = varve_year-0.5, resolution = "Yearly", W = 0.7) %>% 
  select(!varve_year) %>% 
  relocate(time, .before = no_of_layers) -> fpy

floods_per_decade %>% 
  mutate(time = varve_decade*10 -5, resolution = "Decadal", W = 7) %>% 
  select(!varve_decade) %>% 
  relocate(time, .before = no_of_layers) -> fpd

floods_per_century %>% 
  mutate(time = varve_century*100 -50,  resolution = "Centennial", W = 70) %>% 
  relocate(time, .before = no_of_layers)  %>% 
  select(!varve_century) -> fpc
                             

Floods <- bind_rows(fpj, fpd, fpc) %>%
  mutate(resolution = factor(resolution, level=c ("Yearly", "Decadal", "Centennial"))) %>%
  mutate(no_of_layers = as.integer(no_of_layers))


cols <- c("Yearly" = COL_EXP[2], "Decadal" = COL_EXP[3],"Centennial" = COL_EXP[4])




fpy %>%
  ggplot(aes(x = time, y = no_of_layers, color = resolution, fill = resolution, width = W)) + 
  geom_col(alpha=0.7, size=0.25) +  
  annotate( 'text', x = 0, y = 3, label = "Yearly", fontface = 'bold',  size = 4.5, vjust=0, hjust=-0.2, color = COL_EXP[2]) +
  labs(x ="Relative Age [yr]", y = " ") +
  scale_x_continuous(breaks = (0:4)*2000, limits = c(-200, 9500), expand = c(0,0)) + 
  scale_y_continuous(breaks = (0:3), labels = c("  0","  1","  2","  3"), limits=c(-0.5,3.5), expand = c(0,0)) +
  scale_color_manual(values = cols, name = "Resolution", labels=c("Yearly", "Decadal", "Centennial")) +
  scale_fill_manual(values = cols, name = "Resolution", labels=c("Yearly", "Decadal", "Centennial")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") -> plot_fpy_orig
 
fpd %>%
  ggplot(aes(x = time, y = no_of_layers, color = resolution, fill = resolution, width = W)) + 
  geom_col(alpha = 0.7, size = 0.25) + 
  labs(x ="Relative Age [yr]", y = "No. of Floods per Time Interval") +
  scale_x_continuous(breaks = (0:4)*2000, limits = c(-200, 9500), expand = c(0,0)) + 
  scale_y_continuous(breaks = (0:3)*2, labels = c("  0"," 2","  4","  6"), limits = c(-0.5, 7.5),) + 
  scale_color_manual(values = cols, name = "Resolution", labels=c("Yearly", "Decadal", "Centennial")) +
  scale_fill_manual(values = cols, name = "Resolution", labels=c("Yearly", "Decadal", "Centennial")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") -> plot_fpd_orig


fpc %>%
  ggplot(aes(x = time, y = no_of_layers, color = resolution, fill = resolution, width = W)) + 
  geom_col(alpha=0.7, size=0.25) + 
  scale_y_continuous(breaks = (0:3)*10, limits = c(-0.5, 35.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(-200, 9500), expand = c(0,0), breaks = (0:4)*2000) + 
  labs(x ="Relative Age [yr]", y = " ") +
  scale_color_manual(values = cols, name = "Resolution", labels=c("Yearly", "Decadal", "Centennial")) +
  scale_fill_manual(values = cols, name = "Resolution", labels=c("Yearly", "Decadal", "Centennial")) +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") -> plot_fpc_orig

data_hist_fpj <- tibble(no_of_floods = 0:3, counts=tabulate(1+fpy$no_of_layers), resolution = "Yearly") 

data_hist_fpj %>% 
  ggplot(aes(x=no_of_floods, y=counts, color = resolution, fill = resolution)) + 
  geom_bar(stat = "identity", alpha = 0.7) + 
  geom_text(aes(label = counts, y = counts , hjust = -0.2)) +
  labs( x = " ") +
  scale_y_continuous(breaks = (0:4)*2000, limits=c(0,12000), expand = c(0,0)) + 
  scale_x_continuous(breaks = (0:3), labels = c("  0","  1","  2","  3"), limits=c(-0.5,3.5), expand = c(0,0)) +
  coord_flip() +
  scale_color_manual(values = cols, name="Resolution", labels = c("Yearly", "Decadal", "Centennial")) +
  scale_fill_manual(values = cols, name="Resolution", labels = c("Yearly", "Decadal", "Centennial")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x =element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_blank(),
        legend.position = "none") -> plot_hist_y

data_hist_fpd <- tibble(no_of_floods = 0:7, counts=tabulate(1+fpd$no_of_layers), resolution = "Decadal") 

data_hist_fpd %>% 
  ggplot(aes(x=no_of_floods, y=counts, color = resolution, fill = resolution)) + 
  geom_col(alpha = 0.7) + 
  geom_text(aes(label = counts, y = counts , hjust = -0.2)) +
  labs( x = " ") +
  scale_x_continuous(breaks = (0:3)*2, labels = c("  0","  2","  4","  6"), limits = c(-0.5, 7.5), expand = c(0,0)) +
  scale_y_continuous(breaks = (0:2)*1000, labels = c("  ","  ","  "), limits = c(0, 600), expand =c(0,0)) +
  coord_flip() +
  scale_color_manual(values = cols, name="Resolution", labels = c("Yearly", "Decadal", "Centennial")) +
  scale_fill_manual(values = cols, name="Resolution", labels = c("Yearly", "Decadal", "Centennial")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x =element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_blank(),
        legend.position = "none") -> plot_hist_d


data_hist_fpc <- tibble(no_of_floods = 2+ seq(0,30, by=5), counts=tabulate(1+floor((fpc$no_of_layers)/5)), resolution = "Centennial") 



data_hist_fpc %>% 
  ggplot(aes(x = no_of_floods, y=counts,  color = resolution, fill = resolution)) + 
  geom_col(alpha = 0.7) + 
  geom_text(aes(label = counts, y = counts, hjust = -0.2)) +
  scale_x_continuous(breaks = (0:3)*10, limits = c(-0.5, 35.5), expand = c(0,0)) +
  scale_y_continuous(breaks = (0:8)*50, labels = rep(" ",9), limits = c(0,42), expand = c(0,0)) + 
  coord_flip() +
  labs( y = "Counts") +
  scale_color_manual(values = cols, name="Resolution", labels = c("Yearly", "Decadal", "Centennial")) +
  scale_fill_manual(values = cols, name="Resolution", labels = c("Yearly", "Decadal", "Centennial")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
#      axis.text.x = element_blank(),
        plot.title = element_blank(),
        legend.position = "none") -> plot_hist_c

title <- ggdraw() +
      draw_label( "Temporal Succession and Histograms of a 9336-Year Palaeflood Record",
      fontface = 'bold',
      x = 0,
      size = rel(12),
      hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 7) )


ggdraw() +
  draw_plot(plot_fpc_orig, x = 0.01, y = 0.01, width = 0.68, height = 0.33)  +
  draw_plot(plot_fpd_orig, x = 0.01, y = 0.36, width = 0.68, height = 0.28) +
  draw_plot(plot_fpy_orig, x = 0.01, y = 0.66, width = 0.68, height = 0.28) +
  draw_plot(plot_hist_c, x = 0.71, y = 0.01, width = 0.28, height = 0.33)  +
  draw_plot(plot_hist_d, x = 0.71, y = 0.36, width = 0.28, height = 0.28) +
  draw_plot(plot_hist_y, x = 0.71, y = 0.66, width = 0.28, height = 0.28) +
  draw_plot(title, x = 0.06, y = 0.95, width = 0.94, height = 0.05) -> Fig_Data_and_Histograms
ggsave("Figs/Fig_Data_and_Histograms.pdf", width = 18, height = 18, units = "cm")

  draw_plot(plot_hist, x = 0.71, y = 0.01, width = 0.28, height = 0.98)


ggdraw() +
  draw_plot(plot_data_orig, x = 0.01, y = 0.01, width = 0.68, height = 0.98)  +
  draw_plot(plot_hist, x = 0.71, y = 0.01, width = 0.28, height = 0.98)   -> Fig_Data_and_Histograms
#  draw_plot_label( 
#    c("A", "B", "C", "D"),
#    c(0.01, 0.01, 0.33, 0.66),
#    c(0.99, 0.51, 0.51,0.51),
#    size = 15)
ggsave("Figs/Fig_Data_and_Histograms.pdf", width = 18, height = 18, units = "cm")



