
# Load Libraries  ----
library(tidyverse)
library(readr)
library("ggsci")   # special color tables
library("cowplot")
library(grid)

COL_EXP=pal_uchicago("default")(7)
cols <- c("Yearly" = COL_EXP[2], "Decadal" = COL_EXP[3],"Centennial" = COL_EXP[4])


# Read Data Sets  ----

floods_per_year = read_tsv("Data/floods_per_year.csv", show_col_types = FALSE)
max_year= max(floods_per_year$varve_year)

floods_per_century = read_tsv("Data/floods_per_century.csv", show_col_types = FALSE)

## Definition of some helper data files ----
years = 1:max_year

x <- floods_per_year$no_of_layers
x[is.na(x)] = 0


x_data = rep(1,max_year)
x_data[is.na(floods_per_year$no_of_layers)] = 0

bandwidth = 250
filter_coeffs = dnorm(seq(-4,4,by=1./bandwidth)) 
length_filter = length(filter_coeffs)

## Getting prepared for fast filtering: zero padding at the beginning and end of the files ----
xx = c(rep(0,length_filter), x, rep(0,length_filter))
xx_data = c(rep(0,length_filter), x_data, rep(0,length_filter))

## Filter the data with a Gaussian kernel ----
# (note that the resulting  vectors have the original  (before padding with zeros) length) 

no_of_floods_per_bandwidth  <- (stats::filter(xx, filter_coeffs))[length_filter + (1:length(x1))]
effective_no_of_data_points = (stats::filter(xx_data, filter_coeffs))[length_filter + (1:length(x1))]

flood_rate_bw_250yrs = no_of_floods_per_bandwidth/effective_no_of_data_points

## Computation of confidence bands ----
CI_l = 0.5*qchisq(0.025,2*no_of_floods_per_bandwidth)/effective_no_of_data_points
CI_u = 0.5*qchisq(0.975,2*no_of_floods_per_bandwidth+2)/effective_no_of_data_points

# PLOT ----

## Tibble for flood density Plot ----
rate_floods_per_century <- tibble(Time = years, flood_rate_bw_250yrs = 100*flood_rate_bw_250yrs, lower_confidence_interval = 100*CI_l, upper_confidence_interval = 100*CI_u) 
  
## Plot of Upper Panel ----

rate_floods_per_century %>%
  ggplot(aes(x=Time, y= flood_rate_bw_250yrs)) +
  geom_ribbon(aes(x = Time, ymin = lower_confidence_interval, ymax = upper_confidence_interval), fill = COL_EXP[4], alpha=0.8) +
  geom_line(col = COL_EXP[1])+
  geom_hline(yintercept = 8.3, col=COL_EXP[1], linetype = "dashed") +
  annotate( 'text', x = 5100, y = 18.5, label = "Average Flood Rate: 8.3 Floods per 100 yr",  col = COL_EXP[1], size = 3.5, vjust = 0, hjust = 0 ) + 
  annotate( 'segment', x = 6000, y = 18, xend = 6000, yend = 9, col = COL_EXP[1], linewidth = 0.5, arrow = arrow(length = unit(0.25, 'cm'), angle = 22.5, type = "closed")) +
  annotate( 'text', x = 5300, y = 0.5, label = "65 year gap",  size = 3.5, vjust = 0.5, hjust = 0 ) + 
  annotate( 'segment', x = 5200, y = 0.5, xend = 4150, yend = 0.5, linewidth = 0.5, arrow = arrow(length = unit(0.25, 'cm'), angle = 22.5, type = "closed")) +
  labs(x ="Relative Age [yr]", y = " ") +
  scale_x_continuous(limits = c(-200, 9500), expand = c(0,0), breaks = (0:4)*2000) + 
  scale_y_continuous(breaks = (0:3)*10, limits = c(-0.5, 30.5), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") -> plot_smoothed_flood_rate


## Tibble for lower panel ----
floods_per_century %>% 
  mutate(time = varve_century*100 -50) %>% 
  relocate(time, .before = no_of_layers)  %>% 
  select(!varve_century) -> fpc


## Plot of lower panel ----
fpc %>%
  ggplot(aes(x = time, y = no_of_layers, width = 70)) + 
  geom_col(alpha = 0.8, size = 0.25, fill = COL_EXP[4]) +
  scale_y_continuous(breaks = (0:3)*10, limits = c(-0.5, 30.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(-200, 9500), expand = c(0,0), breaks = (0:4)*2000) + 
  labs(x ="Relative Age [yr]", y = " ") +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") -> plot_fpc_orig


## Plot of lower panel ----
fpc %>%
  ggplot(aes(x = time, y = no_of_layers, width = 70)) + 
  annotate('rect', xmin = 4019, xmax = 4083, ymin = 0, ymax = 90,  fill = "grey" ) + 
  scale_y_continuous(breaks = (0:9)*10, limits = c(-0.5, 90.5), expand = c(0,0), labels = rep("    ", 10)) +
  scale_x_continuous(limits = c(-200, 9500), expand = c(0,0), breaks = (0:4)*2000, labels = rep("  ", 5)) + 
  labs(x =" ", y = " ") +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none") -> plot_rect_gap


# Prepare pdf  ----

title <- ggdraw() +
      draw_label( "Flood Frequency Fluctuations",
      fontface = 'bold',
      x = 0,
      size = rel(14),
      hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 8) )

y_axis_title <- ggdraw() +
  draw_label( "Reconstructed Number of Floods per 100 Years",
  #            fontface = 'bold',
              angle = 90,
              x = 0,
              size = rel(12),
              hjust = 0.5, 
              vjust = 1) +
  theme(plot.margin = margin(0, 0, 0, 5) )



## gap  indication works s far just for pdfs with a size of 18 x 18 cm
rect_gap <- rectGrob(x = unit(0.473, "npc"), y = unit(0.078, "npc"),
                     width = unit(0.007, "npc"), height = unit(0.843, "npc"),
                     hjust = 0, vjust = 0,
                     default.units = "npc", name = NULL,
                     gp = gpar(fill = "skyblue4", alpha = 0.5))


# Write pdf  ----

ggdraw() +
  draw_plot(plot_fpc_orig, x = 0.02, y = 0.01, width = 0.97, height = 0.35)  +
  draw_plot(plot_smoothed_flood_rate, x = 0.02, y = 0.36, width = 0.97, height = 0.58) +
  draw_plot(plot_rect_gap, x = 0.02, y = 0.01, width = 0.97, height = 0.93) +
  draw_plot(title, x = 0.09, y = 0.95, width = 0.9, height = 0.05)  +
  draw_plot(y_axis_title, x = 0.01, y = 0.02, width = 0.05, height = 0.95)  
#  draw_grob(rect_gap) -> Fig_Flood_rate

ggsave("Figs/Fig_Floods_per_Century.pdf", width = 18, height = 18, units = "cm")






