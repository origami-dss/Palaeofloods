## extract_time_series_from_raw_data

# Libraries  ----
library(tidyverse)
library(readr)

# Read Data Sets  ----

floods_per_year = read_tsv("Data/floods_per_year.csv", show_col_types = FALSE)
floods_per_decade = read_tsv("Data/floods_per_decade.csv", show_col_types = FALSE)
floods_per_century = read_tsv("Data/floods_per_century.csv", show_col_types = FALSE)

floods_per_year %>% 
  mutate(time = varve_year-0.5, resolution = "Yearly") %>% 
  select(!varve_year) %>% 
  relocate(time, .before = no_of_layers) -> floods_per_year

floods_per_decade %>% 
  mutate(time = varve_decade*10 -5, resolution = "Decadal") %>% 
  select(!varve_decade) %>% 
  relocate(time, .before = no_of_layers) -> floods_per_decade

floods_per_century %>% 
  mutate(time = varve_century*10 -50,  resolution = "Centennial") %>% 
  relocate(time, .before = no_of_layers)  %>% 
  select(!varve_decade) -> floods_per_century
                             

Floods <- bind_cols(floods_per_year, floods_per_decade, floods_per_century)                             



