
## extract_time_series_from_raw_data

# Libraries  ----
library(tidyverse)
library(readr)

# Read raw data  ----

floods = read_tsv(
  file = "Data_Raw/floods_per_year_2018.txt",
  col_names = c("varve_year", "no_of_layers", "total_layer_thicknes"), 
  cols(varve_year = col_integer(), no_of_layers = col_integer(), total_layer_thicknes = col_double()), 
  skip = 9,
  show_col_types = FALSE
)


# Create Data Sets for further use  ----
## Floods per year ----


floods %>% 
  select(varve_year, no_of_layers) %>% 
  mutate(no_of_layers =   if_else(is.na(no_of_layers), 0, no_of_layers)) %>%
#  filter(varve_year < 4019 | varve_year >4083)-> floods_per_year
  mutate(no_of_layers = if_else(varve_year > 4018 & varve_year < 4084, NA, no_of_layers)) -> floods_per_year
  
## Floods per decade ----


floods_per_year %>% 
  mutate(varve_decade = 1+ floor((varve_year-1)/10)) %>%
  group_by(varve_decade)%>%
     summarize(no_of_layers = sum(no_of_layers),
               n = n() ) %>%
  ungroup() %>%
  filter(n == 10) %>%
  select(!n) -> floods_per_decade


## Floods per century ----

floods_per_year %>% 
  mutate(varve_century = 1 + floor((varve_year-1)/100)) %>%
  group_by(varve_century)%>%
  summarize(no_of_layers = sum(no_of_layers), 
            n = n()) %>%
  ungroup() %>%
  filter(n == 100) %>%
  select(!n) -> floods_per_century

# Write data sets ----

write.table(floods_per_year , file = "Data/floods_per_year.csv", sep="\t", row.names=FALSE)
write.table(floods_per_decade , file = "Data/floods_per_decade.csv", sep="\t", row.names=FALSE)
write.table(floods_per_century , file = "Data/floods_per_century.csv", sep="\t", row.names=FALSE)







