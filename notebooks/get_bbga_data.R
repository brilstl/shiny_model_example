# libraries ----
library(tidyverse)
library(arrow)
library(geoarrow)
library(sf)

# read bbga and select variabeles ----

bbga <- read_csv2("https://api.data.amsterdam.nl/dcatd/datasets/rl6-35tFAw2Ljw/purls/2")

bbga <- 
  bbga %>%
  filter(variabele %in% c("IINKQ1_P",
                          "IINKQ2_P",
                          "IINKQ3_P",
                          "IINKQ4_P",
                          "IINKQ5_P",
                          "WCORHUUR_P",
                          "WPARTHUUR_P",
                          "WKOOP_P",
                          "BEVWOONDUUR")) %>%
  mutate(waarde = as.numeric(waarde)) 

# get labels ----

bbga_labels <- read_csv("https://api.data.amsterdam.nl/dcatd/datasets/rl6-35tFAw2Ljw/purls/4", 
                        col_select = c("Variabele", "Label_1")) %>%
  janitor::clean_names() %>%
  rename(label = label_1)

bbga <- 
  bbga_labels %>%
  right_join(bbga)

# get neigbourhoods of Amsterdam ----

geo <- 
  st_read("https://os-amsterdam.gitlab.io/datavisualisatie-onderzoek-en-statistiek/geo/wijken-2022-zw-topo.json",
               quiet = TRUE,
               crs = st_crs(4326))

geo <- 
  geo %>%
  left_join(bbga,
            by = c("code" = "gebiedcode15"))

# write geoparquet ----

write_geoparquet(geo, "data/bbga_geo.parquet")
