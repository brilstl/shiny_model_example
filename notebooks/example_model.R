# libraries ----
library(tidymodels)
library(tidyverse)
library(arrow)

# get data -----

x <- 
  open_dataset("data/bbga_geo.parquet") %>%
  filter(jaar == 2019) %>%
  select(naam,
         variabele,
         waarde) %>%
  collect()

x <- 
  x %>%
  pivot_wider(
    values_from = waarde,
    names_from = variabele
  )

# model engine ----

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

# workflow object ----

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

# create recipe ----

lm_rec <- 
  recipe(BEVWOONDUUR ~ ., data = x) %>%
  update_role(naam, new_role = "ID") %>%
  step_interact(terms = ~ ends_with("HUUR_P"):starts_with("IINKQ"))

# add formula ----

lm_wflow <- 
  lm_wflow %>% 
  add_recipe(lm_rec) 

# fit model ----

fit_workflow <- 
  lm_wflow %>%
  fit(x)

write_rds(fit_workflow, "data/example_model.rds")

fit_workflow %>%
  extract_fit_parsnip() %>%
  vip::vi()


x %>%
  filter(naam == "Buikslotermeer") %>%
  predict(fit_workflow, new_data = ., type = "conf_int")

