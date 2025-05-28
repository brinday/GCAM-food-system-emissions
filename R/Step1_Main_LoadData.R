
# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)
library(purrr)
library(patchwork)
library(RColorBrewer)
library(ggpattern)


source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_DATA <- "data"
DIR_OUTPUT <- "output"
DIR_MODULE <- "figures"

May2025 <- readRDS(file.path(DIR_OUTPUT, "ProjectRDS", paste0("May2025", ".RDS")))
May2025 %>% names

ScenarioMap <- readr::read_csv("data/maps/ScenMap.csv")


PluckBind <- function(.query, .Listnm = "May2025"){

  get(.Listnm) %>% purrr::pluck(.query) %>%
    select(-ss) %>% filter(year %in% 2015:2100) %>%
    # Join Scen Map to make scenario factor
    left_join_error_no_match(ScenarioMap, by = "scenario") %>%
    mutate(scenario = name) %>% select(-name, -scenid, -ExoFooddemand, -WasteRed, -MitigationScen, -DietScen)
}


ProcReg <- function(.df, reg_map_col = REG10_AR){
  .df %>%
    rename(region0 = region) %>%
    left_join_error_no_match(
      Regmapping %>%
        select(region0 = region, region = reg_map_col), by = "region0")
}

