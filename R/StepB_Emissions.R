# READ IN FILES -----------------------------------------------------------

## EMISSIONS
readr::read_csv("data/maps/GWP.csv") -> GWP
readr::read_csv("data/maps/EM_All_mapping.csv") -> EM_All_mapping

# ALL GHG EMISSIONS PROCESSING --------------------------------------------

"NCEM" %>% PluckBind() %>%
  bind_rows(
    "CLUC" %>% PluckBind() %>% Agg_reg(region, Units) %>% mutate(sector = "LULUCF", GHG = "CO2")
  ) %>%
  Agg_reg(region, GHG, Units) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  #mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, region, GHG1, year) %>%
  summarise(GHG_AR6 = sum(value * AR6all)/1000,
            GHG_AR5 = sum(value * AR5all)/1000,
            GHG_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  rename(value = GHG_AR6) %>%
  mutate(Units = "GtCO2e") %>%
  select(-GHG_AR5, -GHG_AR4) %>%
  na.omit() %>%
  Agg_reg(region, Units) -> GHG

# calculate the imbal between CEM and NCEM_sector, which is resource prod emissions
# for both CO2 and nonCO2
#ISSUE WITH RES PROD?
"NCEM" %>% PluckBind() %>%
  Agg_reg(region, GHG, Units) %>%
  rename(Total = value) %>%
  left_join("NCEM_sector" %>% PluckBind() %>% Agg_reg(region, GHG, Units),
            by = c("scenario", "region", "GHG", "Units", "year") ) %>%
  mutate(value = Total - value)  %>% select(-Total) %>%
  mutate(sector = "Resource production") %>%
  ## bind NCEM_sector
  bind_rows("NCEM_sector" %>% PluckBind() ) %>%
  ## bind CLUC
  bind_rows(
    "CLUC" %>% PluckBind() %>% Agg_reg(region, Units) %>% mutate(sector = "LULUCF", GHG = "CO2")
  ) %>%
  Agg_reg(region, GHG, Units, sector) -> EM_All

# generate mapping file
# EM_All_mapping <- EM_All %>%
#   select(sector) %>%
#   distinct() %>%
#   write.csv("EM_All_mapping.csv", row.names = F)

# Convert to CO2e
EM_All %>%
  mutate(GHG = replace(GHG, grepl("SO2_", GHG), "SO2_3")) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, region, sector, GHG1, year) %>%
  summarise(GHG_AR6 = sum(value * AR6all)/1000,
            GHG_AR5 = sum(value * AR5all)/1000,
            GHG_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  rename(value = GHG_AR6) %>%
  mutate(Units = "GtCO2e") %>%
  select(-GHG_AR5, -GHG_AR4) %>%
  mutate(GHG1 = case_when(
    GHG1 %in% c("CH4_AGR", "CH4_AWB") ~ "CH4_Ag",
    GHG1 %in% c("CH4") ~ "CH4_En",
    GHG1 %in% c("N2O_AGR", "N2O_AWB") ~ "N2O_Ag",
    GHG1 %in% c("N2O") ~ "N2O_En",
    TRUE ~ GHG1)) %>%
  Agg_reg(sector, GHG1, region, Units) %>%
  filter(year >= 2015, sector != "Other GHGs_UnMGMTLand") %>%
  mutate(year = as.integer(year)) %>%
  # removes CO2_FUG which is NA
  na.omit() %>% ungroup() -> EM_All_GtCO2e



# TOTAL CO2 ---------------------------------------------------------------

CO2 <- EM_All_GtCO2e %>%
  filter(GHG1 == "CO2")

CO2_GLO <- CO2 %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))




# FS WASTE EMISSIONS PROCESSING -------------------------------------------

source("R/StepA_FoodIntakeWaste.R")


pcFood %>%
  group_by(scenario, region, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), POP = first(POP), .groups = "drop") %>%
  mutate(waste = avail - intake, POP = POP / 1000000) ->
  Intake_Waste_pop

#obtain urban process by tech emissions
# only from landfills, waste_incineration, wastewater
WasteEM_tech <- "WasteEM" %>% PluckBind() %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  #mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, subsector, region, GHG1, year) %>%
  summarise(GHG_AR6 = sum(value * AR6all)/1000,
            GHG_AR5 = sum(value * AR5all)/1000,
            GHG_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  rename(value = GHG_AR6,
         sector = subsector) %>%
  mutate(Units = "GtCO2e") %>%
  select(-GHG_AR5, -GHG_AR4) %>%
  na.omit() %>%
  mutate(GHG1 = case_when(
    GHG1 %in% c("CH4_AGR", "CH4_AWB") ~ "CH4_Ag",
    GHG1 %in% c("CH4") ~ "CH4_En",
    GHG1 %in% c("N2O_AGR", "N2O_AWB") ~ "N2O_Ag",
    GHG1 %in% c("N2O") ~ "N2O_En",
    TRUE ~ GHG1)) %>%
  filter(sector %in% c("landfills", "waste_incineration", "wastewater"),
         GHG1 %in% c("CH4_En", "N2O_En")) %>%
  mutate(sector = replace(sector, sector == "landfills", "Landfills"),
         sector = replace(sector, sector == "waste_incineration", "Incineration"),
         sector = replace(sector, sector == "wastewater", "Wastewater")) %>%
  # For now, adding CH4 and N2O together, will apply same adjustments over both.
  group_by(scenario, sector, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(sector, value) %>%
  mutate(Landfills = if_else(is.na(Landfills), 0, Landfills))

#Use intake and waste per capita growth rates to more accurately reflect waste emissions
WasteEM_wide <- Intake_Waste_pop %>% filter(year >= 2020) %>%
  group_by(scenario, region) %>%
  mutate(r_intake = intake / intake[year == 2020],
         r_waste = waste / waste[year == 2020]) %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join_error_no_match(
    WasteEM_tech, by = c("scenario", "year", "region")) %>%
  mutate(Landfills = Landfills * r_waste,
         Incineration = Incineration * r_waste,
         Wastewater = Wastewater * r_intake) %>% ungroup() %>%
  select(scenario, region, year, intake, waste, Incineration, Landfills, Wastewater, POP)


WasteEM_wide %>%
  gather(sector, value, Incineration, Landfills, Wastewater) %>%
  mutate(Units = "GtCO2e") %>%
  select(scenario, region, sector, year, Units, value) ->
  WasteEM_updated



# WASTE EM (CH4) ----------------------------------------------------------


#obtain urban process by tech emissions
# only from landfills, waste_incineration, wastewater
WasteEM_tech_CH4 <- "WasteEM" %>% PluckBind() %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  #mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, subsector, region, GHG1, year) %>%
  summarise(GHG_AR6 = sum(value * AR6all)/1000,
            GHG_AR5 = sum(value * AR5all)/1000,
            GHG_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  rename(value = GHG_AR6,
         sector = subsector) %>%
  mutate(Units = "GtCO2e") %>%
  select(-GHG_AR5, -GHG_AR4) %>%
  na.omit() %>%
  mutate(GHG1 = case_when(
    GHG1 %in% c("CH4_AGR", "CH4_AWB") ~ "CH4_Ag",
    GHG1 %in% c("CH4") ~ "CH4_En",
    GHG1 %in% c("N2O_AGR", "N2O_AWB") ~ "N2O_Ag",
    GHG1 %in% c("N2O") ~ "N2O_En",
    TRUE ~ GHG1)) %>%
  filter(sector %in% c("landfills", "waste_incineration", "wastewater"),
         GHG1 %in% c("CH4_En")) %>%
  mutate(sector = replace(sector, sector == "landfills", "Landfills"),
         sector = replace(sector, sector == "waste_incineration", "Incineration"),
         sector = replace(sector, sector == "wastewater", "Wastewater")) %>%
  # For now, adding CH4 and N2O together, will apply same adjustments over both.
  group_by(scenario, sector, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(sector, value) %>%
  mutate(Landfills = if_else(is.na(Landfills), 0, Landfills))

#Use intake and waste per capita growth rates to more accurately reflect waste emissions
WasteEM_wide_CH4 <- Intake_Waste_pop %>% filter(year >= 2020) %>%
  group_by(scenario, region) %>%
  mutate(r_intake = intake / intake[year == 2020],
         r_waste = waste / waste[year == 2020]) %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join_error_no_match(
    WasteEM_tech_CH4, by = c("scenario", "year", "region")) %>%
  mutate(Landfills = Landfills * r_waste,
         Incineration = Incineration * r_waste,
         Wastewater = Wastewater * r_intake) %>% ungroup() %>%
  select(scenario, region, year, intake, waste, Incineration, Landfills, Wastewater, POP)


WasteEM_wide_CH4 %>%
  gather(sector, value, Incineration, Landfills, Wastewater) %>%
  mutate(Units = "GtCO2e") %>%
  select(scenario, region, sector, year, Units, value) ->
  WasteEM_updated_CH4


# WASTE EM (N2O) ----------------------------------------------------------


#obtain urban process by tech emissions
# only from landfills, waste_incineration, wastewater
WasteEM_tech_N2O <- "WasteEM" %>% PluckBind() %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  #mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, subsector, region, GHG1, year) %>%
  summarise(GHG_AR6 = sum(value * AR6all)/1000,
            GHG_AR5 = sum(value * AR5all)/1000,
            GHG_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  rename(value = GHG_AR6,
         sector = subsector) %>%
  mutate(Units = "GtCO2e") %>%
  select(-GHG_AR5, -GHG_AR4) %>%
  na.omit() %>%
  mutate(GHG1 = case_when(
    GHG1 %in% c("CH4_AGR", "CH4_AWB") ~ "CH4_Ag",
    GHG1 %in% c("CH4") ~ "CH4_En",
    GHG1 %in% c("N2O_AGR", "N2O_AWB") ~ "N2O_Ag",
    GHG1 %in% c("N2O") ~ "N2O_En",
    TRUE ~ GHG1)) %>%
  filter(sector %in% c("landfills", "waste_incineration", "wastewater"),
         GHG1 %in% c("N2O_En")) %>%
  mutate(sector = replace(sector, sector == "landfills", "Landfills"),
         sector = replace(sector, sector == "waste_incineration", "Incineration"),
         sector = replace(sector, sector == "wastewater", "Wastewater")) %>%
  # For now, adding CH4 and N2O together, will apply same adjustments over both.
  group_by(scenario, sector, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  spread(sector, value) %>%
  mutate(Landfills = if_else(is.na(Landfills), 0, Landfills))

#Use intake and waste per capita growth rates to more accurately reflect waste emissions
WasteEM_wide_N2O <- Intake_Waste_pop %>% filter(year >= 2020) %>%
  group_by(scenario, region) %>%
  mutate(r_intake = intake / intake[year == 2020],
         r_waste = waste / waste[year == 2020]) %>%
  mutate(scenario = as.character(scenario)) %>%
  left_join_error_no_match(
    WasteEM_tech_N2O, by = c("scenario", "year", "region")) %>%
  mutate(Landfills = Landfills * r_waste,
         Incineration = Incineration * r_waste,
         Wastewater = Wastewater * r_intake) %>% ungroup() %>%
  select(scenario, region, year, intake, waste, Incineration, Landfills, Wastewater, POP)


WasteEM_wide_N2O %>%
  gather(sector, value, Incineration, Landfills, Wastewater) %>%
  mutate(Units = "GtCO2e") %>%
  select(scenario, region, sector, year, Units, value) ->
  WasteEM_updated_N2O

# FOOD SYSTEM EMISSIONS (BY SECTOR) ---------------------------------------------------
#
# FS_Ag_EM <- EM_All_GtCO2e %>%
#   filter(GHG1 %in% c("CH4_Ag", "N2O_Ag")) %>%
#   mutate(sector = GHG1) %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: CH4"),
#          sector = replace(sector, sector == "N2O_Ag", "Ag: N2O")) %>%
#   group_by(scenario, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   ungroup() %>%
#   group_by(scenario) %>%
#   Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2020)



FS_Crop_EM <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag", "N2O_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat", "biomass")) %>%
  mutate(sector = GHG1) %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: Crop: CH4"),
         sector = replace(sector, sector == "N2O_Ag", "Ag: Crop: N2O"))

FS_Livestock_EM <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag", "N2O_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  mutate(sector = GHG1) %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: Animal: CH4"),
         sector = replace(sector, sector == "N2O_Ag", "Ag: Animal: N2O"))

FS_UnmgdLand_EM <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_UnMGMTLand", "N2O_UnMGMTLand")) %>%
  mutate(sector = "Forest fires") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# # 68% of urban processes
# FS_Waste_EM <- EM_All_GtCO2e %>%
#   filter(sector == "urban processes", GHG1 %in% c("CH4_En", "N2O_En")) %>%
#   mutate(value = value*0.68,
#          sector = "Waste") %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value))

# Now using updated waste emissions, see section above
# 68% of total waste emissions - TO-DO : specify different fractions by region??
FS_Waste_EM <- WasteEM_updated%>%
  mutate(value = value*0.68,
         sector = "Waste") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Quick check: comparing old unadjusted emissions with new
 FS_Waste_EM_OLD <- EM_All_GtCO2e %>%
   filter(sector == "urban processes", GHG1 %in% c("CH4_En", "N2O_En")) %>%
   mutate(value = value*0.68,
          sector = "Waste") %>%
   group_by(scenario, region, sector, Units, year) %>%
   dplyr::summarise(value = sum(value))

#38% of F-gases
FS_Ind_EM <- EM_All_GtCO2e %>%
  filter(GHG1 == "Other GHGs") %>%
  mutate(value = value*0.38,
         sector = "Retail: Refrigeration") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on farm
FS_EnergyFarm_EM <- EM_All_GtCO2e %>%
  filter(sector == "agricultural energy use", GHG1 != "Other GHGs") %>%
  mutate(sector = "Energy: On-farm") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on Food proc
FS_FoodProc_EM <-  EM_All_GtCO2e %>%
  filter(sector == "process heat food processing", GHG1 != "Other GHGs") %>%
  mutate(sector = "Energy: Food processing") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Fertilizer
FS_Fert_EM <- EM_All_GtCO2e %>%
  filter(sector == "ammonia", GHG1 != "Other GHGs") %>%
  mutate(sector = "Energy: Fertilizer") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

FS_LUC_EM <- CLUC_reg <- "CLUC" %>%
  PluckBind() %>%
  mutate(sector = "LULUCF",
         value = value / 1000 * 44 / 12,
         Units = "GtCO2e") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Food System Emissions
FS_EM <- bind_rows(#FS_Ag_EM,
  FS_Crop_EM,
  FS_Livestock_EM,
  FS_UnmgdLand_EM,
  FS_Waste_EM,
  FS_Ind_EM,
  FS_Fert_EM,
  FS_LUC_EM,
  FS_FoodProc_EM,
  FS_EnergyFarm_EM) %>% ungroup()

FS_EM_GLO <- FS_EM %>%
  group_by(scenario, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "Global")

cum_FS_EM <- FS_EM %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2020)

cum_FS_EM_GLO <- cum_FS_EM %>%
  group_by(scenario, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

cum_FS_EM_total <- cum_FS_EM %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))


cum_Ag_FS_EM_total <- cum_FS_EM %>%
  filter(sector %in% c("Ag: Crop: CH4", "Ag: Crop: N2O", "Ag: Animal: CH4", "Ag: Animal: N2O")) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value))






# FS EM BY SECTOR AND INCOME GROUP ----------------------------------------

FS_EM_sector_WBIncome <- FS_EM %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, sector, region, Units, year) %>%
  dplyr::summarise(value = sum(value))



# ANNUAL EMISSIONS BY INCOME GROUP ----------------------------------------

FS_EM_WBIncome_ProdBased <- FS_EM %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value))


# CUMULATIVE FOOD SYSTEM EMISSIONS (PRODUCTION-BASED BY INCOME GROUP) ----------------

# Emissions by income group

cum_FS_EM_WBIncome_ProdBased <- cum_FS_EM %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value))


# COMPARISON TOTAL GLO GHG EMISSIONS VS NON-FOOD SYSTEM EMISSIONS -------------------


FS_EM_WBIncome <- FS_EM %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value))

total_FS_EM_GLO_WBIncome <- bind_rows(FS_EM_GLO, FS_EM_WBIncome) %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(system = "Food")

GHG_WBIncome <- GHG %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value))

GHG_GLO <- GHG %>%
  mutate(region = "Global") %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value))

total_nonFS_EM_GLO_WBIncome <- bind_rows(GHG_GLO, GHG_WBIncome) %>%
  left_join_error_no_match(total_FS_EM_GLO_WBIncome, by = c("scenario", "region", "Units", "year"),
                           suffix = c(".total", ".food")) %>%
  mutate(value = value.total - value.food,
         system =  "Non-Food") %>%
  ungroup() %>%
  select(scenario, region, Units, year, value, system)

cum_total_EM_GLO_WBIncome <- bind_rows(total_FS_EM_GLO_WBIncome,
                              total_nonFS_EM_GLO_WBIncome) %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(system = "Total") %>%
  bind_rows(total_FS_EM_GLO_WBIncome,
            total_nonFS_EM_GLO_WBIncome) %>%
  group_by(scenario, system, region, Units) %>%
  Fill_annual(CUMULATIVE = TRUE, CUM_YEAR_START = 2020)

cum_FS_EM_GLO_WBIncome <- total_FS_EM_GLO_WBIncome %>%
  group_by(scenario, system, region, Units) %>%
  Fill_annual(CUMULATIVE = TRUE, CUM_YEAR_START = 2020)

cum_nonFS_EM_GLO_WBIncome <- total_nonFS_EM_GLO_WBIncome %>%
  group_by(scenario, system, region, Units) %>%
  Fill_annual(CUMULATIVE = TRUE, CUM_YEAR_START = 2020)

avg_ann_total_EM_GLO_WBIncome <- cum_total_EM_GLO_WBIncome %>%
  mutate(value = value/(year+1 - 2020),
         Units = "GtCO2e/yr")


# FOOD SYSTEM EM AS A SHARE OF TOTAL --------------------------------------

FS_EM_GLO_WBIncome_share <- bind_rows(total_FS_EM_GLO_WBIncome,
                                      total_nonFS_EM_GLO_WBIncome) %>%
  group_by(scenario, region, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  left_join(total_FS_EM_GLO_WBIncome, by = c("scenario", "region", "Units", "year"), suffix = c(".total", ".FS")) %>%
  mutate(share = value.FS/value.total)

cum_FS_EM_GLO_WBIncome_share <- cum_total_EM_GLO_WBIncome %>%
  filter(system == "Total") %>%
  left_join(cum_FS_EM_GLO_WBIncome, by = c("scenario", "region", "Units", "year"), suffix = c(".total", ".FS")) %>%
  mutate(share = value.FS/value.total)

# FOOD SYSTEM EMISSIONS (CH4 ONLY) ---------------------------------------------------
#
# FS_Ag_EM <- EM_All_GtCO2e %>%
#   filter(GHG1 %in% c("CH4_Ag", "N2O_Ag")) %>%
#   mutate(sector = GHG1) %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: CH4"),
#          sector = replace(sector, sector == "N2O_Ag", "Ag: N2O")) %>%
#   group_by(scenario, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   ungroup() %>%
#   group_by(scenario) %>%
#   Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2020)



FS_Crop_EM_CH4 <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat", "biomass")) %>%
  mutate(sector = GHG1) %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: Crop: CH4"))

FS_Livestock_EM_CH4 <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  mutate(sector = GHG1) %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: Animal: CH4"))

FS_UnmgdLand_EM_CH4 <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_UnMGMTLand")) %>%
  mutate(sector = "Forest fires") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# # 68% of urban processes
# FS_Waste_EM <- EM_All_GtCO2e %>%
#   filter(sector == "urban processes", GHG1 %in% c("CH4_En", "N2O_En")) %>%
#   mutate(value = value*0.68,
#          sector = "Waste") %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value))

# Now using updated waste emissions, see section above
# 68% of total waste emissions - TO-DO : specify different fractions by region??
FS_Waste_EM_CH4 <- WasteEM_updated_CH4 %>%
  mutate(value = value*0.68,
         sector = "Waste") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on farm
FS_EnergyFarm_EM_CH4 <- EM_All_GtCO2e %>%
  filter(sector == "agricultural energy use", GHG1 != "Other GHGs",
         GHG1 %in% c("CH4_En")) %>%
  mutate(sector = "Energy: On-farm") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on Food proc
FS_FoodProc_EM_CH4 <-  EM_All_GtCO2e %>%
  filter(sector == "process heat food processing", GHG1 != "Other GHGs",
         GHG1 %in% c("CH4_En")) %>%
  mutate(sector = "Energy: Food processing") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Fertilizer
FS_Fert_EM_CH4 <- EM_All_GtCO2e %>%
  filter(sector == "ammonia", GHG1 != "Other GHGs", GHG1 %in% c("CH4_En")) %>%
  mutate(sector = "Energy: Fertilizer") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Food System Emissions
FS_EM_CH4 <- bind_rows(#FS_Ag_EM,
  FS_Crop_EM_CH4,
  FS_Livestock_EM_CH4,
  FS_UnmgdLand_EM_CH4,
  FS_Waste_EM_CH4,
  FS_Fert_EM_CH4,
  FS_FoodProc_EM_CH4,
  FS_EnergyFarm_EM_CH4) %>% ungroup()

FS_EM_GLO_CH4 <- FS_EM_CH4 %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "CH4",
         system = "Food")


# FOOD SYSTEM EMISSIONS (N2O ONLY) ---------------------------------------------------
#
# FS_Ag_EM <- EM_All_GtCO2e %>%
#   filter(GHG1 %in% c("CH4_Ag", "N2O_Ag")) %>%
#   mutate(sector = GHG1) %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: CH4"),
#          sector = replace(sector, sector == "N2O_Ag", "Ag: N2O")) %>%
#   group_by(scenario, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   ungroup() %>%
#   group_by(scenario) %>%
#   Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2020)



FS_Crop_EM_N2O <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("N2O_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat", "biomass")) %>%
  mutate(sector = GHG1) %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = replace(sector, sector == "N2O_Ag", "Ag: Crop: N2O"))

FS_Livestock_EM_N2O <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("N2O_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  mutate(sector = GHG1) %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = replace(sector, sector == "N2O_Ag", "Ag: Animal: N2O"))

FS_UnmgdLand_EM_N2O <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("N2O_UnMGMTLand")) %>%
  mutate(sector = "Forest fires") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# # 68% of urban processes
# FS_Waste_EM <- EM_All_GtCO2e %>%
#   filter(sector == "urban processes", GHG1 %in% c("CH4_En", "N2O_En")) %>%
#   mutate(value = value*0.68,
#          sector = "Waste") %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value))

# Now using updated waste emissions, see section above
# 68% of total waste emissions - TO-DO : specify different fractions by region??
FS_Waste_EM_N2O <- WasteEM_updated_N2O %>%
  mutate(value = value*0.68,
         sector = "Waste") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on farm
FS_EnergyFarm_EM_N2O <- EM_All_GtCO2e %>%
  filter(sector == "agricultural energy use", GHG1 != "Other GHGs",
         GHG1 %in% c("N2O_En")) %>%
  mutate(sector = "Energy: On-farm") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on Food proc
FS_FoodProc_EM_N2O <-  EM_All_GtCO2e %>%
  filter(sector == "process heat food processing", GHG1 != "Other GHGs",
         GHG1 %in% c("N2O_En")) %>%
  mutate(sector = "Energy: Food processing") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Fertilizer
FS_Fert_EM_N2O <- EM_All_GtCO2e %>%
  filter(sector == "ammonia", GHG1 != "Other GHGs", GHG1 %in% c("N2O_En")) %>%
  mutate(sector = "Energy: Fertilizer") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Food System Emissions
FS_EM_N2O <- bind_rows(#FS_Ag_EM,
  FS_Crop_EM_N2O,
  FS_Livestock_EM_N2O,
  FS_UnmgdLand_EM_N2O,
  FS_Waste_EM_N2O,
  FS_Fert_EM_N2O,
  FS_FoodProc_EM_N2O,
  FS_EnergyFarm_EM_N2O) %>% ungroup()

FS_EM_GLO_N2O <- FS_EM_N2O %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "N2O",
         system = "Food")



# FOOD SYSTEM EMISSIONS (F-GAS) -------------------------------------------

FS_EM_GLO_Fgas <- FS_Ind_EM %>%
  group_by(scenario, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "Other",
         system = "Food")

# FOOD SYSTEM EMISSIONS (NON-CO2) -----------------------------------------

FS_EM_GLO_NonCO2 <- bind_rows(FS_EM_GLO_CH4,
                              FS_EM_GLO_N2O,
                              FS_EM_GLO_Fgas) %>%
  group_by(scenario, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "Non-CO2",
         system = "Food")


# FOOD SYSTEM EMISSIONS (CO2) ---------------------------------------------------
#
# FS_Ag_EM <- EM_All_GtCO2e %>%
#   filter(GHG1 %in% c("CH4_Ag", "N2O_Ag")) %>%
#   mutate(sector = GHG1) %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   mutate(sector = replace(sector, sector == "CH4_Ag", "Ag: CH4"),
#          sector = replace(sector, sector == "N2O_Ag", "Ag: N2O")) %>%
#   group_by(scenario, year) %>%
#   dplyr::summarise(value = sum(value)) %>%
#   ungroup() %>%
#   group_by(scenario) %>%
#   Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2020)



# # 68% of urban processes
# FS_Waste_EM <- EM_All_GtCO2e %>%
#   filter(sector == "urban processes", GHG1 %in% c("CH4_En", "N2O_En")) %>%
#   mutate(value = value*0.68,
#          sector = "Waste") %>%
#   group_by(scenario, region, sector, Units, year) %>%
#   dplyr::summarise(value = sum(value))

# Now using updated waste emissions, see section above
# 68% of total waste emissions - TO-DO : specify different fractions by region??

# Energy on farm
FS_EnergyFarm_EM_CO2 <- EM_All_GtCO2e %>%
  filter(sector == "agricultural energy use", GHG1 != "Other GHGs", GHG1 %in% c("CO2")) %>%
  mutate(sector = "Energy: On-farm") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

# Energy on Food proc
FS_FoodProc_EM_CO2 <-  EM_All_GtCO2e %>%
  filter(sector == "process heat food processing", GHG1 != "Other GHGs", GHG1 %in% c("CO2")) %>%
  mutate(sector = "Energy: Food processing") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Fertilizer
FS_Fert_EM_CO2 <- EM_All_GtCO2e %>%
  filter(sector == "ammonia", GHG1 != "Other GHGs", GHG1 %in% c("CO2")) %>%
  mutate(sector = "Energy: Fertilizer") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

FS_LUC_EM <- CLUC_reg <- "CLUC" %>%
  PluckBind() %>%
  mutate(sector = "LULUCF",
         value = value / 1000 * 44 / 12,
         Units = "GtCO2e") %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))

#Food System Emissions
FS_EM_CO2 <- bind_rows(#FS_Ag_EM,
  FS_Fert_EM_CO2,
#  FS_LUC_EM,
  FS_FoodProc_EM_CO2,
  FS_EnergyFarm_EM_CO2) %>% ungroup()

FS_EM_GLO_CO2 <- FS_EM_CO2 %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "CO2",
         system = "Food")

FS_EM_GLO_LUC <- FS_LUC_EM %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "LUC CO2",
         system = "Food")



# TOTAL EM BY SPECIES -----------------------------------------------

CH4_total <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag", "CH4_En")) %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "CH4")

N2O_total <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("N2O_Ag", "N2O_En")) %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "N2O")

NonCO2_total <- bind_rows(CH4_total,
                          N2O_total) %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "Non-CO2")

CO2_total <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CO2")) %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "CO2")


# NON-FOOD SYSTEM EM BY SPECIES -------------------------------------------

NonFood_CH4 <- FS_EM_GLO_CH4 %>%
  mutate(value = value*-1) %>%
  bind_rows(CH4_total) %>%
  group_by(scenario, Units, year, GHG) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "CH4",
         system = "NonFood")

NonFood_N2O <- FS_EM_GLO_N2O %>%
  mutate(value = value*-1) %>%
  bind_rows(N2O_total) %>%
  group_by(scenario, Units, year, GHG) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "N2O",
         system = "NonFood")

NonFood_Fgas <-  EM_All_GtCO2e %>%
  filter(GHG1 == "Other GHGs") %>%
  mutate(value = value*0.62) %>%
  group_by(scenario, Units, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(GHG = "Other",
         system = "NonFood")


NonFood_NonCO2 <- bind_rows(NonFood_CH4,
                            NonFood_N2O,
                            NonFood_Fgas) %>%
  mutate(GHG = "Non-CO2",
         system = "NonFood") %>%
  group_by(scenario, Units, year, GHG, system) %>%
  dplyr::summarise(value = sum(value))

NonFood_CO2 <- bind_rows(FS_EM_GLO_CO2,
                         FS_EM_GLO_LUC) %>%
  mutate(value = value*-1) %>%
  bind_rows(CO2_total) %>%
  mutate(GHG = "CO2",
         system = "NonFood") %>%
  group_by(scenario, Units, year, GHG, system) %>%
  dplyr::summarise(value = sum(value))



# COMBINED EM BY SPECIES --------------------------------------------------

EM_species <- bind_rows(FS_EM_GLO_CH4,
                        FS_EM_GLO_N2O,
                        FS_EM_GLO_Fgas,
                        FS_EM_GLO_LUC,
                        FS_EM_GLO_CO2,
                        NonFood_CH4,
                        NonFood_N2O,
                        NonFood_Fgas,
                        NonFood_CO2)
#


# EMISSION FACTORS --------------------------------------------------------


"Agprod" %>% PluckBind() %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) -> Agprod

"MeatProd" %>% PluckBind() %>%
  group_by(scenario, region, sector, Units, year) %>%
  dplyr::summarise(value = sum(value)) -> Meatprod


Prod <- bind_rows(Agprod, Meatprod)

Prod_GLO <- Prod %>%
  rename(sector0 = sector) %>%
  left_join(MapAgCOMM %>% select(sector0 = AgCOMM, sector = AgCOMM3), by = "sector0") %>%
  group_by(scenario, sector, year, Units) %>%
  dplyr::summarise(value = sum(value))


AgNonCO2_EmFactor <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag", "N2O_Ag"),
                sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                              "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat",
                              "Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)


#An CH4

AnCH4_EmFactor <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)


# An N2O
AnN2O_EmFactor <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("N2O_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)

# Crop CH4
CropCH4_EmFactor <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c( "CH4_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)

# Crop N2O
CropN2O_EmFactor <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c( "N2O_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)



# EMISSIONS INTENSITY BY SECTOR ---------------------------------------------

CropCH4_EmFactor_sector <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c( "CH4_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)

CropN2O_EmFactor_sector <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c( "N2O_Ag"),
         sector %in% c("Corn", "FiberCrop", "FodderGrass", "FodderHerb", "Fruits", "Legumes", "MiscCrop", "NutsSeeds", "OilCrop",
                       "OilPalm", "OtherGrain", "Rice", "RootTuber", "Soybean", "SugarCrop", "Vegetables", "Wheat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)

AnCH4_EmFactor_sector <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("CH4_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)


# An N2O
AnN2O_EmFactor_sector <- EM_All_GtCO2e %>%
  filter(GHG1 %in% c("N2O_Ag"),
         sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  left_join(Prod, by = c("scenario", "region", 'year', "sector"), suffix = c(".GtCO2e", ".Mt")) %>%
  mutate(value.Mt = value.Mt/1000) %>%
  rename(Units.Gt = Units.Mt,
         value.Gt = value.Mt) %>%
  group_by(scenario, sector, year) %>%
  dplyr::summarise(value.GtCO2e = sum(value.GtCO2e),
                   value.Gt = sum(value.Gt)) %>%
  mutate(value = value.GtCO2e/value.Gt)

