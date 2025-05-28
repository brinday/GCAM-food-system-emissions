

# READ IN FILES -----------------------------------------------------------

# FOOD INTAKE
readr::read_csv("data/other/GCAM_region_names.csv", comment = "#") ->
  GCAM_region_names

readr::read_csv("data/other/GCAM_FoodWaste_Share_Pathway_SSP.csv", comment = "#") %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  rename(SSP = scenario) ->
  GCAM_FoodWaste_Share_Pathway_SSP


# RegHet:
  #Low/lower-middle: Static
  #Upper-middle: No
  #High: No

# RegHet_HalfWaste (with waste)
  #Low/lower-middle: Static
  #Upper-middle: HalfWaste2100
  #High: HalfWaste2050

GCAM_FoodWaste_Share_Pathway_SSP_RegHet <- GCAM_FoodWaste_Share_Pathway_SSP %>%
  mutate(RegHet = if_else(region %in% REG_low_lowermiddle_income, StaticWaste,
                                    if_else(region %in% REG_uppermiddle_income, WasteShare, WasteShare)),
         RegHet_HalfWaste = if_else(region %in% REG_low_lowermiddle_income, StaticWaste,
                                    if_else(region %in% REG_uppermiddle_income, HalfWaste2100, HalfWaste2050)))





# PROCESS FOOD INTAKE/WASTE (32 REG) ---------------------------------

#Raw food demand (Pcal)
PluckBind("FooddemandExo") %>%
  filter(scenario %in% SCENARIOS_FooddemandExo) %>%
  Agg_reg(region, technology) %>%
  bind_rows(
    PluckBind("Fooddemand") %>%
      filter(scenario %in% SCENARIOS_Fooddemand) %>%
      Agg_reg(region, technology)
  ) ->
  Food



#Aggregated food demand (Pcal) for aggregate sectors
Food %>%
  mutate(SSP = if_else(grepl("SSP2", scenario), "SSP2", "SSP1")) %>%
  left_join_error_no_match(
    GCAM_FoodWaste_Share_Pathway_SSP_RegHet %>% rename(technology = GCAM_commodity),
    by = c("region", "technology", "year", "SSP")
  ) %>%
  # avail = value (intake) + waste
  mutate(avail = case_when(scenario %in% SCENARIOS_NoWasteRed ~ value/(1-WasteShare),
                           scenario %in% SCENARIOS_StaticWaste ~ value/(1-StaticWaste),
                           scenario %in% SCENARIOS_HalfWaste_2050 ~ value/(1-HalfWaste2050),
                           scenario %in% SCENARIOS_HalfWaste_2100 ~ value/(1-HalfWaste2100),
                           scenario %in% SCENARIOS_RegHet ~ value/(1-RegHet),
                           scenario %in% SCENARIOS_RegHet_HalfWaste ~ value/(1-RegHet_HalfWaste))) %>%
  rename(AgCOMM = technology) %>%
  left_join_error_no_match(
    MapAgCOMM %>% distinct(AgCOMM, sector = AgCOMM3) %>%
      filter(!is.na(sector)), by = "AgCOMM"
  ) %>%
  group_by(scenario, region, sector, year) %>%
  summarize(intake = sum(value), avail = sum(avail), .groups = "drop") ->
  Food_AggSector

# Food waste by region
Food_Waste <- Food_AggSector %>%
  group_by(scenario, region, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), .groups = "drop") %>%
  mutate(value = avail - intake) %>%
  mutate(sector = "Waste") %>% select(-avail, -intake)

#Food consumption (total) by region
Food_IntakeWaste <- Food_AggSector %>%
  select(-avail) %>%
  rename(value = intake) %>%
  bind_rows(Food_Waste) %>%
  mutate(Units = "Pcal")

# Food consumption (total) global
Food_IntakeWaste_GLO <- Food_IntakeWaste %>%
  group_by(scenario, sector, Units, year) %>%
  dplyr::summarise(value = sum(value))


# 3C Ref Food

Food %>%
  filter(scenario == "3C", year %in% c(2015, 2020, 2025)) %>%
  write.csv("Diet_3C_Ref.csv", row.names = F)

# PROCESS FOOD INTAKE/WASTE PER CAPITA (32 REG) ---------------------------------


# Per capita food demand (kcal/p/d)
Food_AggSector %>%
  #group_by(Scenario, Region, Year) %>%
  #summarize(intake = sum(intake), avail = sum(avail), .groups = "drop") %>%
  left_join_error_no_match(
    PluckBind("POP") %>%
      Agg_reg(region) %>% rename(POP = value),
    by = c("scenario", "region", "year")) %>%
  mutate(intake = intake /365/POP * 10^9,
         avail = avail /365/POP * 10^9) ->
  pcFood


pcFood %>%
  group_by(scenario, region, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), .groups = "drop") %>%
  mutate(value = avail - intake) %>%
  mutate(sector = "Waste") %>% select(-avail, -intake) ->
  pcFood_Waste

pcFood %>% select(-avail, -POP) %>%
  rename(value = intake) %>%
  bind_rows(pcFood_Waste) ->
  pcFood_IntakeWaste

# PROCESS FOOD INTAKE/WASTE PER CAPITA (GLOBAL) -----------------------------------------------------

# Per capita food demand (kcal/p/d)
POP_GLO <- PluckBind("POP") %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  rename(POP = value)

Food_AggSector %>%
  group_by(scenario, sector, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), .groups = "drop") %>%
  left_join_error_no_match(POP_GLO, by = c("scenario", "year")) %>%
  mutate(intake = intake /365/POP * 10^9,
         avail = avail /365/POP * 10^9) %>%
  mutate(region = "Global") ->
  pcFood_GLO


pcFood_GLO %>%
  group_by(scenario, region, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), .groups = "drop") %>%
  mutate(value = avail - intake) %>%
  mutate(sector = "Waste") %>% select(-avail, -intake) ->
  pcFood_Waste_GLO

pcFood_GLO %>% select(-avail, -POP) %>%
  rename(value = intake) %>%
  bind_rows(pcFood_Waste_GLO) ->
  pcFood_IntakeWaste_GLO

# PROCESS FOOD INTAKE/WASTE PER CAPITA (REG WB INCOME) -----------------------------------------------------

# Per capita food demand (kcal/p/d)
POP_WBIncome <- PluckBind("POP")  %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  rename(POP = value)

Food_AggSector %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, sector, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), .groups = "drop") %>%
  left_join_error_no_match(POP_WBIncome, by = c("scenario", "region", "year")) %>%
  mutate(intake = intake /365/POP * 10^9,
         avail = avail /365/POP * 10^9)  ->
  pcFood_WBIncome


pcFood_WBIncome %>%
  group_by(scenario, region, year) %>%
  summarize(intake = sum(intake), avail =sum(avail), .groups = "drop") %>%
  mutate(value = avail - intake) %>%
  mutate(sector = "Waste") %>% select(-avail, -intake) ->
  pcFood_Waste_WBIncome

pcFood_WBIncome %>% select(-avail, -POP) %>%
  rename(value = intake) %>%
  bind_rows(pcFood_Waste_WBIncome) ->
  pcFood_IntakeWaste_WBIncome

# POPULATION --------------------------------------------------------------


POP_WBIncome <- PluckBind("POP")  %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value))

POP_GLO <- PluckBind("POP")  %>%
  group_by(scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "Global")

POP <- bind_rows(POP_WBIncome,
                 POP_GLO)




# GDP ---------------------------------------------------------------------

"GDPNS" %>% PluckBind() %>%
  mutate(value = value * 105.361/61.310,
         Units = "million 2020$")-> GDPNS


GDP_WBIncome <- GDPNS %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG_WB_INCOME),
                           by = "region0") %>%
  group_by(scenario, region, year) %>%
  dplyr::summarise(value = sum(value))

pcGDP_WBIncome <- GDP_WBIncome %>%
  left_join_error_no_match(POP_WBIncome, by = c("scenario", "region", "year"), suffix = c(".GDP", ".POP")) %>%
  mutate(value.GDPpc = (value.GDP*10^6)/(value.POP*1000),
         Units.GDPpc = "2020$/p")
