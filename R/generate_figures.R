# Master script for figure sequence


source("R/Step1_Main_LoadData.R")


# SCENARIO/REGION DEFINITIONS -----------------------------------------------------------

##SCENARIOS
# "Reference" Scenarios
SCENARIO_Ref_3C <- c("3C")
SCENARIO_Ref_2C <- c("2C")
SCENARIO_Ref_1.5C <- c("1.5C")

SCENARIO_Static_3C <- c("3C_Diet_Static")
SCENARIO_Static_2C <- c("2C_Diet_Static")
SCENARIO_Static_1.5C <- c("1.5C_Diet_Static")

SCENARIOS_1.5C <- unique(ScenarioMap %>% filter(grepl("1.5C", name)))$name
SCENARIOS_2C <- unique(ScenarioMap %>% filter(grepl("2C", name)))$name
SCENARIOS_3C <- unique(ScenarioMap %>% filter(grepl("3C", name)))$name




# There are two related queries
# FooddemandExo: when diet is exogenous (dietary change scenarios driven by inc. elas.)
# Fooddemand: original query for food calories
# Both producing Pcal by sector
SCENARIOS_FooddemandExo <- unique(ScenarioMap %>% filter(ExoFooddemand == "Yes"))$name
SCENARIOS_Fooddemand <- unique(ScenarioMap %>% filter(ExoFooddemand == "No"))$name

SCENARIOS_NoWasteRed <- unique(ScenarioMap %>% filter(WasteRed == "No"))$name
SCENARIOS_StaticWaste <- unique(ScenarioMap %>% filter(WasteRed == "StaticWaste"))$name
SCENARIOS_HalfWaste_2050 <- unique(ScenarioMap %>% filter(WasteRed == "HalfWaste_2050"))$name
SCENARIOS_HalfWaste_2100 <- unique(ScenarioMap %>% filter(WasteRed == "HalfWaste_2100"))$name
SCENARIOS_RegHet <- unique(ScenarioMap %>% filter(WasteRed == "RegHet"))$name
SCENARIOS_RegHet_HalfWaste <- unique(ScenarioMap %>% filter(WasteRed == "RegHet_HalfWaste"))$name

SCENARIOS_WasteScen <- c(SCENARIOS_HalfWaste_2050, SCENARIOS_HalfWaste_2100, SCENARIOS_RegHet_HalfWaste)



##REGIONS
#Region income groups
REG_low_lowermiddle_income <- c("Africa_Eastern", "Pakistan", "Africa_Southern",
                                "South Asia", "Africa_Western", "India",
                                "Europe_Eastern", "Africa_Northern", "South America_Northern",
                                "Southeast Asia", "Indonesia")

REG_uppermiddle_income <- c("Central America and Caribbean", "Central Asia",
                            "South Africa", "South America_Southern", "Colombia",
                            "China", "Argentina", "Mexico", "Middle East", "Brazil",
                            "Russia")

REG_high_income <- c("Europe_Non_EU", "EU-12", "Taiwan", "South Korea", "EU-15", "Japan",
                     "Canada", "USA", "Australia_NZ", "European Free Trade Association")


#SCENARIO LEVELS
scenario_levels <- c("Ref.",
                     "Static",
                     "Med.",
                     "High",
                     "Reg. Het.")

scenario_labels_all <- c("1.5C" = "1.5C Ref",
                         "2C" = "2C Ref",
                         "3C" = "3C Ref",
                         "1.5C_Diet_Static" = "1.5C Static",
                         "2C_Diet_Static" = "2C Static",
                         "3C_Diet_Static" = "3C Static",
                         "1.5C_Diet_2100" = "1.5C Med",
                         "2C_Diet_2100" = "2C Med",
                         "3C_Diet_2100" = "3C Med",
                         "1.5C_Diet_2050" = "1.5C High",
                         "2C_Diet_2050" = "2C High",
                         "3C_Diet_2050" = "3C High",
                         "1.5C_DietWaste_2100" = "1.5C Med",
                         "2C_DietWaste_2100" = "2C Med",
                         "3C_DietWaste_2100" = "3C Med",
                         "1.5C_DietWaste_2050" = "1.5C High",
                         "2C_DietWaste_2050" = "2C High",
                         "3C_DietWaste_2050" = "3C High")

scenario_labels_all_new <- c("1.5C" = "WB2C Ref",
                         "2C" = "Near2C Ref",
                         "3C" = "NoPrice Ref",
                         "1.5C_Diet_Static" = "WB2C Static",
                         "2C_Diet_Static" = "Near2C Static",
                         "3C_Diet_Static" = "NoPrice Static",
                         "1.5C_Diet_2100" = "WB2C Med",
                         "2C_Diet_2100" = "Near2C Med",
                         "3C_Diet_2100" = "NoPrice Med",
                         "1.5C_Diet_2050" = "WB2C High",
                         "2C_Diet_2050" = "Near2C High",
                         "3C_Diet_2050" = "NoPrice High",
                         "1.5C_DietWaste_2100" = "WB2C Med",
                         "2C_DietWaste_2100" = "Near2C Med",
                         "3C_DietWaste_2100" = "NoPrice Med",
                         "1.5C_DietWaste_2050" = "WB2C High",
                         "2C_DietWaste_2050" = "Near2C High",
                         "3C_DietWaste_2050" = "NoPrice High")


# COLORS  -----------------------------------------------------------------

#GENERAL
pal_16 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442","#CC79A7","#333333", "#D55E00", "#0072B2",
            "#333333", "#FFCC00", "#CC6600", "#006600", "#3333CC", "#CC0033", "#0099CC", "#999966")



# MAIN FIGURES ------------------------------------------------------------


# FIGURE 1: FOOD INTAKE/WASTE PER CAPITA  ----------------------------------------------------------------
source("R/StepA_FoodIntakeWaste.R")

plot_pcFood <-  bind_rows(pcFood_IntakeWaste_GLO,
                            pcFood_IntakeWaste_WBIncome) %>%
  mutate(category = if_else(sector != "Waste", "Intake", "Waste")) %>%
  mutate(region = factor(region, levels = c("Low/lower-middle", "Upper-middle", "High", "Global"))) %>%
  filter(year >= 2015)

#Calculate the difference between Ref and Diet scenarios
plot_pcFood_diff_Ref <- diff_from_scen(plot_pcFood,
                                   ref_scenario = SCENARIO_Ref_1.5C,
                                   diff_scenarios = c(SCENARIOS_1.5C),
                                   join_var = c("sector", "year", "region", "category")) %>%
  group_by(scenario.diff, year, region, category) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Reductions_Diet_Ref",
          value = value*-1) %>%
  rename(scenario = scenario.diff) %>%
  mutate(patt = "DietRed")


# Calculate the difference between Diet and Diet Waste scenarios
plot_pcFood_WasteScen_1.5C <- plot_pcFood %>%
  filter(scenario %in% c("1.5C_DietWaste_2050", "1.5C_DietWaste_2100", "1.5C_DietWaste_RegHetStatic")) %>%
  mutate(scenario = gsub("Waste", "", scenario))

plot_pcFood_Waste_diff_WasteScen_1.5C <- plot_pcFood %>%
  filter(scenario %in% SCENARIOS_1.5C) %>%
  filter(scenario %!in% c(SCENARIO_Ref_1.5C,SCENARIO_Static_1.5C, SCENARIOS_WasteScen)) %>%
  left_join(plot_pcFood_WasteScen_1.5C, by = c("scenario", "sector", "region", "year", "category"),
                           suffix = c(".CoreScen", ".WasteScen")) %>%
  mutate(diff = value.CoreScen - value.WasteScen) %>%
  filter(category == "Waste") %>%
  select(-sector, -value.CoreScen) %>%
  pivot_longer(cols = c("value.WasteScen", "diff"), names_to = "sector") %>%
  mutate(patt = if_else(sector == "value.WasteScen", "Core", "WasteRed")) %>%
  mutate(sector = "Waste")


# data set with all needed categories for plot
#1. Intake
plot_pcFood_all_1.5C <- plot_pcFood %>%
  filter(category != "Waste") %>%
#2. Waste in Ref scenario
  bind_rows(filter(plot_pcFood, sector == "Waste", scenario %in% c(SCENARIO_Ref_1.5C, SCENARIO_Static_1.5C))) %>%
  mutate(patt = "Core") %>%
#3. Ref - Diet (grey bars)
  bind_rows(plot_pcFood_diff_Ref) %>%
#4. DietWaste - Diet (Dashed lines) + Remaining Waste
  bind_rows(plot_pcFood_Waste_diff_WasteScen_1.5C) %>%
  mutate(sector = factor(sector, levels = c("Livestock", "Oil crops", "Other crops", "Staple crops", "Waste", "Reductions_Diet_Ref"))) %>%
  filter(scenario %in% c(SCENARIOS_1.5C)) %>%
 # left_join_error_no_match(ScenarioMap %>% select(name, label), by = c("scenario" = "name")) %>%
  # reset small negative values to 0
  mutate(value = if_else(value < 0, 0, value)) %>%
  mutate(scenario = factor(scenario, levels = SCENARIOS_1.5C))


Fig1 <- plot_pcFood_all_1.5C %>%
  filter(scenario %in% c("1.5C", "1.5C_Diet_Static","1.5C_Diet_2050"), year %in% c(2100)) %>%
  mutate(scenario = factor(scenario, levels = c("1.5C", "1.5C_Diet_Static","1.5C_Diet_2050"))) %>%
  ggplot() +
  facet_grid(scenario ~ category, scales = "fixed", label = as_labeller(c("1.5C" = "Reference",
                                                                          "1.5C_Diet_Static" = "Static",
                                                                          "1.5C_Diet_2050" = "Transformation",
                                                                       "Intake" = "Intake",
                                                                       "Waste" = "Waste"))) +
  geom_bar_pattern(
    aes(x = interaction(region, year), y = value, fill = sector, pattern = patt),
    color = 1, stat = "identity", position = position_stack(reverse = TRUE),
    pattern_fill = "grey50", pattern_color = NA, pattern_density = 0.4, pattern_spacing = 0.03) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  scale_x_discrete(
    limits = rev(c("Low/lower-middle.2100",
                   "Upper-middle.2100",
                   "High.2100",
                   "Global.2100")),
    labels = c("Low/lower-middle.2100" = "Low/lower-middle",
               "Upper-middle.2100" = "Upper-middle",
               "High.2100" = "High",
               "Global.2100" = "Global")
  ) +
  scale_fill_manual(
    values = c("Livestock" = "#CC79A7",
               "Oil crops" = "#F0E442",
               "Other crops" = "#009E73",
               "Staple crops" = "#56B4E9",
               "Waste" = "#E69F00"
    ),
    labels = c("Livestock" = "Livestock",
               "Oil crops" = "Oil crops",
               "Other crops" = "Other crops",
               "Staple crops" = "Staple crops",
               "Waste" = "Waste")
  ) +
  scale_pattern_manual(
    values = c("Core" = "none", "DietRed" = "none",  "WasteRed" = "stripe"),
    breaks = c("DietRed", "WasteRed"),
    labels = c("DietRed" = "Reductions from dietary changes",
               "WasteRed" = "Reductions from waste reduction efforts")
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        pattern = c("none", "stripe"),
        pattern_fill = c("grey50", "grey50"),
        fill = c("grey50", "white"),
        pattern_density = c(1, 0.4),
        pattern_spacing = c(0, 0.015)
      )
    )
  ) +
  scale_y_continuous(limits = c(0, 2500)) +
  labs(y = "kcal/cap/d", x = "", fill = "Sector", pattern = "Reductions") +
  theme_bw() + theme0 + coord_flip() +
  theme(legend.position = "left")

print(Fig1)


Fig1 %>% Write_png(.name = "Fig1", .DIR_MODULE = DIR_MODULE, h = 6, w = 12, r = 300)


# FIGURE 2: FOOD SYS EM BY SECTOR -----------------------------------------------------------

source("R/StepB_Emissions.R")

  # FS EM SECTOR (REF) --------------------------------------------------------------

  plot_FS_EM_GLO <- FS_EM_GLO %>%
    mutate(sector = factor(sector, levels = c("LULUCF",
                                              "Forest fires",
                                              "Ag: Crop: N2O",
                                              "Ag: Crop: CH4",
                                              "Ag: Animal: N2O",
                                              "Ag: Animal: CH4",
                               "Energy: Fertilizer",
                               "Energy: Food processing",
                               "Energy: On-farm",
                               "Retail: Refrigeration",
                               "Waste")))




  # FS EM SECTOR (DIFF) --------------------------------------------------------

  FS_EM_GLO_diff_1.5C <- diff_from_scen(FS_EM_GLO,
                                        ref_scenario = SCENARIO_Ref_1.5C,
                                        diff_scenarios = SCENARIOS_1.5C,
                                        join_var = c("sector", "Units", "year"))

  FS_EM_GLO_diff_2C <- diff_from_scen(FS_EM_GLO,
                                          ref_scenario = SCENARIO_Ref_2C,
                                          diff_scenarios = SCENARIOS_2C,
                                          join_var = c("sector", "Units", "year"))

  FS_EM_GLO_diff_3C <- diff_from_scen(FS_EM_GLO,
                                          ref_scenario = SCENARIO_Ref_3C,
                                          diff_scenarios = SCENARIOS_3C,
                                          join_var = c("sector", "Units", "year"))

  FS_EM_GLO_diff <- bind_rows(FS_EM_GLO_diff_1.5C,
                              FS_EM_GLO_diff_2C,
                              FS_EM_GLO_diff_3C)

  FS_EM_GLO_diff_total <- FS_EM_GLO_diff %>%
    group_by(scenario.diff, Units, year) %>%
    dplyr::summarise(value = sum(value))


  # PLOT TOTAL FS EM LINE -------------------------------------

  plot_FS_EM_GLO_total <- plot_FS_EM_GLO %>%
    group_by(scenario, Units, year, region) %>%
    dplyr::summarise(value = sum(value)) %>%
    left_join_error_no_match(ScenarioMap %>% select(name, MitigationScen, DietScen, WasteRed), by = c("scenario" = "name"))%>%
    mutate(WasteRed = if_else(WasteRed == "StaticWaste", "No", WasteRed),
           WasteRed = if_else(WasteRed != "No", "WasteRed", "No")) %>%
    mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High", "Reg. Het.")),
           MitigationScen = factor(MitigationScen, levels = c("WB2C", "Near2C", "NoPrice")))


  p0 <- plot_FS_EM_GLO_total %>%
    filter(scenario %in% c("3C", "2C", "1.5C",
                           "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                           "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050"
                          )) %>%
    filter(year >= 2020) %>%
    ggplot() +
    geom_line(aes(x = year, y = value, color = interaction(DietScen, WasteRed), linetype = interaction(DietScen, WasteRed)), size = 1) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    labs(y = "GtCO2e/yr", x = "") +
    facet_wrap(~MitigationScen, scales = "fixed") +
    #geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    scale_color_manual(name = "Scenario", values = c("Ref..No" = "#999999",
                                  "High.No" = "#CC79A7",
                                  "High.WasteRed" = "#CC79A7"),
                       labels = c("Ref..No" = "Ref.",
                                  "High.No" = "High (Diet only)",
                                  "High.WasteRed" = "High (With waste reduction efforts)")) +
    scale_linetype_manual(name = "Scenario", values = c("Ref..No" = "solid",
                                     "High.No" = "solid",
                                     "High.WasteRed" = "dashed"),
                          labels = c("Ref..No" = "Ref.",
                                     "High.No" = "High (Diet only)",
                                     "High.WasteRed" = "High (With waste reduction efforts)"))+
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 13)) +
    theme(legend.position = "right",
          legend.key.width = unit(1, "cm"))

  print(p0)

  # PLOT FS EM SECTOR BAR (REF) --------------------------------------------------------


  p1 <- plot_FS_EM_GLO %>%
    filter(scenario %in% c("1.5C", "2C", "3C"),
           year >= 2020 & year <= 2100) %>%
    ggplot() +
    facet_grid(~scenario, scales = "fixed", label = as_labeller(c("1.5C" = "WB2C Ref",
                                                                  "2C" = "Near2C Ref",
                                                                  "3C" = "NoPrice Ref"))) +
    geom_bar(aes(x = year, y = value, fill = sector), color = 1, stat = "identity", position = position_stack(reverse = FALSE)) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    labs(y = "GtCO2e/yr", x = "", fill = "Sector", title = "Reference scenarios") +
    #scale_fill_brewer(palette = "Set3", direction = 1) +
    scale_fill_manual(values = c(
      "Ag: Animal: CH4" = "#B22222",  # Dark red
      "Ag: Animal: N2O" = "#E41A1C",  # Crimson red
      "Ag: Crop: CH4" = "#FF4500",    # Warm orange
      "Ag: Crop: N2O" = "#FF7F00",    # Deep orange
      "Energy: Fertilizer" = "#56B4E9",  # Teal blue
      "Energy: Food processing" = "#0072B2",  # Sky blue
      "Energy: On-farm" = "#003366",  # Navy blue
      "Forest fires" = "#FFD700",  # Bright yellow
      "LULUCF" = "#DAA520",     # Golden brown
      "Retail: Refrigeration" = "#984EA3",  # Purple
      "Waste" = "#009E73"             # Forest green
    ))+
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(legend.position = "none")

  print(p1)


  # PLOT FS EM SECTOR BAR (DIFF - HIGH) --------------------------------------------------------

  plot_FS_EM_GLO_diff <-  FS_EM_GLO_diff %>%
    filter(scenario.diff %in% c("1.5C_Diet_2050",
                                "2C_Diet_2050",
                                "3C_Diet_2050")) %>%
    mutate(sector = factor(sector, levels = c("LULUCF",
                                              "Forest fires",
                                              "Ag: Crop: N2O",
                                              "Ag: Crop: CH4",
                                              "Ag: Animal: N2O",
                                              "Ag: Animal: CH4",
                                              "Energy: Fertilizer",
                                              "Energy: Food processing",
                                              "Energy: On-farm",
                                              "Retail: Refrigeration",
                                              "Waste"))) %>%
    select(scenario.diff, sector, Units, year, region.diff, value)



  # Calculate the difference between Diet and Diet Waste scenarios
  plot_FS_EM_GLO_WasteScen_diff <- FS_EM_GLO_diff %>%
    filter(scenario.diff %in% c("1.5C_DietWaste_2050",
                                "2C_DietWaste_2050",
                                "3C_DietWaste_2050")) %>%
    mutate(scenario.diff = gsub("Waste", "", scenario.diff)) %>%
    select(scenario.diff, sector, Units, year, region.diff, value)

  # Get total FS EM in the waste scenarios (relative to Diet)
  # But rename them to match the core scenarios
  plot_FS_EM_GLO_diff_WasteScen_total <- plot_FS_EM_GLO_diff %>%
    left_join(plot_FS_EM_GLO_WasteScen_diff, by = c("scenario.diff", "region.diff", "Units", "sector", "year"),
                             suffix = c(".CoreScen", ".WasteScen")) %>%
    mutate(diff = value.WasteScen - value.CoreScen,
           patt = "WasteRed",
           sector = "WasteRed",
           Units = "GtCO2e/yr") %>%
    rename(value = diff) %>%
    group_by(scenario.diff, region.diff, sector, patt, year, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    select(scenario.diff, region.diff, year, sector, patt, value, Units)

  #Bind with the rest of the diff
  plot_FS_EM_GLO_diff_all <- plot_FS_EM_GLO_diff %>%
    select(scenario.diff, region.diff, sector, Units, year, value) %>%
    mutate(patt = "Core",
           Units = "GtCO2e/yr") %>%
    bind_rows(plot_FS_EM_GLO_diff_WasteScen_total) %>%
    mutate(value = if_else(year == 2020, 0, value)) %>%
    mutate(sector = factor(sector, levels = c("WasteRed",
                                              "LULUCF",
                                              "Forest fires",
                                              "Ag: Crop: N2O",
                                              "Ag: Crop: CH4",
                                              "Ag: Animal: N2O",
                                              "Ag: Animal: CH4",
                                              "Energy: Fertilizer",
                                              "Energy: Food processing",
                                              "Energy: On-farm",
                                              "Retail: Refrigeration",
                                              "Waste")))


  p2 <- plot_FS_EM_GLO_diff_all %>%
    filter(year >= 2020 & year <= 2100) %>%
    ggplot() +
    facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("1.5C_Diet_2050" = "WB2C High - Ref",
                                                                     "2C_Diet_2050" = "Near2C High - Ref",
                                                                     "3C_Diet_2050" = "NoPrice High - Ref"))) +
    geom_bar_pattern(aes(x = year, y = value, fill = sector, pattern = patt), color = 1,
                     pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                     stat = "identity", position = position_stack(reverse = FALSE)) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    labs(y = "GtCO2e/yr", x = "", fill = "Sector", pattern = "", title = "High ambition transformation - Reference scenarios") +
    scale_y_continuous(limits = c(-8.1,NA))+
    #scale_fill_brewer(palette = "Set3", direction = 1) +
    scale_fill_manual(values = c(
      "Ag: Animal: CH4" = "#B22222",  # Dark red
      "Ag: Animal: N2O" = "#E41A1C",  # Crimson red
      "Ag: Crop: CH4" = "#FF4500",    # Warm orange
      "Ag: Crop: N2O" = "#FF7F00",    # Deep orange
      "Energy: Fertilizer" = "#56B4E9",  # Teal blue
      "Energy: Food processing" = "#0072B2",  # Sky blue
      "Energy: On-farm" = "#003366",  # Navy blue
      "Forest fires" = "#FFD700",  # Bright yellow
      "LULUCF" = "#DAA520",     # Golden brown
      "Retail: Refrigeration" = "#984EA3",  # Purple
      "Waste" = "#009E73"             # Forest green
    ))+
    scale_pattern_manual(
      values = c("Core" = "none", "WasteRed" = "stripe"),
      breaks = c( "WasteRed"),
      labels = c("WasteRed" = "With waste reduction efforts")
    )+
    guides(
      fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
      pattern = guide_legend(
        order = 2,
        override.aes = list(
          pattern = c("stripe"),
          pattern_fill = c("grey50"),
          fill = c( "white"),
          pattern_density = c(0.4),
          pattern_spacing = c(0.015)
        )
      )
    ) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(legend.position = "right")

  print(p2)



  # FIG 2 COMBINED ----------------------------------------------------------


  patchwork_FS_EM <- p1 / p2 +
    plot_layout(guides = "collect", heights = c(1, 1)) +
    theme(legend.position = "right") +
    plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations


  patchwork_FS_EM_with_line <- p0 / patchwork_FS_EM +
    plot_layout(heights = c(1, 2.5)) +
    plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

  print(patchwork_FS_EM_with_line)

  patchwork_FS_EM_with_line %>% Write_png(.name = "Fig2", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)







# FIGURE 3: CUM FOOD SYS EM BY INCOME GROUP --------------------------------
  # CUM FS EM INCOME BAR (REF) ---------------------------------------------------------

  plot_cum_FS_EM_WBIncome <- cum_FS_EM_WBIncome_ProdBased %>%
    mutate(region = factor(region, levels = c("Low/lower-middle", "Upper-middle", "High"))) %>%
    left_join_error_no_match(ScenarioMap %>% select(name, MitigationScen, DietScen, WasteRed), by = c("scenario" = "name")) %>%
    mutate(WasteRed = if_else(WasteRed == "StaticWaste", "No", WasteRed),
           WasteRed = if_else(WasteRed != "No", "WasteRed", "No")) %>%
    mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High", "Reg. Het.")),
           MitigationScen = factor(MitigationScen, levels = c("WB2C", "Near2C", "NoPrice")))



  # CUM FS EM INCOME BAR (DIFF) ---------------------------------------------

  cum_FS_EM_WBIncome_ProdBased_diff_1.5C <- diff_from_scen(cum_FS_EM_WBIncome_ProdBased,
                                                           ref_scenario = SCENARIO_Ref_1.5C,
                                                           diff_scenarios = SCENARIOS_1.5C,
                                                           join_var = c("region", "Units", "year"))

  cum_FS_EM_WBIncome_ProdBased_diff_2C <- diff_from_scen(cum_FS_EM_WBIncome_ProdBased,
                                                         ref_scenario = SCENARIO_Ref_2C,
                                                         diff_scenarios = SCENARIOS_2C,
                                                         join_var = c("region", "Units", "year"))

  cum_FS_EM_WBIncome_ProdBased_diff_3C <- diff_from_scen(cum_FS_EM_WBIncome_ProdBased,
                                                         ref_scenario = SCENARIO_Ref_3C,
                                                         diff_scenarios = SCENARIOS_3C,
                                                         join_var = c("region", "Units", "year"))

  cum_FS_EM_WBIncome_ProdBased_diff <- bind_rows(cum_FS_EM_WBIncome_ProdBased_diff_1.5C,
                                                 cum_FS_EM_WBIncome_ProdBased_diff_2C,
                                                 cum_FS_EM_WBIncome_ProdBased_diff_3C)



  # Calculate the difference between Diet and Diet Waste scenarios
  cum_FS_EM_WBIncome_ProdBased_WasteScen_diff <- cum_FS_EM_WBIncome_ProdBased_diff %>%
    filter(scenario.diff %in% c("1.5C_DietWaste_2050", "1.5C_DietWaste_2100",
                                "2C_DietWaste_2050", "2C_DietWaste_2100",
                                "3C_DietWaste_2050", "3C_DietWaste_2100")) %>%
    mutate(scenario.diff = gsub("Waste", "", scenario.diff))

  cum_FS_EM_WBIncome_ProdBased_WasteScen_diff_total <- cum_FS_EM_WBIncome_ProdBased_diff %>%
    filter(scenario.diff %in% c("1.5C_Diet_2050", "1.5C_Diet_2100",
                                "2C_Diet_2050", "2C_Diet_2100",
                                "3C_Diet_2050", "3C_Diet_2100")) %>%
    left_join_error_no_match(cum_FS_EM_WBIncome_ProdBased_WasteScen_diff, by = c("scenario.diff", "region", "year", "Units"),
                             suffix = c(".CoreScen", ".WasteScen")) %>%
    mutate(diff = value.WasteScen - value.CoreScen,
           patt = "WasteRed",
           Units = "GtCO2e/yr") %>%
    rename(value = diff) %>%
    group_by(scenario.diff, Units, year, patt) %>%
    dplyr::summarise(value = sum(value)) %>%
    mutate(region = "WasteRed") %>%
    select(scenario.diff, region, Units, year, value, patt)

  #Bind with the rest of the diff
  cum_FS_EM_WBIncome_ProdBased_diff_all <- cum_FS_EM_WBIncome_ProdBased_diff %>%
    select(scenario.diff, region, Units, year, value) %>%
    mutate(patt = "Core",
           Units = "GtCO2e") %>%
    bind_rows(cum_FS_EM_WBIncome_ProdBased_WasteScen_diff_total) %>%
    mutate(value = if_else(year == 2020, 0, value))


  plot_cum_FS_EM_GLO_total <- plot_cum_FS_EM_WBIncome %>%
    group_by(scenario, Units, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    left_join_error_no_match(ScenarioMap %>% select(name, MitigationScen, DietScen, WasteRed), by = c("scenario" = "name")) %>%
    mutate(WasteRed = if_else(WasteRed == "StaticWaste", "No", WasteRed),
           WasteRed = if_else(WasteRed != "No", "WasteRed", "No")) %>%
    mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High", "Reg. Het.")),
           MitigationScen = factor(MitigationScen, levels = c("WB2C", "Near2C", "NoPrice")))




  # PLOT TOTAL CUM FS EM LINE (MULTI PANEL) ----------------------------------

  #Facet mitigation levels
  p0 <- plot_cum_FS_EM_GLO_total %>%
    filter(scenario %in% c("3C", "2C", "1.5C",
                           "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                           "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050",
                           "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100",
                           "3C_DietWaste_2100", "2C_DietWaste_2100", "1.5C_DietWaste_2100",
                           "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static")) %>%
    mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High"))) %>%
    filter(year >= 2020) %>%
    ggplot() +
    geom_line(aes(x = year, y = value, color = interaction(DietScen, WasteRed), linetype = interaction(DietScen, WasteRed)), size = 1) +
    facet_wrap(~MitigationScen, scales = "fixed") +
    #geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    scale_color_manual(name = "Scenario", values = c("Ref..No" = "#999999",
                                                     "Static.No" = "#E69F00",
                                                     "Med..No" = "#56B4E9",
                                                     "Med..WasteRed" = "#56B4E9",
                                                     "High.No" = "#CC79A7",
                                                     "High.WasteRed" = "#CC79A7"
    ),
    labels = c("Ref..No" = "Ref.",
               "Static.No" = "Static",
               "Med..No" = "Med. (Diet only)",
               "Med..WasteRed" = "Med. (With waste reduction efforts)",
               "High.No" = "High (Diet only)",
               "High.WasteRed" = "High (With waste reduction efforts)")) +
    scale_linetype_manual(name = "Scenario", values = c("Ref..No" = "solid",
                                                        "Static.No" = "solid",
                                                        "Med..No" = "solid",
                                                        "Med..WasteRed" = "dashed",
                                                        "High.No" = "solid",
                                                        "High.WasteRed" = "dashed"),
                          labels = c("Ref..No" = "Ref.",
                                     "Static.No" = "Static",
                                     "Med..No" = "Med. (Diet only)",
                                     "Med..WasteRed" = "Med. (With waste reduction efforts)",
                                     "High.No" = "High (Diet only)",
                                     "High.WasteRed" = "High (With waste reduction efforts)"))+
    labs(y = "GtCO2e", x = "", color = "Food demand pathway", linetype = "") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 13)) +
    theme(legend.position = "right",
          legend.key.width = unit(1, "cm"))

  print(p0)



  # PLOT CUM FS EM INCOME BAR (REF) ---------------------------------------------

  p1 <- plot_cum_FS_EM_WBIncome %>%
    filter(scenario %in% c("1.5C", "2C", "3C"),
           year >= 2020 & year <= 2100) %>%
    ggplot() +
    facet_grid(~MitigationScen, scales = "fixed") +
    geom_bar(aes(x = year, y = value, fill = region), color = 1, stat = "identity", position = position_stack(reverse = FALSE)) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    labs(y = "GtCO2e", x = "", fill = "Income category", title = "Reference scenarios") +
    scale_fill_brewer(palette = "Dark2", direction = 1) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(legend.position = "none")

  print(p1)




  # PLOT CUM FS EM INCOME BAR (DIFF - 1.5C DEMAND RED) -----------------------------



  p2 <- cum_FS_EM_WBIncome_ProdBased_diff_all %>%
    filter(scenario.diff %in% c("1.5C_Diet_Static", "1.5C_Diet_2100", "1.5C_Diet_2050"),
           year >= 2020 & year <= 2100) %>%
    mutate(scenario.diff = factor(scenario.diff, levels = c("1.5C_Diet_Static", "1.5C_Diet_2100", "1.5C_Diet_2050"))) %>%
    mutate(region = factor(region, levels = c("WasteRed", "Low/lower-middle", "Upper-middle", "High"))) %>%
    ggplot() +
    facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("1.5C_Diet_Static" = "WB2C Static - Ref",
                                                                       "1.5C_Diet_2100" = "WB2C Medium - Ref",
                                                                       "1.5C_Diet_2050" = "WB2C High - Ref"))) +
    geom_bar_pattern(aes(x = year, y = value, fill = region, pattern = patt), color = 1,
                     pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                     stat = "identity", position = position_stack(reverse = FALSE)) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    labs(y = "GtCO2e", x = "", fill = "Income category", pattern = "", title = "WB2C food demand scenarios - WB2C Ref") +
    scale_pattern_manual(
      values = c("Core" = "none", "WasteRed" = "stripe"),
      breaks = c( "WasteRed"),
      labels = c("WasteRed" = "With waste reduction efforts")
    )+
    scale_fill_manual(values = c(
      "Low/lower-middle" = "#1B9E77",
      "Upper-middle" = "#D95F02",
      "High" = "#7570B3"
    ))+
    guides(
      fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
      pattern = guide_legend(
        order = 2,
        override.aes = list(
          pattern = c("stripe"),
          pattern_fill = c("grey50"),
          fill = c( "white"),
          pattern_density = c(0.4),
          pattern_spacing = c(0.015)
        )
      )
    ) +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(legend.position = "right")

  print(p2)


  # FIG 3 COMBINED ----------------------------------------------------------


  patchwork_cum_FS_EM_WBIncome <- p1 / p2 +
    plot_layout(guides = "collect", heights = c(1, 1)) +
    theme(legend.position = "right") +
    plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

  patchwork_cum_FS_EM_WBIncome_with_line <- p0 / patchwork_cum_FS_EM_WBIncome  +
    plot_layout(heights = c(1, 2.5)) +
    plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

  print(patchwork_cum_FS_EM_WBIncome_with_line)

  patchwork_cum_FS_EM_WBIncome_with_line %>% Write_png(.name = "Fig3", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)




# FIGURE 4: TEMPERATURE -------------------------------------------------------------


  MeanTemp <- "MeanTemp" %>% PluckBind()



  plot_MeanTemp <- MeanTemp %>%
    filter(scenario %in% c("3C", "2C", "1.5C",
                           "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                           "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050",
                           "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100",
                           "3C_DietWaste_2100", "2C_DietWaste_2100", "1.5C_DietWaste_2100",
                           "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static",
                           "3C_Diet_RegHetStatic", "2C_Diet_RegHetStatic", "1.5C_Diet_RegHetStatic",
                           "3C_DietWaste_RegHetStatic", "2C_DietWaste_RegHetStatic", "1.5C_DietWaste_RegHetStatic")) %>%
    mutate(region = factor(region, levels = c("Global"))) %>%
    left_join_error_no_match(ScenarioMap %>% select(name, MitigationScen, DietScen, WasteRed), by = c("scenario" = "name")) %>%
    mutate(WasteRed = if_else(WasteRed == "StaticWaste", "No", WasteRed),
           WasteRed = if_else(WasteRed == "RegHet", "No", WasteRed),
           WasteRed = if_else(WasteRed != "No", "WasteRed", "No")) %>%
    mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High", "Reg. Het.")),
           MitigationScen = factor(MitigationScen, levels = c("WB2C", "Near2C", "NoPrice"))) %>%
    filter(year >= 2020)

  plot_MeanTemp_peak_1.5C_2C <- plot_MeanTemp %>%
    filter(MitigationScen %in% c("WB2C", "Near2C")) %>%
    group_by(scenario) %>%
    filter(value == max(value)) %>%
    ungroup()



  #Facet mitigation levels
  p0 <- ggplot() +
    geom_line(data = plot_MeanTemp, aes(x = year, y = value, color = interaction(DietScen, WasteRed), linetype = interaction(DietScen, WasteRed)), size = 0.75) +
    geom_point(data = plot_MeanTemp_peak_1.5C_2C, aes(x = year, y = value, color = interaction(DietScen, WasteRed)), size = 2) +
    facet_wrap(~MitigationScen, scales = "fixed") +
    #geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    scale_color_manual(name = "Scenario", values = c("Ref..No" = "#999999",
                                                     "Static.No" = "#E69F00",
                                                     "Med..No" = "#56B4E9",
                                                     "Med..WasteRed" = "#56B4E9",
                                                     "High.No" = "#CC79A7",
                                                     "High.WasteRed" = "#CC79A7",
                                                     "Reg. Het..No" = "#006600",
                                                     "Reg. Het..WasteRed" = "#006600"),
                       labels = c("Ref..No" = "Ref.",
                                  "Static.No" = "Static",
                                  "Med..No" = "Med. (Diet only)",
                                  "Med..WasteRed" = "Med. (With waste reduction efforts)",
                                  "High.No" = "High (Diet only)",
                                  "High.WasteRed" = "High (With waste reduction efforts)",
                                  "Reg. Het..No" = "Reg. Het. (Diet only)",
                                  "Reg. Het..WasteRed" = "Reg. Het. (With waste reduction efforts)")) +
    scale_linetype_manual(name = "Scenario", values = c("Ref..No" = "solid",
                                                        "Static.No" = "solid",
                                                        "Med..No" = "solid",
                                                        "Med..WasteRed" = "dashed",
                                                        "High.No" = "solid",
                                                        "High.WasteRed" = "dashed",
                                                        "Reg. Het..No" = "solid",
                                                        "Reg. Het..WasteRed" = "dashed"),
                          labels = c("Ref..No" = "Ref.",
                                     "Static.No" = "Static",
                                     "Med..No" = "Med. (Diet only)",
                                     "Med..WasteRed" = "Med. (With waste reduction efforts)",
                                     "High.No" = "High (Diet only)",
                                     "High.WasteRed" = "High (With waste reduction efforts)",
                                     "Reg. Het..No" = "Reg. Het. (Diet only)",
                                     "Reg. Het..WasteRed" = "Reg. Het. (With waste reduction efforts)"))+
    labs(y = "deg C", x = "", color = "Food demand pathway", linetype = "") +
    theme_bw() + theme0 +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 13)) +
    theme(legend.position = "right",
          legend.key.width = unit(1, "cm"))

  print(p0)


  p0 %>% Write_png(.name = "Fig4", .DIR_MODULE = DIR_MODULE, h = 4, w = 12, r = 300)




# SUPPLEMENTARY FIGURES ---------------------------------------------------




  # FIGURE S3: POPULATION BY INCOME GROUP ---------------------------------------------------


  pcGDP_WBIncome$region <- factor(pcGDP_WBIncome$region, levels = c("Low/lower-middle",
                                                                    "Upper-middle",
                                                                    "High"))

  p0 <-
    ggplot(data = filter(pcGDP_WBIncome, scenario %in% c("3C")),
           aes(x = year, y = value.POP/(10^6))) +
    geom_point() +
    geom_line()+
    facet_wrap(~region, scale = "fixed", nrow = 1) +
    theme_bw() + theme0 +
    labs(y = "billions", x = "", title = "SSP1 population by income-region") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 13)) +
    theme(legend.position = "right",
          legend.key.width = unit(1, "cm"))

  p0 %>% Write_png(.name = "FigS3", .DIR_MODULE = DIR_MODULE, h = 4, w = 15, r = 300)

  # FIGURE S4: GDP PER CAPITA BY INCOME GROUP ---------------------------------------------------

  p0 <-
    ggplot(data = filter(pcGDP_WBIncome, scenario %in% c("3C")),
           aes(x = year, y = value.GDPpc)) +
    geom_point() +
    geom_line()+
    facet_wrap(~region, scale = "fixed", nrow = 1) +
    theme_bw() + theme0 +
    labs(y = "2020$/p", x = "", title = "SSP1 GDP per capita by income-region") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          strip.text = element_text(size = 13)) +
    theme(legend.position = "right",
          legend.key.width = unit(1, "cm"))

  p0 %>% Write_png(.name = "FigS4", .DIR_MODULE = DIR_MODULE, h = 4, w = 15, r = 300)

  # FIGURE S5: PER CAP FOOD INTAKE WASTE -------------------------------------------------------------

  # Calculate the difference between Diet and Diet Waste scenarios
  plot_pcFood_WasteScen_all <- plot_pcFood %>%
    filter(scenario %in% c("1.5C_DietWaste_2050", "1.5C_DietWaste_2100", "1.5C_DietWaste_RegHetStatic",
                           "2C_DietWaste_2050", "2C_DietWaste_2100", "2C_DietWaste_RegHetStatic",
                           "3C_DietWaste_2050", "3C_DietWaste_2100", "3C_DietWaste_RegHetStatic")) %>%
    mutate(scenario = gsub("Waste", "", scenario))

  plot_pcFood_Waste_diff_WasteScen_all <- plot_pcFood %>%
    filter(scenario %in% c("3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                           "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100")) %>%
    left_join(plot_pcFood_WasteScen_all, by = c("scenario", "sector", "region", "year", "category"),
              suffix = c(".CoreScen", ".WasteScen")) %>%
    mutate(diff = value.CoreScen - value.WasteScen) %>%
    filter(category == "Waste") %>%
    select(-sector, -value.CoreScen) %>%
    pivot_longer(cols = c("value.WasteScen", "diff"), names_to = "sector") %>%
    mutate(patt = if_else(sector == "value.WasteScen", "Core", "A_WasteRed")) %>%
    mutate(sector = "Waste")


  # data set with all needed categories for plot
  #1. Intake
  plot_pcFood_all <- plot_pcFood %>%
    filter(category != "Waste", scenario %in% c("3C", "2C", "1.5C",
                                                "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                                                "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100",
                                                "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static")) %>%
    #2. Waste in Ref scenarios
    bind_rows(filter(plot_pcFood, sector == "Waste", scenario %in% c(SCENARIO_Ref_1.5C, SCENARIO_Ref_2C, SCENARIO_Ref_3C,
                                                                     SCENARIO_Static_1.5C, SCENARIO_Static_2C, SCENARIO_Static_3C))) %>%
    mutate(patt = "Core") %>%
    #4. DietWaste - Diet (Dashed lines) + Remaining Waste
    bind_rows(plot_pcFood_Waste_diff_WasteScen_all) %>%
    mutate(sector = factor(sector, levels = c("Livestock", "Oil crops", "Other crops", "Staple crops", "Waste", "Reductions_Diet_Ref"))) %>%
    # reset small negative values to 0
    mutate(value = if_else(value < 0, 0, value)) %>%
    filter(region == "Global") %>%
    mutate(value = if_else(sector == "Waste", value*-1, value)) %>%
    mutate(scenario = factor(scenario, levels = c("1.5C", "2C", "3C",
                                                  "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                                  "1.5C_Diet_2100", "2C_Diet_2100", "3C_Diet_2100",
                                                  "1.5C_Diet_2050", "2C_Diet_2050", "3C_Diet_2050")))

  FigS5 <- plot_pcFood_all %>%
    ggplot() +
    facet_wrap(~scenario, scales = "fixed", nrow = 4, label = as_labeller(scenario_labels_all_new)) +
    geom_bar_pattern(
      aes(x = year, y = value, fill = sector, pattern = patt),
      color = 1, stat = "identity", position = position_stack(reverse = FALSE),
      pattern_fill = "grey50", pattern_color = NA, pattern_density = 0.4, pattern_spacing = 0.03) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    scale_fill_manual(
      values = c("Livestock" = "#CC79A7",
                 "Oil crops" = "#F0E442",
                 "Other crops" = "#009E73",
                 "Staple crops" = "#56B4E9",
                 "Waste" = "#E69F00"
      ),
      labels = c("Livestock" = "Livestock",
                 "Oil crops" = "Oil crops",
                 "Other crops" = "Other crops",
                 "Staple crops" = "Staple crops",
                 "Waste" = "Waste")
    ) +
    scale_pattern_manual(
      values = c("Core" = "none", "A_WasteRed" = "stripe"),
      breaks = c( "A_WasteRed"),
      labels = c("A_WasteRed" = "Reductions from waste reduction efforts")
    )+
    guides(
      fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
      pattern = guide_legend(
        order = 2,
        override.aes = list(
          pattern = c("stripe"),
          pattern_fill = c("grey50"),
          fill = c( "white"),
          pattern_density = c(0.4),
          pattern_spacing = c(0.015)
        )
      )
    ) +
    labs(y = "kcal/cap/d", x = "", fill = "Sector", pattern = "", title = "Per capita food intake and waste") +
    theme_bw() + theme0 +
    theme(legend.position = "right")

  print(FigS5)

  FigS5 %>% Write_png(.name = "FigS5", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)


  # FIGURE S6: FOOD INTAKE WASTE ---------------------------------------------------------------


  plot_Food <- Food_IntakeWaste_GLO %>%
    mutate(category = if_else(sector == "Waste", "Waste", "Intake"))

  # Calculate the difference between Diet and Diet Waste scenarios
  plot_Food_WasteScen_all <- plot_Food %>%
    filter(scenario %in% c("1.5C_DietWaste_2050", "1.5C_DietWaste_2100", "1.5C_DietWaste_RegHetStatic",
                           "2C_DietWaste_2050", "2C_DietWaste_2100", "2C_DietWaste_RegHetStatic",
                           "3C_DietWaste_2050", "3C_DietWaste_2100", "3C_DietWaste_RegHetStatic")) %>%
    mutate(scenario = gsub("Waste", "", scenario))

  plot_Food_Waste_GLO_diff_WasteScen_all <- plot_Food %>%
    filter(scenario %in% c("3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                           "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100")) %>%
    left_join(plot_Food_WasteScen_all, by = c("scenario", "sector", "year", "category"),
              suffix = c(".CoreScen", ".WasteScen")) %>%
    mutate(diff = value.CoreScen - value.WasteScen) %>%
    filter(category == "Waste") %>%
    ungroup() %>%
    select(-sector, -value.CoreScen) %>%
    pivot_longer(cols = c("value.WasteScen", "diff"), names_to = "sector") %>%
    mutate(patt = if_else(sector == "value.WasteScen", "Core", "A_WasteRed")) %>%
    mutate(sector = "Waste")


  # data set with all needed categories for plot
  #1. Intake
  plot_Food_all <- plot_Food %>%
    filter(category != "Waste", scenario %in% c("3C", "2C", "1.5C",
                                                "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                                                "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100",
                                                "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static")) %>%
    #2. Waste in Ref scenarios
    bind_rows(filter(plot_Food, sector == "Waste", scenario %in% c(SCENARIO_Ref_1.5C, SCENARIO_Ref_2C, SCENARIO_Ref_3C,
                                                                   SCENARIO_Static_1.5C, SCENARIO_Static_2C, SCENARIO_Static_3C))) %>%
    mutate(patt = "Core") %>%
    #4. DietWaste - Diet (Dashed lines) + Remaining Waste
    bind_rows(plot_Food_Waste_GLO_diff_WasteScen_all) %>%
    mutate(sector = factor(sector, levels = c("Livestock", "Oil crops", "Other crops", "Staple crops", "Waste", "Reductions_Diet_Ref"))) %>%
    # reset small negative values to 0
    mutate(value = if_else(value < 0, 0, value)) %>%
    mutate(value = if_else(sector == "Waste", value*-1, value)) %>%
    mutate(scenario = factor(scenario, levels = c("1.5C", "2C", "3C",
                                                  "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                                  "1.5C_Diet_2100", "2C_Diet_2100", "3C_Diet_2100",
                                                  "1.5C_Diet_2050", "2C_Diet_2050", "3C_Diet_2050")))

  FigS6 <- plot_Food_all %>%
    ggplot() +
    facet_wrap(~scenario, scales = "fixed", nrow = 4, label = as_labeller(scenario_labels_all_new)) +
    geom_bar_pattern(
      aes(x = year, y = value, fill = sector, pattern = patt),
      color = 1, stat = "identity", position = position_stack(reverse = FALSE),
      pattern_fill = "grey50", pattern_color = NA, pattern_density = 0.4, pattern_spacing = 0.03) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    scale_fill_manual(
      values = c("Livestock" = "#CC79A7",
                 "Oil crops" = "#F0E442",
                 "Other crops" = "#009E73",
                 "Staple crops" = "#56B4E9",
                 "Waste" = "#E69F00"
      ),
      labels = c("Livestock" = "Livestock",
                 "Oil crops" = "Oil crops",
                 "Other crops" = "Other crops",
                 "Staple crops" = "Staple crops",
                 "Waste" = "Waste")
    ) +
    scale_pattern_manual(
      values = c("Core" = "none", "A_WasteRed" = "stripe"),
      breaks = c( "A_WasteRed"),
      labels = c("A_WasteRed" = "Reductions from waste reduction efforts")
    )+
    guides(
      fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
      pattern = guide_legend(
        order = 2,
        override.aes = list(
          pattern = c("stripe"),
          pattern_fill = c("grey50"),
          fill = c( "white"),
          pattern_density = c(0.4),
          pattern_spacing = c(0.015)
        )
      )
    ) +
    labs(y = "Pcal", x = "", fill = "Sector", pattern = "", title = "Food intake and waste") +
    theme_bw() + theme0 +
    theme(legend.position = "right")

  print(FigS6)

  FigS6 %>% Write_png(.name = "FigS6", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)




  # FIGURE S7: PER CAP FOOD WASTE ---------------------------------------------------------------

  plot_pcFood <-  bind_rows(pcFood_IntakeWaste_GLO,
                            pcFood_IntakeWaste_WBIncome) %>%
    mutate(category = if_else(sector != "Waste", "Intake", "Waste")) %>%
    mutate(region = factor(region, levels = c("Low/lower-middle", "Upper-middle", "High", "Global"))) %>%
    filter(year >= 2015)

  #Calculate the difference between Ref and Waste only scenarios
  plot_pcWaste_diff_Ref_WasteOnly <- diff_from_scen(plot_pcFood,
                                                    ref_scenario = SCENARIO_Ref_1.5C,
                                                    diff_scenarios = c("1.5C_Waste_2050", "1.5C_Waste_2100"),
                                                    join_var = c("sector", "year", "region", "category")) %>%
    group_by(scenario.diff, year, region, category) %>%
    dplyr::summarise(value = sum(value)) %>%
    filter(category == "Waste") %>%
    mutate(sector = "Waste",
           value = value*-1) %>%
    rename(scenario = scenario.diff) %>%
    mutate(patt = "WasteRed")

  #Calculate the difference between Ref and Diet scenarios
  plot_pcWaste_diff_Ref_Diet <- diff_from_scen(plot_pcFood,
                                               ref_scenario = SCENARIO_Ref_1.5C,
                                               diff_scenarios = c(SCENARIOS_1.5C),
                                               join_var = c("sector", "year", "region", "category")) %>%
    group_by(scenario.diff, year, region, category) %>%
    dplyr::summarise(value = sum(value)) %>%
    filter(category == "Waste") %>%
    mutate(sector = "Reductions_Diet_Ref",
           value = value*-1) %>%
    rename(scenario = scenario.diff) %>%
    mutate(patt = "DietRed") %>%
    filter(scenario %!in% c("1.5C_Waste_2050", "1.5C_Waste_2100"))

  plot_pcWaste_diff_Ref <- bind_rows(plot_pcWaste_diff_Ref_WasteOnly,
                                     plot_pcWaste_diff_Ref_Diet)

  # Calculate the difference between Diet and Diet Waste scenarios
  plot_pcWaste_WasteScen_1.5C <- plot_pcFood %>%
    filter(scenario %in% c("1.5C_DietWaste_2050", "1.5C_DietWaste_2100", "1.5C_DietWaste_RegHetStatic")) %>%
    mutate(scenario = gsub("Waste", "", scenario))

  plot_pcWaste_Waste_diff_WasteScen_1.5C <- plot_pcFood %>%
    filter(scenario %in% SCENARIOS_1.5C) %>%
    filter(scenario %!in% c(SCENARIO_Ref_1.5C,SCENARIO_Static_1.5C, SCENARIOS_WasteScen)) %>%
    left_join(plot_pcWaste_WasteScen_1.5C, by = c("scenario", "sector", "region", "year", "category"),
              suffix = c(".CoreScen", ".WasteScen")) %>%
    mutate(diff = value.CoreScen - value.WasteScen) %>%
    filter(category == "Waste") %>%
    select(-sector, -value.CoreScen) %>%
    pivot_longer(cols = c("value.WasteScen", "diff"), names_to = "sector") %>%
    mutate(patt = if_else(sector == "value.WasteScen", "Core", "WasteRed")) %>%
    mutate(sector = "Waste")

  # data set with all needed categories for plot

  plot_pcWaste_all_1.5C <-
    bind_rows(filter(plot_pcFood, sector == "Waste", scenario %in% c(SCENARIO_Ref_1.5C, SCENARIO_Static_1.5C, "1.5C_Waste_2050", "1.5C_Waste_2100"))) %>%
    mutate(patt = "Core") %>%
    #3. Ref - Diet (grey bars)
    bind_rows(plot_pcWaste_diff_Ref_Diet) %>%
    #4. Ref - Waste (grey bars)
    bind_rows(plot_pcWaste_diff_Ref_WasteOnly) %>%
    #4. DietWaste - Diet (Dashed lines) + Remaining Waste
    bind_rows(plot_pcWaste_Waste_diff_WasteScen_1.5C) %>%
    mutate(sector = factor(sector, levels = c("Waste", "Reductions_Diet_Ref"))) %>%
    # left_join_error_no_match(ScenarioMap %>% select(name, label), by = c("scenario" = "name")) %>%
    # reset small negative values to 0
    mutate(value = if_else(value < 0, 0, value))

  FigS7 <- plot_pcWaste_all_1.5C %>%
    filter(scenario %in% c("1.5C", "1.5C_Waste_2050","1.5C_Diet_2050"), year %in% c(2100)) %>%
    mutate(scenario = factor(scenario, levels = c("1.5C", "1.5C_Waste_2050","1.5C_Diet_2050"))) %>%
    ggplot() +
    facet_grid(scenario~category, scales = "fixed", label = as_labeller(c("1.5C" = "Reference",
                                                                          "1.5C_Waste_2050" = "Waste Red.",
                                                                          "1.5C_Diet_2050" = "Transformation",
                                                                          "Intake" = "Intake",
                                                                          "Waste" = "Waste"))) +
    geom_bar_pattern(
      aes(x = interaction(region, year), y = value, fill = sector, pattern = patt),
      color = 1, stat = "identity", position = position_stack(reverse = TRUE),
      pattern_fill = "grey50", pattern_color = NA, pattern_density = 0.4, pattern_spacing = 0.03) +
    geom_hline(yintercept = 0, color = "grey", linetype = 2) +
    scale_x_discrete(
      limits = rev(c("Low/lower-middle.2100",
                     "Upper-middle.2100",
                     "High.2100",
                     "Global.2100")),
      labels = c("Low/lower-middle.2100" = "Low/lower-middle",
                 "Upper-middle.2100" = "Upper-middle",
                 "High.2100" = "High",
                 "Global.2100" = "Global")
    ) +
    scale_fill_manual(
      values = c("Waste" = "#E69F00"
      ),
      labels = c("Waste" = "Waste")
    ) +
    scale_pattern_manual(
      values = c("Core" = "none", "DietRed" = "none",  "WasteRed" = "stripe"),
      breaks = c("DietRed", "WasteRed"),
      labels = c("DietRed" = "Reductions from dietary changes",
                 "WasteRed" = "Reductions from waste reduction efforts")
    ) +
    guides(
      fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
      pattern = guide_legend(
        order = 2,
        override.aes = list(
          pattern = c("none", "stripe"),
          pattern_fill = c("grey50", "grey50"),
          fill = c("grey50", "white"),
          pattern_density = c(1, 0.4),
          pattern_spacing = c(0, 0.015)
        )
      )
    ) +
    labs(y = "kcal/cap/d", x = "", fill = "Sector", pattern = "Reductions") +
    theme_bw() + theme0 + coord_flip() +
    theme(legend.position = "right")

  print(FigS7)


  FigS7 %>% Write_png(.name = "FigS7", .DIR_MODULE = DIR_MODULE, h = 6, w = 10, r = 300)




  # FIGURE S8: FOOD SYS EM BY SECTOR (ALL) ----------------------------------


    # PLOT TOTAL FS EM LINE (MULTI PANEL - SI) -------------------------------------

    plot_FS_EM_GLO_total <- plot_FS_EM_GLO %>%
      group_by(scenario, Units, year, region) %>%
      dplyr::summarise(value = sum(value)) %>%
      left_join_error_no_match(ScenarioMap %>% select(name, MitigationScen, DietScen, WasteRed), by = c("scenario" = "name"))%>%
      mutate(WasteRed = if_else(WasteRed == "StaticWaste", "No", WasteRed),
             WasteRed = if_else(WasteRed != "No", "WasteRed", "No")) %>%
      mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High", "Reg. Het.")),
             MitigationScen = factor(MitigationScen, levels = c("WB2C", "Near2C", "NoPrice")))


    p0 <- plot_FS_EM_GLO_total %>%
      filter(scenario %in% c("3C", "2C", "1.5C",
                             "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                             "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050",
                             "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100",
                             "3C_DietWaste_2100", "2C_DietWaste_2100", "1.5C_DietWaste_2100",
                             "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static")) %>%
      mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High"))) %>%
      filter(year >= 2020) %>%
      ggplot() +
      geom_line(aes(x = year, y = value, color = interaction(DietScen, WasteRed), linetype = interaction(DietScen, WasteRed)), size = 1) +
      geom_hline(yintercept = 0, color = "grey", linetype = 2) +
      labs(y = "GtCO2e/yr", x = "") +
      facet_wrap(~MitigationScen, scales = "fixed") +
      #geom_hline(yintercept = 0, color = "grey", linetype = 2) +
      scale_color_manual(name = "Scenario", values = c("Ref..No" = "#999999",
                                                       "Static.No" = "#E69F00",
                                                       "Med..No" = "#56B4E9",
                                                       "Med..WasteRed" = "#56B4E9",
                                                       "High.No" = "#CC79A7",
                                                       "High.WasteRed" = "#CC79A7"
      ),
      labels = c("Ref..No" = "Ref.",
                 "Static.No" = "Static",
                 "Med..No" = "Med. (Diet only)",
                 "Med..WasteRed" = "Med. (With waste reduction efforts)",
                 "High.No" = "High (Diet only)",
                 "High.WasteRed" = "High (With waste reduction efforts)")) +
      scale_linetype_manual(name = "Scenario", values = c("Ref..No" = "solid",
                                                          "Static.No" = "solid",
                                                          "Med..No" = "solid",
                                                          "Med..WasteRed" = "dashed",
                                                          "High.No" = "solid",
                                                          "High.WasteRed" = "dashed"),
                            labels = c("Ref..No" = "Ref.",
                                       "Static.No" = "Static",
                                       "Med..No" = "Med. (Diet only)",
                                       "Med..WasteRed" = "Med. (With waste reduction efforts)",
                                       "High.No" = "High (Diet only)",
                                       "High.WasteRed" = "High (With waste reduction efforts)"))+
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            strip.text = element_text(size = 13)) +
      theme(legend.position = "right",
            legend.key.width = unit(1, "cm"))

    print(p0)


    # PLOT FS EM SECTOR BAR (DIFF - MED) --------------------------------------------------------



    plot_FS_EM_GLO_diff <-  FS_EM_GLO_diff %>%
      filter(scenario.diff %in% c("1.5C_Diet_2100",
                                  "2C_Diet_2100",
                                  "3C_Diet_2100")) %>%
      mutate(sector = factor(sector, levels = c("LULUCF",
                                                "Forest fires",
                                                "Ag: Crop: N2O",
                                                "Ag: Crop: CH4",
                                                "Ag: Animal: N2O",
                                                "Ag: Animal: CH4",
                                                "Energy: Fertilizer",
                                                "Energy: Food processing",
                                                "Energy: On-farm",
                                                "Retail: Refrigeration",
                                                "Waste"))) %>%
      select(scenario.diff, sector, Units, year, region.diff, value)



    # Calculate the difference between Diet and Diet Waste scenarios
    plot_FS_EM_GLO_WasteScen_diff <- FS_EM_GLO_diff %>%
      filter(scenario.diff %in% c("1.5C_DietWaste_2100",
                                  "2C_DietWaste_2100",
                                  "3C_DietWaste_2100")) %>%
      mutate(scenario.diff = gsub("Waste", "", scenario.diff)) %>%
      select(scenario.diff, sector, Units, year, region.diff, value)

    # Get total FS EM in the waste scenarios (relative to Diet)
    # But rename them to match the core scenarios
    plot_FS_EM_GLO_diff_WasteScen_total <- plot_FS_EM_GLO_diff %>%
      left_join_error_no_match(plot_FS_EM_GLO_WasteScen_diff, by = c("scenario.diff", "region.diff", "Units", "sector", "year"),
                               suffix = c(".CoreScen", ".WasteScen")) %>%
      mutate(diff = value.WasteScen - value.CoreScen,
             patt = "WasteRed",
             sector = "WasteRed",
             Units = "GtCO2e/yr") %>%
      rename(value = diff) %>%
      group_by(scenario.diff, region.diff, sector, patt, year, Units) %>%
      dplyr::summarise(value = sum(value)) %>%
      select(scenario.diff, region.diff, year, sector, patt, value, Units)

    #Bind with the rest of the diff
    plot_FS_EM_GLO_diff_all <- plot_FS_EM_GLO_diff %>%
      select(scenario.diff, region.diff, sector, Units, year, value) %>%
      mutate(patt = "Core",
             Units = "GtCO2e/yr") %>%
      bind_rows(plot_FS_EM_GLO_diff_WasteScen_total) %>%
      mutate(value = if_else(year == 2020, 0, value)) %>%
      mutate(sector = factor(sector, levels = c("WasteRed",
                                                "LULUCF",
                                                "Forest fires",
                                                "Ag: Crop: N2O",
                                                "Ag: Crop: CH4",
                                                "Ag: Animal: N2O",
                                                "Ag: Animal: CH4",
                                                "Energy: Fertilizer",
                                                "Energy: Food processing",
                                                "Energy: On-farm",
                                                "Retail: Refrigeration",
                                                "Waste")))


    p2 <- plot_FS_EM_GLO_diff_all %>%
      filter(year >= 2020 & year <= 2100) %>%
      ggplot() +
      facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("1.5C_Diet_2100" = "WB2C Med - Ref",
                                                                         "2C_Diet_2100" = "Near2C Med - Ref",
                                                                         "3C_Diet_2100" = "NoPrice Med - Ref"))) +
      geom_bar_pattern(aes(x = year, y = value, fill = sector, pattern = patt), color = 1,
                       pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                       stat = "identity", position = position_stack(reverse = FALSE)) +
      geom_hline(yintercept = 0, color = "grey", linetype = 2) +
      labs(y = "GtCO2e/yr", x = "", fill = "Sector", pattern = "", title = "Medium ambition transformation - Reference scenarios") +
      #scale_fill_brewer(palette = "Set3", direction = 1) +
      scale_y_continuous(limits = c(-8.1,NA))+
      scale_fill_manual(values = c(
        "Ag: Animal: CH4" = "#B22222",  # Dark red
        "Ag: Animal: N2O" = "#E41A1C",  # Crimson red
        "Ag: Crop: CH4" = "#FF4500",    # Warm orange
        "Ag: Crop: N2O" = "#FF7F00",    # Deep orange
        "Energy: Fertilizer" = "#56B4E9",  # Teal blue
        "Energy: Food processing" = "#0072B2",  # Sky blue
        "Energy: On-farm" = "#003366",  # Navy blue
        "Forest fires" = "#FFD700",  # Bright yellow
        "LULUCF" = "#DAA520",     # Golden brown
        "Retail: Refrigeration" = "#984EA3",  # Purple
        "Waste" = "#009E73"             # Forest green
      ))+
      scale_pattern_manual(
        values = c("Core" = "none", "WasteRed" = "stripe"),
        breaks = c( "WasteRed"),
        labels = c("WasteRed" = "With waste reduction efforts")
      )+
      guides(
        fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
        pattern = guide_legend(
          order = 2,
          override.aes = list(
            pattern = c("stripe"),
            pattern_fill = c("grey50"),
            fill = c( "white"),
            pattern_density = c(0.4),
            pattern_spacing = c(0.015)
          )
        )
      ) +
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme(legend.position = "right")

    print(p2)



    # PLOT FS EM SECTOR BAR (DIFF - STATIC) --------------------------------------------------------

    plot_FS_EM_GLO_diff <-  FS_EM_GLO_diff %>%
      filter(scenario.diff %in% c("1.5C_Diet_Static",
                                  "2C_Diet_Static",
                                  "3C_Diet_Static")) %>%
      mutate(value = if_else(year == 2020, 0, value)) %>%
      mutate(sector = factor(sector, levels = c("LULUCF",
                                                "Forest fires",
                                                "Ag: Crop: N2O",
                                                "Ag: Crop: CH4",
                                                "Ag: Animal: N2O",
                                                "Ag: Animal: CH4",
                                                "Energy: Fertilizer",
                                                "Energy: Food processing",
                                                "Energy: On-farm",
                                                "Retail: Refrigeration",
                                                "Waste")))

    p1 <- plot_FS_EM_GLO_diff %>%
      filter(year >= 2020 & year <= 2100,
      ) %>%
      ggplot() +
      facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("1.5C_Diet_Static" = "WB2C Static - Ref",
                                                                         "2C_Diet_Static" = "Near2C Static - Ref",
                                                                         "3C_Diet_Static" = "NoPrice Static - Ref"))) +
      geom_bar(aes(x = year, y = value, fill = sector), color = 1, stat = "identity", position = position_stack(reverse = FALSE)) +
      geom_hline(yintercept = 0, color = "grey", linetype = 2) +
      labs(y = "GtCO2e/yr", x = "", fill = "Sector", title = "Static - Reference scenarios") +
      scale_y_continuous(limits = c(-8.1,NA))+
      #scale_fill_brewer(palette = "Set3", direction = 1) +
      scale_fill_manual(values = c(
        "Ag: Animal: CH4" = "#B22222",  # Dark red
        "Ag: Animal: N2O" = "#E41A1C",  # Crimson red
        "Ag: Crop: CH4" = "#FF4500",    # Warm orange
        "Ag: Crop: N2O" = "#FF7F00",    # Deep orange
        "Energy: Fertilizer" = "#56B4E9",  # Teal blue
        "Energy: Food processing" = "#0072B2",  # Sky blue
        "Energy: On-farm" = "#003366",  # Navy blue
        "Forest fires" = "#FFD700",  # Bright yellow
        "LULUCF" = "#DAA520",     # Golden brown
        "Retail: Refrigeration" = "#984EA3",  # Purple
        "Waste" = "#009E73"             # Forest green
      ))+
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme(legend.position = "none")

    print(p1)



    # FIG S8 COMBINED ---------------------------------------------------------


    patchwork_FS_EM <- p1 / p2 +
      plot_layout(guides = "collect", heights = c(1, 1)) +
      theme(legend.position = "right") +
      plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations


    patchwork_FS_EM_with_line <- p0 / patchwork_FS_EM +
      plot_layout(heights = c(1, 2.5)) +
      plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

    print(patchwork_FS_EM_with_line)

    patchwork_FS_EM_with_line %>% Write_png(.name = "FigS8", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)



  # FIGURE S9: AG PRODUCTION  ---------------------------------------------------------------

    AgBal <- "AgBal" %>% PluckBind()


    # Ag balances by demand sector

    AgBalDemandSector <- AgBal %>%
      # remove intermediate demands (fooder, pasture) and forest
      filter(sector %!in% c("Pasture_FodderGrass", "FodderHerb_Residue",
                            "regional industrial_roundwood", "traded industrial_sawnwood", "traded industrial_roundwood")) %>%
      mutate(sector_input = paste(sector, input)) %>%
      left_join_error_no_match(MapAgBalDemand, by = c("sector_input")) %>%
      group_by(scenario, region, sector0, year, Units) %>%
      dplyr::summarise(value = sum(value))

    AgBalDemandSector_GLO <- AgBalDemandSector %>%
      group_by(scenario, sector0, year, Units) %>%
      dplyr::summarise(value = sum(value))


    plot_AgBalDemandSector_GLO <- AgBalDemandSector_GLO %>%
      mutate(sector0 = factor(sector0, levels = c("Non-food",
                                                  "Energy",
                                                  "Feed",
                                                  "Food - Livestock",
                                                  "Food - Oil crops",
                                                  "Food - Other crops",
                                                  "Food - Staple crops"))) %>%
      mutate(scenario = factor(scenario, levels = c("1.5C", "2C", "3C",
                                                    "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                                    "1.5C_DietWaste_2100", "2C_DietWaste_2100", "3C_DietWaste_2100",
                                                    "1.5C_DietWaste_2050", "2C_DietWaste_2050", "3C_DietWaste_2050")))



    FigS9 <- plot_AgBalDemandSector_GLO %>%
      filter(scenario %in% c("3C", "2C", "1.5C",
                             "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050",
                             "3C_DietWaste_2100", "2C_DietWaste_2100", "1.5C_DietWaste_2100",
                             "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static"),
             year >= 2020) %>%
      ggplot() +
      facet_wrap(~scenario, scales = "fixed", nrow = 4, label = as_labeller(scenario_labels_all_new)) +
      geom_bar(aes(x = year, y = value, fill = sector0), color = 1,
               stat = "identity") +
      geom_hline(yintercept = 0, color = "black", linetype = 1) +
      scale_fill_manual(
        values = c("Non-food" = "#5A3E29",
                   "Energy" = "#984EA3",
                   "Feed" = "#B22222",
                   "Food - Livestock" = "#CC79A7",
                   "Food - Oil crops" = "#F0E442",
                   "Food - Other crops" = "#009E73",
                   "Food - Staple crops" = "#56B4E9"

        ),
        labels = c("Non-food" = "Non-food",
                   "Energy" = "Biofuels",
                   "Feed" = "Feed",
                   "Food - Livestock" = "Food - Livestock",
                   "Food - Oil crops" = "Food - Oil crops",
                   "Food - Other crops" = "Food - Other crops",
                   "Food - Staple crops" = "Food - Staple crops"

        ))+
      scale_x_continuous()+
      labs(y = "Mt", x = "", fill = "Sector", title = "Agircultural production by demand sector") +
      theme_bw() + theme0

    print(FigS9)

    FigS9 %>% Write_png(.name = "FigS9", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)


  # FIGURE S10: LAND ALLOCATION (DIFF FROM 2025) ---------------------------------------------------------

    Land_reg <- PluckBind("Aggland") %>%
      left_join_error_no_match(select(LandMapping, LandLeaf, LandCover4), by = c("LandLeaf")) %>%
      rename(landtype = LandCover4) %>%
      group_by(scenario, region, landtype, year, Units) %>%
      dplyr::summarise(value = sum(value)) %>%
      mutate(value = value*100,
             Units = "Thous. Ha")


    Land_GLO <- Land_reg %>%
      group_by(scenario, landtype, year, Units) %>%
      dplyr::summarise(value = sum(value)) %>%
      mutate(region = "Global")

    # Cropland except energy
    Cropland_GLO <- Land_GLO %>%
      filter(landtype %in% c("Cropland - Oil Crops",
                             "Cropland - Others",
                             "Cropland - Staples")) %>%
      group_by(scenario, region, year, Units) %>%
      dplyr::summarise(value = sum(value))

    #FOrest land
    Forestland_GLO <- Land_GLO %>%
      filter(landtype %in% c("Forest - Managed",
                             "Forest - Unmanaged")) %>%
      group_by(scenario, region, year, Units) %>%
      dplyr::summarise(value = sum(value))



    Land_GLO_diff_2025 <- diff_from_year(Land_GLO,
                                         diff_year = 2025,
                                         ref_scenario = SCENARIO_Ref_1.5C,
                                         diff_scenarios =  c(SCENARIOS_1.5C, SCENARIOS_2C, SCENARIOS_3C),
                                         join_var = c("region", "landtype", "Units")) %>%
      mutate(scenario.diff = factor(scenario.diff, levels = c("1.5C", "2C", "3C",
                                                              "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                                              "1.5C_DietWaste_2100", "2C_DietWaste_2100", "3C_DietWaste_2100",
                                                              "1.5C_DietWaste_2050", "2C_DietWaste_2050", "3C_DietWaste_2050")))

    Land_GLO_diff_Ref_1.5C <- diff_from_scen(Land_GLO,
                                             ref_scenario = SCENARIO_Ref_1.5C,
                                             diff_scenarios = SCENARIOS_1.5C,
                                             join_var = c("region", "year", "landtype", "Units"))

    Land_GLO_diff_Ref_2C <- diff_from_scen(Land_GLO,
                                           ref_scenario = SCENARIO_Ref_2C,
                                           diff_scenarios = SCENARIOS_2C,
                                           join_var = c("region", "year", "landtype", "Units"))

    Land_GLO_diff_Ref_3C <- diff_from_scen(Land_GLO,
                                           ref_scenario = SCENARIO_Ref_3C,
                                           diff_scenarios = SCENARIOS_3C,
                                           join_var = c("region", "year", "landtype", "Units"))

    Land_GLO_diff_Ref <- bind_rows(Land_GLO_diff_Ref_1.5C,
                                   Land_GLO_diff_Ref_2C,
                                   Land_GLO_diff_Ref_3C)




    FigS10 <- Land_GLO_diff_2025 %>%
      filter(scenario.diff %in% c("1.5C", "2C", "3C",
                                  "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                  "1.5C_DietWaste_2100", "2C_DietWaste_2100", "3C_DietWaste_2100",
                                  "1.5C_DietWaste_2050", "2C_DietWaste_2050", "3C_DietWaste_2050"),
             year.diff >= 2025,
             landtype != "Other Fixed") %>%
      ggplot() +  facet_wrap(~scenario.diff, scales = "fixed", nrow = 4, label = as_labeller(scenario_labels_all_new)) +
      geom_bar(aes(x = year.diff, y = value/(10^3), fill = landtype), color = 1,
               stat = "identity") +
      geom_hline(yintercept = 0, color = "black", linetype = 1) +
      scale_fill_manual(values = c(
        "Cropland - Energy" = "#984EA3",
        "Cropland - Oil Crops" = "#F0E442",
        "Cropland - Others" = "#009E73",
        "Cropland - Staples" = "#56B4E9",
        "Forest - Managed" = "#A0522D",
        "Forest - Unmanaged" = "#5A3E29",
        "Other Natural" = "#C2C287",
        "Pasture - Managed" = "#CC79A7",
        "Pasture - Unmanaged" = "#882D61"
      ))+
      scale_x_continuous()+
      labs(y = "MHa", x = "", fill = "Land type", title = "Change in land allocation from 2025") +
      theme_bw() + theme0


    print(FigS10)

    FigS10 %>% Write_png(.name = "FigS10", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)


  # FIGURE S11: LAND ALLOCATION (DIFF FROM REF) -----------------------------


    plot_Land_GLO_diff_Ref <- Land_GLO_diff_Ref %>%
      filter(scenario.diff %in% c("1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                  "1.5C_DietWaste_2100", "2C_DietWaste_2100", "3C_DietWaste_2100",
                                  "1.5C_DietWaste_2050", "2C_DietWaste_2050", "3C_DietWaste_2050"))



    p2 <- plot_Land_GLO_diff_Ref %>%
      filter(scenario.diff %in% c(
        "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static"),
        year >= 2020) %>%
      ggplot() +
      facet_wrap(~scenario.diff, scales = "fixed", nrow = 1, label = as_labeller(c("1.5C_Diet_Static" = "WB2C Static - Ref",
                                                                                   "2C_Diet_Static" = "Near2C Static - Ref",
                                                                                   "3C_Diet_Static" = "NoPrice Static - Ref"))) +
      geom_bar(aes(x = year, y = value/(10^3), fill = landtype), color = 1,
               stat = "identity") +
      geom_hline(yintercept = 0, color = "black", linetype = 1) +
      scale_fill_manual(values = c(
        "Cropland - Energy" = "#984EA3",
        "Cropland - Oil Crops" = "#F0E442",
        "Cropland - Others" = "#009E73",
        "Cropland - Staples" = "#56B4E9",
        "Forest - Managed" = "#A0522D",
        "Forest - Unmanaged" = "#5A3E29",
        "Other Natural" = "#C2C287",
        "Pasture - Managed" = "#CC79A7",
        "Pasture - Unmanaged" = "#882D61"
      ))+
      scale_x_continuous()+
      scale_y_continuous(limits = c(-1250, 1250))+
      labs(y = "MHa", x = "", fill = "Sector", title = "Static - Reference scenarios") +
      theme_bw() + theme0

    p3 <- plot_Land_GLO_diff_Ref %>%
      filter(scenario.diff %in% c(
        "3C_DietWaste_2100", "2C_DietWaste_2100", "1.5C_DietWaste_2100"),
        year >= 2020) %>%
      ggplot() +
      facet_wrap(~scenario.diff, scales = "fixed", nrow = 1, label = as_labeller(c("1.5C_DietWaste_2100" = "WB2C Med - Ref",
                                                                                   "2C_DietWaste_2100" = "Near2C Med - Ref",
                                                                                   "3C_DietWaste_2100" = "NoPrice Med - Ref"))) +
      geom_bar(aes(x = year, y = value/(10^3), fill = landtype), color = 1,
               stat = "identity") +
      geom_hline(yintercept = 0, color = "black", linetype = 1) +
      scale_fill_manual(values = c(
        "Cropland - Energy" = "#984EA3",
        "Cropland - Oil Crops" = "#F0E442",
        "Cropland - Others" = "#009E73",
        "Cropland - Staples" = "#56B4E9",
        "Forest - Managed" = "#A0522D",
        "Forest - Unmanaged" = "#5A3E29",
        "Other Natural" = "#C2C287",
        "Pasture - Managed" = "#CC79A7",
        "Pasture - Unmanaged" = "#882D61"
      ))+
      scale_x_continuous()+
      scale_y_continuous(limits = c(-1250, 1250))+
      labs(y = "MHa", x = "", fill = "Sector", title = "Medium ambition transformation - Reference scenarios") +
      theme_bw() + theme0

    p4 <- plot_Land_GLO_diff_Ref %>%
      filter(scenario.diff %in% c(
        "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050"),
        year >= 2020) %>%
      ggplot() +
      facet_wrap(~scenario.diff, scales = "fixed", nrow = 1, label = as_labeller(c("1.5C_DietWaste_2050" = "WB2C High - Ref",
                                                                                   "2C_DietWaste_2050" = "Near2C High - Ref",
                                                                                   "3C_DietWaste_2050" = "NoPrice High - Ref"))) +
      geom_bar(aes(x = year, y = value/(10^3), fill = landtype), color = 1,
               stat = "identity") +
      geom_hline(yintercept = 0, color = "black", linetype = 1) +
      scale_fill_manual(values = c(
        "Cropland - Energy" = "#984EA3",
        "Cropland - Oil Crops" = "#F0E442",
        "Cropland - Others" = "#009E73",
        "Cropland - Staples" = "#56B4E9",
        "Forest - Managed" = "#A0522D",
        "Forest - Unmanaged" = "#5A3E29",
        "Other Natural" = "#C2C287",
        "Pasture - Managed" = "#CC79A7",
        "Pasture - Unmanaged" = "#882D61"
      ))+
      scale_x_continuous()+
      scale_y_continuous(limits = c(-1250, 1250))+
      labs(y = "MHa", x = "", fill = "Sector", title = "High ambition transformation - Reference scenarios") +
      theme_bw() + theme0


    Fig_Land_diff_Ref <- p2 / p3 / p4  +
      plot_layout(guides = "collect", heights = c(1, 1, 1)) +
      theme(legend.position = "right") +
      plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

    print(Fig_Land_diff_Ref)

    Fig_Land_diff_Ref %>% Write_png(.name = "FigS11", .DIR_MODULE = DIR_MODULE, h = 14, w = 12, r = 300)




  # FIGURE S12: CDR ---------------------------------------------------------

    CSQ <- "CSQ" %>% PluckBind()

    CDR_GLO <- CSQ %>%
      filter(subsector %in% c("dac", "biomass", "biomass (IGCC CCS)", "biomass (conv CCS)", "biomass liquids")) %>%
      mutate(sector0 = if_else(grepl("biomass", subsector), "BECCS", "DACCS"),
             value = value*44/12,
             Units = "MtCO2e") %>%
      group_by(scenario, sector0, Units, year) %>%
      dplyr::summarise(value = sum(value))


    CDR_LUC_GLO <- FS_LUC_EM %>%
      group_by(scenario, sector, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      rename(sector0 = sector) %>%
      mutate(value = value*-1000,
             Units = "MtCO2e") %>%
      filter(value >= 0) # keep only the sequestrations

    all_CDR_GLO <- bind_rows(CDR_GLO,
                             CDR_LUC_GLO)

    plot_CDR_GLO <- all_CDR_GLO %>%
      mutate(scenario = factor(scenario, levels = c("1.5C", "2C", "3C",
                                                    "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                                    "1.5C_DietWaste_2100", "2C_DietWaste_2100", "3C_DietWaste_2100",
                                                    "1.5C_DietWaste_2050", "2C_DietWaste_2050", "3C_DietWaste_2050")))


    p1 <- plot_CDR_GLO %>%
      filter(year >= 2020, scenario %in% c("1.5C", "2C", "3C",
                                           "1.5C_Diet_Static", "2C_Diet_Static", "3C_Diet_Static",
                                           "1.5C_DietWaste_2100", "2C_DietWaste_2100", "3C_DietWaste_2100",
                                           "1.5C_DietWaste_2050", "2C_DietWaste_2050", "3C_DietWaste_2050")) %>%
      ggplot() +
      facet_wrap(~scenario, scales = "fixed", nrow = 4, label = as_labeller(scenario_labels_all_new)) +
      geom_bar(aes(x = year, y = value/1000, fill = sector0), color = 1,
               stat = "identity") +
      geom_hline(yintercept = 0, color = "black", linetype = 1) +
      scale_x_continuous()+
      labs(y = "GtCO2e", x = "", fill = "Sector", title = "CO2 sequestration") +
      theme_bw() + theme0

    print(p1)

    p1 %>% Write_png(.name = "FigS12", .DIR_MODULE = DIR_MODULE, h = 12, w = 12, r = 300)

  # FIGURE S13: EMISSIONS INTENSITY ------------------------------------------

    plot_AgNonCO2_EmFactor <- AgNonCO2_EmFactor %>%
      left_join_error_no_match(ScenarioMap %>% select(name, MitigationScen, DietScen, WasteRed), by = c("scenario" = "name"))%>%
      mutate(WasteRed = if_else(WasteRed == "StaticWaste", "No", WasteRed),
             WasteRed = if_else(WasteRed != "No", "WasteRed", "No")) %>%
      mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High", "Reg. Het.")),
             MitigationScen = factor(MitigationScen, levels = c("WB2C", "Near2C", "NoPrice")))

    FigS13 <- plot_AgNonCO2_EmFactor %>%
      filter(scenario %in% c("3C", "2C", "1.5C",
                             "3C_Diet_2050", "2C_Diet_2050", "1.5C_Diet_2050",
                             "3C_DietWaste_2050", "2C_DietWaste_2050", "1.5C_DietWaste_2050",
                             "3C_Diet_2100", "2C_Diet_2100", "1.5C_Diet_2100",
                             "3C_DietWaste_2100", "2C_DietWaste_2100", "1.5C_DietWaste_2100",
                             "3C_Diet_Static", "2C_Diet_Static", "1.5C_Diet_Static")) %>%
      mutate(DietScen = factor(DietScen, levels = c("Ref.", "Static", "Med.", "High"))) %>%
      filter(year >= 2020) %>%
      ggplot() +
      geom_line(aes(x = year, y = value, color = interaction(DietScen, WasteRed), linetype = interaction(DietScen, WasteRed)), size = 1) +
      geom_hline(yintercept = 0, color = "grey", linetype = 1.5) +
      labs(y = "kg CO2e per kg", x = "", title = "Agricultural non-CO2 emissions intensity") +
      facet_wrap(~MitigationScen, scales = "fixed") +
      #geom_hline(yintercept = 0, color = "grey", linetype = 2) +
      scale_color_manual(name = "Scenario", values = c("Ref..No" = "#999999",
                                                       "Static.No" = "#E69F00",
                                                       "Med..No" = "#56B4E9",
                                                       "Med..WasteRed" = "#56B4E9",
                                                       "High.No" = "#CC79A7",
                                                       "High.WasteRed" = "#CC79A7"
      ),
      labels = c("Ref..No" = "Ref.",
                 "Static.No" = "Static",
                 "Med..No" = "Med. (Diet only)",
                 "Med..WasteRed" = "Med. (With waste reduction efforts)",
                 "High.No" = "High (Diet only)",
                 "High.WasteRed" = "High (With waste reduction efforts)")) +
      scale_linetype_manual(name = "Scenario", values = c("Ref..No" = "solid",
                                                          "Static.No" = "solid",
                                                          "Med..No" = "solid",
                                                          "Med..WasteRed" = "dashed",
                                                          "High.No" = "solid",
                                                          "High.WasteRed" = "dashed"),
                            labels = c("Ref..No" = "Ref.",
                                       "Static.No" = "Static",
                                       "Med..No" = "Med. (Diet only)",
                                       "Med..WasteRed" = "Med. (With waste reduction efforts)",
                                       "High.No" = "High (Diet only)",
                                       "High.WasteRed" = "High (With waste reduction efforts)"))+
      theme_bw() + theme0 +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            strip.text = element_text(size = 13)) +
      theme(legend.position = "right",
            legend.key.width = unit(1, "cm"))

    print(FigS13)

    FigS13 %>% Write_png(.name = "FigS13", .DIR_MODULE = DIR_MODULE, h = 4, w = 10, r = 300)



# FIGURE S14: CUM FOOD SYS EM BY INCOME GROUP (ALL) ---------------------------------------


    # PLOT CUM FS EM INCOME BAR (DIFF - NEAR 2C SCEN) -----------------------------



p1 <- cum_FS_EM_WBIncome_ProdBased_diff_all %>%
  filter(scenario.diff %in% c("2C_Diet_Static", "2C_Diet_2100", "2C_Diet_2050"),
         year >= 2020 & year <= 2100) %>%
  mutate(scenario.diff = factor(scenario.diff, levels = c("2C_Diet_Static", "2C_Diet_2100", "2C_Diet_2050"))) %>%
  mutate(region = factor(region, levels = c("WasteRed", "Low/lower-middle", "Upper-middle", "High"))) %>%
  ggplot() +
  facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("2C_Diet_Static" = "Near2C Static - Ref",
                                                                     "2C_Diet_2100" = "Near2C Medium - Ref",
                                                                     "2C_Diet_2050" = "Near2C High - Ref"))) +
  geom_bar_pattern(aes(x = year, y = value, fill = region, pattern = patt), color = 1,
                   pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                   stat = "identity", position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  labs(y = "GtCO2e", x = "", fill = "Income category", pattern = "", title = "Near2C food demand scenarios - Near2C Ref") +
  scale_pattern_manual(
    values = c("Core" = "none", "WasteRed" = "stripe"),
    breaks = c( "WasteRed"),
    labels = c("WasteRed" = "With waste reduction efforts")
  )+
  scale_fill_manual(values = c(
    "Low/lower-middle" = "#1B9E77",
    "Upper-middle" = "#D95F02",
    "High" = "#7570B3"
  ))+
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        pattern = c("stripe"),
        pattern_fill = c("grey50"),
        fill = c( "white"),
        pattern_density = c(0.4),
        pattern_spacing = c(0.015)
      )
    )
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "right")

print(p1)


    # PLOT CUM FS EM INCOME BAR (DIFF - NO PRICE SCEN) -----------------------------



p2 <- cum_FS_EM_WBIncome_ProdBased_diff_all %>%
  filter(scenario.diff %in% c("3C_Diet_Static", "3C_Diet_2100", "3C_Diet_2050"),
         year >= 2020 & year <= 2100) %>%
  mutate(scenario.diff = factor(scenario.diff, levels = c("3C_Diet_Static", "3C_Diet_2100", "3C_Diet_2050"))) %>%
  mutate(region = factor(region, levels = c("WasteRed", "Low/lower-middle", "Upper-middle", "High"))) %>%
  ggplot() +
  facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("3C_Diet_Static" = "NoPrice Static - Ref",
                                                                     "3C_Diet_2100" = "NoPrice Medium - Ref",
                                                                     "3C_Diet_2050" = "NoPrice High - Ref"))) +
  geom_bar_pattern(aes(x = year, y = value, fill = region, pattern = patt), color = 1,
                   pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                   stat = "identity", position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  labs(y = "GtCO2e", x = "", fill = "Income category", pattern = "", title = "NoPrice food demand scenarios - NoPrice Ref") +
  scale_pattern_manual(
    values = c("Core" = "none", "WasteRed" = "stripe"),
    breaks = c( "WasteRed"),
    labels = c("WasteRed" = "With waste reduction efforts")
  )+
  scale_fill_manual(values = c(
    "Low/lower-middle" = "#1B9E77",
    "Upper-middle" = "#D95F02",
    "High" = "#7570B3"
  ))+
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        pattern = c("stripe"),
        pattern_fill = c("grey50"),
        fill = c( "white"),
        pattern_density = c(0.4),
        pattern_spacing = c(0.015)
      )
    )
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "right")

print(p2)



    # FIG S14 COMBINED ---------------------------------------------------------


FigS14 <- p1 / p2 +
  plot_layout(guides = "collect", heights = c(1, 1)) +
  theme(legend.position = "right") +
  plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

print(FigS14)

FigS14 %>% Write_png(.name = "FigS14", .DIR_MODULE = DIR_MODULE, h = 8, w = 12, r = 300)



# FIGURE S15: PER CAPITA CUM FS EM BY INCOME GROUP ----------------------------------------------------

POP_WBIncome_GLO <- bind_rows(POP_WBIncome,
                              POP_GLO) %>%
  mutate(region = if_else(region == "Global", "WasteRed", region))


pc_cum_FS_EM_WBIncome <- plot_cum_FS_EM_WBIncome %>%
  left_join(POP_WBIncome, by = c("scenario", "region", "year"), suffix = c(".em", ".pop")) %>%
  mutate(value = (value.em*(10^3)/value.pop))

pc_cum_FS_EM_WBIncome_ProdBased_diff_all <- cum_FS_EM_WBIncome_ProdBased_diff_all %>%
  left_join(POP_WBIncome_GLO, by = c("scenario.diff" = "scenario", "region", "year"), suffix = c(".em", ".pop")) %>%
  mutate(value = (value.em*(10^3)/value.pop))


p1 <- pc_cum_FS_EM_WBIncome %>%
  filter(scenario %in% c("1.5C", "2C", "3C"),
         year >= 2020 & year <= 2100) %>%
  ggplot() +
  facet_grid(~MitigationScen, scales = "fixed") +
  geom_bar(aes(x = year, y = value, fill = region), color = 1, stat = "identity", position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  labs(y = "MtCO2e/cap", x = "", fill = "Income category", title = "Reference scenarios") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "none")

print(p1)


p2 <- pc_cum_FS_EM_WBIncome_ProdBased_diff_all %>%
  filter(scenario.diff %in% c("1.5C_Diet_Static", "1.5C_Diet_2100", "1.5C_Diet_2050"),
         year >= 2020 & year <= 2100) %>%
  mutate(scenario.diff = factor(scenario.diff, levels = c("1.5C_Diet_Static", "1.5C_Diet_2100", "1.5C_Diet_2050"))) %>%
  mutate(region = factor(region, levels = c("WasteRed", "Low/lower-middle", "Upper-middle", "High"))) %>%
  ggplot() +
  facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("1.5C_Diet_Static" = "WB2C Static - Ref",
                                                                     "1.5C_Diet_2100" = "WB2C Medium - Ref",
                                                                     "1.5C_Diet_2050" = "WB2C High - Ref"))) +
  geom_bar_pattern(aes(x = year, y = value, fill = region, pattern = patt), color = 1,
                   pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                   stat = "identity", position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  labs(y = "MtCO2e/cap", x = "", fill = "Income category", pattern = "", title = "WB2C food demand scenarios - WB2C Ref") +
  scale_pattern_manual(
    values = c("Core" = "none", "WasteRed" = "stripe"),
    breaks = c( "WasteRed"),
    labels = c("WasteRed" = "With waste reduction efforts")
  )+
  scale_fill_manual(values = c(
    "Low/lower-middle" = "#1B9E77",
    "Upper-middle" = "#D95F02",
    "High" = "#7570B3"
  ))+
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        pattern = c("stripe"),
        pattern_fill = c("grey50"),
        fill = c( "white"),
        pattern_density = c(0.4),
        pattern_spacing = c(0.015)
      )
    )
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "right")

print(p2)

p3 <- pc_cum_FS_EM_WBIncome_ProdBased_diff_all %>%
  filter(scenario.diff %in% c("2C_Diet_Static", "2C_Diet_2100", "2C_Diet_2050"),
         year >= 2020 & year <= 2100) %>%
  mutate(scenario.diff = factor(scenario.diff, levels = c("2C_Diet_Static", "2C_Diet_2100", "2C_Diet_2050"))) %>%
  mutate(region = factor(region, levels = c("WasteRed", "Low/lower-middle", "Upper-middle", "High"))) %>%
  ggplot() +
  facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("2C_Diet_Static" = "Near2C Static - Ref",
                                                                     "2C_Diet_2100" = "Near2C Medium - Ref",
                                                                     "2C_Diet_2050" = "Near2C High - Ref"))) +
  geom_bar_pattern(aes(x = year, y = value, fill = region, pattern = patt), color = 1,
                   pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                   stat = "identity", position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  labs(y = "MtCO2e/cap", x = "", fill = "Income category", pattern = "", title = "Near2C food demand scenarios - Near2C Ref") +
  scale_pattern_manual(
    values = c("Core" = "none", "WasteRed" = "stripe"),
    breaks = c( "WasteRed"),
    labels = c("WasteRed" = "With waste reduction efforts")
  )+
  scale_fill_manual(values = c(
    "Low/lower-middle" = "#1B9E77",
    "Upper-middle" = "#D95F02",
    "High" = "#7570B3"
  ))+
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        pattern = c("stripe"),
        pattern_fill = c("grey50"),
        fill = c( "white"),
        pattern_density = c(0.4),
        pattern_spacing = c(0.015)
      )
    )
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "right")

print(p3)


p4 <- pc_cum_FS_EM_WBIncome_ProdBased_diff_all %>%
  filter(scenario.diff %in% c("3C_Diet_Static", "3C_Diet_2100", "3C_Diet_2050"),
         year >= 2020 & year <= 2100) %>%
  mutate(scenario.diff = factor(scenario.diff, levels = c("3C_Diet_Static", "3C_Diet_2100", "3C_Diet_2050"))) %>%
  mutate(region = factor(region, levels = c("WasteRed", "Low/lower-middle", "Upper-middle", "High"))) %>%
  ggplot() +
  facet_grid(~scenario.diff, scales = "fixed", label = as_labeller(c("3C_Diet_Static" = "NoPrice Static - Ref",
                                                                     "3C_Diet_2100" = "NoPrice Medium - Ref",
                                                                     "3C_Diet_2050" = "NoPrice High - Ref"))) +
  geom_bar_pattern(aes(x = year, y = value, fill = region, pattern = patt), color = 1,
                   pattern_fill = "white", pattern_color = NA, pattern_density = 0.6, pattern_spacing = 0.03,
                   stat = "identity", position = position_stack(reverse = FALSE)) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  labs(y = "MtCO2e/cap", x = "", fill = "Income category", pattern = "", title = "NoPrice food demand scenarios - NoPrice Ref") +
  scale_pattern_manual(
    values = c("Core" = "none", "WasteRed" = "stripe"),
    breaks = c( "WasteRed"),
    labels = c("WasteRed" = "With waste reduction efforts")
  )+
  scale_fill_manual(values = c(
    "Low/lower-middle" = "#1B9E77",
    "Upper-middle" = "#D95F02",
    "High" = "#7570B3"
  ))+
  guides(
    fill = guide_legend(order = 1, override.aes = list(pattern = "none")),  # Removes stripes from the fill legend
    pattern = guide_legend(
      order = 2,
      override.aes = list(
        pattern = c("stripe"),
        pattern_fill = c("grey50"),
        fill = c( "white"),
        pattern_density = c(0.4),
        pattern_spacing = c(0.015)
      )
    )
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(legend.position = "right")

print(p4)

FigS15 <- p1 / p2 / p3 / p4 +
  plot_layout(guides = "collect", heights = c(1, 1, 1, 1)) +
  theme(legend.position = "right") +
  plot_annotation(tag_levels = "A")  # Automatically adds "A" and "B" annotations

print(FigS15)

FigS15 %>% Write_png(.name = "FigS15", .DIR_MODULE = DIR_MODULE, h = 16, w = 12, r = 300)

