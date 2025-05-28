library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Load mappings ----
readr::read_csv("data/maps/LandMappingV7p1.csv") -> LandMapping
readr::read_csv("data/maps/Regmapping.csv") -> Regmapping
readr::read_csv("data/maps/AgCommMapping.csv") -> MapAgCOMM
readr::read_csv("data/maps/AgBalDemandMap.csv") -> MapAgBalDemand

# Load ggplot themes ----
fontfamily = "Arial"
fontfamily = "sans"
#windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey75"),
  #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  #axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  #plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.title = element_text(hjust = 0, face = "bold"),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

## theme1 ----
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid = element_blank(),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())

# Load other utility funcs ----

Write_png <- function(.plot, .name, .DIR_MODULE, w = 10, h = 10, r = 300){

  # create a fig data output folder
  dir.create(file.path(DIR_OUTPUT, .DIR_MODULE), showWarnings = F)

  ggsave(file.path(DIR_OUTPUT, .DIR_MODULE, paste0(.name,".png")),
         plot = .plot,
         width = w, height = h, dpi = r)

  # ggsave(file.path(DIR_OUTPUT, .DIR_MODULE, paste0(.name,".svg")),
  #        plot = .plot,
  #        width = w, height = h, dpi = r)

  # png(file.path(DIR_OUTPUT, Project, paste0(name,".png")), width = w, height = h, res = r)
  # print(.plot)
  # dev.off()
}


Agg_reg <- function(.data, ...){
  .data %>%
    group_by(scenario, ... , year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    ungroup() %>%
    return()
}


Proc_Diff <- function(.df, type = "R", ...){
  if (type == "R"){
    .df %>%
      group_by_at(vars(-value, ...)) %>%
      mutate(value = value / first(value)) %>%
      ungroup() %>%
      return()
  } else
    if (type == "A") {
      .df %>%
        group_by_at(vars(-value, ...)) %>%
        mutate(value = value - first(value)) %>%
        ungroup() %>%
        return()
    }
}


#returns difference from a scenario
diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)

  ref_df <- df %>%
    filter(scenario %in% ref_scenario)

  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.ref = if_else(is.na(value.ref),0,value.ref),
           value = value.diff - value.ref)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}

#returns % difference from a scenario
pct_diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var){

  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)

  ref_df <- df %>%
    filter(scenario %in% ref_scenario)

  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%
    mutate(value = ((value.diff - value.ref)/value.ref) * 100)

  return(output_df)
}


#returns difference from year
diff_from_year <- function(df, diff_year = 2020, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_year_df <- df %>%
    filter(scenario == ref_scenario,year == diff_year)

  output_df <- df %>%
    full_join(diff_year_df, by = join_var,
              suffix = c(".diff", ".base")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.base = if_else(is.na(value.base),0,value.base),
           value = value.diff - value.base)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}

#returns difference from year
diff_R_from_year <- function(df, diff_year = 2020, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_year_df <- df %>%
    filter(scenario == ref_scenario,year == diff_year)

  output_df <- df %>%
    full_join(diff_year_df, by = join_var,
              suffix = c(".diff", ".base")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.base = if_else(is.na(value.base),0,value.base),
           value = value.diff / value.base)

  output_df_new <- output_df

  return(output_df_new)
}


Fill_annual <- function(.df, CUMULATIVE = FALSE,
                        CUM_YEAR_START = 2020,
                        CUM_OUT_STEP = 5){
  YEAR_START <- min(unique(.df$year))
  YEAR_END <- max(unique(.df$year))
  .df %>% mutate(year = as.integer(year)) -> .df


  .df %>% filter(year >= YEAR_START) %>%
    bind_rows(
      .df %>%
        #assuming YEAR_END has values for all
        filter(year == YEAR_END) %>% select(-year) %>%
        mutate(value = NA) %>%
        gcamdata::repeat_add_columns(tibble(year = setdiff(seq(YEAR_START,YEAR_END), unique(.df$year))))
    ) %>% arrange(year) %>%
    mutate(value = gcamdata::approx_fun(year, value, rule = 2)) -> .df1

  if (CUMULATIVE == TRUE ) {
    assertthat::assert_that(CUM_YEAR_START >= YEAR_START)
    .df1 %>% filter(year >= CUM_YEAR_START) %>%
      mutate(value = cumsum(value)) %>% filter(year >= CUM_YEAR_START) %>%
      filter(year %in% seq(YEAR_START,YEAR_END, CUM_OUT_STEP))-> .df1
  }
  return(.df1)
}



## Function to plot basin map directly ----


ggcamMapBasin <- function(.data,
                          Facet_Var = NULL,
                          cut = FALSE, # note that add on scale_fill_viridis_b is much better
                          nbreak = 10,
                          return_data_only = FALSE){

  windowsFonts("Arial" = windowsFont("Arial"))
  assertthat::assert_that(is.character(all_of(Facet_Var))|is.null(Facet_Var))

  # Join sf
  # adding in areas exist in map but not in .data as zero
  merge(map_424_sf %>% as_tibble() %>%  select(region, basin),
        .data %>% distinct_at( vars(Facet_Var)))  %>%
    # setdiff existing combinations
    setdiff(.data %>% distinct_at(vars("region", "basin", Facet_Var)) ) ->
    .data_needadd

  .data_needadd %>%
    merge(
      .data %>% select_at(vars(-"value", -"region", -"basin", -Facet_Var)) %>% distinct()
    ) %>%
    mutate(value = 0) %>%
    bind_rows(.data) ->
    .data

  map_424_sf %>%
    left_join(.data, by = c("region", "basin")) %>%
    replace_na(list(value = 0)) -> .data

  # using discrete data when cut with nbreak
  if (cut == TRUE) {
    Fr = floor(min(.data$value, na.rm = T)*10)/10
    Ce =  ceiling(max(.data$value, na.rm = T) *10)/10
    breaks = round(seq(Fr, Ce, (Ce - Fr)/nbreak), 1)

    .data %>%
      mutate(value = cut(value,unique(breaks))) -> .data
  }

  # return data only otherwise base map
  if (return_data_only == TRUE) {
    return(.data)
  }

  # mapping use ggplot
  .data %>%
    ggplot() +
    geom_sf(aes(fill = value), color = "black", size = 0.3) +
    theme_bw() +
    scale_y_continuous(expand = c(0.03, 0.03)) + scale_x_continuous(expand = c(0.01, 0.01)) +
    theme(panel.grid.major = element_line(colour = 'transparent'), #panel.border =element_blank(),
          panel.border = element_rect(color = "black", size =1),
          axis.text = element_blank(), axis.ticks = element_blank(),
          text = element_text(family= "Arial", size = 15),
          strip.background = element_blank(),
          strip.text = element_text(vjust = 0.5, margin = margin(0.4,0.4,0.4,0.4, "lines"),
                                    size = 16 #, face = "bold"
          ),
          panel.spacing.y = unit(0.8, "lines"),
          panel.spacing.x = unit(0.8, "lines") )

}

#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )
