# Comparison plots for the PR and documentation associated PR#755
# and the V3.5.0 release. 

# 0. Set Up --------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(paletteer)
library(ggpmisc)
library(scales)
library(hector)

# Plotting aesthics
theme_set(theme_bw())
JW <- 0.15

# Set up colors 
vs <- c("3.2.0", "3.5.0")
num <- length(unique(vs))
selected_colors <- hue_pal()(num)

COLORS <- selected_colors
names(COLORS) <- unique(vs)
COLORS <- c(COLORS, "obs" = "black", "ipcc ar6" = "black", "scms from ar6" = "grey")

COLORS.BENCHMARKS <- COLORS




# Helper function to normalize hector output
# Args
#   d: data frame of hector results
#   yrs: vector of reference period
# Returns: data frame of normalized results
normalize_fxn <- function(d, yrs){
  
  # check inputs
  req_cols <- c("variable", "scenario", "value")
  #invisible(check_req_names(d, req_cols))
  stopifnot({
    d %>%
      select(variable, scenario) %>% distinct %>%
      nrow == 1})
  stopifnot(all(yrs %in% d$year))
  
  
  d %>%
    filter(year %in% yrs) %>%
    pull(value) %>%
    mean ->
    ref_value
  
  d %>%
    mutate(value = value - ref_value) ->
    out
  
  return(out)
}
# 1. Load Data -----------------------------------------------------------------

# The hector results
file.path(BASE_DIR, "data", "hector_output_archive_rslts.csv") %>%  
  read.csv -> 
  hector_output

# Load the hector benchmark results
file.path(BASE_DIR, "data", "hector_benchmarks.csv") %>%  
  read.csv ->
  hector_benchmarks

# AR6 scm benchmark results
file.path(BASE_DIR, "data", "ar6_scms.csv") %>%  
  read.csv ->
  ar6_scm_benchmarks

file.path(BASE_DIR, "data", "ar6.csv") %>%  
  read.csv ->
  ar6_ipcc_benchmarks

# Natural emissions 
file.path(BASE_DIR, "data", "natural_v32.csv") %>% 
  read.csv -> 
  natural_v32

file.path(BASE_DIR, "data", "natural_v35.csv") %>% 
  read.csv -> 
  natural_v35


# 2. Metric  Plots -------------------------------------------------------------
## 2A. Key Metrics  ------------------------------------------------------------
# Key metrics to compare 
VARS <- c("tcre", "tcr")

hector_benchmarks %>% 
  filter(variable %in% VARS) -> 
  hec_key

ar6_scm_benchmarks %>% 
  mutate(variable = tolower(variable)) %>% 
  filter(variable %in% VARS) -> 
  scm_key

ar6_ipcc_benchmarks %>% 
  mutate(variable = tolower(variable)) %>% 
  filter(variable %in% VARS) -> 
  ipcc_key

ggplot() + 
  geom_errorbar(data = ipcc_key,
                aes(variable, ymin = min, ymax = max),
                width=.2, alpha = 0.5) +
  geom_point(data = hec_key, aes(variable, value, color = version),
             position = position_jitter(height = 0, width = 0)) + 
  geom_point(data = scm_key, aes(variable, value, color = "scms from ar6"),
             shape = 4, position = position_jitter(height = 0, width = JW)) +
  geom_point(data = ipcc_key, aes(variable, value, color = "ipcc ar6"), shape = 4) +
  labs(y = NULL, x = NULL, title = "Key Metrics") + 
  scale_color_manual(values = COLORS.BENCHMARKS) + 
  theme(legend.title = element_blank()) -> 
  plot; plot

ggsave(filename = "figs/tcre_tcr.png", plot, width = 6, height = 4)

## 2B. Historical Values  ------------------------------------------------------
# Historical Values
VARS <- c("OHC", "RF_CH4", "wmghg RF",  "total aerosol RF", "hist. warming")

hector_benchmarks %>% 
  filter(variable %in% VARS) -> 
  hec_his

ar6_scm_benchmarks %>% 
  filter(variable %in% VARS) -> 
  scm_his

ar6_ipcc_benchmarks %>% 
  filter(variable %in% VARS) -> 
  ipcc_his

ggplot() + 
  geom_errorbar(data = ipcc_his,
                aes(variable, ymin = min, ymax = max),
                width=.2, alpha = 0.5) +
  geom_point(data = hec_his, aes(variable, value, color = version), alpha = 0.75) + 
  geom_point(data = scm_his, aes(variable, value, color = "scms from ar6"),
             shape = 4, position = position_jitter(height = 0, width = JW)) +  
  geom_point(data = ipcc_his, aes(variable, value, color = "ipcc ar6"), shape = 4) +
  labs(y = NULL, x = NULL, title = "Historic Metrics") + 
  scale_color_manual(values = COLORS.BENCHMARKS) + 
  theme(legend.title = element_blank()) + 
  facet_wrap("variable", scales = "free") -> 
  plot; plot

ggsave(filename = "figs/hist_metrics.png", plot, width = 6, height = 4)


## 2C. Warming Values  ------------------------------------------------------

VARS <- c(GLOBAL_TAS())

hector_benchmarks %>% 
  filter(variable %in% VARS) %>%  
  filter(scenario %in% c("ssp119", "ssp126", "ssp245", "ssp370", "ssp585")) -> 
  hec_warm

ar6_scm_benchmarks %>% 
  filter(variable %in% VARS) -> 
  scm_warm

ar6_ipcc_benchmarks %>% 
  filter(variable %in% VARS) -> 
  ipcc_warm

ggplot() +
  geom_errorbar(data = ipcc_warm,
                aes(year, ymin = min, ymax = max),
                width=.2) +
  geom_point(data = hec_warm, aes(year, value, color = version), alpha = 0.75) + 
  geom_point(data = scm_warm, aes(year, value, color = "scms from ar6"),
             shape = 4, position = position_jitter(height = 0, width = JW)) +  
  geom_point(data = ipcc_warm, aes(year, value, color = "ipcc ar6"), shape = 4) +
  facet_wrap("scenario", scales = "free") + 
  theme(axis.text.x = element_text(angle = 35, vjust = 0.6)) + 
  labs(y = NULL, x = NULL, title = "Future Warming (deg C)") + 
  scale_color_manual(values = COLORS.BENCHMARKS) + 
  theme(legend.title = element_blank()) -> 
  plot; plot

ggsave(filename = "figs/future_warm.png", plot, width = 8, height = 6)






# 3. Output Comparisons --------------------------------------------------------

# Helper function that quick plots the ssp scenarios 
# Args 
#   d: data.frame of the hector results 
#   var: vector name of the variables to plot 
# Returns: ggplot figure comparing the hector results
quick_ssp_comparison_plot_fxn <- function(d, var){
  
  d %>% 
    filter(grepl(x = scenario,  pattern = "ssp")) %>% 
    filter(variable %in% var) %>% 
    filter(year <= 2100) %>% 
    ggplot() +
    geom_line(aes(year, value, color = version, 
                  group = interaction(scenario, version))) + 
    facet_wrap("variable", scales = "free") + 
    labs(x = NULL, y = NULL) + 
    scale_color_manual(values = COLORS.BENCHMARKS) + 
    theme(legend.title = element_blank()) -> 
    out
  
  return(out)
  
}


quick_ssp_comparison_plot_fxn(hector_output, RF_VOL()) -> 
  plot; plot
ggsave(filename = "figs/rf_vol.png", plot, width = 4, height = 4)


quick_ssp_comparison_plot_fxn(hector_output, c(CONCENTRATIONS_CH4(), CONCENTRATIONS_N2O())) + 
  facet_wrap("variable", scales = "free", ncol = 1) -> 
  plot; plot
ggsave(filename = "figs/ch4_n2o_conc.png", plot, width = 4, height = 4)


quick_ssp_comparison_plot_fxn(hector_output, c(CONCENTRATIONS_CO2())) + 
  facet_wrap("variable", scales = "free", ncol = 1) -> 
  plot; plot
ggsave(filename = "figs/co2_conc.png", plot, width = 4, height = 4)


quick_ssp_comparison_plot_fxn(hector_output, c(RF_TOTAL())) + 
  facet_wrap("variable", scales = "free", ncol = 1) -> 
  plot; plot
ggsave(filename = "figs/rf_total.png", plot, width = 4, height = 4)


# 4. Natural Emissions ---------------------------------------------------------

# Natural CH4 emissions from Hector
natural_v32 %>% 
  filter(variable == "CH4N") -> 
  nat_ch4_v32

natural_v35 %>% 
  filter(variable == "CH4N") -> 
  nat_ch4_v35


# Some benchmarks
prather <- data.frame(year = 1750:2300, value = 202, sd = 0, name = "Prather 2012")
fair <- data.frame(year =  1750:2300,lower = 140, upper = 220, name = "Fair 2018")

ggplot() + 
  geom_ribbon(data = fair, aes(year, ymin =lower, ymax = upper, fill = name), alpha = 0.2) +
  geom_line(data = prather, aes(year, value, color = name)) +
  
  geom_hline(data = nat_ch4_v32, aes(yintercept = value, color = version)) + 
  geom_line(data = nat_ch4_v35, aes(year, value, color = version)) + 
  labs(title = "Natural CH4 emissions", y = getunits("CH4N"), x = NULL) + 
  scale_color_manual(values = c(COLORS.BENCHMARKS, "Prather 2012" = "black")) + 
  scale_fill_manual(values = c(COLORS.BENCHMARKS, "Fair 2018" = "black")) + 
  theme(legend.title = element_blank()) -> 
  plot; plot
ggsave(filename = "figs/nat_ch4_emiss.png", plot, width = 6, height = 4)




# Natural N2O emissions from Hector
natural_v32 %>% 
  filter(variable == "N2O_natural_emissions") -> 
  nat_n2o_v32

natural_v35 %>% 
  filter(variable == "N2O_natural_emissions") -> 
  nat_n2o_v35


# Some benchmarks
prather <- data.frame(year = 1750:2300, value = 9.1, sd = 1.0, name = "Prather 2012")
fair <- data.frame(year =  1750:2300,lower = 8, upper = 11, name = "Fair 2018")

ggplot() + 
  geom_ribbon(data = fair, aes(year, ymin =lower, ymax = upper, fill = name), alpha = 0.7) +
  geom_ribbon(data = prather, aes(year, ymin =value-sd, ymax = value+sd, fill = name), alpha = 0.7) +
  geom_hline(data = nat_n2o_v32, aes(yintercept = value, color = version)) + 
  geom_line(data = nat_n2o_v35, aes(year, value, color = version)) + 
  labs(title = "Natural N2O emissions", y = getunits("N2O_natural_emissions"), x = NULL) + 
  scale_color_manual(values = c(COLORS.BENCHMARKS)) + 
  scale_fill_manual(values = c(COLORS.BENCHMARKS, "Fair 2018" = "black", "Prather 2012" = "grey")) + 
  theme(legend.title = element_blank()) -> 
  plot; plot
ggsave(filename = "figs/nat_n2o_emiss.png", plot, width = 6, height = 4)

# 5. Historical Observations ---------------------------------------------------
file.path("data", "obs_hist.csv") %>% 
  read.csv -> 
  comparison_data

file.path("data", "hector_hist.csv") %>% 
  read.csv -> 
  hector_data


hector_v_obs_fxn <- function(hector_data, vars, SAVE = FALSE){
  
  comparison_data %>%
    filter(variable %in% vars) ->
    comp_to_plot
  
  hector_data %>%
    filter(variable %in% vars) %>%
    filter(year %in% comp_to_plot$year) %>% 
    distinct ->
    hector_to_plot
  
  comp_to_plot %>%
    select(year, variable, obs = value) %>% 
    left_join(hector_to_plot) %>%
    filter(!is.na(value)) %>% 
    summarise(MAE = mean(abs(obs - value)), .by = c(variable, version)) %>%
    mutate(MAE = signif(MAE, digits = 3)) ->
    MAE_table
  
  tbs <- lapply(split(MAE_table, MAE_table$variable), "[")
  
  df <- tibble(x = rep(Inf, length(tbs)),
               y = rep(-Inf, length(tbs)),
               variable = vars,
               tbl = tbs)
  
  ggplot() +
    geom_ribbon(data = comp_to_plot, aes(year, ymin = lower, ymax = upper, fill = "obs"), alpha = 0.25) +
    geom_line(data = comp_to_plot, aes(year, value, color = "obs"), linewidth = 1, alpha = 0.25) +
    geom_line(data = hector_to_plot, aes(year, value, color = version), linewidth = 0.75) +
    facet_wrap("variable", scales = "free") +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    scale_color_manual(values = COLORS.BENCHMARKS) +
    scale_fill_manual(values = COLORS.BENCHMARKS) +
    
    geom_table(data = df, aes(x = x, y = y, label = tbl),
               hjust = 1, vjust = 0) ->
    plot; plot
  
  return(plot)
  
  if(SAVE){
    
    pfile <- file.path("figs", paste0("hector_", paste0(vars, collapse = ""), ".png"))
    ggsave(plot = plot,
           filename = file.path("figs", pfile),
           height = 5, width = 5)
  }
  
}


hector_v_obs_fxn(hector_data, vars = GMST()) ->
  plot; plot

ggsave(plot, filename = "figs/obs_v_hector_gmst.png", height = 6, width = 6)

hector_v_obs_fxn(hector_data, vars = "OHC") ->
  plot; plot

ggsave(plot, filename = "figs/obs_v_hector_ohc.png", height = 6, width = 6)


hector_v_obs_fxn(hector_data, vars = CONCENTRATIONS_CO2()) ->
  plot; plot

ggsave(plot, filename = "figs/obs_v_hector_co2.png", height = 6, width = 6)

# 6. CMIP6 Comparisons  --------------------------------------------------------
# CMIP6 Comparison
# Make a plot with the ESMs colored by their ECS labels.
scns <- c("ssp245", "ssp126", "ssp585")

# G. A. Meehl, C. A. Senior, V. Eyring, G. Flato, J.-F. Lamarque, R. J. Stouffer, K. E. Taylor,
# M. Schlund, Context for interpreting equilibrium climate sensitivity and transient climate response
# from the CMIP6 Earth system models. Sci. Adv. 6, eaba1981 (2020).
# Schlund, Manuel, Axel Lauer, Pierre Gentine, Steven C. Sherwood, and Veronika Eyring. 2020.
# “Emergent Constraints on Equilibrium Climate Sensitivity in CMIP5: Do They Hold for CMIP6?” Earth System Dynamics 11 (4): 1233–58.

# Lovato, T., Peano, D., Butenschön, M., Materia, S., Iovino, D., Scoccimarro, E.,
# et al. (2022). CMIP6 simulations with the CMCC Earth System Model (CMCCESM2).
# Journal of Advances in Modeling Earth Systems, 14, e2021MS002814.
# https://doi.org/10.1029/2021MS002814
cmip6_ecs <- as.data.frame(rbind(c("ACCESS-CM2", 4.7),
                                 c("ACCESS-ESM1-5", 3.9),
                                 c("CAMS-CSM1-0", 2.3),
                                 c("CanESM5", 5.6),
                                 c("CESM2", 5.2),
                                 c("CESM2-WACCM", 4.8),
                                 c("CMCC-CM2-SR5", 3.52) ,
                                 c("HadGEM3-GC31-LL", 5.6),
                                 c("MIROC-ES2L", 2.7),
                                 c("MIROC6", 2.6),
                                 c("MRI-ESM2-0", 3.2),
                                 c("NorESM2-MM", 2.5),
                                 c("TaiESM1", 4.31),
                                 c("UKESM1-0-LL", 5.3),
                                 c("CMCC-ESM2", 3.57)))
names(cmip6_ecs) <- c("model", "ecs")


# TS.3.2 Climate Sensitivity and Earth System Feedbacks
# the likely range is 2.5°C to 4°C and the very likely range is 2°C to 5°C.
cmip6_ecs %>%
  mutate(id = "not very likely") %>%
  mutate(id = ifelse(ecs >= 2 & ecs <= 5, "very likely", id)) ->
  ecs_table


VARS <- c(GLOBAL_TAS(), LAND_TAS(), SST())


file.path("data", "cmip6_model_means.csv") %>% 
  read.csv %>% 
  dplyr::filter(scenario %in% scns)  %>%
  filter(model %in% cmip6_ecs$model) %>%
  filter(variable %in% VARS) %>%
  full_join(ecs_table, by = "model") ->
  cmip6_rslts

hector_output %>%
  filter(variable %in% VARS) %>% 
  filter(year %in% 1850:2100) %>%
  filter(variable %in% VARS) %>%
  filter(scenario %in% cmip6_rslts$scenario) %>% 
  split(., interaction(.$variable, .$scenario, .$version), drop = TRUE) %>%
  lapply(FUN = normalize_fxn, yrs = 1850:1900) %>%
  do.call(what = "rbind") ->
  hector_temp

cmip6_rslts %>%
  group_by(scenario, year, variable, id) %>%
  summarise(min = min(value),
            max = max(value)) %>%
  filter(variable %in% VARS) ->
  cmip6_temp_summary

# Create the labels
temp_labs <- c("Global Mean Air Temp.", "Mean Land Surface Temp.", "Mean Sea Surface Temp.")
names(temp_labs) <-  c(GLOBAL_TAS(), LAND_TAS(), SST())

ggplot() +
  geom_ribbon(data = cmip6_temp_summary, aes(year, ymin = min, ymax = max, fill = id), alpha = 0.9) +
  geom_line(data = hector_temp, aes(year, value, color = version), size = 0.75) +
  facet_grid(scenario~variable, labeller = labeller(variable = temp_labs), scales = "free") +
  labs(y = expression("Temperature anomaly relative to 1850-1860 ("~degree~"C)"), x = "Year") +
  scale_fill_manual(values = c("very likely" = "#5A5A5A",
                               "not very likely" = "#D3D3D3")) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_color_manual(values = COLORS.BENCHMARKS)->
  plot; plot

ggsave(plot, filename = "figs/cmip6_ribbon.png", height = 6, width = 6)


## Idealized Runs
max_yr <- 150

file.path("data", "hector_output_archive_rslts.csv") %>% 
  read.csv() %>% 
  filter(scenario %in% c("1pctCO2", "abruptx4CO2"), 
         variable %in% hector_idealized$variable) %>%
  mutate(scenario = if_else(scenario ==  "abruptx4CO2", "abrupt-4xCO2", scenario)) %>%
  mutate(year = year - 1799) %>%
  filter(year >= 0 & year <= max_yr) ->
  hector_idealized

cmip6_idealized <- read.csv(here::here("data", "cmip6_idealized.csv")) %>%
  filter(year <= max_yr) %>%
  rename(scenario = experiment)


ggplot() +
  geom_line(data = cmip6_idealized, aes(year, value, group = interaction(model, ensemble),
                                        color = "CMIP6 ESM"), alpha = 0.5) +
  geom_line(data = hector_idealized, aes(year, value, color = version), linewidth = 1) +
  facet_wrap("scenario", scales = "free") +
  labs(y = expression("Temperature anomaly ("~degree~"C)"), x = "Years") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_color_manual(values = COLORS.BENCHMARKS) ->
  plot; plot

ggsave(plot, filename = "figs/cmip6_idealized.png", height = 4, width = 6)


