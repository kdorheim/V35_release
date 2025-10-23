# Select and format the data from the hector-archive to compare with one 
# another. 

# 0 Set Up --------------------------------------------------------------------- 
source(file.path("scripts", "env.R"))

# Since the upgrades to Hector V3.5.0 were related to CH4, N2O, & RF VOL, let's 
# look at some of those variables in addition to the usual suspects of 
# global temp, total rf, ocean heat flux, & CO2 concentrations. 

VARS <- c(CONCENTRATIONS_CH4(), RF_CH4(), CONCENTRATIONS_N2O(), RF_VOL(), 
          RF_TOTAL(), RF_N2O(), CONCENTRATIONS_CO2(), HEAT_FLUX(), GLOBAL_TAS(), 
          CONCENTRATIONS_CH4(), RF_CO2(), RF_CH4(), RF_VOL(), GMST(), LAND_TAS(), SST())

# 1. Hector V32 Data -----------------------------------------------------------

file.path(BASE_DIR, "data", "raw-data", "output-V3.2.0.csv") %>% 
  read.csv %>% 
  filter(variable %in% VARS) -> 
  wide_df

# Change from wide to long format 
wide_df %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year") %>% 
  mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) -> 
  out1

# Data frame of the V32 natural emissions. 
data.frame(variable = c("CH4N", "N2O_natural_emissions"), 
           version = "3.2.0", 
           value = c(335, 9.72)) -> 
  natural_v32

# 2. Hector V35 Data -----------------------------------------------------------

file.path(BASE_DIR, "data", "raw-data", "output-V3.5.0.csv") %>% 
  read.csv %>% 
  filter(variable %in% VARS) -> 
  wide_df

# Change from wide to long format 
wide_df %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year") %>% 
  mutate(year = as.integer(gsub(replacement = "", x = year, pattern = "X"))) -> 
  out2

output <- bind_rows(out1, out2)

# The natural emissions 
file.path(BASE_DIR, "data", "raw-data", "core_inputs-V3.5.0.csv") %>% 
  read.csv(comment.char = ";") %>% 
  pivot_longer(-Date, names_to = "variable") %>% 
  rename(year = Date) %>% 
  filter(variable %in% natural_v32$variable) %>% 
  mutate(version = "3.5.0") -> 
  natural_v35


# 3. Archive Data --------------------------------------------------------------

bind_rows(read.csv(file.path(BASE_DIR, "data", "raw-data", "AR6_benchmarks-V3.2.0.csv")), 
      read.csv(file.path(BASE_DIR, "data", "raw-data", "AR6_benchmarks-V3.5.0.csv")))  -> 
  benchmarks

# 4. Historical Data -----------------------------------------------------------
# Prep Hector data for the comparison with historical observations. The 
# temperature results need to be 
output %>%  
  filter(scenario == "ssp245") %>% 
  filter(year <= 2015) %>% 
  filter(variable %in% c(CONCENTRATIONS_CO2(), GMST(), HEAT_FLUX())) %>% 
  filter(!is.na(value)) -> 
  historical_rslts 


# Ocean heat content constants
OCEAN_AREA <- 5100656e8 * (1 - 0.29) # The total area of the ocean
W_TO_ZJ <- 3.155693e-14              # Watts to ZJ


# Prep the temperature results 
historical_rslts %>% 
  filter(variable == GMST()) %>% 
  filter(year %in% 1951:1980) %>% 
  summarise(ref = mean(value), .by =c(version, variable)) -> 
  ref_values

historical_rslts %>% 
  filter(variable == GMST()) %>% 
  left_join(ref_values, by = join_by(version, variable)) %>% 
  mutate(value = value - ref) %>% 
  select(-ref) -> 
  temp_rslts

historical_rslts %>% 
  filter(variable == CONCENTRATIONS_CO2()) -> 
  co2_rslts

historical_rslts %>% 
  filter(variable == HEAT_FLUX()) %>% 
  mutate(value = value * OCEAN_AREA * W_TO_ZJ) %>% 
  group_by(version) %>% 
  mutate(value = cumsum(value)) %>% 
  ungroup %>%  
  mutate(variable = "OHC") -> 
  ohc_rslts
  

ohc_rslts %>% 
  filter(year %in% 2005:2014) %>% 
  summarise(ref = mean(value), .by =c(version, variable)) -> 
  ref_values

ohc_rslts %>% 
  left_join(ref_values, by = join_by(version, variable)) %>% 
  mutate(value = value - ref) %>% 
  select(-ref) -> 
  ohc_rslts

his_rslts <- rbind(temp_rslts, co2_rslts, ohc_rslts)


# 5. Observations  -------------------------------------------------------------

file.path(BASE_DIR, "data", "raw-data") %>% 
  list.files(pattern = "C.", full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows() %>% 
  filter(variable %in% c("gmst", "OHC", "CO2_concentration")) -> 
  obs_data

# Z. Save Data -----------------------------------------------------------------

write.csv(x = output, file = file.path(BASE_DIR, "data", "hector_output_archive_rslts.csv"), row.names = FALSE)
write.csv(x = benchmarks, file = file.path(BASE_DIR, "data", "hector_benchmarks.csv"), row.names = FALSE)
write.csv(x = natural_v32, file = file.path(BASE_DIR, "data", "natural_v32.csv"), row.names = FALSE)
write.csv(x = natural_v35, file = file.path(BASE_DIR, "data", "natural_v35.csv"), row.names = FALSE)
write.csv(x = his_rslts, file = file.path(BASE_DIR, "data", "hector_hist.csv"), row.names = FALSE)
write.csv(x = obs_data, file = file.path(BASE_DIR, "data", "obs_hist.csv"), row.names = FALSE)

