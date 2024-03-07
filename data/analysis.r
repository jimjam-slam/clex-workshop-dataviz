library(tidyverse)
library(here)

weather_path <-  here("data", "weather")

# --- weather data --------------------

# load the min and max temperature obs in
obs_min <-
  read_csv(here("data", "weather", "IDCJAC0011_086068_1800_Data.csv")) |>
  janitor::clean_names() |>
  select(year, month, day, tmin = minimum_temperature_degree_c)
obs_max <-
  read_csv(here("data", "weather", "IDCJAC0010_086068_1800_Data.csv")) |>
  janitor::clean_names() |>
  select(year, month, day, tmax = maximum_temperature_degree_c)

# bring min and max obs together; subset to 2015-2018
obs <-
  full_join(obs_min, obs_max, join_by(year, month, day)) |>
  filter(between(year, 2005, 2018)) |>
  mutate(date = ymd(paste(year, month, day)))

# --- electricity data -----------------

site_ids <- c("NC", "PM", "DA")

# get provider info for citipower
info <-
  read_csv(
    here(data_path, "electricity",
      "DNSP_Zone_Substation_Characteristics.csv")) |>
  janitor::clean_names() |>
  # get first word of of the provider name
  mutate(dnsp_short = tolower(word(distribution_network_service_provider))) |>
  # focus on citipower only
  filter(dnsp_short == tolower("citipower")) |>
  select(
    -distribution_network_service_provider,
    -energy_asset,
    -zone_substation_area_km2) |>
  rename(
    name = zone_substation_name,
    id = zone_substation_id) |>
  select(dnsp_short, name, id, everything())

# get the 3 most common uses for each site
info_3sites <-
  info |>
  filter(id %in% site_ids) |>
  select(-dnsp_short, -dwellings, -persons) |>
  pivot_longer(-c(name, id), names_to = "Use", values_to = "fraction") |>
  group_by(id, name) |>
  arrange(desc(fraction)) |>
  slice(1:3) |>
  mutate(use_string = paste0(
    str_to_sentence(Use), ": ",
    scales::percent(fraction))) |>
  summarise(area_string = paste(use_string, collapse = "\n")) |>
  ungroup()

# get citipower electricity demand
demand <-
  read_csv(
    here(data_path, "electricity", "citipower_2005_2017.csv"),
    na = c("nan", "NA", "")) |>
  # remove neagtive demand values
  mutate(across(-datetime, ~ replace(.x, .x < 0, NA_real_))) |>
  # calculate each day's max demand (half hourly is a bit much)
  mutate(date = as.Date(datetime), .before = everything()) |>
  group_by(date) |>
  summarise(across(everything(), ~ max(.x, na.rm = TRUE))) |>
  select(-datetime)

# --- join and merge -------------------

# merge obs and demand; separate out weekends (and further slice to 2015 onward)
df <-
  inner_join(obs, demand, join_by(date)) |>
  filter(year >= 2015) |>
  mutate(
    working = if_else(
      weekdays(date) %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"))

# make this longer (separate row for each site) and focus on 3 sites  
df_long <-
  df |>
  tidyr::pivot_longer(
    -c(year, month, day, tmin, tmax, date, working),
    names_to = "site",
    values_to = "demand") |>
  filter(site %in% site_ids) |>
  filter(!is.infinite(demand)) |>
  # also merge in the site names and use info
  left_join(info_3sites, join_by(site == id), multiple = "all") |>
  select(site, name, everything())
