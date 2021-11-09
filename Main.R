library(tidyverse)

data1 <- readRDS("data/world_bank_data.rds")


names(data1)

data1 %>% 
  select(country, continent, region, year, perc_rural_pop, perc_college_complete)


# Capítulo 1 (Select HELPERS)---------------------------------------------------

continents_vector <- c("Africa","Asia")

# Seleccionamos sólo ciertos registros
asia_africa_results <- data1 %>% 
  select(country, continent, region, year, perc_rural_pop, perc_college_complete) %>% 
  filter(continent %in% continents_vector)

asia_africa_results

# Repasamos mutate
asia_africa_results <- asia_africa_results %>% 
  mutate(perc_urban_pop = 100 - perc_rural_pop)

asia_africa_results

# Repasamos group_by
asia_africa_results %>% 
  group_by(region) %>% 
  summarise(
    mean_urban = mean(perc_urban_pop)
  )



imf_data <- readRDS("data/imf_data.rds")
countries_vector <- c("Argentina","Austria","Bangladesh","Belize","New Zealand","Tanzania")

selected_imf <- imf_data %>% 
  select(country, year, usd_conversion_rate, gdp_in_billions_of_usd)

# Retrieve rows for countries given
subsetted_imf <- selected_imf %>% 
  filter(country %in% countries_vector)


# Print out this subsetted data
subsetted_imf


# Divide columns to get GDP in each country's currency
subsetted_imf %>% 
  mutate(gdp_in_billions = gdp_in_billions_of_usd / usd_conversion_rate) %>% 
  # Get the median GDP in billions for each country
  group_by(country) %>% 
  summarise(median_gdp = median(gdp_in_billions))

# HELPERS -----------------------------------------------------------------

# Columnas que comienzan por...
data1 %>% 
  select(starts_with("perc"))

# un poco más de info (dónde y cuándo)
data1 %>% 
  select(country, year, starts_with("perc"))

# Ahora columnas que terminan en...
data1 %>% 
  select(country, year, ends_with("rate"))


# Choose columns starting with "gov"
imf_data %>% 
  select(starts_with("gov"))

# Choose year, country, and columns starting with "gov"
imf_data %>% 
  select(year, country, starts_with("gov"))

# Choose year, country, and columns ending with "change"
imf_data %>% 
  select(year, country, ends_with("change"))


# Columnas que contienen la letra "y"
data1 %>% 
  select(contains("y"))

# Expresiones regulares!! (matches)
# Contienen "y" o "perc"
data1 %>% 
  select(matches("y|perc"))

# Comienzan por "co"
data1 %>% 
  select(matches("^co"))

# Terminan en "on"
data1 %>% 
  select(country, matches("on$"))


# Choose country, year, and columns with "gdp" in their name
imf_data %>% 
  select(country, year, contains("gdp"))

# Choose country, year, and columns with "as_perc" in their name
imf_data %>% 
  select(country, year, contains("as_perc"))

# Choose country, year, and "perc" or "rate" columns
imf_data %>% 
  select(country, year, matches("perc|rate"))

# Choose country, year, and columns beginning with "gov"
imf_data %>% 
  select(country, year, matches("^gov"))

# Choose country, year, and columns finishing with "gdp"
imf_data %>% 
  select(country, year, matches("gdp$"))


# Capítulo 2 (Relocación y ACROSS) ----------------------------------------------

# Relocation
reordered_wb <- data1 %>% 
  select(iso:year, matches("^perc"), everything())

names(reordered_wb)

data1 %>% 
  select(iso:year, matches("^perc"), infant_mortality_rate:last_col()) %>% 
  names()

# relocate() + .after
data1 %>% 
  relocate(matches("^perc"), .after = year) %>% 
  names()

data1 %>% 
  relocate(matches("^perc"), .before = infant_mortality_rate) %>% 
  names()


# Get imf_data column names
names(imf_data)

# Move gdp ending columns after year
reordered_imf <- imf_data %>% 
  select(iso:year,
         matches("gdp$"),
         everything())

# Get reordered_imf column names
names(reordered_imf)


imf_data %>% 
  select(-
    # Choose columns iso to year
    iso:year,
    # Choose columns starting with "gov" using regular expression
    matches("^gov"),
    # Keep remaining columns too
    gdp_in_billions_of_usd:last_col()) %>% 
  names()


# Move consumer_price_index after usd_conversion_rate
relocated_cpi <- imf_data %>% 
  relocate(consumer_price_index, 
           .after = usd_conversion_rate)

# Glimpse at this data, focusing on the column order
glimpse(relocated_cpi)


# Move population_in_millions to before consumer_price_index
relocated_cpi %>% 
  relocate(population_in_millions, .before = consumer_price_index) %>% 
  glimpse()



# MUTATE ACROSS MULTIPLE COLUMNS

# Multiply as_perc_gdp columns by gdp_in_billions_of_usd
imf_new_cols <- imf_data %>% 
  mutate(across(.cols = contains("as_perc_gdp"), 
                .fns = ~ .x * gdp_in_billions_of_usd),
         .keep="used") 

# Replace _as_perc_gdp with empty string in tibble names
names(imf_new_cols) <- sub(pattern="_as_perc_gdp",
                           replacement="",
                           x = names(imf_new_cols))

# Display tibble with new names
imf_new_cols



imf_data %>% 
  # Focus on Bolivia rows
  filter(country == "Bolivia") %>% 
  summarize(
    # Compute for all column names including "perc"
    across(.cols = contains("perc"), 
           # Calculate the minimum value
           .fns = ~ min(.x),
           # Prepend "min_" to column names
           .names = "min_{.col}")
  )

# Combinamos ACROSS() con COUNT()
imf_data %>% 
  count(
    # Count over character class columns
    across(
      where(is.character)
    ),
    # Arrange the results by descending count
    sort = TRUE)


# rowwise() con c_across() -> ( c_across se usa con rowwise casi siempre!! )

data1 %>% 
  rowwise() %>% 
  mutate(num_missing = sum(is.na(
    c_across(infant_mortality_rate:last_col())
  ))) %>% 
  select(country:year, num_missing) %>% 
  arrange(desc(num_missing))


# Filtrar si ALGUNA de las columnas cumple con la condición
data1 %>% 
  filter(if_any(
    .cols = starts_with("perc"),
    .fns = ~ .x < 5
  )) %>% 
  select(country, year, starts_with("perc"))

# Filtrar si TODAS las columnas cumplen con la condición
data1 %>% 
  filter(if_all(
    .cols = starts_with("perc"),
    .fns = ~ .x >= 25
  )) %>% 
  select(country, year, starts_with("perc"))


# Ejemplos

imf_data %>% 
  # Specify that calculations are done across the row
  rowwise() %>% 
  # Count missings in gdp_in_billions_of_usd to last column
  mutate(num_missing = sum(is.na(
    c_across(gdp_in_billions_of_usd:last_col()))
  )) %>% 
  select(country:year, num_missing) %>% 
  # Arrange by descending number of missing entries
  arrange(desc(num_missing))


# Rows with less than 0 for any "perc_change" ending columns
imf_data %>%
  filter(if_any(.cols = matches("perc_change$"), 
                .fns = ~ .x < 0)) %>% 
  select(country, year, ends_with("perc_change"))

# Rows from -1 to 1 on ALL "perc_change" ending columns
imf_data %>%
  filter(if_all(
    .cols = matches("perc_change$"),
    .fns = ~ between(., -1, 1))) %>%
  select(country, year, ends_with("perc_change"))



# Capítulo 3 (JOINS) ------------------------------------------------------------

# IMF data from Uruguay
uruguay_imf <- imf_data %>% 
  select(iso, country, year, consumer_price_index) %>% 
  filter(country == "Uruguay", year > 2010)

uruguay_imf

# World Bank data from Uruguay
uruguay_wb <- data1 %>% 
  select(iso, country, year, perc_rural_pop) %>% 
  filter(country == "Uruguay")

uruguay_wb

# LEFT
uruguay_imf %>% 
  left_join(uruguay_wb)

# INNER
uruguay_imf %>% 
  inner_join(uruguay_wb,
             by = c("iso","country","year"))

# ANTI
uruguay_imf %>% 
  anti_join(uruguay_wb,
            by = c("iso","country","year"))

# Ejemplos
imf_subset <- imf_data %>% 
  select(iso, country, year, population_in_millions)

asia_wb <- data1 %>% 
  filter(continent=="Asia") %>% 
  select(iso, country, year, fertility_rate, perc_college_complete)

# Return all rows of asia_wb and matching rows from imf_subset
asia_wb %>%
  left_join(imf_subset)

# Return rows with matches in asia_wb and imf_subset
asia_wb %>%
  inner_join(imf_subset)

# Return rows from asia_wb with no match in imf_subset
asia_wb %>%
  anti_join(imf_subset)


suspect_isos <- c("KOR","BRN","IRN")

# Distinct iso and country combos for each tibble
asia_wb_combos <- distinct(asia_wb %>% select(iso, country))
imf_combos <- distinct(imf_subset %>% select(iso, country))

# Anti join to find only those in asia_wb_combos
asia_wb_combos %>%
  anti_join(imf_combos)


# Compare country names of the suspect isos in both tibbles
imf_subset %>% 
  filter(iso %in% suspect_isos) %>%
  distinct(iso, country)
asia_wb %>% 
  filter(iso %in% suspect_isos) %>%
  distinct(iso, country)

# INTERSECT VS INNER_JOIN
# INTERSECT BUSCA FILAS EN COMÚN
# INNER_JOIN BUSCA CLAVES INDIVIDUALES EN COMÚN

# Identify distinct country names for asia_wb
asia_wb %>%
  distinct(country)

# Find rows in both of the two combos datasets
asia_wb_combos %>%
  intersect(imf_combos)


# Find rows in one or the other tibble
asia_wb_combos %>%
  union(imf_combos)


# Intersect Asia World Bank data combos with IMF combos
asia_wb_combos %>%
  intersect(imf_combos)

# Union Asia World Bank data combos with IMF combos
asia_wb_combos %>%
  union(imf_combos)

# Give all rows in both Asia World Bank data and IMF combos
asia_wb_combos %>%
  union_all(imf_combos)



# Isolate on Portugal rows and year column in both datasets
prt_imf <- imf_data %>% 
  filter(iso=="PRT") %>% 
  select(year)
prt_wb <- data1 %>% 
  filter(iso=="PRT") %>%
  select(year)

# Check for set equality
setequal(prt_imf, prt_wb)

# Identify years are in prt_imf but not in prt_wb
setdiff(prt_imf, prt_wb)


# Find distinct ISO and country names in the two datasets
imf_countries <- imf_data %>% 
  distinct(country, iso)
world_bank_countries <- data1 %>% 
  distinct(country, iso)

# Find rows in imf_countries but not in world_bank_countries
setdiff(imf_countries, world_bank_countries)

# Find isos in world_bank_countries but not in imf_countries
setdiff(world_bank_countries %>% select(iso), 
        imf_countries %>% select(iso))

# Use anti_join() to figure out which country this is
world_bank_countries %>%
  anti_join(imf_countries, by="iso")



# Capítulo 4 --------------------------------------------------------------

imf_data %>% 
  select(iso, country, year, consumer_price_index) %>% 
  filter(country == "Uruguay", year > 2010)


# Some functions
cpi_by_country <- function(country_name) {
  imf_data %>% 
    select(iso, country, year, consumer_price_index) %>% 
    filter(country == country_name,
           year > 2010)
}

cpi_by_country(country_name = "Samoa")



unemployment_by_region <- function(region_name) {
  data1 %>%
    select(iso, country, year, 
           continent, region, unemployment_rate) %>%
    filter(region == region_name)
}

# Call the function to get Eastern Europe results
unemployment_by_region("Eastern Europe")

# Para group_by necesitamos otro tipo de operador: {{}}
grouped_median_unemploy <- function(group_col) {
  data1 %>%
    select(iso, country, year, 
           continent, region, unemployment_rate) %>%
    # NOTAR EL OPERADOR PARA EL PARÁMETRO
    group_by({{ group_col }}) %>% 
    summarize(
      median_unemployment = median(unemployment_rate))
}

# De esta manera podemos pasar nombres de columna a la función
grouped_median_unemploy(region)


# {{}} ES LO MISMO QUE USAR !!enquo()
grouped_median_for_column <- function(group_col, col_to_median) {
  data1 %>% 
    group_by(!!enquo(group_col)) %>% 
    summarize(median(!!enquo(col_to_median), 
                     na.rm = TRUE))
}

# Find median infant mortality rate by region
grouped_median_for_column(region, infant_mortality_rate)


# ¿CÓMO LE DAMOS NOMBRE A LA COLUMNA CALCULADA EN MUTATE? usamos as_name y := (walrus operator)
library(rlang) # as_name

grouped_median_for_column <- function(.data, group_col, col_to_median) {
  name_of_col_to_median <- as_name(enquo(col_to_median)) # nombre de la nueva columna
  new_col_name <- paste0("median_of_", name_of_col_to_median) # completamos nombre
  
  .data %>% 
    group_by( {{group_col}} ) %>% 
    # notar uso de: bang-bang + walrus operator (!!  +  :=)
    summarize(!!new_col_name := median( {{col_to_median}} ,
                                        na.rm = TRUE))
}

# Group world_bank_data by continent to find college done %
data1 %>% 
  grouped_median_for_column(continent, perc_college_complete)


# Ejercicios

# Median fertility rate by region for world_bank_data
region_fert_rate <- data1 %>% 
  grouped_median_for_column(region, fertility_rate)

# Sort by descending median rate
region_fert_rate %>% 
  arrange(desc(median_of_fertility_rate))

# Median infant mortality rate by region (descending)
world_bank_data %>% 
  grouped_median_for_column(region, infant_mortality_rate) %>% 
  arrange(desc(median_of_infant_mortality_rate))

# Se puede observar que Africa tiene resultados altos para ambos mortalidad infantil
# y tasa de fertilidad


