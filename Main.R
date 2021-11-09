library(tidyverse)

data1 <- readRDS("data/world_bank_data.rds")


names(data1)

data1 %>% 
  select(country, continent, region, year, perc_rural_pop, perc_college_complete)


# Capitulo 1 --------------------------------------------------------------

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


# Capitulo 2 --------------------------------------------------------------

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
  select(
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
