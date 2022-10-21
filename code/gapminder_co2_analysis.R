# Data Analysis (workshop day 2)

library(tidyverse)

gapminder_data <- read_csv("../data/gapminder_data.csv") # load from a diff directory
# read _ csv is from readr package. Don't want read.csv

# Summarize, using dplyr package
summarize(gapminder_data, averageLifeExp=mean(lifeExp))
# ^ averageLifeExp is not being created as a variable, it is just for the display in console
# if you don't name it, the display would just say mean(lifeExp)

gapminder_data %>% summarize(averageLifeExp=mean(lifeExp))
gapminder_data_summarized <- gapminder_data %>%
  summarize(averageLifeExp=mean(lifeExp))

# Filter
gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

gapminder_data %>%
  summarize(min(year)) # you could also name this such as summarize(first_yr = min(year))
gapminder_data %>%
  filter(year == min(year)) %>%
  summarize(avg_gdp = mean(gdpPercap))

# Group By
gapminder_data %>%
  group_by(year) %>%
  summarize(avg_LE = mean(lifeExp))

gapminder_data %>%
  group_by(year) %>%
  summarize(avg_life <- mean(lifeExp)) # still works

gapminder_data %>%
  group_by(year) %>%
  summarise(average = mean(lifeExp), min = min(lifeExp))

# Mutate - add new columns
gapminder_data %>%
  mutate(gdp = pop * gdpPercap,
         popInMillions = pop / 1000000)
# make multiple columns in same function using comma

# Select - subset columns or change their order
gapminder_data %>%
  select(pop, year)

gapminder_data %>%
  select(continent, country)
# this narrows down the data and tells what order we want the columns in

gapminder_data %>%
  select(gdpPercap, everything())

# Pivot long data to wide data
gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# Dataset for analysis

gapminder_data_2007 <- read_csv("../data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

# Data cleaning
read_csv("../data/co2-un-data.csv", skip = 1)
read_csv("../data/co2-un-data.csv", skip = 2,
         col_names = c("Region","Country","Year","Series","Value","Footnotes","Source"))
read_csv("../data/co2-un-data.csv", skip = 1) %>%
  rename(Country = ...2) # you could also put it in quotes
read_csv("../data/co2-un-data.csv", skip = 1) %>%
  rename_all(tolower)

co2_emissions_dirty <- read_csv("../data/co2-un-data.csv", skip = 2,
         col_names = c("region","country","year","series","value","footnotes","source"))

# Practicing select()
#country, year, series, value
co2_emissions_dirty %>%
  select(country, year, series, value)

co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  # number of observations per year
  count(year)

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year == 2005) %>% 
  select(-year)

# Joining data frames

inner_join(gapminder_data_2007, co2_emissions)
anti_join(gapminder_data_2007, co2_emissions,
          by = "country")

co2_emissions <- read_csv("../data/co2-un-data.csv",
                          skip = 2,
                          col_names = c("region","country","year","series","value","footnotes","source")) %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year == 2005) %>% 
  select(-year) %>%
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))
anti_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("../data/gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States"))

anti_join(gapminder_data_2007, co2_emissions, by = "country") 

gapminder_data_2007 <- read_csv("../data/gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop) / sum(pop),
            gdpPerCap = sum(gdpPercap * pop) / sum(pop),
            pop = sum(pop))

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")

gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" |
                            country == "United States" |
                            country == "Mexico","north","south"))
# if true, it will be labelled as north, if false it will be south
# the above column will not be included in the export

# Write a new data frame
write_csv(gapminder_co2, "../data/gapminder_co2.csv")
