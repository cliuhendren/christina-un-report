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
