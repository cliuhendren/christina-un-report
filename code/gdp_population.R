# GDP Population Script

library(tidyverse)

gapminder_1997 <- read_csv("gapminder_1997.csv") # loads to object

#getwd() # get working directory
#sum(5,6) # sum of two numbers
#rm(flower,new_flower) # remove a variable

#
# Plotting
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) + # aesthetics
  labs(x = "GDP Per Capita ($)") + # label
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (years)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) + # continent was a category within our dataset
  aes(size = pop/1000000) +
  labs(size = "Population (millions)")

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  geom_point() + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Cap", y = "Life Expectancy (yrs)", title = "Wealth and Life Exp")
  
# See color brewer website to play with more color options.


#
# Plotting for data exploration
gapminder_data <- read_csv("gapminder_data.csv")
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent, group = country) +
  geom_line() + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Life Expectancy (yrs)", title = "Life Expectancy over Time")

ggplot(data = gapminder_data) +
  aes(x = continent, y = lifeExp, color = continent) +
  geom_violin() + 
  labs(x = "Continent", y = "Life Expectancy (yrs)", title = "Life Expectancy by Continent")

# geom_boxplot
# geom_violin() # shows spread of data and density at the y-values
# geom_violin() + geom_point()

ggplot(data = gapminder_1997) +
  aes(x = continent, y = lifeExp) +
  geom_violin(aes(fill = continent)) +
  geom_jitter(alpha = 0.5) + # adds a level of transparency
  labs(x = "Continent", y = "Life Expectancy (yrs)", title = "Life Expectancy with Jitter")

#
# Plotting univariable
ggplot(data = gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 24) +
  labs(title = "Histogram")

ggplot(data = gapminder_1997) +
  aes(x = lifeExp) +
  geom_density() +
  labs(title = "geom density")

ggplot(data = gapminder_1997) +
  aes(x = lifeExp) +
  geom_freqpoly() +
  labs(title = "geom freq poly")

#
# Making the plot look nice - ggplot2 Themes
ggplot(data = gapminder_data) +
  aes(x = lifeExp) +
  geom_histogram(binwidth = 1, aes(fill = continent)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
# ^ this plot might not be meaningful, the count might be absorbing the years data...

# theme_classic(), theme_minimal()
# vjust and hjust seem to adjust the position of the axis labels

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

#
# Saving plots
ggsave("facet_plot.jpg", width = 6, height = 4)
# the height and width are in inches

violin_plot <- ggplot(data = gapminder_1997) + # there was more here I missed
  aes(x = gdpPercap, y = lifeExp)
violin_plot <- violin_plot + theme_bw()

View(violin_plot) # can use in the Console below

#
# Animated Plot
install.packages(c("gganimate"))
install.packages(c("gifski"))
library(gganimate) # brings the packages into the tidyverse(?)
library(gifski)

ggplot(data = gapminder_data) +
  aes(x = log(gdpPercap), y = lifeExp, size = pop, color = continent) +
  geom_point() +
  labs(title = "Life Expectancy by GDP Over Time", x = "log(GDP Per Capita)", y = "Life Expectancy")

staticHansPlot <- ggplot(data = gapminder_data) +
  aes(x = log(gdpPercap), y = lifeExp, size = pop/1000000, color = continent) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Life Expectancy by GDP Over Time", x = "log(GDP Per Capita)", y = "Life Expectancy") +
  theme_classic()
staticHansPlot # this line displays it

animatedHansPlot <- staticHansPlot +
  transition_states(year, transition_length = 1, state_length = 1) + # note year is a variable and transition_length is a specified argument
  ggtitle("{closest_state}")

animatedHansPlot

# export to gif
anim_save("hansAnimatedPlot.gif",
          plot = animatedHansPlot,
          renderer = gifski_renderer())
