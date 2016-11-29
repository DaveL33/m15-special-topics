# Exercise-1
# Implement code from this book chapter: http://r4ds.had.co.nz/many-models.html

library(gapminder)
library(modelr)
library(tidyverse)
library(dplyr)

# Initial view of the data with ggplot
gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# Look only at new zealand
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line()

# Better yet, write your own function to accept a country as a parameter,
# and produce the same graphics
country1 <- function(country2){
  hi <- filter(gapminder, country == country2)
  hi %>% 
    ggplot(aes(year, lifeExp)) +
    geom_line()
  return(hi)
}

# Nest the data by country/continent
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()


# Define a statistical model, and store it in a function
country_lm <- function(data) {
  lm(pop ~ year, data = data)
}

# Use the `map` functionality to run the same model for each country separately


# Add additional columns to store your residuals (distance between data and prediction)


# Unnest your residual


# Plot the residuals



# Plot residuals by continent


# Use `glance` to look at model quality


# Compare model quality to continent


# View country that have an r.squared value of less than .25

