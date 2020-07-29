library(tidyverse)
library(scales)
library(gapminder)

# establish base plot
gapminder_filtered <- gapminder %>%
  filter(year > 2000)

base_plot <- ggplot(
  data = gapminder_filtered,
  mapping = aes(
    x = gdpPercap, y = lifeExp,
    color = continent, size = pop
  )
) +
  geom_point() +
  # Use dollars, and get rid of the cents part (i.e. $300 instead of $300.00)
  scale_x_log10(labels = dollar_format(accuracy = 1)) +
  # Format with commas
  scale_size_continuous(labels = comma) +
  # Use viridis
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    x = "GDP per capita", y = "Life expectancy",
    color = "Continent", size = "Population",
    title = "Here's a cool title",
    subtitle = "And here's a neat subtitle",
    caption = "Source: The Gapminder Project"
  ) +
  facet_wrap(vars(year))

base_plot
