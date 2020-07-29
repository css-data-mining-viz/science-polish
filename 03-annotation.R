library(tidyverse)  # For ggplot, dplyr, and friends
library(ggrepel)    # For non-overlapping labels

# You need to install ggtext from GitHub. Follow the instructions at 
# https://github.com/wilkelab/ggtext
library(ggtext)     # For fancier text handling

# load/clean data
wdi_co2_raw <- read_csv("data/wdi_co2.csv")
wdi_clean <- wdi_co2_raw %>% 
  filter(region != "Aggregates") %>% 
  select(iso2c, iso3c, country, year, 
         population = SP.POP.TOTL,
         co2_emissions = EN.ATM.CO2E.PC, 
         gdp_per_cap = NY.GDP.PCAP.KD, 
         region, income)

# generate co2 emissions rankings
co2_rankings <- wdi_clean %>% 
  # Get rid of smaller countries
  filter(population > 200000) %>% 
  # Only look at two years
  filter(year %in% c(1995, 2014)) %>% 
  # Get rid of all the rows that have missing values in co2_emissions
  drop_na(co2_emissions) %>% 
  # Look at each year individually and rank countries based on their emissions that year
  group_by(year) %>% 
  mutate(ranking = rank(co2_emissions)) %>% 
  ungroup() %>% 
  # Only select a handful of columns, mostly just the newly created "ranking"
  # column and some country identifiers
  select(iso3c, country, year, region, income, ranking) %>% 
  # Right now the data is tidy and long, but we want to widen it and create
  # separate columns for emissions in 1995 and in 2014. pivot_wider() will make
  # new columns based on the existing "year" column (that's what `names_from`
  # does), and it will add "rank_" as the prefix, so that the new columns will
  # be "rank_1995" and "rank_2014". The values that go in those new columns will
  # come from the existing "ranking" column
  pivot_wider(names_from = year, names_prefix = "rank_", values_from = ranking) %>% 
  # Find the difference in ranking between 2014 and 1995
  mutate(rank_diff = rank_2014 - rank_1995) %>% 
  # Remove all rows where there's a missing value in the rank_diff column
  drop_na(rank_diff) %>% 
  # Make an indicator variable that is true of the absolute value of the
  # difference in rankings is greater than 25. 25 is arbitrary here—that just
  # felt like a big change in rankings
  mutate(big_change = abs(rank_diff) >= 25) %>% 
  # Make another indicator variable that indicates if the rank improved by a
  # lot, worsened by a lot, or didn't change much. We use the case_when()
  # function, which is like a fancy version of ifelse() that takes multiple
  # conditions. This is how it generally works:
  #
  # case_when(
  #  some_test ~ value_if_true,
  #  some_other_test ~ value_if_true,
  #  TRUE ~ value_otherwise
  #)
  mutate(better_big_change = case_when(
    rank_diff <= -25 ~ "Rank improved",
    rank_diff >= 25 ~ "Rank worsened",
    TRUE ~ "Rank changed a little"
  ))

# core plot
ggplot(co2_rankings,
       aes(x = rank_1995, y = rank_2014)) +
  # Add points and color them by the type of change in rankings
  geom_point(aes(color = better_big_change)) +
  # Add repelled labels. Only use data where big_change is TRUE. Fill them by
  # the type of change (so they match the color in geom_point() above) and use
  # white text
  geom_label_repel(data = filter(co2_rankings, big_change == TRUE),
                   aes(label = country, fill = better_big_change),
                   color = "white") +
  # Use three different colors for the points
  scale_color_manual(values = c("grey50", "#0074D9", "#FF4136")) +
  # Use two different colors for the filled labels. There are no grey labels, so
  # we don't have to specify that color
  scale_fill_manual(values = c("#0074D9", "#FF4136")) +
  # Make the x and y axes expand all the way to the edges of the plot area and
  # add breaks every 25 units from 0 to 175
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 175, 25)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 175, 25))
