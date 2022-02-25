library(dplyr)
library(stringr)
library(tidyverse)

urlfile <-'https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv'
incarceration_data <- read.csv(urlfile)

aapi_df <- incarceration_data %>%
  select(year, state, county_name, aapi_pop_15to64)

mean(aapi_df$aapi_pop_15to64, na.rm = TRUE)
# finds mean across all years

# minWA_ year <- aapi_df %>%
#   filter(year == min(year), state == "WA") this was my original code, but there was no data in the minimum year

minWA_year <- aapi_df %>%
  filter(year == 1990, state == "WA") # 1990 is the earliest year with data in this column

max_in_minWA <- max(minWA_year$aapi_pop_15to64, na.rm = TRUE)

maxWA_year <- aapi_df %>%
  filter(year == 2016, state == "WA") # 2016 is the most recent year with data in this column

max_in_maxWA <- max(maxWA_year$aapi_pop_15to64, na.rm = TRUE)

diffWA_year <- max_in_maxWA - max_in_minWA # 223168
# how has the maximum in Washington changed between the most recent and oldest year


merged_df <- aapi_df %>%
  unite("location", county_name, state, remove = FALSE)

location_max <- merged_df %>%
  filter(aapi_pop_15to64 == max(aapi_pop_15to64, na.rm = TRUE)) %>% 
  pull(location)
# In what state & county was the highest number of AAPI 15-64 year olds recorded?


min_yearNY <- aapi_df %>%
  filter(state == "NY") %>%
  filter(aapi_pop_15to64 == min(aapi_pop_15to64, na.rm = TRUE))%>% 
  pull(year) 
# In what year was the minimum number of AAPI 15-64 year-olds in New York recorded?
  # Every year from 1990-1999 tied for the minimum value


aapi_TX <- aapi_df %>%
  filter(state == "TX",
         year == "2003")

TX_median <- median(aapi_TX$aapi_pop_15to64, na.rm = TRUE) # 39 people

aapi_CA <- aapi_df %>%
  filter(state == "CA",
         year == "2003")

CA_median <- median(aapi_CA$aapi_pop_15to64, na.rm = TRUE) # 5318.5 people

TX_and_CA_df <- aapi_df %>%
  filter(state == "TX" | state == "CA") %>%
  filter(year == "2003")

combined_median <- median(TX_and_CA_df$aapi_pop_15to64, na.rm = TRUE) # 70.5 people

# In the year 2003, what was the difference between the median number of incarcerated AAPI 15-64 year-olds
# in California and Texas? What was the combined median?


# a chart that shows trends over time for the variable that i have been using thus far - aapi pop 15-64 yr olds
aapi_df2 <- aapi_df %>%
  select(state, year, aapi_pop_15to64) %>%
  filter(substr(state, 1, 1) == "A" | substr(state, 1, 1) == "C") %>%
  group_by(state, year) %>%
  summarize(aapi_pop_15to64 = sum(aapi_pop_15to64, na.rm = TRUE))

aapi_chart <- ggplot(aapi_df2, aes(x = year, y = aapi_pop_15to64, color = state)) +
  geom_path() +
  scale_y_continuous(name= "Incarcerated 15-64 year-old AAPI", labels = scales::comma)

aapi_plot <- print(aapi_chart + ggtitle("Incarcerated AAPI in the first 8 states") + labs (x = "Years (1970 - 2018)", colour = "States"))

# a chart that compares two variables to one another
library(areaplot)

race_df <- incarceration_data %>%
  select(year, white_jail_pop, black_jail_pop) %>%
  group_by(year) %>%
  summarize(White = sum(white_jail_pop, na.rm = TRUE),
            Black = sum(black_jail_pop, na.rm = TRUE))

x <- race_df$year
y <- race_df[, c(2, 3)]
cols <- hcl.colors(4, palette = "ag_Sunset")

race_comp_graph <- areaplot(.~year, data = race_df,
         main = "White vs. Black Incarceration Rates",
         xlab = "Year (1970-2018)", 
         ylab = "Incarcerations",
         label = scales::comma,
         col = cols,
         border = "white",
         lwd = 1,
         lty = 1,
         legend = TRUE,
         args.legend = list(x = "topleft", cex = 0.7,
                            bg = "white", bty = "o"))

# a map that shows how your measure of interest varies geographically
library(usmap)

latinx_df <- incarceration_data %>%
  select(year, state, latinx_jail_pop) %>%
  group_by(state) %>%
  summarize(latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE))

us_map <- plot_usmap(data = latinx_df, values = "latinx_jail_pop", color = "red") + 
  labs(title = "Number of Latinx People Incarcerated by State") +
  scale_fill_continuous(name = "Number of Incarcerations", label = scales::comma) + 
  theme(legend.position = "right")


