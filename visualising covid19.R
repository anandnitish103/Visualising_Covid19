
# Load the readr, ggplot2, and dplyr packages
library(readr)
library(ggplot2)
library(dplyr)

# Read datasets/confirmed_cases_worldwide.csv into confirmed_cases_worldwide

confirmed_cases_worldwide <- read_csv("confirmed_cases_worldwide.csv")

# See the result

confirmed_cases_worldwide




soln_confirmed_cases_worldwide <- read_csv("confirmed_cases_worldwide.csv")





#   line plot to visualize the confirmed cases worldwide

ggplot(confirmed_cases_worldwide ,aes(date,cum_cases)) +
  geom_line() +
  ylab("Cumulative confirmed cases")


# Let's plot confirmed COVID-19 cases in China and the rest of the world separately to see if it gives us any insight.

confirmed_cases_china_vs_world <- read_csv("confirmed_cases_china_vs_world.csv")

# See the result
glimpse(confirmed_cases_china_vs_world)

# Drawing a line plot of cumulative cases vs. date, grouped and colored by is_china

plt_cum_confirmed_cases_china_vs_world <- ggplot(confirmed_cases_china_vs_world) +
  geom_line(aes(date,cum_cases,color = is_china , group = is_china)) +
  ylab("Cumulative confirmed cases")

# See the plot
plt_cum_confirmed_cases_china_vs_world

soln_confirmed_cases_china_vs_world <- read_csv("confirmed_cases_china_vs_world.csv")

#  In February, the majority of cases were in China. That changed in 
# March when it really became a global outbreak: around March 14, the total number
# of cases outside China overtook the cases inside China.This was days after the WHO declared a pandemic.''

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health\nemergency declared",
  "2020-03-11", "Pandemic\ndeclared",
  "2020-02-13", "China reporting\nchange"
) %>%
  mutate(date = as.Date(date))



# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 100000 on the y-axis
plt_cum_confirmed_cases_china_vs_world +
  geom_vline(aes(xintercept = date), data = who_events, linetype = "dashed") +
  geom_text(aes(date, label = event), data = who_events, y = 1e5)




# Filter for China, from Feb 15
china_after_feb15 <- confirmed_cases_china_vs_world %>%
  filter(is_china =="China" & date >= "2020-02-15")

# Using china_after_feb15, drawing a line plot cum_cases vs. date
# Adding a smooth trend line using linear regression

ggplot(china_after_feb15,aes(date,cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm",se = FALSE) +
  ylab("Cumulative confirmed cases")




# Filter confirmed_cases_china_vs_world for not China

not_china <- confirmed_cases_china_vs_world %>% 
  filter(is_china != "China")

# Using not_china, draw a line plot cum_cases vs. date
# Adding a smooth trend line using linear regression, no error bars

plt_not_china_trend_lin <- ggplot(not_china , aes(date , cum_cases)) +
  geom_line() +
  geom_smooth(method = "lm",se = FALSE) +
  ylab("Cumulative confirmed cases")

# See the result
plt_not_china_trend_lin 


# Modify the plot to use a logarithmic scale on the y-axis
plt_not_china_trend_lin + scale_y_log10()




# geting the data for each country
confirmed_cases_by_country <- read_csv("confirmed_cases_by_country.csv")
glimpse(confirmed_cases_by_country)

# Group by country, summarize to calculate total cases, finding the top 7
top_countries_by_total_cases <- confirmed_cases_by_country %>%
  group_by(country) %>%
  summarize(total_cases = max(cum_cases)) %>%
  top_n(7)

# See the result
top_countries_by_total_cases




# geting the data for the top 7 countries
confirmed_cases_top7_outside_china <- read_csv("confirmed_cases_top7_outside_china.csv")

# 
glimpse(confirmed_cases_top7_outside_china)



# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, grouped and colored by country

ggplot(confirmed_cases_top7_outside_china,aes(date , cum_cases)) + 
  geom_line(aes(group = country,color = country)) + 
  ylab("Cumulative confirmed cases")

