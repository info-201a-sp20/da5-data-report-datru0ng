library(dplyr)
library(ggplot2)

# Loading my data
shootings <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# total number of shootings occurred in 2018
total_shootings <- nrow(shootings)

# Calculate which state were impacted by the most shootings
# impacted = how many times the state appears on the list
# Results had two states, this one pulls out California
state_most_impacted1 <- shootings %>%
  count(state) %>%
  filter(n == max(n)) %>%
  slice(1) %>%
  pull(state)

# Calculate which state were impacted by the most shootings
# impacted = how many times the state appears on the list
# Results had two states, this one pulls out Illinois
state_most_impacted2 <- shootings %>%
  count(state) %>%
  filter(n == max(n)) %>%
  slice(2) %>%
  pull(state)

# Calculate total shooting in cali / illinois (since same)
num_impacted <- shootings %>%
  count(state) %>%
  filter(n == max(n)) %>%
  slice(1) %>%
  pull(n)

# Calculate how many lives were lost for 2018
total_lives_lost <- shootings %>%
  summarise(total_lost = sum(num_killed)) %>%
  pull(total_lost)

# Calculate how many people were injured for 2018
total_injured <- shootings %>%
  summarise(injured = sum(num_injured)) %>%
  pull(injured)

# Calculate what day had most killed and injured
# There were two dates, so this is for the first date
most_shooting_day1 <- shootings %>%
  group_by(date) %>%
  summarise(most_shooting = sum(num_killed + num_injured)) %>%
  filter(most_shooting == max(most_shooting)) %>%
  slice(1) %>%
  pull(date)

# Calculate what day had most killed and injured
# There were two dates, so this is for the second date
most_shooting_day2 <- shootings %>%
  group_by(date) %>%
  summarise(most_shooting = sum(num_killed + num_injured)) %>%
  filter(most_shooting == max(most_shooting)) %>%
  slice(2) %>%
  pull(date)

# Calculate the amount of shooting in the day mentioned above
most_shooting <- shootings %>%
  group_by(date) %>%
  summarise(most_shooting = sum(num_killed + num_injured)) %>%
  filter(most_shooting == max(most_shooting)) %>%
  slice(1) %>%
  pull(most_shooting)

# Calculate which city was most impacted by shootings
# Impact = which city shows up the most in the list
city_impacted <- shootings %>%
  count(city) %>%
  filter(n == max(n)) %>%
  pull(city)

# Calculate the amount of times chicago appeared
chicago_num <- shootings %>%
  count(city) %>%
  filter(n == max(n)) %>%
  pull(n)

# Calculate the number of total injuries from chicago
chicago_total_injured <- shootings %>%
  group_by(state) %>%
  summarise(tot_injured = sum(num_injured)) %>%
  filter(tot_injured == max(tot_injured)) %>%
  pull(tot_injured)

####################################

# Calculate the number of shootings occurred in each state
number_states <- shootings %>%
  group_by(state) %>%
  count(state) %>%
  rename(number_shooting_occurred = n)

# Calculate individually the number of total killed
# and total injured in each state
calculate_damages <- shootings %>%
  group_by(state) %>%
  summarise(total_killed = sum(num_killed), total_injured = sum(num_injured))

# Combined both data frames above
summary_table <- full_join(calculate_damages, number_states, by = "state")

####################################

library(leaflet)

# Blue dots represents number of people injured. The darker the blue,
# the more there are in the current area.The larger the blue circle,
# the more number of people injured occurred in the area.
# Red dots represents number of pople killed. The more red it is,
# the more there are in the current area.
# The larger the red circle, the more number of people were killed in that area.
map_us <- shootings %>%
  mutate(popup = paste("City name:", city, "Number killed:",
    num_killed, "Number injured:", num_injured,
    sep = "<br/>"
  )) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    popup = ~popup,
    radius = ~num_injured,
    stroke = FALSE, fillOpacity = .5
  ) %>%
  addCircleMarkers(
    popup = ~popup,
    color = "red",
    radius = ~num_killed,
    stroke = FALSE, fillOpacity = .5
  )

#################################################

# Create a bar graph of each state and their number of shootings
most_state_shooting <- ggplot(data = number_states) +
  geom_col(mapping = aes(state, number_shooting_occurred,
    fill = number_shooting_occurred
  )) +
  coord_flip()
