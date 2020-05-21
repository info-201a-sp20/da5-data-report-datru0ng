library(dplyr)
library(ggplot2)

# Loading my data
shootings <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

# Rearranging the shootings data by their state and alphabetizing the cities by name
# This is for me
grouped_states_shootings <- shootings %>% 
  arrange(state, city)

# total number of shootings occurred in 2018
total_shootings <- nrow(grouped_states_shootings)

# Calculate which state were impacted by the most shootings
# impacted = how many times the state appears on the list
# Results had two states, this one pulls out California
state_most_impacted1 <- grouped_states_shootings %>% 
  count(state) %>% 
  filter(n == max(n)) %>% 
  slice(1) %>% 
  pull(state)

# Calculate which state were impacted by the most shootings
# impacted = how many times the state appears on the list
# Results had two states, this one pulls out Illinois
state_most_impacted2 <- grouped_states_shootings %>% 
  count(state) %>% 
  filter(n == max(n)) %>% 
  slice(2) %>% 
  pull(state)

# Calculate total shooting in cali / illinois (since same)
num_impacted <- grouped_states_shootings %>% 
  count(state) %>% 
  filter(n == max(n)) %>% 
  slice(1) %>% 
  pull(n)

# Calculate how many lives were lost for 2018
total_lives_lost <- grouped_states_shootings %>% 
  summarise(total_lost = sum(num_killed)) %>% 
  pull(total_lost)

# Calculate how many people were injured for 2018
total_injured <- grouped_states_shootings %>% 
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
city_impacted <- grouped_states_shootings %>% 
  count(city) %>% 
  filter(n == max(n)) %>% 
  pull(city)

# Calculate the amount of times chicago appeared
chicago_num <- grouped_states_shootings %>% 
  count(city) %>% 
  filter(n == max(n)) %>% 
  pull(n)

chicago_total_injured <- grouped_states_shootings %>% 
  group_by(state) %>%
  summarise(tot_injured = sum(num_injured)) %>% 
  filter(tot_injured == max(tot_injured)) %>% 
  pull(tot_injured)

####################################

number_states <- grouped_states_shootings %>% 
  group_by(state) %>%
  count(state) %>% 
  rename(number_shooting_occurred = n)

calculate_damages <- grouped_states_shootings %>% 
  group_by(state) %>%
  summarise(total_killed = sum(num_killed), injured_total = sum(num_injured))

summary_table <- full_join(calculate_damages, number_states, by = "state")

is.data.frame(summary_table)

####################################

library(leaflet)

# Blue dots represents number of people injured. The darker the blue, the more there
# are in the current area.The larger the blue circle, the more number of people injured
# occurred in the area.
# Red dots represents number of pople killed. The more red it is, the more there are
# in the current area.
# The larger the red circle, the more number of people were killed in that area.
map_US <- grouped_states_shootings %>% 
  mutate(popup = paste("City name:", city, "Number killed:",
                       num_killed, "Number injured:", num_injured, sep = "<br/>")) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(popup = ~popup,
                   radius = ~num_injured,
                   stroke = FALSE, fillOpacity = .5) %>% 
  addCircleMarkers(popup = ~popup,
                   color = "red",
                   radius = ~num_killed,
                   stroke = FALSE, fillOpacity = .5)

##############################################################

most_state_impacted <- grouped_states_shootings %>% 
  count(state) %>% 
  rename(number_of_shooting = n)

most_state_shooting <- ggplot(data = most_state_impacted) +
  geom_col(mapping = aes(state, number_of_shooting, fill = number_of_shooting)) +
  coord_flip()

