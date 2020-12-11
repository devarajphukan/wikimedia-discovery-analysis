library(dplyr)
library(ggplot2)
library(reshape2)

events_log = read.csv("events_log.csv")

events_log_ts_fixed = events_log %>% 
  mutate(timestamp =  as.POSIXct(as.character(timestamp), 
                                 format = "%Y%m%d%H%M%S", 
                                 origin = as.Date("1970-01-01"))) %>% 
  select(-uuid)

# Which results do people tend to try first? How does it change day-to-day?
# Create 2 plots, 1 for overall and other faceted by day
events_log_ts_fixed %>% 
  mutate(date = as.Date(timestamp)) %>% 
  select(result_position, date) %>% 
  na.omit() %>% 
  # group_by(date, result_position) %>%
  group_by(result_position) %>%
  summarize(count = n()) %>% 
  mutate(percentage_count = 100*(count/sum(count))) %>% 
  top_n(percentage_count, n = 10) %>% 
  ungroup() %>% as.data.frame()


events_session_group = events_log_ts_fixed %>% 
  mutate(date = as.Date(timestamp)) %>% 
  group_by(session_id) %>% 
  arrange(timestamp, desc = F) %>%
  summarise(user_group = group[1],
            date = as.Date(timestamp[1]),
            session_time = max(checkin, na.rm = T),
            minimum_n_results = min(n_results, na.rm = T),
            num_serp_visits = sum(action == "searchResultPage"),
            num_results_clicked = sum(action == "visitPage"),
            order_of_events = paste(unique(action), collapse = "-->"),
            num_unique_pages_visited = length(unique(page_id))) %>% 
  mutate(session_time = ifelse(session_time == -Inf, NA, session_time),
         minimum_n_results = ifelse(minimum_n_results == Inf, NA, minimum_n_results))

# What is our daily overall clickthrough rate? How does it vary between the groups?
# Create 2 plots, 1 for overall and other faceted by day
events_session_group %>% 
  group_by(date, user_group) %>%
  # group_by(user_group) %>%
  summarise(count = n(), 
            atleast_one_result_clicked = sum(num_results_clicked>0)) %>% 
  mutate(percentage_clickthrough = 100*(atleast_one_result_clicked/count))

# What is our daily overall zero results rate? How does it vary between the groups?
# Create 2 plots, 1 for overall and other faceted by day
events_session_group %>% 
  group_by(date, user_group) %>%
  # group_by(user_group) %>%
  summarise(count = n(), 
            num_zero_results = sum(minimum_n_results==0, na.rm = T)) %>% 
  mutate(percentage_zero_results = 100*(num_zero_results/count))


# Let *session length* be approximately the time between the first event and the 
# last event in a session. Choose a variable from the dataset and describe its 
# relationship to session length. Visualize the relationship.

# No strong relation,  Point out anomalies, 
# problem in data sampling rate being too slow, create correlation plot

events_session_group %>% 
  filter(!is.na(session_time) & !is.na(minimum_n_results)) %>% 
  # filter(num_serp_visits < 10) %>%
  # filter(num_unique_pages_visited < 10) %>%
  select(session_time, minimum_n_results, num_serp_visits, num_results_clicked, num_unique_pages_visited) %>% 
  cor()
