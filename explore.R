library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)

events_log = read_csv("events_log.csv")

events_log_ts_fixed = events_log %>% 
  mutate(timestamp =  as.POSIXct(as.character(timestamp), 
                                 format = "%Y%m%d%H%M%S", 
                                 origin = as.Date("1970-01-01"))) %>% 
  select(-uuid)

# Which results do people tend to try first? How does it change day-to-day?
# Create 2 plots, 1 for overall and other faceted by day

result_position_df = events_log_ts_fixed %>% 
  mutate(date = as.Date(timestamp)) %>% 
  select(result_position, date) %>% na.omit()

summarize_result_position_df <- function(grouped_df){
  grouped_df %>% 
    summarize(count = n()) %>% 
    mutate(percentage_count = 100*(count/sum(count))) %>% 
    top_n(percentage_count, n = 10) %>% 
    ungroup() %>% as.data.frame()
}

result_position_overall = result_position_df %>% 
  group_by(result_position) %>%
  summarize_result_position_df()
  
ggplot(data= result_position_overall, 
       aes(x=factor(result_position), y=percentage_count)) + 
  geom_bar(stat="identity") + ylab("Users clicking the result %") + xlab("result position") +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Frequency of Users clicking the result number")

result_position_by_date = result_position_df %>% 
  group_by(date, result_position) %>%
  summarize_result_position_df()

ggplot(data= result_position_by_date, 
       aes(x=factor(result_position), y=percentage_count)) + 
  geom_bar(stat="identity") + ylab("Users clicking the result %") + xlab("result position") +
  facet_wrap("date", scales = "free") + 
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Frequency of Users clicking the result number by date")

events_session_group = 
  events_log_ts_fixed %>% 
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

summarize_clickthrough_rate <- function(grouped_df){
  grouped_df %>% 
  summarise(count = n(), 
            atleast_one_result_clicked = sum(num_results_clicked>0)) %>% 
    mutate(percentage_clickthrough = 100*(atleast_one_result_clicked/count)) %>% 
    ungroup() %>% as.data.frame()
}

clickthrough_rate_overall = events_session_group %>% 
  group_by(user_group) %>%
  summarize_clickthrough_rate()

ggplot(data= clickthrough_rate_overall, 
       aes(x=factor(user_group), y=percentage_clickthrough)) + 
  geom_bar(stat="identity") + ylab("Clickthrough rate %") + xlab("user group") +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Clickthrough rate of users by group")

clickthrough_rate_by_date = events_session_group %>% 
  group_by(date, user_group) %>%
  summarize_clickthrough_rate()

ggplot(data= clickthrough_rate_by_date, 
       aes(x=factor(user_group), y=percentage_clickthrough)) + 
  geom_bar(stat="identity") + ylab("Clickthrough rate %") + xlab("user group") + 
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap("date", scales = "free") +
  ggtitle("Clickthrough rate of users by group by date")

# What is our daily overall zero results rate? How does it vary between the groups?
# Create 2 plots, 1 for overall and other faceted by day
summarize_zero_results <- function(grouped_df){
  grouped_df %>% 
    summarise(count = n(), 
            num_zero_results = sum(minimum_n_results==0, na.rm = T)) %>% 
    mutate(percentage_zero_results = 100*(num_zero_results/count))
}

zero_results_overall = events_session_group %>% 
  group_by(user_group) %>%
  summarize_zero_results()

ggplot(data= zero_results_overall, 
       aes(x=factor(user_group), y=percentage_zero_results)) + 
  geom_bar(stat="identity") + ylab("zero results %") + xlab("user group") +
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Zero results shown to users by group")

zero_results_by_date = events_session_group %>% 
  group_by(date, user_group) %>%
  summarize_zero_results()

ggplot(data= zero_results_by_date, 
       aes(x=factor(user_group), y=percentage_zero_results)) + 
  geom_bar(stat="identity") + ylab("zero results %") + xlab("user group") + 
  theme(axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap("date", scales = "free") +
  ggtitle("Zero results shown to users by group by date")


# Let *session length* be approximately the time between the first event and the 
# last event in a session. Choose a variable from the dataset and describe its 
# relationship to session length. Visualize the relationship.

# No strong relation,  Point out anomalies, 
# problem in data sampling rate being too slow, create correlation plot

correlation_matrix = events_session_group %>% 
  filter(!is.na(session_time) & !is.na(minimum_n_results)) %>% 
  select(session_time, minimum_n_results, num_serp_visits, 
         num_results_clicked, num_unique_pages_visited) %>% 
  cor() %>% round(4)

melted_correlation_matrix = melt(correlation_matrix)

ggplot(data = melted_correlation_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + 
  scale_fill_gradient2(low = "red", high = "green", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme( axis.title.x = element_blank(), axis.title.y = element_blank(),
         axis.text.x = element_text(angle = 90, face = "bold", size = 10),
         axis.text.y = element_text(face = "bold", size = 10),
         plot.title = element_text(hjust = 0.5)) +
  ggtitle("Correlation Heatmap")
