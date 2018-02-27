source("reading.R")
source("preparation.R")

library("dplyr")
library("ggvis")
library("data.table")

dataset <- read_dataset("0421.txt")

# Question 1
total_apps <- length(unique(dataset$app_id))
sprintf("Dataset contains %d application", total_apps)

# Question 2
intervals <- dataset %>% 
  group_by(app_id) %>% 
  summarize(min = min(traffic_volume), max = max(traffic_volume), size = max - min)

intervals %>% filter(size == max(intervals$size)) %>% 
  do(., print(paste0("For app ", .$app_id, " range is [", .$min, ", ", .$max, "]")))

intervals %>% filter(size == min(intervals$size)) %>% 
  do(., print(paste0("For app ", .$app_id, " range is [", .$min, ", ", .$max, "]")))

# Question 3
min.packet.size <- min(dataset$traffic_volume)

intervals1 <- copy(intervals)

similar.intervals <- intervals %>% 
  group_by(app_id) %>%
  mutate(., 
   similar = paste(intervals[app_id != .$app_id & abs(min - .$min) < 0.1*min & abs(max - .$max) < 0.1*max,]$app_id, collapse = ","),
   count = nrow(intervals[app_id != .$app_id & abs(min - .$min) < 0.1*min & abs(max - .$max) < 0.1*max,]))

library(ggvis)

similar.intervals %>% 
  group_by(count) %>% 
  summarise(n = n()) %>%
  mutate(color = ifelse(count > 0, "red", "green")) %>%
  group_by(count) %>%
  ggvis(x=~count, y=~n, fill:=~color) %>%
  layer_bars() %>%
  add_axis("x", title="Number of similar intervals") %>%
  add_axis("y", title="Number of applications with same number of similar intervals")

tt <- ecdf(similar.intervals$count)

library("ggplot2")

ggplot(similar.intervals, aes(count)) + stat_ecdf(geom="step", colour="#000000")

intervals1 <- setDT(intervals)

setkey(intervals1, min, max)
overlaps <- foverlaps(intervals1, intervals1, type="any") %>% group_by(app_id) %>% summarise(n=n())

# Question 4

samples.numbers <- dataset %>% group_by(app_id) %>% summarize(samples = n())

samples.numbers %>% filter(samples == max(samples.numbers$samples)) %>% 
  do(., print(paste0("For app ", .$app_id, " samples number is ", .$samples)))

samples.numbers %>% filter(samples == min(samples.numbers$samples)) %>% 
  do(., print(paste0("For app ", .$app_id, " samples number is ", .$samples)))

# Question 8

users <- unique(dataset$user_id)

print(paste0("There are ", length(users), " in the dataset"))

# Set 2

# Question

library("GGally")
library("network")
library(sna)
library(ggplot2)

user.paths <- dataset %>% 
  group_by(user_id) %>% 
  select(user_id, time, base_station) %>% 
  arrange(time) %>% 
  mutate(
    duration = ifelse(base_station != lag(base_station), time - ifelse(is.na(lag(time)), time, lag(time)), NA),
    from =  ifelse(base_station != lag(base_station), lag(base_station), NA),
    to =  ifelse(base_station != lag(base_station), base_station, NA),
    from_time = lag(time),
    to_time = time
  ) %>%
  filter(!is.na(duration)) %>%
  select(user_id, from, to, from_time, to_time, duration)

unique.path <- unique(user.paths[,2:3])

user.paths.updated <- user.paths %>% 
  mutate(from1 = ifelse(from < to, from, to), to1 = ifelse(from > to, from, to)) %>%
  group_by(from1, to1) %>%
  summarise(travel_time = min(duration))

tt <- user.paths %>% group_by(to) %>% summarize(num = n()) %>% filter(num <= 10) %>% top_n(10, num)

user.paths.1 <- user.paths %>% filter(to %in% tt$to) %>% mutate(duration = duration + 1)

user.paths.table <- as.matrix(as.data.frame.matrix(xtabs(duration ~ from + to, user.paths.1)))

net <- network(user.paths.table, directed = TRUE)

edges <- rgraph(10, tprob=0.25)
net<-network(edges, directed = TRUE)

ggnet2(net, label = TRUE, arrow.size = 12, arrow.gap = 0.02)

median.traffic <- dataset %>% group_by(app_id, base_station) %>% summarise(median_traffic = median(traffic_volume), samples = n())

library(ggvis)

top.stations <- dataset %>% group_by(base_station) %>% summarise(n = n()) %>% top_n(10, n)
  
top.apps <- dataset %>% group_by(app_id) %>% summarise(n = n(),) %>% top_n(10, n)

deviance <- median.traffic %>% group_by(app_id) %>% summarise(sd = sd(median_traffic))

median.traffic %>% filter(app_id == 1683) %>% ggvis(x = ~base_station, y = ~median_traffic) %>% layer_bars()

median.traffic <- dataset %>% group_by(app_id, user_id, base_station, time) %>% summarise(median_traffic = median(traffic_volume))


nrow(deviance %>% filter(!is.na(sd) & sd > 0))