source("reading.R")
source("preparation.R")

library("dplyr")
library("ggvis")

dataset <- read_dataset("0421.txt")

samples.info <- dataset %>% group_by(app_id) %>% summarize(n = n(), traffic = sum(traffic_volume))

median((samples.info %>% filter(traffic > 949893))$traffic)

samples.info %>% filter(traffic == 6772736)

dataset %>% group_by(app_id) %>% summarize(traffic = sum(traffic_volume)) %>% 
  arrange(traffic) %>%
  ggvis(y=~traffic, x=~app_id) %>% layer_paths()


dataset %>% group_by(app_id) %>% summarize(traffic = mean(traffic_volume)) %>% 
  arrange(traffic) %>%
  ggvis(y=~traffic, x=~app_id) %>% layer_paths()

app.9 <- dataset %>% filter(app_id == 58)
app.1718 <- dataset %>% filter(app_id == 1980)

wilcox.test(app.9$traffic_volume, app.1718$traffic_volume, mu = 0, alternative = "two.sided", conf.level = 0.95)

dataset %>% filter(app_id %in% c(2, 753)) %>% ggvis(~traffic_volume) %>% group_by(app_id) %>% 
  scale_numeric("y", trans="sqrt") %>%
  layer_densities(fill=~factor(app_id), stroke =~factor(app_id))

app.9 %>% ggvis(~traffic_volume) %>% layer_densities()
app.1718 %>% ggvis(~traffic_volume) %>% layer_densities()

dataset <- dataset %>% mutate(traffic_volume = log(traffic_volume))

app.1999.traffic <- (dataset %>% filter(app_id == 2))$traffic_volume

t.tests <- dataset %>% 
  filter(app_id != 2) %>% 
  group_by(app_id) %>% 
  filter(n() >= 30) %>%
  summarize(p_value = 
          t.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests %>% filter(p_value > 0.05))


dataset.hours <- dataset %>% mutate(time = trunc(time / 10000) - 2100)

hour.0 <- dataset.hours %>% filter(time == 0)

samples.info.0 <- hour.0 %>% group_by(app_id) %>% summarize(n = n(), traffic = median(traffic_volume))

samples.info.0 %>% arrange(desc(traffic)) %>% ggvis(y=~traffic,x=~app_id) %>% layer_bars()

hour.medians <- dataset %>% group_by(time, app_id) %>% summarize(traffic = median(traffic_volume)) %>% arrange(time)

hour.medians %>% ggvis(y=~traffic, x=~time) %>% group_by(app_id) %>% 
  layer_lines() %>%
  hide_legend("fill")


dataset.hours <- dataset %>% mutate(time = trunc(time / 10000) - 2100)

tt<-scale(dataset.hours %>% select(traffic_volume))

colnames(tt)<-c("traffic_volume_scaled")

dataset.hours <- cbind(dataset.hours, tt)

max.traffic.hours <- dataset.hours %>% group_by(app_id, time) %>% summarize(total_traffic = sum(traffic_volume))

traffic.table.table <- xtabs(total_traffic ~ time + app_id, max.traffic.hours)

dt <- data.frame(traffic.table.table)

samples.hour.0 <- hour.0 %>% group_by(app_id) %>% summarise(total_traffic=sum(traffic_volume))

app.1999.traffic <- (hour.0 %>% filter(app_id == 2))$traffic_volume

t.tests <- hour.0 %>% 
  filter(app_id != 2) %>% 
  group_by(app_id) %>% 
  filter(n() >= 2837) %>%
  summarize(p_value = 
             wilcox.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests %>% filter(p_value > 0.05))

hour.0 %>% filter(app_id %in% c(1528, 250)) %>% ggvis(~traffic_volume) %>% group_by(app_id) %>% 
  #scale_numeric("y", trans="sqrt") %>%
  layer_densities(fill=~factor(app_id), stroke =~factor(app_id))

suspect.group <- (t.tests %>% filter(p_value > 0.05))$app_id

hour.0 %>% filter(app_id %in% suspect.group) %>% ggvis(~traffic_volume) %>% group_by(app_id) %>% 
  scale_numeric("y", trans="sqrt") %>%
  layer_densities(fill=~factor(app_id), stroke =~factor(app_id))

hour.0.1 <- hour.0 %>% filter(!(app_id %in% suspect.group))

samples.hour.0.1 <- hour.0.1 %>% group_by(app_id) %>% summarise(total_traffic=sum(traffic_volume))

app.1999.traffic <- (hour.0 %>% filter(app_id == 1374))$traffic_volume

# For 0 hour after 2837 samples all apps can be distinguished with wilcox

t.tests.1 <- hour.0 %>% 
  filter(app_id != 1374) %>% 
  group_by(app_id) %>% 
  filter(n() >= 30) %>%
  summarize(p_value = 
              wilcox.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests.1 %>% filter(p_value > 0.05))

distinguished.apps <- hour.0 %>% group_by(app_id) %>% summarize(n = n()) %>% filter(n >= 2837)

hour.0 %>% filter(app_id %in% c(15,1882)) %>% ggvis(~traffic_volume) %>% group_by(app_id) %>% 
  scale_numeric("x", trans="sqrt") %>%
  layer_densities(fill=~factor(app_id), stroke =~factor(app_id))

t.test(
  (hour.0 %>% filter(app_id == 15))$traffic_volume,
  (hour.0 %>% filter(app_id == 1882))$traffic_volume,
  mu = 0, alternative = "two.sided", conf.level = 0.95
)

hour.1 <- dataset.hours %>% filter(time == 1)
samples.hour.1 <-  hour.1 %>% group_by(app_id) %>% summarise(total_traffic=sum(traffic_volume))

largest.app.id <- samples.hour.1 %>% arrange(desc(total_traffic)) %>% top_n(1)

t.tests.1 <- hour.1 %>% 
  filter(app_id != largest.app.id$app_id) %>% 
  group_by(app_id) %>% 
  filter(n() >= 4074) %>%
  summarize(p_value = 
              wilcox.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests.1 %>% filter(p_value > 0.05))

# For 1 hour after 4074 samples all apps can be distinguished with wilcox

hour.2 <- dataset.hours %>% filter(time == 2)
samples.hour.2 <-  hour.2 %>% group_by(app_id) %>% summarise(total_traffic=sum(traffic_volume))

largest.app.id <- samples.hour.2 %>% arrange(desc(total_traffic)) %>% top_n(1)

t.tests.2 <- hour.2 %>% 
  filter(app_id != largest.app.id$app_id) %>% 
  group_by(app_id) %>% 
  filter(n() >= 1613) %>%
  summarize(p_value = 
              wilcox.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests.2 %>% filter(p_value > 0.05))

# For 2 hour after 1613 samples all apps can be distinguished with wilcox

hour.3 <- dataset.hours %>% filter(time == 3)
samples.hour.3 <-  hour.3 %>% group_by(app_id) %>% summarise(total_traffic=sum(traffic_volume))

largest.app.id <- samples.hour.3 %>% arrange(desc(total_traffic)) %>% top_n(1)

t.tests.3 <- hour.3 %>% 
  filter(app_id != largest.app.id$app_id) %>% 
  group_by(app_id) %>% 
  filter(n() >= 1979) %>%
  summarize(p_value = 
              wilcox.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests.3 %>% filter(p_value > 0.05))

# For 3 hour after 1979 samples all apps can be distinguished with wilcox

hour.4 <- dataset.hours %>% filter(time == 4)
samples.hour.4 <-  hour.4 %>% group_by(app_id) %>% summarise(total_traffic=sum(traffic_volume))

largest.app.id <- samples.hour.4 %>% arrange(desc(total_traffic)) %>% top_n(1)

t.tests.4 <- hour.3 %>% 
  filter(app_id != largest.app.id$app_id) %>% 
  group_by(app_id) %>% 
  filter(n() >= 1979) %>%
  summarize(p_value = 
              wilcox.test(traffic_volume, app.1999.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)

nrow(t.tests.3 %>% filter(p_value > 0.05))

# For 3 hour after 1979 samples all apps can be distinguished with wilcox

min.sample.size <- data.frame()

for (hour in 0:23){
  print(hour)
  hour.set <- dataset.hours %>% filter(time == hour)
  samples.info <-  hour.set %>% group_by(app_id) %>% summarise(samples=n(),total_traffic=sum(traffic_volume))
  largest.app.id <- samples.info %>% arrange(desc(total_traffic)) %>% top_n(1)
  same.apps <- 1
  min.samples.number <- 30
  max.samples.number <- max(samples.info$samples)
  mid <- 0
  app.traffic <- (hour.set %>% filter(app_id == largest.app.id$app_id))$traffic_volume
  while (min.samples.number < max.samples.number){
    print(paste0("Check value in range [", min.samples.number, ", ", max.samples.number, "]"))
    mid <-  min.samples.number + ceiling((max.samples.number - min.samples.number) / 2)
    print(mid)
    t.tests<- hour.set %>%
      filter(app_id != largest.app.id$app_id) %>%
      group_by(app_id) %>%
      filter(n() >= mid) %>%
      summarize(p_value =
                  wilcox.test(traffic_volume, app.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)
    same.apps <- nrow(t.tests %>% filter(p_value > 0.05))
    if (same.apps > 0){
      min.samples.number <- mid
    }
    else{
      if (max.samples.number == mid){
        min.samples.number <- mid
      }
      max.samples.number <- mid
    }
  }
  min.sample.size <- rbind(min.sample.size, data.frame(c(hour, mid)))
}

# t.tests<- hour.set %>%
#   filter(app_id != largest.app.id$app_id) %>%
#   group_by(app_id) %>%
#   filter(n() >= 7588) %>%
#   summarize(p_value =
#               wilcox.test(traffic_volume, app.traffic, mu = 0, alternative = "two.sided", conf.level = 0.95)$p.value)
# same.apps <- nrow(t.tests %>% filter(p_value > 0.05))





