

library(dplyr)
library(ggplot2)

hours <- read.csv("hours.csv")

hours <- mutate(hours, Date.of.Session = as.POSIXct(Date.of.Session, format = "%m/%d/%Y"))

recent_date <- max(hours$Date.of.Session)

subtotals <- hours %>% 
  group_by(Last.Name, First.Name, Email.Address, Building) %>% 
  mutate(total_hours = sum(X..of.Hours)) %>% 
  distinct()

total_hours <- sum(subtotals$total_hours)

ggplot(subtotals, aes(total_hours)) +
  geom_histogram() +
  ggtitle("2015-16 Certificated Staff Tech Training Hours Usage") +
  ylab("Count of Staff") +
  xlab("Number of Hours Earned So Far")

ggplot(subtotals, aes(x = total_hours, fill = Building)) +
  # geom_histogram() +
  geom_dotplot(binwidth = .15, stackgroups = TRUE) +
  ggtitle("2015-16 Certificated Staff Tech Training Hours Usage") +
  ylab("Count of Staff") +
  xlab("Number of Hours Earned So Far") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  scale_y_continuous(breaks = NULL)


##### session info #####

sessions <- hours %>% 
  distinct(Date.of.Session, Name.of.Session, Possible.Hours) %>% 
  select(Date.of.Session, Name.of.Session, Possible.Hours) %>% 
  arrange(Date.of.Session)

num_of_sessions <- nrow(sessions)
hours_possible <- sum(sessions$Possible.Hours)


##### 2014-15 #####

hours_14_15 <- read.csv("hours-14-15.csv")

subtotals_14_15 <- hours_14_15 %>% 
  group_by(Last.Name, First.Name, uniquename) %>% 
  summarise(total_hours = sum(X..of.Hours))

# ggplot(subtotals_14_15, aes(total_hours)) +
#   geom_histogram() +
#   ggtitle("2014-15 Certificated Staff Tech Training Hours Usage") +
#   ylab("Count of Staff") +
#   xlab("Number of Hours Earned")

