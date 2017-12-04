library(lubridate)
library(tidyverse)

df <- read_csv("data/guns.csv")
df

# Load and clean data -----------------------------------------------------


df$Race <- tolower(df$Race)
df$Race[df$Race == 'unclear'] <- NA


# First analysis ----------------------------------------------------------


hist(df$Year)
range(df$Year)
hist(df$Year, breaks = 35)

tallies <- df %>% 
  select(Year, Fatalities, Injured, `Total victims`) %>% 
  group_by(Year) %>% 
  summarise(sum_fatalities = sum(Fatalities),
            sum_injuries = sum(Injured),
            sum_victims = sum(Fatalities, Injured)) %>% 
  gather(victim_type, number, -Year) 

ggplot(tallies, aes(Year, number, colour = victim_type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(tallies$Year),
                                  max(tallies$Year),
                                  5)) +
  ggtitle("US mass shootings per year") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  ylab("Number of victims (fatalities + injuries)")

df %>% mutate(sum_victims = Fatalities + Injured) %>% 
  select(Case, sum_victims, Race) %>% 
  ggplot(aes(Race)) +
  geom_bar() +
  ggtitle("US mass shootings by race") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  ylab("Number of shootings")
  

# Deadliness of shootings -------------------------------------------------

df %>% mutate(sum_victims = Fatalities + Injured) %>% 
  select(Case, Date, sum_victims)

test <- df$Date
Month <- ".*/.*/"
str_count(test, Month)
str_split(test[1], "/")
str_split_fixed(test[1], n=3, "/")

# First parse date field
parse_dates <- function(string, sep = "/") {
  temp_list <- str_split(string, sep)
  return(temp_list)
}

date_array <- data.frame(matrix(ncol = 3, nrow = nrow(df)))
for(i in 1:length(df$Date)){
  temp_list <- parse_dates(df$Date[i])[[1]]
  date_array[i, 1] <- temp_list[1]
  date_array[i, 2] <- temp_list[2]
  date_array[i, 3] <- temp_list[3]
}

# fix_year <- function(x) {
#   if (nchar(x) == 2){
#     x <- str_c("20", x)
#   } 
#   x
# }
# 
# date_array[, 3] <- map_chr(as_vector(date_array[, 3]), fix_year)
Date <- vector()
for (i in 1:nrow(date_array)) {
  Date[i] <- str_c(date_array[i, ], collapse = '-')
}
Date
df$Date2 <- mdy(Date)
# df$Date2 <- as.Date(Date, format = "%m-%d-%y")

df %>% 
  select(Date2, Fatalities) %>% 
  ggplot(aes(Date2, Fatalities, size = Fatalities)) +
  geom_point() +
  ggtitle("Deadliness of shootings by year") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  ylab("Number of fatalities") +
  xlab("Date") 

df %>% 
  mutate(month = month(Date2),
         year = year(Date2)) %>% 
  select(year, month, Date2) %>% 
  group_by(year)  %>% 
  ggplot(aes(year)) +
  geom_bar() +
  ggtitle("US mass shootings by year") +
  theme(plot.title = element_text(lineheight = 0.8, face = 'bold', size = 20)) +
  scale_x_continuous(breaks = seq(min(df$Year),
                                  max(df$Year),
                                  5))

names(df)
table(df$`Prior signs of mental health issues`)

# Looking at mental health ------------------------------------------------


mental_health = logical(length = nrow(df))
for(i in 1:length(df$`Prior signs of mental health issues`)) {
  if (df$`Prior signs of mental health issues`[i] == "Yes"){
    mental_health[i] <- TRUE
  } else {
    mental_health[i] <- FALSE
  }
}
df$mental_health <- mental_health

df %>% 
  select(Date2, Fatalities, mental_health) %>% 
  ggplot(aes(Date2, Fatalities, color = mental_health)) +
  geom_point(size = 3)

df %>% 
  select(Fatalities, mental_health) %>% 
  ggplot(aes(mental_health, Fatalities)) +
  geom_boxplot() +
  ylim(0, 20)

df %>% 
  select(mental_health, Fatalities, Injured) %>% 
  mutate(victims = Fatalities + Injured) %>% 
  ggplot(aes(mental_health, victims)) +
  geom_boxplot() +
  ylim(0, 50)

df %>% 
  select(`Prior signs of mental health issues`, Fatalities, Injured) %>% 
  mutate(victims = Fatalities + Injured) %>% 
  ggplot(aes(`Prior signs of mental health issues`, victims)) +
  geom_boxplot()
