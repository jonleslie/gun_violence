library(tidyverse)
library(lubridate)

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
  ggtitle("US mass shootings per year",) + 
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
Date <- date()
for (i in 1:nrow(date_array)) {
  Date[i] <- str_c(date_array[i, ], collapse = '-')
}
Date
df$Date2 <- mdy(Date)

df %>% 
  select(Date2, Fatalities) %>% 
  ggplot(aes(Date2, Fatalities, size = Fatalities)) +
  geom_point() +
  ggtitle("Deadliness of shootings by year") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
  ylab("Number of fatalities") +
  xlab("Date") 
names(df)                     
table(df$`Where obtained`)

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
