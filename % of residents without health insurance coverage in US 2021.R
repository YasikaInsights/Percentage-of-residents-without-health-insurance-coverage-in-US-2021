#Problem 1- A) Stock Performance of th four largest US banks,(2022-2023)

getwd()
setwd("C:/Visual Analytics")

library(readr)
library(tidyverse)

stock.data <- read.csv("bank_stocks_2022_2023.csv")

library(lubridate)
stock.data$date <- mdy(stock.data$date)

library(ggplot2)
library(scales)

p1 <- ggplot(stock.data, aes(x=date, y= close_price, color= bank_name)) +
  geom_line(linewidth=0.9) +
  theme_classic() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") + 
  labs(title = "Stock Performance of th four largest US banks,(2022-2023)",
       x = "Date",
       y = "Price",
       color = "Banks") +
  theme(legend.position = "right")

ggsave(filename="bank_stocks_2022_2023.png", p1)



##Problem 1- B)



Average_stock <- stock.data %>% 
  group_by(date) %>%
  
  summarise(Average_stock= mean(close_price))


p1 <- ggplot(stock.data, aes(x=date, y= close_price, color= bank_name)) +
  geom_line(linewidth=0.9) +
  geom_line(data=Average_stock, aes(y= Average_stock, color= "Average")) +
  theme_classic() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") + 
  labs(title = "Stock Performance of th four largest US banks,(2022-2023)",
       x = "Date",
       y = "Price",
       color = "Banks") +
  theme(legend.position = "right")

ggsave(filename="bank_stocks_2022_2023.png", p1)



#Problem - 2 Percentage of residents without health insurance coverage in the U.S.,2021

library(maps)
library(mapproj)

us.states <- map_data("state")
us.states <- rename(us.states, State=region)

uninsured_data <- read.csv("state_health_insurance_coverage_2021.csv")


uninsured_data$State <- tolower(uninsured_data$State)

data <- subset(uninsured_data, State=c("alaska", "hawaii"))

uninsured_data1 <- data[ !(data$State %in% c("alaska", "hawaii")),]

merged.data <- merge(us.states, uninsured_data1 , by="State")

m1<- ggplot(data=merged.data, aes(x=long, y=lat, group=State, fill=Uninsured)) +
  geom_polygon(color="black", linewidth=0.1) +
  theme_void() +
  coord_map(projection= "albers", lat0=39, lat1=45)

m2 <- m1 +
  scale_fill_gradient(low = "skyblue", high = "blue4", name = "Uninsured Rate (%)") +
  labs(title = "Percentage of residents without health insurance coverage in the U.S.,2021",
       fill = "Uninsured rate") +
  theme_void()

ggsave(filename="state_health_insurance_coverage_2021.png", m2)

