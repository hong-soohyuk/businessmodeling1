#1
  #(1)
sales <- data.frame(product = c("apple", "strawberry", "watermelon"), price = c(1800, 1500, 3000), sold = c(24, 38, 13))
sales
  #(2)
mean(sales$price)
mean(sales$sold)

#2
library(ggplot2)
library(dplyr)
raw_data <- ggplot2::mpg
mpg <- as.data.frame(raw_data)
View(mpg)
  #(1)
mpg %>%
  filter(!is.na(cty) & manufacturer == "audi") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty))
mpg %>%
  filter(!is.na(cty) & manufacturer == "toyota") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty))
#toyota의 cty 연비가 더 높게 나타난다.

  #(2)
mpg %>%
  filter(manufacturer == "audi") %>%
  group_by(manufacturer, model) %>%
  arrange(desc(hwy)) %>%
  head(5)

  #(3)
mpg %>%
  mutate(total = cty + hwy) %>%
  mutate(average = total / 2) %>%
  arrange(desc(average)) %>%
  head(3)
  
  #(4)
mpg %>%
  filter(class == "compact") %>%
  group_by(manufacturer) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
  
  #(5)
mpg_hwy <- mpg %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(5)
mpg_hwy
ggplot(data = mpg_hwy, aes(x = reorder(manufacturer, -mean_hwy), y=mean_hwy)) + geom_col()

