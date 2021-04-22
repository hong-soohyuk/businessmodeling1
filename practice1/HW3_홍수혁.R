library(ggplot2)
library(dplyr)

midwest <- as.data.frame(ggplot2::midwest)

#1
midwest <- 
  midwest %>%
  mutate(popnotadult = (poptotal - popadults)/poptotal*100)

#2
midwest %>%
  arrange(desc(popnotadult)) %>%
  select(county, popnotadult) %>%
  head(5)

#3
midwest <- midwest %>% 
  mutate(notadultSize = ifelse(popnotadult >= 40, "Large",
                  ifelse(popnotadult >= 30, "Middle", "Small")))

table(midwest$notadultSize)
qplot(midwest$notadultSize)

#4
midwest %>%
  mutate(poptotalasian = (popasian/poptotal)*100) %>%
  select(state, county, poptotalasian) %>%
  arrange(poptotalasian) %>%
  tail(10) ##it should've been head(10)
