# 0511
install.packages("foreign")
library(foreign)

raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = "T") #T -> data frame
welfare <- raw_welfare

View(welfare)
head(welfare)
library(dplyr)
welfare <- rename(welfare,
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  code_jib = h10_eco9,
                  income = p1002_8aq1,
                  cone_region = h10_reg7)

class(welfare$sex)
class(welfare$income)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex ==9, NA, welfare$sex) # unknown/unanswered = 9999
table(welfare$sex)
table(is.na(welfare$sex))

library(ggplot2)
qplot(welfare$sex)

welfare$sex <- ifelse(welfare$sex ==1, "male", "female")
ggplot(data = welfare, aes(x = sex)) + geom_bar()

class(welfare$income)
table(welfare$income) # meaningless!

summary(welfare$income)

qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)

welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income) # %in% logical operator
summary(welfare$income)

table(is.na(welfare$income))
View(welfare)

sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income = mean(income))

sex_income

ggplot(data = sex_income, aes(x=sex, y = mean_income)) + geom_col() #we use geom col, when the data that we uses is avg data.

#correlation between age and income
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) # unknown/unanswered = 9999
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
# we can use xlim for the readability

age_income <- welfare %>%
  filter(!is.na(income)) %>% # even though we know that there is no NA value in this data, MAKE SURE not available values are filtered so we can use is code for THE OTHER DATA.
  group_by(age) %>%
  summarise(mean_income = mean(income))

age_income
ggplot(data = age_income, aes(x=age, y=mean_income)) + geom_line()

