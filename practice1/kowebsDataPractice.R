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
                  code_job = h10_eco9,
                  income = p1002_8aq1,
                  code_region = h10_reg7)
welfare <- rename(welfare, code_region = cone_region)
class(welfare$sex)
class(welfare$income)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex ==9, NA, welfare$sex) # unknown/unanswered = 9
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


#correlation between age(group, categorical) and income
welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young",
                      ifelse(age <= 59, "middle", "old")))

welfare$ageg
table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income
qplot(ageg_income)
ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() + 
  scale_x_discrete(limits = c("young", "middle", "old"))


getwd()
setwd("/Users/i/RforBusiness/practice1")


#4 
#correlation between age(group, categorical)/sex and income
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>% # All variable other than variables that are used in aggregate func
  summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill=sex)) + 
  geom_col(position = "dodge") + 
  scale_x_discrete(limits = c("young", "middle","old"))

#5 나이별 성별 월급 차이 (line graph) 
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))

sex_age
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

#6 직업별 월급차이
class(welfare$code_job)

library(readxl)
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)
View(welfare)

welfare <- left_join(welfare, list_job, id = "code_job")
welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(10)

job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
job_income


income_top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
income_top10

ggplot(data = income_top10, aes(x = job, mean_income)) + geom_col() + #not bar but col
  coord_flip()

ggplot(data = income_top10, aes(x = reorder(job, mean_income), y =mean_income)) +geom_col() + coord_flip()

#

income_bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)
income_bottom10

ggplot(data = income_bottom10, aes(x = reorder(job, -mean_income), y =mean_income)) +geom_col() +
  coord_flip() +
  ylim(0, 800)


#성별 직업빈도

job_male <- welfare %>%
  filter(!is.na(job) & sex =="male") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

job_female <- welfare %>%
  filter(!is.na(job) & sex =="female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

ggplot(data = job_male, aes(x = reorder(job, n), y = n)) + geom_col() + coord_flip()

# 0518
# 종교 유무에 따른 이혼율
class(welfare$religion) # 1 : true 2 false 9 unknown/unanswered
table(welfare$religion)

welfare$religion <- ifelse(welfare$religion == 1, "YES", "NO")
table(welfare$religion)

qplot(welfare$religion)

class(welfare$marriage) # divorce : 3
table(welfare$marriage)

welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage", ifelse(welfare$marriage ==3, "divorce", NA))
table(welfare$group_marriage)

table(is.na(welfare$group_marriage))

qplot(welfare$group_marriage)

religion_marriage <-welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(religion, group_marriage) %>%
  summarise(n=n()) %>%
  mutate(total_group = sum(n)) %>%
  mutate(pct = round(n/total_group * 100, 1))
religion_marriage

divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)
divorce

ggplot(data = divorce, aes(x = religion, y=pct)) + geom_col()

# 연령대에 따른 이혼율

ageg_marriage <- welfare %>%
  filter(!is.na(group_marriage)) %>%
  group_by(ageg, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(total_group = sum(n)) %>%
  mutate(pct = round(n/total_group*100, 1))
ageg_marriage

ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marriage == "divorce") %>%
  select(ageg, pct)
ageg_divorce

ggplot(data = ageg_divorce, aes(x=ageg, y = pct)) + geom_col()



# 연령대별, 종교유무에 따른 이혼율
ageg_religion_marriage <- welfare %>%
  filter(!is.na(group_marriage) & ageg != "young") %>%
  group_by(ageg, religion, group_marriage) %>%
  summarise(n = n()) %>%
  mutate(total_group = sum(n)) %>%
  mutate(pct = round(n/total_group * 100, 1))
ageg_religion_marriage

df_divorce <- ageg_religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(ageg, religion, pct)
df_divorce

ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) + geom_col(position="dodge")

# 지역별 연령대 비율(노년층이 많은 지역을 나타내시오.)
class(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region
welfare <- left_join(welfare,list_region, id="code_region")

welfare %>%
  select(code_region, region)

#지역별 연령대 비율
region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n=n()) %>%
  mutate(total_group = sum(n)) %>%
  mutate(pct = round(n/total_group * 100, 2))
region_ageg

ggplot(data = region_ageg, aes(x= region, y = pct, fill = ageg)) + geom_col() + coord_flip()

list_order_old <- region_ageg %>%
  filter(ageg == "old") %>%
  arrange(pct)
list_order_old
order <- list_order_old$region
order

ggplot(data = region_ageg, aes(x=region, y=pct, fill = ageg)) +geom_col() + coord_flip() + 
  scale_x_discrete(limits = order)

class(region_ageg$ageg) # character, 카테코리컬 팩터일때 레벨즈 할 수 있다.

levels(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg, level = c("old", "middle", "young"))

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + geom_col() +
  coord_flip() + 
  scale_x_discrete(limits = order)

#

# pca = principle component analysis

cereal.df <- read.csv("Cereals.csv")
View(cereal.df)
pcs <- prcomp(data.frame(cereal.df$calories, cereal.df$rating))
summary(pcs)
#pc1 과 pc2