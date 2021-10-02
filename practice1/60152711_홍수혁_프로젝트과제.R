install.packages("haven")
library("haven")
library(dplyr)

getwd()
#working directory를 해당 프로젝트가 존재하는 디렉토리로 설정되어있음.
#과제에 사용한 파일은 "/project_file/kowebs_spss/"안에 저장되어있고,
#데이터 병합을 위한 코딩북 파일은 "/project_file/codingBook/" 안에 저장되어있음.

project_file <- read_spss("project_file/kowebs_spss/Koweps_p15_2020_beta1.sav")
View(project_file)
project_file.df <- as.data.frame(project_file)
View(project_file.df)

# 1. 수도권 대학교 소재지 별 출신지 분석 차이.

project_3.df <- project_file.df
project_3.df <- rename(project_3.df,
                       raised_city = np1506_1,
                       metro_code = p1507_3aq6
                       )

list_metro <- read_excel("project_file/codingBook/(2020년 15차 한국복지패널조사) 조사설계서-가구원용(beta1).xlsx",
                         col_names = T, sheet = 2, range = "B2:C22")
project_3.df <- left_join(project_3.df, list_metro, id = "metro_code")


project_3.df$metro_code <- ifelse(project_3.df$metro_code == 99, NA, project_3.df$metro_code)
project_3.df$raised_city <- ifelse(project_3.df$raised_city == 9, NA, project_3.df$raised_city)

table(project_3.df$raised_city)

amount_metro <- project_3.df %>%
  filter(!is.na(metro_code) & metro=="서울특별시"| metro=="경기도") %>%
  group_by(metro, raised_city) %>%
  summarise(n = n())
amount_metro

# 서울을 포함한 경기도 소재 대학교 출신자는 아동기에 주로 대도시와 중소도시에서 성장한 사람들 임을 볼 수 있지만, 아동기를 농어촌 지역에서 성장한 표본이 많이 존재하지 않아 절대적인 판단은 무리가 있다.  




# 2. 평균 이상의 음주량을 가진 사람들의 음주와 생활 형태(평균 음주횟수, 음주시 주량, 평소 우울감을 보이는 빈도와 음주로 인해 타인을 유해 하는 등).
project_2.df <- project_file.df
project_2.df <- rename(project_2.df,
                       avgDrink = p1505_2,
                       howMuch = p1505_3,
                       depression = p1505_11,
                       hurtSomeone = p1505_4aq7
                       )

table(project_2.df$avgDrink)
project_2.df$avgDrink <- ifelse(project_2.df$avgDrink == 1, "less than 1 time a month", 
                                ifelse(project_2.df$avgDrink == 2, "2~4 times a month",
                                       ifelse(project_2.df$avgDrink == 3, "2~3 times a week",
                                              ifelse(project_2.df$avgDrink == 4, "over 4 times a week", NA))))
#-----------#-----------#-----------#-----------#-----------#-----------#-----------
table(project_2.df$howMuch)
project_2.df$howMuch <- ifelse(project_2.df$howMuch == 1, 1,
                               ifelse(project_2.df$howMuch == 2, 4,
                                      ifelse(project_2.df$howMuch == 3, 9,
                                             ifelse(project_2.df$howMuch == 4, 16, 
                                                    ifelse(project_2.df$howMuch == 5, 25, NA)))))

#-----------#-----------#-----------#-----------#-----------#-----------#-----------
table(project_2.df$hurtSomeone)
project_2.df$hurtSomeone <- ifelse(project_2.df$hurtSomeone == 1, "Have Never",
                                   ifelse(project_2.df$hurtSomeone == 2 | project_2.df$hurtSomeone == 3, "have hurt someone", NA))

table(project_2.df$depression)
project_2.df$depression <- ifelse(project_2.df$depression == 1, "very few",
                                  ifelse(project_2.df$depression == 2, "occasionally", 
                                         ifelse(project_2.df$depression == 3, "often",
                                                ifelse(project_2.df$depression == 4, "almost always", NA))))
#-----------#-----------#-----------#-----------#-----------#-----------#-----------

drinkGroup_amount <- project_2.df %>%
  filter(!is.na(howMuch)) %>%
  group_by(avgDrink) %>%
  summarise(mean_amount = mean(howMuch))
drinkGroup_amount

ggplot(data = drinkGroup_amount, aes(x = avgDrink, y = mean_amount)) + geom_col() + 
  scale_x_discrete(limits = c("less than 1 time a month", "2~4 times a month", "2~3 times a week", "over 4 times a week"))
# 음주 횟수가 많을 경우, 술 자리당 마시는 음주량 또한 높게 나타났다.

drinkGroup_hurtSomeone <- project_2.df %>%
  filter((avgDrink == "2~3 times a week" | avgDrink == "over 4 times a week")&!is.na(howMuch) & !is.na(hurtSomeone)) %>%
  group_by(avgDrink, hurtSomeone) %>%
  summarise(mean_amount = mean(howMuch))
drinkGroup_hurtSomeone
# 술로 인해 자신이 다치거나, 다른 사람을 다치게 한 경험이 있는 사람들의 평균 음주량이 조금 더 높게 나타났다.

depression_meanAmount <- project_2.df %>%
  filter(!is.na(depression) & !is.na(hurtSomeone)) %>%
  group_by(depression, hurtSomeone) %>%
  summarise(mean_amount = mean(howMuch))
depression_meanAmount
# 우울감을 느끼는 정도에 따라 음주량이 증가하지만 절대적이진 않고, 음주 후 술로 인해 자신이 다치거나, 다른 사람을 다치게 한 경험이 있을 경우 음주량에 확연한 차이가 있음을 알 수 있다.




# 3. 부모의 교육 수준에 따른 본인의 교육 수준에 상관관계 존재 유무. 
project_1.df <- project_file.df
project_1.df <- rename(project_1.df,
                       educationFather = np1506_39,
                       educationMother = np1506_40,
                       education = p1507_3aq1,
                       )
project_1.df$educationFather <- ifelse(project_1.df$educationFather == 9, NA, project_1.df$educationFather)
project_1.df$educationMother <- ifelse(project_1.df$educationMother == 9, NA, project_1.df$educationMother)
project_1.df$education <- ifelse(project_1.df$education == 9, NA, project_1.df$education)

class(project_1.df$education)
table(is.na(project_1.df$education))

project_1.df <- project_1.df %>%
  mutate(eduFaGroup = ifelse(educationFather < 4, "무학 ~ 초졸",
                       ifelse(educationFather < 6 , "고졸", "고학력")))
project_1.df <- project_1.df %>%
  mutate(eduMoGroup = ifelse(educationMother < 4, "무학 ~ 초졸",
                           ifelse(educationMother < 6 , "고졸", "고학력")))
project_1.df <- project_1.df %>%
  mutate(eduGroup = ifelse(education < 3, "고등졸업 이하", 
                           ifelse(education < 4, "전문대학 재학, 중퇴, 졸업", "대학교 재학, 중퇴, 졸업 또는 그 이상")))
#
project_1.df <- project_1.df %>%
  mutate(eduParents = ifelse(educationFather < 4 & educationMother < 4, "부, 모 무학~초졸",
                             ifelse(educationFather < 6 & educationMother < 6, "부, 모 중~고졸",
                                    ifelse(educationFather >= 6 & educationMother >= 6, "부, 모 고학력", "부모 중 1명 고학력"))))
table(project_1.df$eduParents)

education_level <- project_1.df %>%
  filter(!is.na(eduGroup)& !is.na(eduParents)) %>%
  group_by(eduParents, eduGroup) %>%
  summarise(n=n()) %>%
  mutate(total_group = sum(n)) %>%
  mutate(pct = round(n/total_group * 100, 2))

education_level
ggplot(data = education_level, aes(x= eduParents , y = pct, fill = eduGroup)) + geom_col() + coord_flip()

# 교육수준의 격차가 크게 벌어지지는 않았지만 "부, 모 무학~초졸"또는 "부, 모 중~고졸"인 경우, 고등졸업 이하의 학벌 수준을 갖는 경우의 수가 더 많다.
#또한, "부모 중 1명 고학력"또는 "부, 모 고학력"인 경우에 대학에 진학하는 경우의 수가 더 많음을 알 수 있다.
