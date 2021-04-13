#encoding = utf-8

  # 1번 문제
library(ggplot2)

midwest_df <- as.data.frame(ggplot2::midwest)
View(midwest_df)
dim(midwest_df)

min(midwest_df$poptotal)
max(midwest_df$poptotal)

min(midwest_df$percasian)
max(midwest_df$percasian)

summary(midwest_df)

#미 동북중부 데이터를 담은 Midwest 데이터에는 28개의 column을 가지고 437개의 레코드를 담고있다.
#많은 지역중 가장 인구가 작은 지역의 인구수는 1701, 가장 많은 인구는 5105067이다.
#아시아인이 한 명도 기록되지 않은 지역이 존재하며, 전체 지역중 아시아인이 가장 많은 지역은 5.07%가 기록되었다.
#각 속성별로 최대값, 평균, 최소값 등 summary 함수를 통해서 전부 나타낼 수 있다.

  # 2번 문제
library(dplyr)
midwest_df <- rename(midwest_df, total = poptotal)
midwest_df <- rename(midwest_df, asian = popasian)

  #3번 문제
midwest_df$asianpercent <- (midwest_df$asian / midwest_df$total) * 100
hist(midwest_df$asianpercent)
# 300개 지역이 넘는 많은 지역들이 0.0% ~ 0.5% 사이의 값을 가지고, 1% 이상인 지역은 매우 드물게 나타난다.

  #4번 문제
asianmean <- mean(midwest_df$asianpercent)
asianmean
midwest_df$grade <- ifelse(midwest_df$asianpercent > asianmean, "large", "small")

  #5번 문제
table(midwest_df$grade)
qplot(midwest_df$grade)
# 평균보다 큰 비율의 아시아인이 기록된 지역은 전체 437개 지역중 119개 지역이다.
# 전 지역에 고르게 아시아인이 분포되어 있는것이 아니라,
# Large 등급을 받은 지역들에 특별히 아시아인이 모여서 거주하는 경우가 많음을 알 수 있다.
