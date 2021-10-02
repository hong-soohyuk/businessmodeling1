
#디렉토리 설정, mac환경에선 같은 프로젝트에 있는 데이터 파일을 인식하기위해 아래 디렉토리 설정 코드를 실행하여야합니다.
getwd()
setwd("/Users/i/RforBusiness/practice1")

library(ggplot2)
library(dplyr)
#
# 1
# (1)	Bar Chart (막대차트): 자료의 범주별로 통계적인 빈도에 비례하는 막대를 그린 그래프이다. 히스토그램과 달리 어떤 값이 더 많은지를 알 수 있는 분포도를 표현하기에 적합하지 않다.

# (2)	Line Graph (선 그래프): x축 대비 y축의 관측값들을 선으로 이어 그린 그래프이다, x축이 변화함에 따라서 y축의 변화량을 관측할 수 있다. 보통 타임시리즈에도 활용이 된다.
  
# (3)	Scatter Plot (산점도): 그래프의 x축과 y축 점들로 표현된다. 점들을 관찰해서 군집, 또는 우상향, 우하향과 같은 두 변수의 상관관계를 발견할 수 있다.
  
# (4)	Histogram (Bar Chart와 구분하여 설명 할 것): histogram은 연속적인 값을 분할해 빈도표를 보여준다. bar chart는 범주형 자료를 빈도에 비례하는 그래프를 그리기 때문에 관측값의 대소 비교가 불가한 반면, histogram에선 관측값을 큰 쪽과 작은 쪽으로 분할해 어느쪽에서 더 많이 관측되는지 분포의 편향도를 알 수 있다.  //이것도 막대 차트인데, 히스토 그램은 컨티뉴어스. 범위별로 쪼개서 나타낸다. 20대 30대, 또는 binning해서 쪼ㅅ개서 나타내는 것. 히스토그램과 박스플랏은 분포도를 볼 수 있다는것 중요.
  
# (5)	Boxplot: histogram과 마찬가지로 빈도를 알 수 있는 그래프이다. 연속적인 값을 사분위수로 나눠 보여준다. Box의 밑면까지가 1사분위, 상자의 윗면은 3사분위 값에 해당한다. 상자 안에 그어진 2사분위 선은 median(중위값)을 나타낸다. 박스 위 아래로 뻗어진 줄은 수염이라고 표현되는데, 각각 최솟값과 최대값을 나타낸다. 데이터의 이상치 (outlier)는 수염 밖의 범위에서 점으로 관측값을 표현한다.
#어느구간에 편중되어있는지 효과적으로 볼 수 있다. 어디에 주로 많이 모여있다라는 정보를 1~3사분위 안에 표현할 수 있다.

# 2-1

shipment_csv <- read.csv("ApplianceShipments.csv")
View(shipment_csv)

shipment.ts <- ts(shipment_csv$Shipments, start = c(1985, 1), end = c(1989, 4), freq = 4)
shipment.ts
plot(shipment.ts, xlab = "Year", ylab = "Shipmemts")

# 2-2
plot(shipment.ts, xlab = "Year", ylab = "Shipmemts", ylim = c(3500, 5000))

# 2-3
# 매년 4분기 ~ 1분기마다 출하량이 줄고, 2분기와 3분기에 높은 출하량이 기록되는 패턴이 발견되었다.



# 3-1
mpg.df <- as.data.frame(ggplot2::mpg)
ggplot(mpg.df) + geom_point(aes(x = cty, y = hwy), colour = "navy")

# 3-2
midwest_df <- as.data.frame(ggplot2::midwest)

ggplot(midwest_df) + geom_point(aes(x = poptotal, y = popasian)) + coord_cartesian(xlim = c(0, 500000), ylim = c(0, 10000))


# 3-3
View(mpg.df)

suv_cty <- mpg.df %>%
  filter(class == "suv") %>%
  group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty)) %>%
  head(5)
suv_cty
ggplot(data = suv_cty, aes(x = reorder(manufacturer, -mean_cty), mean_cty)) + geom_col()

# 3-4
ggplot(data = mpg.df, aes(x = class)) + geom_bar()
