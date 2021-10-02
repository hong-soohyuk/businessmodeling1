getwd()
setwd("/Users/i/RforBusiness/practice1")
cereal.df <- read.csv("Cereals.csv")

#(1)
  #Quantitative(Numerical) variable: 수치적 변수, 관측값의 숫자를 세거나 양을 측정할 때 사용한다. 예를 들어, Cereal 데이터에선 calories, protein, fat 등이  수치적 변수에 해당한다.
  # Ordinal variable: 순서가 존재하는 변수를 Ordinal variable이라고 한다. Cereal 데이터의 Shelf는 선반의 높낮이를  비교하는 변수이므로 Ordinal variable이 된다.
  #Nominal variable: Category로 분류된 값들을 본질적인 순서로 정렬할 수 없는 변수들이다. mfr(Manufacturer), name, Type 변수는 대소를 비교해서 크기로 정렬할 수 없는 변수이므로 Nominal(명목형) variable이다.

#(2)
sapply(cereal.df[,-c(1:3, 13)], function(x) 
  c(mean = mean(x,na.rm = TRUE), median = median(x,na.rm = TRUE), min = min(x,na.rm = TRUE), max = max(x,na.rm = TRUE), sd=sd(x,na.rm = TRUE)))

#(3)
ggplot(cereal.df) + geom_histogram(aes(x = sugars), binwidth = 1)
  # (a)변동이 가장 큰 변수는 sugars, sodium, carbo, shelf이다.
  # (b)가장 평향된 변수는 가장 큰 변수는 fiber, fat, potass이다.
  # (c)극단으로 보이는 변수는 vitamin(100), rating(100), potass(300), fiber(15)이다.

#(4)
ggplot(cereal.df) + geom_boxplot(aes(type, calories))
  # 고온용 시리얼의 경우 관측수가 너무 적어서 변수에 따라 칼로리를 예측하기 어렵다. 반면 저온용 시리얼의 어떤 변수를 통해 칼로리를 정확하게 예측할 수 있는지 분석 할 수 있다.

#(5)
cereal.df$shelf <- as.factor(cereal.df$shelf)
View(cereal.df)
class(cereal.df$shelf)
ggplot(cereal.df) + geom_boxplot(aes(shelf, rating))
#진열대 높이에 따라 평점에 차이가 존재함을 볼 수 있다. 2번 shelf에 있는 제품을 다른 진열대로 이동시킬 필요가 있다.

#(6)
library(ggplot2)
install.packages("corrgram")
library(corrgram)
cereal.df$shelf <- as.integer(cereal.df$shelf)
cereal_cor <- subset(cereal.df, select = -c(name, mfr, type))
cor(cereal_cor, use = "complete.obs")
heatmap(cor(cereal_cor, use = "complete.obs") , Rowv = NA, Colv = NA)
corrgram(cor(cereal_cor, use = "complete.obs"))

plot(cereal_cor$potass ~ cereal_cor$fiber, xlab = "potass", ylab = "fiber")
plot(cereal_cor$rating ~ cereal_cor$sugars, xlab = "rating", ylab = "sugars")
plot(cereal_cor$rating ~ cereal_cor$calories, xlab = "rating", ylab = "calories")
  #(a) potassuim과 fiber, rating과 sugars, rating과 calories가 높은 상관관계를 갖는다.
  #(b) 상관관계가 높게 측정된 변수는 둘 중에 하나만 남기고 나머지 하나의 변수는 사용하지 않도록 한다.
  #(c) 변수들 사이의 분산이 정규화가 되면, 평균이나 중앙값에 대한 편차가 달라지기 때문에 변수들의 차이 정도에 변동이 생기게된다.
  
#(7) 주성분분석(PCA, Principle component analysis)의 목적은 데이터의 대다수의 정보를 함축하고 설명할 수 있는 의존 변수들의 조합을 정의하는 것이다. Cereal데이터를 분석하는데 있어서, 13개의 numerical variable을 전부 사용하지 않는 이유는 모든 변수를 사용했을때 상관관계가 짙은 변수들을 중복적으로 모델링에 적용하는 것이 되고, 오히려 결과적으로 분석을 방해할 수 있기 때문이다.
# potasium과 sodium은 mg으로 측정 된 값들이다. 단위가 작기 때문에 관측값들의 수치 차이가 커질 수 밖에 없으며, g으로 측정된 다른 변수들의 분산보다 훨씬 크게 관측된다.