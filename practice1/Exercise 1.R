# pound symbol for comment. install.packages("ggplot2", dependencies = T)
a <- 5
b <- 3
a*b

c <- "hey"
c

d<- c(1,2,3,4,5) # d <- c(1 : 5)
d

e <- c(c,"there")
e <- c(c, 1)
e

f<-seq(1, 10)
f# 12345678910

g <- seq(1, 10, by = 2)
g #1 3 5 7 9
mean(g)
max(g)
min(g)

paste(e, collapse =", ")

install.packages("ggplot2")
library(ggplot2)

h <- c("m", "m", "f", "f", "m", "f", "m")

qplot(h)

qplot(data = mpg, x = drv, y = hwy, geom = "boxplot")

english <- c(90, 80, 85, 70, 60)
math <- c(80, 70, 90, 90, 70)

df_exam <- data.frame(english, math) # csv, df_csv <- read.csv("")
df_exam
mean(df_exam$math)

df_exam2 <- 
  data.frame(english = c(90, 80, 85, 70, 60),
             math = c(80, 70, 90, 90, 70),
             class = c(1,2,2,1,1))
df_exam2

# 엑셀 형태로 공개된 데이터가 많이 있다.
# 일단 메인메모리에 데이터를 사용 가능한 형태로 올려놓는 작업부터 해야한다.
install.packages("readxl")
library(readxl)


df_exam3 <- read_excel("datamodeling.xlsx")
# the file should be in the same project dir, so don't need to write the whole absolute path down.
#df_exam3 <- read_excel("datamodeling.xlsx", colNames) 칼럼의 이름을 지정할 수 있는 기능. sheet - 1 몇번째 sheet? 지정하는 기능.
df_exam3

mean(df_exam3$english)
View(df_exam3)

csv_exam <- read.csv("RforBusiness/practice1/datamodeling.csv")
csv_exam

#head(), tail(), View(), dim(), str(), summary()
#dim stands for dimension, str stands for structure
dim(df_exam3)

mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
head(mpg)
dim(mpg)
str(mpg)
summary(mpg)

mean(mpg$hwy)

install.packages("dplyr")
library(dplyr)

#rename()
df_var <- data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_var

df_new <- rename(df_var, v2 = var2)
df_new

#derived variable, in korean 파생 변수
df_exam3
df_exam3$sum <- df_exam3$english + df_exam3$math

df_exam3$avg <- (df_exam3$english + df_exam3$math)/2
df_exam3$mean <- (df_exam3$english + df_exam3$math)/2

mpg_new <- mpg
View(mpg_new)
mpg_new$total <- (mpg$cty + mpg$hwy)/2
head(mpg_new)

summary(mpg_new$total)

hist(mpg_new$total)

View(mpg_new)
mpg_new$eval <- ifelse(mpg_new$total >= 20, "Pass", "Fail")
mpg_new$grade <- ifelse(mpg_new$total > 30, "A", 
                        ifelse(mpg_new$total > 20, "B", "C"))
  # using multiple ifelse func

#빈도표
table(mpg_new$eval)
qplot(mpg_new$eval)

housing.df <- read.csv("WestRoxbury.csv", header = TRUE)
View(housing.df)

housing.df %>% filter(TAX > 6000)
housing.df %>% select(-TAX) # select * but tax.
housing.df %>% select(-TAX) %>% head() # chain func can be used several times.
df_exam3 %>% arrange(desc(class))

df_exam3 %>% filter(class == 1)
df_exam3 %>% filter(class %in% c(1,2)) #SQL의 Subquery와 비슷하다.

df_exam3 %>% 
  mutate(total = english + math)
df_exam3 %>% 
  mutate(total = english + math, mean = (english+math)/2)

df_exam3 #total 변수가 실제로 생긴것이 아니다. 파생된 변수를 잠시 보고싶은 경우에 사용할 수 있다.

df_exam3 %>%
  mutate(test = ifelse(math>=70, "pass", "fail"))

df_exam3 %>% summarise(english_mean = mean(math)) # aggregate function max, min, avg mean, n

df_exam3 %>% group_by(class) %>%
  summarise(eng_mean = mean(english), sum_english = sum(english), n= n())

#n() is used to get the number of the records
# group_by(평균과 합산의 기준).

### Binding data frame
  #adding the other attributes for the same Identifier
data1 <- data.frame(id = c(1,2,3,4,5), english = c(90,70,60,80,90))
data2 <- data.frame(id = c(1,2,3,4,5), math = c(60,40,80,70,90))
total_data <- left_join(data1, data2, by = 'id')
total_data
# They must have a common column same number of the records for join.
  
  # Binding row, adding more records
data_a <- data.frame(id = c(1,2,3,4,5),english = c(90,70,60,80,90))
data_b <- data.frame(id = c(6,7,8,9,10), english = c(10,80,30,40,100))

total_data2 <- bind_rows(data_a, data_b)
total_data2

View(housing.df)
t(t(names(housing.df))) ## a good tip for checking out the whole attributes
colnames(housing.df)[1]

class(housing.df) #data.frame
class(housing.df$REMODEL) #Character
class(housing.df[ ,13]) # which is fireplace, its type is integer
levels(housing.df$TAX) # why is it Null??
as.factor(housing.df$TAX)

xtotal <- model.matrix(~ 0 + BEDROOMS + REMODEL, data = housing.df) 

xtotal$BEDROOMS[1:5] #xtotal(class : matrix model) doesn't get a proper output

xtotal <-as.data.frame(xtotal)
xtotal
housing.df <- cbind(housing.df[ ,-c[9, 14], xtotal])

rows.to.missing <- sample(row.names(housing.df), 10)

housing.df[rows.to.missing, ]$BEDROOMS <- NA


###/////
#partitioning into training 60%, validation 40%
#rownames(housing.df) prints 5802 row numbers.
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
train.data <- housing.df[train.rows, ]
# going to use 60% of 5802 records as training partition


valid.rows <- setdiff(rownames(housing.df), train.rows)

valid.data <- housing.df[valid.rows, ]
dim(valid.data)

#partitioning into training 50%, validation 30%, test 20%
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1] * 0.3)
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]

###/// linear model
reg <- lm(TOTAL.VALUE ~ ., data = housing.df, subset = train.rows)
reg
tr.res <- data.frame(train.data $TOTAL.VALUE, reg$fitted.values, reg$residuals)
head(tr.res)

pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL.VALUE, pred, residuals = valid.data $TOTAL.VALUE - pred)
head(vl.res)                     

# assess accuracy
install.packages("forecast")
library(forecast)
accuracy(pred, valid.data$TOTAL.VALUE)



# 0415
mean(df$score) #NOT able to do this if ANY NA exists.
mean(df$score, na.rm = T)

df$score <- ifelse(is.na(df$score), 4.25, df$score)

outlier <- data.frame(sex = c(1,2,1,3,2,1), score = c(5,4,3,2,7,2))
# sex value 1 is for male or female, 2 is for male or female. 3 is definitely a outlier.

table(outlier$sex)

outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex) #if it is not in {1,2}.

is.na(outlier)
table(is.na(outlier$sex))

outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier$score


outlier %>% ##NOTE that you don't use dollar sign, using dplyr chain operation.
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(avg_score = mean(score))

mpg <- as.data.frame(ggplot2::mpg)
View(mpg)


boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats # lowest, 1Q, median, 3Q, highest


mpg$hwy <- ifelse(mpg$hwy > 37 | mpg$hwy < 12 , NA, mpg$hwy)
table(is.na(mpg$hwy))

mpg %>%
  group_by(drv) %>%
  summarise(avg_hwy = mean(hwy, na.rm = T))


# Visualization
mpg <- as.data.frame(ggplot2::mpg)
library(ggplot2)

# scatter plot : geom point
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

#Bar chart : geom coll
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(avg_hwy = mean(hwy))
df_mpg
ggplot(data = df_mpg, aes(x = drv, y = avg_hwy)) + geom_col()
ggplot(data = df_mpg, aes(x = reorder(drv, -avg_hwy), y = avg_hwy)) + geom_col() # reorder(x, avg_hwy | -avg_hwy)

ggplot(data = mpg, aes(x = hwy)) + geom_bar() #hwy value is continuous, it gives binned hwy value.

economics <- as.data.frame(ggplot2::economics)
View(economics)

ggplot(data = economics, aes(x = date, y=unemploy)) + geom_line() #시결=time series

ggplot(data = mpg, aes(x=drv, y=hwy)) + geom_boxplot()
# 4륜구동 17마일 22마일 사이에 모여있음, 중위값이 아래쪽에 있는 걸 보니 그 중에서도 낮은 값에 모여있음.
# 전륜구동 중에서도 극단치가 존재하므로, 전륜이니까 무조건 연비가 좋을것이다 라고 판단할 수 없을 것이다.

# 0427
#line chart for the Amtrack data
#read.csv is included in the default R package.

#Amtrak_df <- read.csv("datamodeling.csv").
Amtrak_df <- read.csv("RforBusiness/practice1/Amtrak.csv")
View(Amtrak_df)

ridership.ts <- ts(Amtrak_df$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12) #lab = label, ts stands for time series
ridership.ts

plot(ridership.ts, xlab = "Year", ylab = "Ridership(in 000s)", ylim = c(1300, 2300)) #limit to Y
class(Amtrak_df[,1])

#scatter plot
housing_df <- read.csv("RforBusiness/practice1/BostonHousing.csv")
View(housing_df)
plot(housing_df$MEDV ~ housing_df$LSTAT, xlab = "MEDV", ylab = "LSTAT")

#alternative plot using ggplot

ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

library(ggplot2)
ggplot(housing_df) + geom_point(aes(x = LSTAT, y = MEDV), colour = "navy", alpha = 0.7)

#Bar chart of CHAS vs mean MEDV

# compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(housing_df$MEDV, by = list(housing_df$CHAS), FUN = mean)
data.for.plot

names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV, names.arg = data.for.plot$CHAS, xlab = "CHAS", ylab = "Avg, MEDV") 
#names.arg : names the each bars' names. data.for.plot$CHAS consists of 0,1 so its bars names will be 0 and 1.

ggplot(data.for.plot) + geom_bar(aes(x = CHAS, y = MeanMEDV), stat = "identity")

ggplot(data = data.for.plot, aes(x = CHAS, y = MeanMEDV)) + geom_col()
ggplot(data = data.for.plot, aes(x = CHAS, y = MeanMEDV)) + geom_bar(stat = "identity") #practically they are the same

