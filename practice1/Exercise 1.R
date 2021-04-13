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





xtotal <- model.matrix(~ 0 + BEDROOMS + REMODEL, data = housing.df)

xtotal$BEDROOMS[1:5]
#데이터 프레임이 아니라 행렬이라서 작동 안함. 
xtotal <-as.data.frame(xtotal)

housing.df <- cbind(housing.df[ ,-c[9, 14], xtotal])

rows.to.missing <- sample(row.names(housing.df), 10)

housing.df[rows.to.missing, ]$BEDROOMS <- NA
