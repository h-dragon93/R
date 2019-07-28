x <- c(1,2,3,4,5,6)
matrix(x, nrow = 2, ncol = 3)
matrix(x, nrow = 2, ncol = 3, byrow = TRUE)

y <- c(1,2,3,4,5,6)
array(y, dim = c(2,2))
array(y, dim = c(2,2,3))

list1 <- list(c(1,2,3), "Hello")
list1

name <- c("민철", "지수", "지영")
korean <- c(100, 70, 50)
english <- c(80, 70, 100)
computer <- c(85, 100, 80)
df <- data.frame(name, korean, english, computer)
df

ID <- c(1,2,3,4,5)
SEX <- c("f", "m", "f", "m", "f")
DATA <- data.frame(ID_S = ID, SEX_S = SEX)
DATA

# header, skip(특정행 제외하고 가져오기), nrows(특정행까지), sep(데이터 구분자)
ex1_data <- read.table("C:/Users/HanYong/Desktop/example/data_ex1.txt", header = TRUE, sep = ",")
ex1_data

varname <- c("ID", "SEX", "AGE", "AREA")
ex2_data <- read.table("C:/Users/HanYong/Desktop/example/data_ex2.txt", sep = ",", col.names = varname)
ex2_data

# R 데이터 저자
save(DATA, file = "data_ex.rda")장
# CSV 파일로 저장
write.csv(DATA, file = "C:/Users/HanYong/Desktop/example/data_ex.csv")
# TxT 파일로 저장
write.table(DATA, file = "C:/Users/HanYong/Desktop/example/data_ex.txt")

x <- 1:3
y <- 3:1
(x>0) & (y>1)
(x>0) | (y>1)

# View, str(속성), dim(데이터 프레임), ls(변수 항목 리스트)

library(readxl)
exdata1 <- read_excel("C:/Users/HanYong/Desktop/example/Sample1.xlsx")

library(dplyr)
exdata1 <- rename(exdata1, Y17_AMT = AMT17, Y16_AMT = AMT16)
exdata1

exdata1$AMT <- exdata1$Y17_AMT + exdata1$Y16_AMT
exdata1$CNT <- exdata1$Y17_CNT + exdata1$Y16_CNT

View(exdata1)


exdata1 %>% select(ID)
exdata1 %>% select(ID, AREA, Y17_CNT)

exdata1 %>% filter(AREA == "서울" & Y17_CNT >= 10)

exdata1 %>% arrange(AGE)

exdata1 %>% arrange(desc(Y17_AMT))

exdata1 %>% group_by(AREA) %>% summarise(mean(AGE))
#전체 합
exdata1 %>% summarise(TOT_Y17_AMT = sum(Y17_AMT))
# 그룹합
exdata1 %>% group_by(AREA) %>% summarise(sum(Y17_AMT))

help(bind_rows)

m_history <- exdata1 %>% select(ID, AREA) %>% filter(AREA == c("서울" , "경기"))
m_history
f_history <- exdata1 %>% select(ID, AREA) %>% filter(AREA == c("인천"))
f_history
exdata_bindjoin <- bind_rows(m_history, f_history)
full_join(m_history, f_history, by = 'ID')

### plot 그리는거 주의!!!!
library(descr)
freq_test = freq(exdata1$AREA, plot = F)
freq_test

# n() 빈도, min, max, sum, mean, quantile() 분위수

quantile_test = quantile(exdata1$AGE, plot = F)
quantile_test

stem(exdata1$AGE)

hist(exdata1$AGE)

freq(exdata1$SEX, plot = T, main = '성별(barplot)')

boxplot(exdata1$Y17_CNT, exdata1$Y16_CNT)

count = table(exdata1$SEX)
barplot(count)

count

###########################################
library(reshape2)
name <- c("민철", "지수", "지영")
korean <- c(100, 70, 50)
english <- c(80, 70, 100)
computer <- c(85, 100, 80)
df <- data.frame(name, korean, english, computer)
df

df_melt <- melt(df, id.vars = "name")
df_melt

help(melt)
head(airquality)
names(airquality) <- tolower(names(airquality))

melt_test <- melt(airquality)
head(melt_test)
tail(melt_test)

melt_test2 <- melt(airquality, id.vars = c("month", "wind"), measure.vars = "ozone")
melt_test
head(melt_test2)

# d cast
names(airquality) <- tolower(names(airquality))
head(airquality)
aq_melt <- melt(airquality, id = c("month", "day"), na.rm = TRUE)
head(aq_melt)

aq_dcast <- dcast(aq_melt, month + day ~ variable)
head(aq_dcast)
View(airquality)
# a cast
head(airquality)
aq_melt <- melt(airquality, id = c("month", "day"), na.rm = TRUE)


aq_acast <-acast(aq_melt, day~month~variable)
aq_acast
aq_acast <- acast(aq_melt, month~variable, mean)
aq_acast


install.packages("KoNLP")
install.packages("wordcloud2")

library(KoNLP)
library(wordcloud2)
library(rJava)

useSejongDic()

word_data <- readLines("C:/Users/HanYong/Desktop/example/애국가(가사).txt")
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)

word_data

add_words <-c("백두산", "남산", "철갑", "가을", "하늘", "달")
buildDictionary(user_dic = data.frame(add_words, rep("ncn", length(add_words))), replace_usr_dic = T)

word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)

undata <- unlist(word_data2)

word_table <- table(undata)

undata2 <- Filter(function(x){nchar(x)>=2}, undata)

word_table2 <- table(undata2)

sort(word_table2, decreasing = T)

wordcloud2(word_table2)

wordcloud2(word_table2, color = "random-light", backgroundColor = "black")

wordcloud2(word_table2, fontFamily = "맑은 고딕", size = 1.2, color = "random-light",
           backgroundColor = "black", shape="star")

??melt
??wordcloud2

wordcloud2(demoFreq, size=1.6, color = rep_len(c("red", "blue"), nrow(demoFreq)))
           
