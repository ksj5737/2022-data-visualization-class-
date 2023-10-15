# 201818945 김선재

rm(list=ls())

install.packages("ggplot2")
library(ggplot2)

x1 = read.csv("C:/Users/YAP/Downloads/박물관.csv")

View(x1)
unique(x1$시도)

summary(x1)

str(x1)


#필요없는 열 제거, 일평균 관람인원에 따른 내림차순으로 정리 
x1 = x1[,c(-11, -22:-26)]
x1 = x1[c(order(x1$일평균.관람인원, decreasing=T)),]


View(x1)

y2 = x1[c(1:10),] #y2는 일평균.관람인원 상위 10개 
View(x3)

plot(x1$일평균.관람인원,x)
k = lm(1$연관람인원연관람인원~일평균.관람인원, data=x1)
abline(k)
k

x2 = subset(x1, 시도 == "서울특별시")
View(x2)

nrow(x2)

plot(x1$일평균.관람인원~x1$소장유물.개수.개설.이후.)

r = lm(소장유물.개수.개설.이후.~일평균.관람인원, data=x1)
r = lm(일평균.관람인원~소장유물.개수.개설.이후., data=x1)
abline(r)
r

View(x1)

x1$개관년월일 = format(as.Date(x1$개관년월일, format="%Y.%m.%d"),"%Y")
x1$개관년월일 = as.numeric(x1$개관년월일)



View(x1)

str(x1)

summary(x1)

boxplot(x1$연관람인원)

y = ts(x1, frequency = 1, start = c(1935,2019))


x2 <- x1[!is.na(x1$연관람인원),]
x3 <- x1[!is.na(x1$일평균.관람인원),]
x4 <- x1[!is.na(x1$소장유물.개수.개설.이후.),]
x5 <- x1[!is.na(x1$건물면적),]
x6 <- x1[!is.na(x1$문화상품점.면적),]
x7 <- x1[!is.na(x1$인력),]
x8 <- x1[!is.na(x1$프로그램.총계),]
x9 <- x1[!is.na(x1$개관년월일),]
x10 <- x1[!is.na(x1$연개관일수),]
x11 <- x1[!is.na(x1$전시실.면적),]
x12 <- x1[!is.na(x1$관람료),]



summary(x1)


par(mfrow = c(1,3))

boxplot(x2$연관람인원, main = "연관람인원", col = "red")
boxplot(x3$일평균.관람인원, main = "일평균.관람인원", col = "yellow")
boxplot(x4$소장유물.개수.개설.이후. , main = "소장유물,개수,개설.이후", col = "green")

par(mfrow = c(1,3))

boxplot(x5$건물면적, main = "건물면적", col = "orange")
boxplot(x6$문화상품점.면적, main = "문화상품점.면적", col = "blue")
boxplot(x7$인력, main = "인력", col = "red")

boxplot(x8$프로그램.총계, main = "프로그램.총계", col = "yellow")
boxplot(x9$개관년월일, main = "개관년월일", col = "green")
boxplot(x10$연개관일수, main = "연개관일수", col = "orange")

par(mfrow = c(1,2))
boxplot(x11$전시실.면적, main = "전시실.면적", col = "blue")
boxplot(x12$관람료, main = "관람료", col = "purple")




View(x2)

summary(x2)

y
class(y)
plot(y)


gp <- ggplot(data=x1, # 데이터 입력            
             aes(x=소장유물.개수.개설.이후., y=개관년월일))+   
  geom_point(   
    aes(color=국립.사립.공립.대학, shape=국립.사립.공립.대학),   # 그래프  모양 입력  
    alpha=0.5,  # 투명도 입력   
    size=2)

gp+ggtitle("공립사립대립대학")

gp2 <- ggplot(data=x1, # 데이터 입력            
              aes(x=개관년월일, y=소장유물.개수.개설.이후.))+geom_line(
                color = "red", # 투명도 입력   
                size=1)

gp2+ggtitle("소장유물개수")


gp3 <- ggplot(data=x1, # 데이터 입력            
              aes(x=개관년월일, y=일평균.관람인원))+geom_line(
                color = "red", # 투명도 입력   
                size=1)

gp3+ggtitle("일평균.관람인원")

gp4 <- ggplot(data=x1, # 데이터 입력            
              aes(x=개관년월일, y=연관람인원))+geom_line(
                color = "red", # 투명도 입력   
                size=1)

gp4+ggtitle("연관람인원")


library(ggmap)
register_google(key = "AIzaSyDnWqVmEK5VhHZVROUVc_aU3W7S0Vdn650")
gc <- geocode(enc2utf8(y2$도로명주소))  # 지점의 경도위도
gc = as.data.frame(gc)
gc
cen <- as.numeric(unlist(gc))  # 경도위도를 숫자로   
class(cen)
cen= as.vector(cen)
lat = cen[1:10]
lon = cen[11:20]

cen = data.frame(lat,lon)

class(cen)
map<-get_googlemap(center=c(lon=mean(cen$lat),lat=mean(cen$lon)),zoom=7, marker = gc)

ggmap(map)


View(x1)

cor(x1$일평균.관람인원, x1$문화상품점.면적)
plot(x1$문화상품점.면적, x1$일평균.관람인원)
b = lm(일평균.관람인원~문화상품점.면적, data=x1)
b
abline(b)

table(x1$오디오가이드.제공여부, x1$국립.사립.공립.대학)


table(x1$소장유물종류)
unique(x1$소장유물종류)
table(x1$전시실.유물.교체횟수)
unique(x1$전시실.유물.교체횟수)

gmap <- ggmap(map) 
gmap+geom_text(data=df,
               aes(x=lon,y=lat), size=5, 
               label=x1$)

m2 = read.csv("C:/Users/YAP/Downloads/박물관2.csv")
View(m2)
str(m2)
m3 = subset(m2, 국보.유무 == 1)
str(m3)

map2<-get_googlemap(center=c(lon=mean(gc2$lon),lat=36),zoom=7)
ggmap(map2)


gc3 <- geocode(enc2utf8(m3$도로명주소))  # 지점의 경도위도
gc3 = as.data.frame(gc3)
gc3

gc3 <- gc3[!is.na(gc3$lon),]

ggmap(map2) + geom_point(data=gc3, aes(x=lon, y=lat), col = "red")
ggmap(map2) + stat_density_2d(data=gc3, aes(x=lon, y=lat))

