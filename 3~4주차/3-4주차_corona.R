#-- install.packages -----------------------
{install.packages('bit64')
install.packages("gridExtra")
install.packages("ggmap")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")
install.packages("viridis")
install.packages("gganimate")
install.packages('tidyverse')
install.packages('fredr')
install.packages('cowplot')
install.packages('shiny')
install.packages('manipulate')
install.packages('gapminder')
install.packages('gifski')
install.packages("data.tab")
}


#-- library ---------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(bit64)
library(gridExtra)
library(ggmap)
library(rgeos)
library(maptools)
library(rgdal)
library(raster)
library(viridis)
library(gganimate)
library(tidyverse)
library(lubridate)
library(fredr)
library(cowplot)
library(shiny)
library(manipulate)
library(gapminder)
theme_set(theme_classic())
library(gifski)

rm(list=ls())

case <- fread("Case.csv")
patient_info <- fread("Patientinfo.csv")
Seoul <- fread("SeoulFloating.csv")
TimeAge <- fread("TimeAge.csv")
TimeProvince <- fread("TimeProvince.csv")
SearchTrend <- fread("SearchTrend.csv")
Seoul <- fread("SeoulFloating.csv")
Policy <- fread("Policy.csv")
#------------------------------------------------------------------------------------#
# Special City : Seoul                                                               #
# Metropolitan City : Busan / Daegu / Daejeon / Gwangju / Incheon / Ulsan            #
# Province(-do): Gyeonggi-do / Gangwon-do / Chungcheongbuk-do / Chungcheongnam-do /  #
#                Jeollabuk-do / Jeollanam-do / Gyeongsangbuk-do / Gyeongsangnam-do   #
#------------------------------------------------------------------------------------#

#--연령대별 감염자 확인---------------------------------------------------------------

##데이터 전처리
dim(patient_info)
table(is.na(patient_info$birth_year))  ##결측치 437개
table(patient_info$age)         ##빈 값 : 74개 존재
 
bincan = as.data.frame(table(patient_info$age))[1,1]


summary(patient_info$contact_number)

patient_info[patient_info$age==bincan,] # 빈칸은 대부분 서울

table(patient_info$province) # 서울에 문제가 있는 것 같다

patient_info %>% 
  filter(age=="")

table(patient_info$age)  #data 없는것 제거하기

summary(patient_info[patient_info$state=="isolated",]$contact_number)
sum(patient_info$state=="isolated")

summary(patient_info[patient_info$state=="released",]$contact_number)
sum(patient_info$state=="released")

length(patient_info$contact_number)
sum(is.na(patient_info$contact_number)) # 굳이 써야하나?




#연령대 없는것 제거
patient_info = patient_info[ !(patient_info$age==rownames(table(patient_info$age))[1]),]  



##그래프로 연령대별 감염자 표현

patient_info %>% 
  group_by(age) %>% 
  summarise(total=n())%>% 
  ggplot( aes(x=reorder(age, -total), y= total) ) + geom_col() #감염자 20대가 제일 많음


#--연령대 별 사망자 확인하기--------------------------------------------------------------
patient_info

table(patient_info$deceased_date)  ##잘못된 data ==> data cleansing 

##전처리
patient_info= 
  patient_info[!patient_info$deceased_date==rownames(table(patient_info$deceased_date))[1] ,]

patient_info %>% 
  group_by(age) %>%
  dplyr::select(deceased_date) %>% 
  summarise(total_death = n()) %>% 
  ggplot( aes(x=reorder(age, -total_death), y=total_death))+
  geom_col()+
  ggtitle("연령대 별 사망자 수")+
  labs( x= "age",y =' total_death')               # 70,60,80대의 사망자가 많음


#--결론----------------------------------------------------------------------------------------
#감염자 수는 20대가 제일 많았음에도 불구하고 사망자 수는 노령층이 제일 많음
#따라서, 사망자 비중이 큰 노령층을 중심으로 분석하기로 함.


table( patient_info$age)

##0~20대 제거
patient_info1=patient_info[!(patient_info$age=="0s"|patient_info$age=="10s"|
                               patient_info$age=="20s"),]

table(patient_info1$age)



#-- 지역별 연령대 시각화 --------------------------------------------------------

patient_info %>% 
  filter( state=='deceased') %>% 
  count(province, age) %>% 
  group_by(province) %>% 
  mutate(pct = round(n/ sum(n)*100,2)) %>% 
  ggplot( aes(x= province, y= pct, fill=age) )+ 
  geom_col()+
  coord_flip()    

# 울산은 60대만, 경기도는 30대에서만 사망자가 나왔고
# 경상북도와 대구는 나이대별 사망자가 다양하다.
  
#-- 시간대/나이대별 확진자수 ------------------------------------------------------

TimeAge$date<-as.Date(TimeAge$date)
TimeAge<- TimeAge %>% 
  dplyr::select(!time)              ##time 변수 제거
 
TimeAge %>% 
  dplyr::select(!deceased) %>% 
  ggplot(aes(x=date, y=confirmed,group=age,colour=age))+
  geom_line(size=1.3)+
  geom_point(aes(x=date, y=confirmed,group=age,colour=age), size=1.5)+
  ggtitle("나이별 확진자 시계열 그래프")+labs(x="날짜",y="확진자수")
  
# 20대의 확진자 누적수가 압도적으로 많다.
# 80대의 확진자 누적수는 나이대 중에서 제일 적다.

  
#--지역별 사망자 수----------------------------------------------------------------

#TimeProvince 로 지역별 사망자 수 확인

#전처리

TimeProvince$date<-as.Date(TimeProvince$date)

TimeProvince1<-TimeProvince %>%
  filter(date=='2020-04-13') 
TimeProvince1$id<-seq(0,16,by=1)
head(TimeProvince1)
TimeProvince1$rate<- TimeProvince1$deceased/sum(TimeProvince1$deceased)  


##서울의 사망자수 확인
patient_info %>% 
  dplyr::select(province, state) %>%
  filter(state== 'deceased') %>% 
  group_by(province) %>% 
  summarise(total = n())

##그래프 모션

##사망자 애니메이션
table(patient_info$state)
table(is.na(patient_info$state))                  #결측치없음--> 전처리 안하고 그대로 사용

patient_info %>% 
  dplyr::select(state, confirmed_date) %>% 
  filter(state=='deceased')                 #사망자별 

  
patient_info %>% 
  filter(state=='deceased') %>% 
  group_by(province) %>% 
  summarise(total=n())                      ##state=deceased 인 사람들의 지역별 총인원수 

patientInfo_all$deceased_date <- ymd(patientInfo_all$deceased_date)
str(patientInfo_all$deceased_date)
date_break <- seq(as.Date("2020-01-20"), as.Date("2020-04-23"), by="5 day")
#선그래프
g <- ggplot(patientInfo_all, aes(x=deceased_date, y=all ,group=province)) +
  geom_point(aes(colour=province), size=2)

g + scale_x_date(breaks=date_break) + theme(axis.text.x=element_text(angle=90, hjust=1))

#선그래프, 마우스 설명
install.packages("plotly")
library(plotly)

g <- ggplot(patientInfo_all, aes(x=deceased_date, y=all ,group=1)) +
  geom_point(aes(colour=province), size=2)
p <- g + scale_x_date(breaks=date_break) + theme(axis.text.x=element_text(angle=90, hjust=1))
ggplotly(p)   

##실제로 7명의 사망자가 더 있지만, 그 사람들의 사망날짜가 나와있지 않아
##그래프에는 표시되지 않았다.(부산, 서울지역 빠짐)

# 경상북도에서 사망자가 제일 많이 나온 것을 알 수 있다.
