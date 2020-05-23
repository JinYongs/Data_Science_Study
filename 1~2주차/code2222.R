#-- 데이터와 비즈니스를 보다 쉽게 이해할 수 있도록 시각화 ----


#-- library ----------------------

install.packages("corrplot")
library(dplyr)       # 전처리
library(data.table)  # 전처리 + 새로운 데이터형식 (효율적인)
library(ggplot2)
library(MASS)
library(lattice)
library(corrplot)
library(corrgram)
library(psych)
library(tidyverse)
library(gganimate)
library(gapminder)
library(gridExtra)
theme_set(theme_classic())
#-- Setting ----------------------

#-- function ---------------------

#-- data -------------------------
dat = fread("train.csv")

dat # data.table 형식은 head, tail이 기본적으로 세팅되어있음

################################################################################
## store_id : 가게 고유ID     ##################################################
## date : 신용카드 결제 날짜  ##################################################
## time : 신용카드 결제 시간  ##################################################
## card_id : 카드 고유 ID     ##################################################
## amount : 매출액, 0보다 작은 음수는 거래 취소(환불)  #########################
## installments : 할부개월수. 일시불은 빈 문자열   #############################
## days_of_week : 날짜 ID, 0~6, 0 월요일 기준   ################################
## holyday : 1이면 공휴일, 0이면 공휴일 아님  ##################################
################################################################################

#--전처리 과정---------------------------------------------------------------------

dat$installments[is.na(dat$installments) ] <-0  # 일시불 인식을 위해 NA->0으로 수정

amount_card<- aggregate(amount~card_id, data=dat, sum ) #카드id 기준으로 매출액의 합

summary(amount_card$amount)  

amount_card %>% 
  dplyr::select(amount) %>% 
  filter(amount<0)            #amount<0 인 데이터 : 6개 ==> 제거 

amount_card[amount_card$amount<0,]

dat = dat[!(dat$card_id %in% c('5c127f94cd','7e74575056','8589f5980e',
                               '8a3dc45eaa','a7416614f6','c43fee0edb')),]


date_time = as.POSIXct(paste(dat$date, dat$time))
dat$year <- year(date_time) #매출날짜에서 연도 추출 / by. data.table
dat$month <- month(date_time)
dat$day <- mday(date_time)
dat$hour <- hour(date_time)
dat$minute <- minute(date_time)


mday(date_time[1])

dat$season <- ifelse(dat$month %in% c(3,4,5),"봄",
                    ifelse(dat$month %in% c(6,7,8),"여름",
                    ifelse(dat$month %in% c(9,10,11),"가을","겨울")
                    )
)

table(dat$season)


#-- 1차원적인 고려 ------------------------------------------------------------------------------

## card_id 가 같다 = 같은 사람이다. (가정)

# example 
dat %>%
  filter(card_id == "d297bba73f") %>%
  group_by(days_of_week) %>%
  summarise(count_day = n()) %>% 
  ggplot(aes(x= reorder(days_of_week,-count_day), y=count_day))+
  geom_col()+
  ggtitle('요일별 d297bba73f 카드의 사용 빈도')+
  labs(x='days_of_Week', y= '빈도')                      # 요일별 d297bba73f 카드의 사용 빈도
                                # 5에 몰려있다 -> 토요일에 자주 사용, 일요일에 무슨 일이 있을것.

#--어느 요일에 거래가 많이 되는지 ----------------------------------------------------------------
x11()
dat %>% 
  dplyr::select(days_of_week) %>% 
  ggplot( aes(x=days_of_week)) + geom_bar()+
  ggtitle("요일에 따른 거래수 비교")#금, 토요일이 많음


#--할부가 많은지 일시불이 많은지 -----------------------------------------------------------------

install_freq<-as.data.frame(  sort( table(dat$installments), decreasing=T)  )
colnames(install_freq)<- c("installments",'freq')
install_freq                                                    #일시불이 압도적으로 많음  
install_freq$pert<- install_freq$freq/sum(install_freq$freq)

x11()
ggplot(install_freq, aes(x="", y=pert, fill=installments ) )+
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y")                                              #파이차트로 구매비율 시각화
#일시불이 압도적으로 많음 --> 주요 분석은 일시불 데이터로


dat1 = dat[dat$installments==0,]
dat2 = dat[dat$installments!=0,] 
#-- 거래 순 많은 가게들 시각화 ---------------------------------------------------------------

dat_store1<- dat1 %>%  
  group_by(store_id) %>% 
  summarise(total= n()) %>% 
  head(10)

dat_store2<- dat2 %>%  
  group_by(store_id) %>% 
  summarise(total= n()) %>% 
  head(10)


store1 = ggplot(dat_store1,aes(x= reorder(store_id, -total) , y=total)) + geom_col() + 
         geom_hline(yintercept=mean(dat_store1$total), 
             linetype='dashed', color='red', size=1)+
         xlab("가게 이름") + ggtitle("일시불")      

store2 = ggplot(dat_store2, aes(x=reorder(store_id, -total) , y=total)) + geom_col() +
         geom_hline(yintercept=mean(dat_store2$total), 
             linetype='dashed', color='red', size=1)+
         xlab("가게 이름") + ggtitle("할부")

x11()
grid.arrange(store1, store2, nrow=2)  # 둘다 0번 가게가 압도적으로 많음 


#-- 매출금액의 분포 확인 ---------------------------------------------------------------------
# 일시불 

dat_amount1 <- dat1 %>%
  group_by(card_id) %>% 
  summarise(total= sum(amount) )

summary(dat_amount1$total)
x11()
boxplot(dat_amount1$total, horizontal = T)   
#주요 매출금액을 파악하기 위해 상위 5개 금액 제외하기로 결정.

head( sort(dat_amount$total,decreasing = T) , 5)  #761667 562793 534700 375918 333884

dat_amount1_1<-dat_amount1 %>% 
  filter(total<333884 )                 #상위 5개 금액 제외함.

x11()
boxplot(dat_amount1_1$total, horizontal = T)   
x11()
plot(density(dat_amount1_1$total))
summary(dat_amount1_1$total)

describe(dat_amount1_1$total)   #왜도 : 46.47 

dat_amount1_2 <-dat_amount1_1[dat_amount1_1$total<=1000, ] #주요 매출값 분포를 위해 1000이하만 보기로 함.

x11()
par(mfrow=c(1,1))
boxplot(dat_amount1_2$total, horizontal = T)
summary(dat_amount1_2$total)

sum(dat_amount1$total<=1000)/length(dat_amount1$total)  # 비율 : 대략 0.89
#비율을 좀 더 높이기 위해 5000 이하만 보기로 함.

sum(dat_amount1$total<=5000)/length(dat_amount1$total)  # 비율 : 대략 0.99
#전체 amount 값에서 상대적으로 작은 금액 단위로 계산한 비율이 높음을 알 수 있다.
#amount<=5000인 데이터로만 다시 분석

dat_amount3<- dat_amount[dat_amount1$total<=5000,]
x11()
hist(dat_amount3$total)
#위의 특징말고는 특별한 특징이 보이지 않음.



#-- 어느 요일에 거래가 많은지 ------------------------------------------------------------------- 

#0번 가게
x11()
dat %>%
  filter(store_id==0) %>% 
  group_by(days_of_week) %>% 
  summarise(total_days=n()) %>% 
  arrange(desc(total_days))       ##가장 거래가 많았던 0번 가게는 일요일, 토요일에 가장 거래 많음.

#전체 가게  

gap <- dat %>%
  group_by(date) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = min_rank(-amount) * 1,
         Value_rel = amount/amount[rank==1],
         Value_lbl = paste0(" ",amount)) %>%
  filter(rank <=10) %>%
  ungroup()

p <- ggplot(gap, aes(rank, group = days_of_week, 
                     fill = as.factor(days_of_week), color = as.factor(days_of_week))) +
  geom_tile(aes(y = amount/2,
                height = amount,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(days_of_week, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=amount,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  transition_states(date, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, 200, fps = 10, duration = 40, width = 700, height = 500, 
        renderer = gifski_renderer("gganim.gif"))


#--[0번 가게]어느 계절에 장사가 잘되는지? --------------------------------------------------
dat_store_season<-dat_store0 %>% 
  group_by(season) %>% 
  summarise(amount_total = n())

dat_store0 %>% 
  group_by(season) %>% 
  summarise(amount_total = n()) %>% 
  ggplot(aes(x=reorder(season,-amount_total), y=amount_total) )+
  geom_col()+
  geom_hline(yintercept=mean(dat_store_season$amount_total), 
             linetype='dashed', color='red', size=1)+
  ggtitle("0번 가게의 계절별 매출상황")                      ##가을에는 매출이 많이 부진


#-- [모든 가게]어느 계절에 장사가 잘되는지? --------------------------------------------------
dat_season<-dat %>% 
  group_by(season) %>% 
  summarise(amount_total = n())

dat %>% 
  group_by(season) %>% 
  summarise(amount_total = n()) %>% 
  ggplot(aes(x=reorder(season,-amount_total), y=amount_total) )+
  geom_col()+
  geom_hline(yintercept=mean(dat_season$amount_total), 
             linetype='dashed', color='red', size=1)+
  ggtitle("전체적인 계절별 매출상황") ##가을에는 매출이 많이 부진


# -- 월별 매출상황 ---------------------------------------------------------------------------

dat %>% 
  group_by(month, card_id) %>% 
  summarise(total_amount = n(amount)) %>% 
  ggplot(aes(x=reorder()))
  

#-- 상관관계 ---------------------------------------------------------------------------------
dat_cor <- dat %>% 
  group_by(month,year,hour,days_of_week, holyday) %>% 
  summarise(sum_am = sum(amount))

head(dat_cor)
str(dat_cor)

cor(dat_cor)
cor_dat = cor(dat_cor)[6,]
#x11()
#corrplot(as.data.frame(cor_dat), method='shade', shade.col=NA, tl.col='black', tl.srt=45)
cor(dat_cor)[6,3] ; cor(dat_cor)[1,2]  
# 시간과 매출합 상관관계 : 0.59
# 시간과 매출합에 영향을 주는 것으로 보인다 
    

#-- 시간에 따른 매출합 -----------------------------------------------------------------------

head(dat,3)

dat_time = dat %>% 
  group_by(hour) %>% 
  summarise(sum_amount = sum(amount))

head(dat_time)

ggplot(dat_time, aes(x = hour, y= sum_amount, group=1))+
  geom_line()+
  ggtitle("시간별 매출 그래프")+ labs(x="시간", y="매출출")

##평일 / 주말 분리하여 시간에 따른 매출합 구하기
dat_weekday = dat[dat$days_of_week<5, ]
dat_weekend = dat[dat$days_of_week>4,]

#평일 시간대 매출 분포

dat_weekday_hour = dat_weekday %>% 
  group_by(hour) %>% 
  summarise(sum_amount = sum(amount))

dat_weekday_hour %>% 
  arrange(desc(sum_amount))

#주말 시간대 매출 분포

dat_weekend_hour = dat_weekend %>% 
  group_by(hour) %>% 
  summarise(sum_amount = sum(amount))

dat_weekend_hour %>% 
  arrange(desc(sum_amount))



x11()
par(mfrow=c(2,1))
plot(dat_weekday_hour$sum_amount ~ dat_weekday_hour$hour, main = "평일 시간별 매출 분포", 
     xlab="평일 시간", ylab ="매출합")
lines(dat_weekday_hour$sum_amount ~ dat_weekday_hour$hour)


plot(dat_weekend_hour$sum_amount~ dat_weekend_hour$hour, main = "주말 시간별 매출 분포",
     xlab = "주말 시간", ylab = "매출합")
lines(dat_weekend_hour$sum_amount~dat_weekend_hour$hour)
# -- 결론 ------------------------------------------------------------------------------------

# 1 . 소액결제 비중이 높음.
# 2.  봄에 매출이 가장 많고, 가을에 매출이 가장 부진.
# 3.  

