# ---------------------------------------------------------------------------

install.packages("tidyverse")
install.packages("gmodels")
install.packages("ggmosaic")
install.packages("corrplot")
install.packages("psych")

library(data.table)
library(tidyverse)
library(gmodels)      # for Cross Table
library(ggmosaic)     # Mosaic plot with ggplot2
library(corrplot)     # Correlation plot
library(psych)

data = fread("bank-full2 .csv")
data
str(data)

table(data$y)
# ----------------------------------------------------------------------------

data = data %>% 
  mutate(y = factor(if_else(y == "yes", "1", "0"), 
                    levels = c("0", "1")))

head(data)
# -- missing value -----------------------------------------------------------

sum(is.na(data))        # 0

sum(data=="unknown")    # 12718  

dim(data)
# unknown 위치 찾기

head(data)
data %>% 
  summarise_all(list(~sum(. =="unknown"))) %>%           
  gather(key = "variable",value = "n_unknown") %>% 
  arrange(-n_unknown)



# -- function setting ---------------------------------------------------------

# default theme for ggplot 
theme_set(theme_bw())      # 그래프 배경색 white 로 설정

# setting default parameters for mosaic plot
mosaic_theme = theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())                 ############

# setting default parameters for crosstables


fun_crosstable = function(df, var1, var2){
  # df: dataframe containing both columns to cross
  # var1, var2: columns to cross together.
  CrossTable(df[, var1], df[, var2],
             prop.r = T,
             prop.c = F,
             prop.t = F,
             prop.chisq = F,
             dnn = c(var1, var2))
}


# -- univariate analysis -----------------------------------------------------------------
# age
summary(data[data$y ==0,]$age)
summary(data[data$y ==1,]$age)


data %>% 
  ggplot(aes(x=age))+geom_bar()+
  geom_vline(xintercept =c(30,60), 
             col ='red', linetype = 'dashed')+
  facet_grid(y~., scales = "free_y")+              ## free_y 를 하면 안되지 않을까? 
  scale_x_continuous(breaks = seq(0,100,5))

# 60세 전까지의 나이대에는 별 다른 특징들이 보이지 않는다.
# 60세 이후의 나이대에는 정기예금에 가입하는 경향이 보인다.

data %>% 
  mutate(elder = if_else(age>60,"1","0")) %>% 
  group_by(y) %>% 
  add_count(nr_y = n()) %>% 
  group_by(elder,y) %>% 
  summarise(abs_freq = n() , relative_freq = round(100*n()/first(nr_y), 2))

data = data %>% 
  mutate(age = if_else(age>60,"high",
                       if_else(age>30,"mid","low")))      # age --> 범주형으로 바꿈

fun_crosstable(data,"age","y")
# 다른 나이대에 비해 60살 이상의 나이대에는 정기예금 비율이 높다.

## job
table(data$job)

fun_crosstable(data,"job","y")        



data %>% 
  ggplot() +
  geom_mosaic(aes(x = product(y, job), fill = y)) +
  mosaic_theme+
  xlab("job")+
  ylab(NULL)

table_job = with(data, table(y,job))
mosaicplot(t(table_job), col=c("dodgerblue","palevioletred1"), main='직업 별 정기예금')


data %>% 
  ggplot()+
  geom_mosaic(aes(x=product(y,education),fill=y))

head(data)

