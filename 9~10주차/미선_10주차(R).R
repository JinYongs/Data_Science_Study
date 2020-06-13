setwd("C:/rdata")

# 연년 드라마 기사 빈도분석

library(tidyverse) 
library(KoNLP) 
library(reshape) 


# 2015년 드라마 기사 빈도분석

doc <- readLines('드라마15.txt', encoding="UTF-8") 
result <- extractNoun(doc) 
filter1 = function(x){  nchar(x) >= 2  } 
filter2 = function(x){   Filter(filter1, x) } 
result2 = sapply(result, filter2) 
result3 = unlist(result2) 
result4 = table(result3) 
result5 = head(sort(result4, decreasing=TRUE), 1000) 
result5 

# 2016년 드라마 기사 빈도분석

doc <- readLines('드라마16.txt', encoding="UTF-8") 
result <- extractNoun(doc) 
filter6 = function(x){  nchar(x) >= 2  } 
filter7 = function(x){   Filter(filter6, x) } 
result7 = sapply(result, filter7) 
result8 = unlist(result7) 
result9 = table(result8) 
result10 = head(sort(result9, decreasing=TRUE), 1000) 
result10

# 2017년 드라마 기사 빈도분석

doc <- readLines('드라마17.txt', encoding="UTF-8") 
result <- extractNoun(doc) 
filter11 = function(x){  nchar(x) >= 2  } 
filter12 = function(x){   Filter(filter11, x) } 
result12 = sapply(result, filter12) 
result13 = unlist(result12) 
result14 = table(result13) 
result15 = head(sort(result14, decreasing=TRUE), 500) 
result15

# 2018년 드라마 기사 빈도분석

doc <- readLines('드라마18.txt', encoding="UTF-8") 
result <- extractNoun(doc) 
filter16 = function(x){  nchar(x) >= 2  } 
filter17 = function(x){   Filter(filter16, x) } 
result17 = sapply(result, filter17) 
result18 = unlist(result17) 
result19= table(result18) 
result20 = head(sort(result19, decreasing=TRUE), 500) 
result20

# 2019년 드라마 기사 빈도분석

doc <- readLines('드라마19.txt', encoding="UTF-8") 
result <- extractNoun(doc) 
filter21 = function(x){  nchar(x) >= 2  } 
filter22 = function(x){   Filter(filter21, x) } 
result22 = sapply(result, filter22) 
result23 = unlist(result22) 
result24 = table(result23) 
result25 = head(sort(result24, decreasing=TRUE), 500) 
result25

# 2020년 드라마 기사 빈도분석

doc <- readLines('드라마20.txt', encoding="UTF-8") 
result <- extractNoun(doc) 
filter26 = function(x){  nchar(x) >= 2  } 
filter27 = function(x){   Filter(filter26, x) } 
result27 = sapply(result, filter27) 
result28 = unlist(result27) 
result29 = table(result28) 
result30 = head(sort(result29, decreasing=TRUE), 500) 
result30