# dataScience_with_r_assignment
library(tidyverse)
library(rvest)
library(dplyr)
library(stringr)

#a.
html<- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
table<- html_table(html)
table_one <- table[[1]]
newTable <- select(table_one,2:13)
View(newTable)

#b.
url[1] <- "https://www.moneyworks4me.com/indianstocks/large-cap/fmcg/household-personal-products/hindustan-unilever/company-info"
url[2] <- "https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/refineries/reliance-industries/company-info"
url[3] <- "https://www.moneyworks4me.com/indianstocks/large-cap/bfsi/insurance/sbi-life-insuran/company-info"
url[4] <- "https://www.moneyworks4me.com/indianstocks/large-cap/metals-mining/steel-iron-products/tata-steel/company-info"
url[5] <- "https://www.moneyworks4me.com/indianstocks/large-cap/miscellaneous/trading/adani-enterprises/company-info"


table<- url[1] %>% read_html() %>% html_table(header = FALSE)

tabbu <- table[[1]] %>% select(1:11) %>% slice(2,7:14)
tannu <- table[[3]] %>% select(1:11) %>% slice(3:8)

tashu <- rbind(tabbu,tannu)
names(tashu) <- tashu[1,]
dataHindustan <- tashu[-1,]
View(dataHindustan)



table<- url[2] %>% read_html() %>% html_table(header = FALSE)

tabbu <- table[[1]] %>% select(1:11) %>% slice(2,7:14)
tannu <- table[[3]] %>% select(1:11) %>% slice(3:8)

tashu <- rbind(tabbu,tannu)
names(tashu) <- tashu[1,]
dataReliance <- tashu[-1,]
View(dataReliance)

table<- url[3] %>% read_html() %>% html_table(header = FALSE)

tabbu <- table[[1]] %>% select(1:11) %>% slice(2,7:14)
tannu <- table[[3]] %>% select(1:11) %>% slice(3:8)

tashu <- rbind(tabbu,tannu)
names(tashu) <- tashu[1,]
dataSBI <- tashu[-1,]
View(dataSBI)

  table<- url[4] %>% read_html() %>% html_table(header = FALSE)
  
  tabbu <- table[[1]] %>% select(1:11) %>% slice(2,7:14)
  tannu <- table[[3]] %>% select(1:11) %>% slice(3:8)
  
  tashu <- rbind(tabbu,tannu)
  names(tashu) <- tashu[1,]
  dataTATA <- tashu[-1,]
  View(dataTATA)
 
  table<- url[5] %>% read_html() %>% html_table(header = FALSE)
  
  tabbu <- table[[1]] %>% select(1:11) %>% slice(2,7:14)
  tannu <- table[[3]] %>% select(1:11) %>% slice(3:8)
  
  tashu <- rbind(tabbu,tannu)
  names(tashu) <- tashu[1,]
  dataAdani <- tashu[-1,]
  View(dataAdani)
  




#c_1.
tennis <- function(p)
{
  num<- 0
 out <- rbinom(n=5,size=1,prob=p)
 # print(out)
 for(i in 1:5)
 {
    num = num + out[i]
  # print(num)
    
   if(num==3){
     return(i)
   }
 }
 
   return(i)
}
# tennis(0.70)

#c_2.
matches <- c(1:1000)
# print(matches)
for(i in 1:1000)
{
  matches[i] <- tennis(0.70) 
}

ans <- mean(matches)
print(ans)


#d_1.
MontyHall <- function(k)
{
  choo= rbinom(n =k, size = 1, prob = 1/3)
  return(as.integer(!choo))
}
 
#d_2.
sum<-0
open <- MontyHall(1000)
for (i in 1:1000)
{
  sum <- sum+open[i]
}
probabilty <- sum/1000
print(probabilty)

# e.
movie <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
ranking <-movie %>% html_elements(".countdown-index-resposive") %>% html_text()

ranking <- str_remove_all(ranking, "[#]") %>% as.numeric()
name <- movie %>% html_elements(".article_movie_title a") %>% html_text()
score  <- movie %>% html_elements(".countdown-adjusted-score") %>% html_text()
for(i in 1:100){
          score[i] <-  strsplit(score[i]," ")[[1]][3]
      }
years  <- movie %>% html_elements(".start-year") %>% html_text()

 movieData <- data.frame("Ranking" = ranking,"Name of Movie"= name, "Tomato % score"= score,"Year of movie"= years)
 View(movieData)






