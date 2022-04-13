rm(list=ls())

library(tidyverse)
library(foreign)


simple<-function(x){
  x<-x %>%
    subset(!is.na(PRICE)) %>%
    subset(PRICE>0) %>%
    mutate(SALEVAL = str_to_upper(SALEVAL))%>%
      subset(SALEVAL != "2" & SALEVAL != "4") %>%
       subset(SALEVAL!="T" & SALEVAL!="D" & SALEVAL!="Q" & SALEVAL!="R" & SALEVAL!="V") %>%
       subset(!is.na(SALEVAL)) %>%
    mutate(SALEDT = as.Date(SALEDT,format="%d-%B-%y")) %>%
       select(c("PARID","SALEDT","PRICE","SALETYPE","SALEVAL")) 
    
  
  return(x)
}

sales20<-simple(read.csv("./Data/sales.csv",header=TRUE,sep="|"))
sales19<-simple(read.csv("./Data/sales19.csv",header=TRUE,sep="|"))

A<-rbind(sales20,sales19)


B<-A %>%
    distinct() %>%
      add_count(PARID)

c<-subset(B,B$PRICE<10000)
