#This file extracts the sales.txt from the assessors eoy zip files and processes the data
#Jeremy R. Groves
#April 4, 2025

rm(list=ls())

library(tidyverse)
library(foreign)

#Set the year of the EOY file to extract from
  i <- 2024

#Extract and Read
  temp <- read.csv(unz(paste0("./Data/STLCOMO_REAL_ASMTROLL_EOY_",i,".zip"), "sales.txt"), sep = "|", header = TRUE)

#Process Data
  sales <- temp %>%
    filter(!is.na(PRICE),
           PRICE > 0,
           SALEVAL == "4" | SALEVAL == "5" | SALEVAL == "F" | SALEVAL == "I" | SALEVAL == "P" |
             SALEVAL == "T" | SALEVAL == "X" | SALEVAL == "Z") %>%
    mutate(SALEDT = as.Date(SALEDT, "%d-%b-%Y"),
           SALEYR = year(SALEDT)) %>%
    select(PARID, SALEDT, SALEYR, PRICE, SALEVAL, SALETYPE) %>%
    filter(SALEYR > 1979)

#Save as RData file
  save(sales, file = "./Data/Sales.RData")

