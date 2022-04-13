#This file pulls out the dwelling data only.
#Jeremy R. Groves
#Created: July 13, 2021

rm(list=ls())

library(tidyverse)
library(foreign)

dwell<-read.csv("./Data/dwelling.csv", sep="|",as.is=TRUE, header=TRUE)

dwell<-dwell %>%
    subset(!is.na(RMTOT)) %>%
      subset(!is.na(RMBED)) %>%
        subset(MGFA>0) %>%
          select(-TAXYYR, -EFFYR, -REMKIT, -REMBATH) %>%
            mutate(attic = case_when(ATTIC == 1 ~ "NONE",
                                     ATTIC == 2 ~ "UNFIN",
                                     ATTIC == 3 ~ "PTFIN",
                                     ATTIC == 4 ~ "FINISH",
                                     ATTIC == 5 ~ "FINWALL",
                                     TRUE ~ "Other")) %>%
            mutate(basement = case_when(BSMT == 1 ~ "NONE",
                                        BSMT == 2 ~ "CRAWL",
                                        BSMT == 3 ~ "PART",
                                        BSMT == 4 ~ "FULL",
                                        TRUE ~ "NONE")) %>%
            mutate(extwall = case_when(EXTWALL == 1 ~ "FRAME",
                                       EXTWALL == 2 ~ "BRICK",
                                       EXTWALL == 3 ~ "MASON_FRAME",
                                       EXTWALL == 4 ~ "BLOCK",
                                       EXTWALL == 5 ~ "STUCCO",
                                       EXTWALL == 6 ~ "SIDING",
                                       EXTWALL == 7 ~ "STONE",
                                       EXTWALL == 8 ~ "ASBESTOS",
                                       EXTWALL == 9 ~ "CONCRETE",
                                       TRUE ~ "OTHER")) %>%
            mutate(fuel = case_when(FUEL == 1 ~ "NONE",
                                    FUEL == 2 ~ "GAS",
                                    FUEL == 3 ~ "ELECTRIC",
                                    FUEL == 4 ~ "OIL",
                                    FUEL == 5 ~ "WOOD",
                                    FUEL == 6 ~ "SOLAR",
                                    TRUE ~ "OTHER")) %>%
            mutate(HVAC = case_when(HEAT == 1 ~ "NONE",
                                    HEAT == 2 ~ "BASIC",
                                    HEAT == 3 ~ "CENTRAL_AIR",
                                    TRUE~"OTHER")) %>%
            mutate(HVACSYS = case_when(HEATSYS == 1 ~ "NONE",
                                       HEATSYS == 2 ~ "WARM_AIR",
                                       HEATSYS == 3 ~ "ELECTRIC",
                                       HEATSYS == 4 ~ "HOTH2O",
                                       HEATSYS == 5 ~ "RADIANT",
                                       TRUE~"OTHER")) %>%
            mutate(style = case_when(STYLE == 1 ~ "sPLIT_FOY",
                                     STYLE == 2 ~ "SPLIT_LEV",
                                     STYLE == 3 ~ "RANCH",
                                     STYLE == 4 ~ "CONTEMP",
                                     STYLE == 5 ~ "OLD_STY",
                                     STYLE == 6 ~ "BUNGALOW",
                                     STYLE == 7 ~ "COLONIAL",
                                     STYLE == 8 ~ "CAPE_COD",
                                     STYLE == 9 ~ "CRAFTSMAN",
                                     STYLE == 10 ~ "RAISED_RANCH",
                                     STYLE == 11 ~ "PUD",
                                     STYLE == 12 ~ "CONVENT",
                                     STYLE == 13 ~ "GARDEN",
                                     STYLE == 14 ~ "VILLIA",
                                     STYLE == 15 ~ "TOWNHOUSE",
                                     STYLE == 16 ~ "MID_RISE",
                                     STYLE == 17 ~ "HIGH_RISE",
                                     TRUE ~ "OTHER")) %>%
              select(-ATTIC, -HEAT, -FUEL, -STYLE, -EXTWALL, -HEATSYS, -BSMT,
                     -CNDBASEVAL)
            
save(dwell, file="./Output/dwell_dat.RData")