rm(list=ls())

library(tidyverse)
library(foreign)


#main<-read.csv("./Data/primary_parcel.csv",sep="|")
gis<-read.dbf("./Data/Parcels.dbf")



gis$PARID<-gis$LOCATOR
gis$CLASS<-gis$PROPCLASS
main<-gis

  m<-main %>%
      subset(CLASS=="A" | CLASS=="R" | CLASS=="C") %>%
        mutate(VALCLASS=CLASS)
  m1<-main %>%
      subset(CLASS=="W") %>%
        mutate(VALCLASS="R")
  m2<-m1 %>%
      mutate(VALCLASS="A")
  m<-rbind(m,m1,m2)
  m1<-main %>%
    subset(CLASS=="X") %>%
    mutate(VALCLASS="R")
  m2<-m1 %>%
    mutate(VALCLASS="C")
  m<-rbind(m,m1,m2)
  m1<-main %>%
    subset(CLASS=="Y") %>%
    mutate(VALCLASS="C")
  m2<-m1 %>%
    mutate(VALCLASS="A")
  m<-rbind(m,m1,m2)
  m1<-main %>%
    subset(CLASS=="Z") %>%
    mutate(VALCLASS="R")
  m2<-m1 %>%
    mutate(VALCLASS="A")
  m3<-m1 %>%
    mutate(VALCLASS="C")
  m<-rbind(m,m1,m2,m3)

  main<-m
    rm(m,m1,m2,m3)
  main$m<-1
  

assessor<-read.csv("./Data/assessment.csv",sep="|")
  assessor<-assessor %>%
    subset(VALCLASS!="") 
  
core<-merge(main,assessor,by=c("PARID","CLASS"), all = TRUE)

#Correct mistakes or changes in the VALCLASS
  a<-core %>%
      subset(is.na(m))
    b<-main %>%
        subset(PARID %in% a$PARID) 
  a<-assessor %>%
      subset(PARID %in% b$PARID) %>%
        select(PARID,CLASS,VALCLASS)
  
  b<-merge(a,gis,by="PARID",all.x = TRUE)
  
  c<-b%>%
      subset(CLASS.x != CLASS.y) %>%
          select(PARID,CLASS.x)
  c<-unique(c)
  
  main<-merge(main,c,by="PARID",all.x=TRUE)
    main$CLASS.x<-as.factor(main$CLASS.x)
  main<-main %>%
          mutate(CLASS = case_when(
                is.na(CLASS.x) ~ CLASS,
                !is.na(CLASS.x) ~ CLASS.x
          )) %>%
          select(!CLASS.x)
  
core<-merge(main,assessor,by=c("PARID","CLASS"),all = TRUE)
  core<-core %>%
    select(c(PARID,TAXYR.x,OWNER_NAME,PROP_ADRNU,PROP_ADD,PROP_ZIP,OWN_ADD,OWN_CITY,OWN_STATE,
                  OWN_ZIP,SCHSUB,MUNYCODE,SUBDIVISIO,TAXCODE.x,LUC.x,LIVUNIT,YEARBLT,RESQFT,
                  ACRES,LOTNUM,LOTDIM,LEGAL))
  names(core)<-gsub(".x","",names(core))

#Pull out the objects with missing property information to clean up using web scrape

  b<-as.data.frame(core[is.na(core$PROP_ADRNU),1])
    source("./Build/Code/Scrape.R")
  core<-subset(core, !(core$PARID %in% b[,1]))
  core<-rbind(core,Data)
    rm(Data)

#This Core covers all of the parcels within the county for which data is available. 
    
    dwell<-read.csv("./Data/dwelling.csv", as.is=TRUE, sep="|")
    com.apt<-read.csv("./Data/commercial_apt.csv",sep="|")
    
    
#Dwelling Data
  
  core1<-merge(core,dwell,by="PARID",all=TRUE)

  temp<-core1 %>%  #Looked into theses and they are either missing or commercial with some living space but no specifics
    subset(is.na(core1$CARD)) %>%
      subset(LIVUNIT>0) %>%
        subset(!(PARID %in% com.apt$PARID))
  
  core.units<-core1 %>%  #Just removed those above (opposite selection)
      subset(!is.na(core1$CARD))

#Apartment Data

  core.apt<-merge(core,com.apt,by="PARID",all.y=TRUE)
  
  temp2<-core.apt[is.na(core.apt$OWNER_NAME),]
    #Only about three apartments are on the web not listed here and the data is nowhere near complete.
  core.apt<-core.apt[!is.na(core.apt$OWNER_NAME),]
  
  rm(list=setdiff(ls(), c("core","core.apt","core.units")))
save.image(file="./Data/CoreData.RData")  
  
  
  
  