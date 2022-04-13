library(rvest)
library(xml2)

URL<-"https://revenue.stlouisco.com/ias/MapsPropertyInfo.aspx?LocatorNum="


for(i in seq(1,nrow(b))){
  tryCatch({
webpage<-read_html(paste0(URL,b[i,1]))

#Parcel Level Data
tables<-webpage %>%
  html_nodes(xpath='//*[@id="divOwnLegData"]/div[2]') %>%
    html_table(trim=TRUE,na.strings="NA") %>%
      as.data.frame()

ifelse(nrow(tables)==20,tables<-tables[-13,],tables<-tables)

temp<-strsplit(tables[1,1],'\r\n')
  PARID<-trimws(temp[[1]][2])
  TAXYR<-trimws(temp[[1]][6])
  SCHSUB<-trimws(temp[[1]][10])
  MUNYCODE<-trimws(temp[[1]][14])

OWNER_NAME<-tables[3,2]
OWN_ADD<-tables[6,2]
PROP_ADD<-tables[4,2]
  temp<-strsplit(PROP_ADD," ")
PROP_ADRNU<-temp[[1]][1]
  c<-length(temp[[1]])
PROP_ZIP<-temp[[1]][c]
  
  ifelse(OWN_ADD=="Same as the taxing address.",OWN_ADD<-PROP_ADD,OWN_ADD<-OWN_ADD)
  temp<-strsplit(OWN_ADD," ")
  #Extract Zip and City from Owner Address
    c<-length(temp[[1]])
    d<-trimws(gsub('([[:upper:]])', ' \\1', temp[[1]][c-2]))
    e<-strsplit(d," ")[[1]][1]
    d<-trimws(gsub(e,"",d))
OWN_CITY<-gsub(",","",d)
OWN_STATE<-temp[[1]][c-1]
OWN_ZIP<-temp[[1]][c]
SUBDIVISIO<-tables[10,2]
  temp<-strsplit(tables[15,2]," ")
  TAXCODE<-temp[[1]][1]

LOTNUM<-tables[13,2]
LOTDIM<-tables[14,2]
ACRES<-tables[14,4]
LUC<-tables[15,4]
  LEGAL<-strsplit(gsub('\r\n', '-\\1', tables[11,2]), "-")[[1]][1]

#Dwellings Data
  
tables<-webpage %>%
  html_nodes(xpath='//*[@id="divPropResData"]') %>%
  html_table(trim=TRUE,na.strings="NA") %>%
  as.data.frame()

if(length(tables)==0){
  LIVUNIT<-NA
  YEARBLT<-NA
  RESQFT<-NA
} else {
    temp<-strsplit(tables[1,1],'\r\n')
      PARID<-trimws(temp[[1]][2])
        c<-length(temp[[1]])
      LIVUNIT<-trimws(temp[[1]][c])
    
    tables<-tables[-c(1,2),-c(5,6,7,8)]
      temp1<-data.frame(rbind(tables[,1],tables[,2]))
        names(temp1)<-gsub(":","",temp1[1,])
        names(temp1)<-gsub("?","",names(temp1))
        temp1<-temp1[-1,]
      temp2<-data.frame(rbind(tables[,3],tables[,4]))
        temp2[,8]<-NULL
        names(temp2)<-gsub(":","",temp2[1,])
        names(temp2)<-gsub("?","",names(temp2))
        names(temp2)<-gsub(" ","_",names(temp2))
        temp2<-temp2[-1,]
    YEARBLT<-temp2$Year_Built
    RESQFT<-trimws(gsub("ft2","",temp2$Total_Living_Area))
      RESQFT<-gsub(",","",RESQFT)
}

ob<-as.data.frame(cbind(PARID,TAXYR,OWNER_NAME,PROP_ADRNU,PROP_ADD,PROP_ZIP,OWN_ADD,OWN_CITY,OWN_STATE,
                        OWN_ZIP,SCHSUB,MUNYCODE,SUBDIVISIO,TAXCODE,LUC,LIVUNIT,YEARBLT,RESQFT,
                        ACRES,LOTNUM,LOTDIM,LEGAL,deparse.level=1)) 

ifelse(exists("Data"), Data<-rbind(Data,ob), Data<-ob)    
    
    tables<-webpage %>%
      html_nodes(xpath='//*[@id="divSalesData"]') %>%
      html_table(trim=TRUE,na.strings="NA") %>%
      as.data.frame()
    
    names(tables)<-tables[1,]
    sale<-tables[-1,]
      sale<-data.frame(PARID,sale)
    
    ifelse(exists("sale"),
           ifelse(exists("sale.dat"), sale.dat<-rbind(sale.dat,sale), sale.dat<-sale),
           d<-1)
    
#Clean Up Work Space
    rm(PARID,TAXYR,OWNER_NAME,PROP_ADRNU,PROP_ADD,PROP_ZIP,OWN_ADD,OWN_CITY,OWN_STATE,
    OWN_ZIP,SCHSUB,MUNYCODE,SUBDIVISIO,TAXCODE,LUC,LIVUNIT,YEARBLT,RESQFT,
    ACRES,LOTNUM,LOTDIM,LEGAL,temp,temp1,temp3,c,ob,sale,d,e,f)

  }, error=function(e){})
}


No.Data<-subset(b, !(b %in% Data$PARID))




