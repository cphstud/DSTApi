library(readxl)
library(dkstat)
library(dplyr)
library(tidyr)


# TEENPREGNANCY
# 1) ResearchGoal: Er antallet af ungre mødre faldet?
# 1.1) ResearchGoal: ud fra en graf over moderens alder for førstfødte

# 2) Retrieve data
# 2.1) External source: DST
# a) get all tables
alltabs <- dst_get_tables(lang = "da")
colnames(alltabs)
head(alltabs)
# b) finder relevant tabel
lv=grepl("fød",alltabs$text,ignore.case = T)
foddf=alltabs[lv,]
# c) get metadata for tabellen
fodpmMeta=dst_meta("FODPM")

# lav liste med filter-variabler
# variabler
v=fodpmMeta$variables
fodpmMeta$values$LFNR
nv=v$id
my_query <- list(
  MODERSALDER="*",
  LFNR="1. barn",
  Tid="*"
  )

# hent tabel 
fodpmdfa=dst_get_data(table = "fodpm", query = my_query)


# 3) Data Prep
# 3.0) Data Description
summary(fodpmdf)
head(fodpmdf)
unique(fodpmdf$MODERSALDER)
# 3.1) Data Cleansing - ikke nødv fra DST
# 3.2) Data transformation 
# vi skal have modersalder lavet om til tal
fodpmdf$agecat=as.numeric(gsub(" år","",fodpmdf$MODERSALDER))
boxplot(fodpmdf$agecat)
barplot(table(fodpmdf$agebins))
# lav nu agecat
agelabs=c("preteen","teen","very young", "young", "normal", "older", "old", "very old")
agebins=c(0,13,16,18,25,30,40,50,Inf)
fodpmdf$agebins=cut(fodpmdf$agecat,labels = agelabs, breaks = agebins)
barplot(table(fodpmdf$agebins))

# aggregér values på kategori og tid
fodpmdfKat=aggregate(fodpmdf,value ~ agebins+TID, FUN=mean)

# normaliser
# én
subt=fodpmdfKat[fodpmdfKat$agebins=="teen", ]
subt$value=scale(subt$value)
plot(subt$TID,subt$value, type="b")

# mange
la=as.character(unique(fodpmdf$agebins))
coldf=as.data.frame(matrix(data=NA,nrow = 0, ncol=3))
colnames(coldf)=c("agebins","TID","value")
for (i in (1:length(la))) {
  print(la[i])
  tmpsubt=fodpmdfKat[fodpmdfKat$agebins==la[i], ]
  tmpsubt$value=scale(tmpsubt$value)
  coldf=rbind(coldf,tmpsubt)
}


# 4) Data exploration
# 4.1) Simple graphs
# 4.2) Complex graphs
# ét eksempel: hvordan fordeler de 15 årige sig over tid?
subsetdf=fodpmdf[fodpmdf$MODERSALDER=="15 år",]
# ét eksempel: hvordan fordeler de 15 og 35 årige sig over tid?
subsetdf=fodpmdf[fodpmdf$MODERSALDER=="15 år"|fodpmdf$MODERSALDER=="30 år",]
ggplot(subsetdf,aes(x=TID, y=value))+geom_line()
ggplot(subsetdf,aes(x=TID, y=value, color=MODERSALDER))+geom_line()
ggplot(subsetdf,aes(x=TID, y=value))+geom_line()+facet_wrap(~MODERSALDER, scales = "free")

# barplot med agekategori over tid
ggplot(fodpmdf, aes(x=TID,y=value, fill=agebins))+geom_col(position="dodge")


# linjeplot med agekategori over tid. Først skal der aggregeres!
ggplot(fodpmdfKat, aes(x=TID,y=value, color=agebins))+geom_line()

# og normaliseres
ggplot(coldf, aes(y=value))+geom_line(aes(x=TID,color=agebins))

