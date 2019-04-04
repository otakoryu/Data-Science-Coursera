storm<-read.csv("repdata_data_StormData.csv",header = T)

####Subsetting data
storm.dm<-storm[,c("EVTYPE","FATALITIES","INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]


####Processing data

##Fatalties
Fatal<-aggregate(FATALITIES~EVTYPE,sum,data=storm.dm)

##Injuries
Inj<-aggregate(INJURIES~EVTYPE,sum,data=storm.dm)


################Q1
##Extract top 8 harmful type to fatal
Fatal.15<-Fatal[order(-Fatal$FATALITIES),][1:15,]
inj.10<-Inj[order(-Inj$INJURIES),][1:15,]

require(ggplot2)
ggplot(Fatal.15,aes(x=reorder(EVTYPE,-FATALITIES),y=FATALITIES))+  ###Plot graph by decending prder with reorder based on Y 
    geom_bar(stat = "identity",position = "dodge",fill="red",alpha=0.5)+
    xlab("Event Type")+
    ylab("Fatalities")+
    labs(title = expression("Number of fatalities by top8 Weather Events"))

##Extract top 8 harmful events to injuries
inj.15<-Inj[order(-Inj$INJURIES),][1:15,]
ggplot(inj.15,aes(x=reorder(EVTYPE,-INJURIES),y=INJURIES))+
    geom_bar(stat = "identity",position = "dodge",fill="blue",alpha=0.8)+
    xlab("Event Type")+
    ylab("Injuries")+
    labs(title=expression("Total number of injuries by Weather"))


################Q2
##Economic damages
#in order to eliminate the wrong inputs in PROPDMGEXP and CROPDMGEXP, it was created additional column with zero and added 
#values only for the letters H=Hundred,K=Thousands,M=Millions and B=Billions

storm.dm$PROP<-0
storm.dm$CROP<-0


storm.dm$PROP<-ifelse(storm.dm$PROPDMGEXP=="H"|storm.dm$PROPDMGEXP=="h",
                         storm.dm$PROPDMG*1e-6,storm.dm$PROP)
storm.dm$CROP<-ifelse(storm.dm$CROPDMGEXP=="H"|storm.dm$CROPDMGEXP=="h",
                         storm.dm$CROPDMG*0.0000001,storm.dm$CROP)

storm.dm$PROP<-ifelse(storm.dm$PROPDMGEXP=="k"|storm.dm$PROPDMGEXP=="k",
                         storm.dm$PROPDMG*1e-6,storm.dm$PROP)
storm.dm$CROP<-ifelse(storm.dm$CROPDMGEXP=="K"|storm.dm$CROPDMGEXP=="k",
                         storm$CROPDMG*1e-6,storm.dm$CROP)

storm.dm$PROP<-ifelse(storm.dm$PROPDMGEXP=="M"|storm.dm$PROPDMGEXP=="m",
                         storm.dm$PROPDMG*1e-3,storm.dm$PROP)
storm.dm$CROP<-ifelse(storm.dm$CROPDMGEXP=="M"|storm.dm$PROPDMGEXP=="m",
                         storm.dm$CROP_US*1e-3,storm.dm$CROP)

storm.dm$PROP<-ifelse(storm.dm$PROPDMGEXP=="B"|storm.dm$PROPDMGEXP=="b",
                         storm.dm$PROP_US*1,storm.dm$PROP)
storm.dm$CROP<-ifelse(storm.dm$CROPDMGEXP=="B"|storm.dm$CROPDMGEXP=="b",
                         storm.dm$CROP_US*1,storm.dm$CROP)

storm.pro<-aggregate(PROP~EVTYPE,sum,data=storm.dm)

storm.crop<-aggregate(CROP~EVTYPE,sum,data=storm.dm)


storm.pro<-storm.pro[order(-storm.pro$PROP),][1:15,]
storm.crop<-storm.crop[order(-storm.crop$CROP),][1:15,]


sum(is.na(storm.pdmg$damage))
ggplot(storm.pro,aes(x=reorder(EVTYPE,-PROP),y=PROP))+
    geom_bar(stat="identity",fill="green",position = "dodge",alpha=0.8)+
    xlab("Event Type")+
    ylab("Economical Damages(USD Billions")+
    labs(title=expression("Total Properties & Crop Damages"),"topcenter")

par(mfrow=c(1,2))
ggplot(storm.crop,aes(x=reorder(EVTYPE,-CROP),y=CROP))+
    geom_bar(stat="identity",fill="green",position = "dodge",alpha=0.8)+
    xlab("Event Type")+
    ylab("Economical Damages(USD Billions")+
    labs(title=expression("Total Properties & Crop Damages"),"topcenter")          
