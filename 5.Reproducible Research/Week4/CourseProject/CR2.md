Health and Economic impacts caused by weather events in USA
===========================================================

Introduction
------------

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

### Loading Data

``` r
storm<-read.csv("repdata_data_StormData.csv",header = T)
```

Q1
--

### Processing Data

``` r
storm.dm<-storm[,c("EVTYPE","FATALITIES","INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

Fatal<-aggregate(FATALITIES~EVTYPE,sum,data=storm.dm)

Inj<-aggregate(INJURIES~EVTYPE,sum,data=storm.dm)

head(Fatal);head(Inj)
```

    ##                  EVTYPE FATALITIES
    ## 1    HIGH SURF ADVISORY          0
    ## 2         COASTAL FLOOD          0
    ## 3           FLASH FLOOD          0
    ## 4             LIGHTNING          0
    ## 5             TSTM WIND          0
    ## 6       TSTM WIND (G45)          0

    ##                  EVTYPE INJURIES
    ## 1    HIGH SURF ADVISORY        0
    ## 2         COASTAL FLOOD        0
    ## 3           FLASH FLOOD        0
    ## 4             LIGHTNING        0
    ## 5             TSTM WIND        0
    ## 6       TSTM WIND (G45)        0

### Extract top15

``` r
Fatal.10<-Fatal[order(-Fatal$FATALITIES),][1:10,]
inj.10<-Inj[order(-Inj$INJURIES),][1:10,]
```

### Plotting Graphs

``` r
require(ggplot2)
```

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.5.2

``` r
ggplot(Fatal.10,aes(x=reorder(EVTYPE,-FATALITIES),y=FATALITIES))+  ###Plot graph by decending prder with reorder based on Y 
    geom_bar(stat = "identity",position = "dodge",fill="red",alpha=0.5)+
    xlab("Event Type")+
    ylab("Fatalities")+
    labs(title = expression("Number of fatalities by Weather Events"))
```

![](CR2_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
ggplot(inj.10,aes(x=reorder(EVTYPE,-INJURIES),y=INJURIES))+
    geom_bar(stat = "identity",position = "dodge",fill="blue",alpha=0.8)+
    xlab("Event Type")+
    ylab("Injuries")+
    labs(title=expression("Total number of injuries by Weather"))
```

![](CR2_files/figure-markdown_github/unnamed-chunk-4-2.png)

Q2
--

### Processing Data

``` r
storm.dm$PROP<-0
storm.dm$CROP<-0

#rescale the data;k=thousands,m=,illion,b=billion

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
```

### Data aggregation

``` r
storm.pro<-aggregate(PROP~EVTYPE,sum,data=storm.dm);storm.pro<-storm.pro[order(-storm.pro$PROP),][1:10,]

storm.crop<-aggregate(CROP~EVTYPE,sum,data=storm.dm);storm.crop<-storm.crop[order(-storm.crop$CROP),][1:10,]

head(storm.pro);head(storm.crop)
```

    ##          EVTYPE     PROP
    ## 833     TORNADO 48.47368
    ## 170       FLOOD 21.27918
    ## 153 FLASH FLOOD 13.73498
    ## 244        HAIL 13.25716
    ## 402   HURRICANE  6.15897
    ## 426   ICE STORM  3.88286

    ##                EVTYPE       CROP
    ## 239              HAIL 0.57712445
    ## 149       FLASH FLOOD 0.17795710
    ## 166             FLOOD 0.16253845
    ## 848         TSTM WIND 0.10875735
    ## 827           TORNADO 0.09954311
    ## 753 THUNDERSTORM WIND 0.06644305

### Plotting Graphs

``` r
ggplot(storm.pro,aes(x=reorder(EVTYPE,-PROP),y=PROP))+
           geom_bar(stat="identity",fill="green",position = "dodge",alpha=0.8)+
    xlab("Event Type")+
    ylab("Property Damages(USD Billions)")+
    labs(title=expression("Total Properties Damages"),"topcenter")
```

![](CR2_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
ggplot(storm.crop,aes(x=reorder(EVTYPE,-CROP),y=CROP))+
    geom_bar(stat="identity",fill="green",position = "dodge",alpha=0.8)+
    xlab("Event Type")+
    ylab("Crop Damages(USD Billions)")+
    labs(title=expression("Crop Damages"),"topcenter")    
```

![](CR2_files/figure-markdown_github/unnamed-chunk-7-2.png)
