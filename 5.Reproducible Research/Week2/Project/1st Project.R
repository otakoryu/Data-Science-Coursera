activity<-read.csv("activity.csv",header=TRUE)

#q1
activity.complete<-activity[complete.cases(activity),]
total_steps<-aggregate(steps~date,sum,data=activity.complete)
total_steps[1:10,]

#q2
library(ggplot2)

ggplot(total_steps,aes(x=steps))+
    geom_histogram(fill="red",alpha=0.5,position = "dodge")+
    xlab("Steps in a day")+
    ylab("Frequency")+
    labs(title=expression("Total steps taken each day"))
 
#q3
mean(total_steps[,2]);median(total_steps[,2])

##q4
#Compute 
avg.total.int<-aggregate(steps~interval,mean,data=activity.complete)

ggplot(avg.total.int,aes(x=interval,y=steps))+
    geom_line(stat = "identity",lwd=1.5,col="green",alpha=0.5)

##Find the highest interval 
high<-which(activity.complete==max(activity.complete$steps))
activity.complete[high,]


#value imputing
###Calculate the number of missing values
missingvalues<-activity[!complete.cases(activity),]
nrow(missingvalues)

for(i in 1:ncol(activity)){
        activity[is.na(activity[,i]),i]<-mean(activity[,i],na.rm = TRUE)
}
write.table(activity,file = "activity.imp.txt",quote = F,col.names = T,sep = "|")


##Histogram
activity.imp<-read.table("activity.imp.txt",header = TRUE,sep="|")



total_steps.imp<-aggregate(steps~date,data=activity.imp,sum)

mean(total_steps.imp$steps);median(total_steps.imp$steps)

ggplot(total_steps.imp,aes(x=steps))+
    geom_histogram(fill="red",alpha=0.5,position="dodge")+
    xlab("Steps in a day")+
    ylab("Frequency")+
    labs(title=expression("Total steps taken each day"))

mean(total_steps[,2]);median(total_steps[,2])
mean(total_steps.imp[,2]);median(total_steps.imp[,2])


activity.imp$date<-as.Date(activity.imp$date)
activity.imp$days<-weekdays(activity.imp$date)

weekdays<-subset(activity.imp,!activity.imp$days %in% c("Satturday","Sunday"))
weekend<-subset(activity.imp,activity.imp$days %in% c("Satturday","Sunday"))

weekdays.avg<-aggregate(steps~interval,mean,data=weekdays)
weekend.avg<-aggregate(steps~interval,mean,data=weekend)


weekdays.avg$days<-"Weekday"
weekend.avg$days<-"weekend"

weekday.end<-rbind(weekdays.avg,weekend.avg)

ggplot(weekday.end,aes(x=interval,y=steps,col=days))+
    geom_line(stat="identity",alpha=0.8)+
    facet_wrap(~days,scales = "free",nrow=2,ncol=1)+
    xlab("Interval")+
    ylab("Steps")+
    labs(title=expression("Patterns for steps taken during Weekdays and Weekends"))+
    scale_color_discrete(name="Days",labels=c("Weekday","Weekend"))+
        theme(legend.title = element_text(face = "bold"))





