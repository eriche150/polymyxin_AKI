#Preliminary data subsetting to remove the data where Time is x OR y... fucking morons 
prelimdf<-df
prelimdf<-prelimdf %>% 
        filter(!str_detect(prelimdf$Time,'OR')) #deletes string, 'OR' within the column, 'Time.' 
prelimdf<-prelimdf %>%
        filter(!str_detect(prelimdf$Time,'0R'))
prelimdf$Time<-as.numeric(prelimdf$Time)
colBdf<-prelimdf %>% 
        mutate(ColBConc=replace(ColBConc,ColBConc==0,NA))

#####ColAConc x Time#####
ggplot(data=prelimdf,aes(x=Time,y=ColAConc,color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="lm")+
        scale_y_continuous(breaks=c(0,1,3,5,7,9),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,8))+
        ggtitle("[ColA] over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColA Concentration (units)")+
        theme_bw()
#####ColBConc x Time#####
#However, many of the values for [ColB] are 0 where for COlA, they are reported as 0. 
ggplot(data=colBdf,aes(x=Time,y=ColBConc,color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="loess")+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,5))+
        scale_x_continuous(breaks=c(0,1,3,5,6,7,9,10),limits=c(0,10))+
        ggtitle("[ColB] over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColB Concentration (units)")+
        theme_bw()

#####ColA/IS x Time#####
ggplot(data=prelimdf,aes(x=Time,y=as.numeric(prelimdf$`ColA/IS`),color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="lm")+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,3))+
        scale_x_continuous(breaks=c(0,1,3,5,6,7,9,10),limits=c(0,10))+
        ggtitle("ColA/IS over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColA/IS (units)")+
        theme_bw()

#####ColA/B x Time#####
ggplot(data=prelimdf,aes(x=Time,y=as.numeric(prelimdf$`ColA/ColB`),color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="lm")+
        scale_y_continuous(breaks=c(0,1,3,5,6),limits=c(0,6))+
        scale_x_continuous(breaks=c(0,1,3,5,6,7,9,10),limits=c(0,10))+
        ggtitle("ColA/B over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColA/B")+
        theme_bw()

#####IS Area x Time#####
ggplot(data=prelimdf,aes(x=Time,y=as.numeric(prelimdf$`IS Area`),color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="lm")

ggplot(data=prelimdf,aes(x=Time,y=ColBConc,color=factor(`Stage (worst)`)))+
        geom_point()+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,10))+
        theme_bw()
