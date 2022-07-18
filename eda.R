#Preliminary data subsetting to remove the data where Time is x OR y... 
prelimdf<-df
prelimdf<-prelimdf %>% 
        filter(!str_detect(prelimdf$Time,'OR')) #deletes string, 'OR' within the column, 'Time.' 
prelimdf<-prelimdf %>%
        filter(!str_detect(prelimdf$Time,'0R'))
prelimdf$Time<-as.numeric(prelimdf$Time)
colBdf<-prelimdf %>% 
        mutate(ColBConc=replace(ColBConc,ColBConc==0,NA)) #Values of 0 for ColB Conc when recorded as NA for ColA Conc
#This replaces values of 0 with NA 

#####ColAConc x Time##### 5.20 x 4.46 
ggplot(data=prelimdf,aes(x=Time,y=ColAConc,color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="loess")+
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
        scale_y_continuous(breaks=c(0,1,3,5,7,9),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6,7,9,10),limits=c(0,8))+
        ggtitle("[ColB] over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColB Concentration (units)")+
        theme_bw()

#####ColA/IS x Time#####
ggplot(data=prelimdf,aes(x=Time,y=as.numeric(prelimdf$`ColA/IS`),color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="loess")+
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
        geom_smooth(method="loess")+
        scale_y_continuous(breaks=c(0,1,3,5,6),limits=c(0,6))+
        scale_x_continuous(breaks=c(0,1,3,4,5,6,7,9,10),limits=c(0,10))+
        ggtitle("ColA/B over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColA/B")+
        theme_bw()

#####IS Area x Time#####
ggplot(data=prelimdf,aes(x=Time,y=as.numeric(prelimdf$`IS Area`),color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="loess")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("IS Area (units)")+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,10))+
        scale_y_continuous()+
        ggtitle("IS Area over Time for ICU Patients with and without AKI")+
        theme_bw()
#####
ggplot(data=prelimdf,aes(x=Time,y=ColBConc,color=factor(`Stage (worst)`)))+
        geom_point()+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,10))+
        theme_bw()

#####Baseline CrCL versus AKI and non-AKI 
ggplot(data=prelimdf, aes(x=prelimdf$AKI,y=prelimdf$`Baseline Clcr`))+
        geom_boxplot()+
        
        theme_bw()

#####
#Look at trends between two groups: + AKI and - AKI 
#Start by subsetting the dataframe into two different df's
AKI<- prelimdf %>% 
        filter(prelimdf$AKI=="Y")
nonAKI<-prelimdf %>% 
        filter(prelimdf$AKI == "N")

#####df subset#####
subgroupdf<-prelimdf[!is.na(prelimdf$`Day AKI`),]
subgroupdf$dDate<-ifelse(subgroupdf$'Day AKI'<= 6, 'A','B') #create new column to separate pt who experienced Day AKI 

#####plot Day AKI on x-axis versus ColA,B, and everything else mentioned, maybe peak Scr as well? 
#Peak Scr
ggplot(data=subgroupdf,
       aes(x=dDate,y=as.numeric(subgroupdf$`Peak Cr`)))+
        geom_boxplot()+
        xlab("Time to reach AKI")+
        ylab("Peak Scr levels during admission (mg/dL)")+
        ggtitle("Peak Scr and Date of AKI")+
        scale_x_discrete(labels=c("AKI achieved < 6 days","AKI achieved >= 6 days"))+
        scale_y_continuous(breaks=c(0,1,3,5,7,9))+
        theme_bw()