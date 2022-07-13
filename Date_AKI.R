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
ggplot(data=subgroupdf,
       aes(x=dDate,y=as.numeric(subgroupdf$`Peak Cr`)))+
        geom_boxplot()+
        xlab("Date of AKI")+
        ylab("Peak Scr levels during admission (mg/dL)")+
        ggtitle("Peak Scr and Date of AKI")+
        scale_x_discrete(labels=c("AKI achieved < 6 days","AKI achieved >= 6 days"))+
        scale_y_continuous(breaks=c(0,1,3,5,7,9))+
        theme_bw()
               

