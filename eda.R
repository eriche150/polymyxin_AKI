#Preliminary data subsetting to remove the data where Time is x OR y... fucking morons 
prelimdf<-df
prelimdf<-prelimdf %>% 
        filter(!str_detect(prelimdf$Time,'OR')) #deletes string, 'OR' within the column, 'Time.' 
prelimdf<-prelimdf %>%
        filter(!str_detect(prelimdf$Time,'0R'))
prelimdf$Time<-as.numeric(prelimdf$Time)
ggplot(data=prelimdf,aes(x=Time,y=ColAConc,color=factor(AKI)))+
        geom_point()+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,10))+
        theme_bw()

ggplot(data=prelimdf,aes(x=Time,y=ColBConc,color=factor(AKI)))+
        geom_point()+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,10))+
        theme_bw()

ggplot(data=prelimdf,aes(x=Time,y=ColBConc,color=factor(`Stage (worst)`)))+
        geom_point()+
        scale_y_continuous(breaks=c(0,1,3,5),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,10))+
        theme_bw()
