#COlA x Time 
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

ggplot(data=prelimdf, aes(x=Time, y=ColAConc, color=factor(AKI)))+
        geom_point()+
        geom_smooth(method="loess")+
        scale_y_log10()+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,8))+
        ggtitle("[ColA] over Time for ICU Patients with and without AKI")+
        labs(color="AKI Incidence")+
        xlab("Time since last dose (hours)")+
        ylab("ColA Concentration (units)")+
        theme_bw()
