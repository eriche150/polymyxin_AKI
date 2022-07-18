#Renal Assessment 
#Import Renal Function Dataset 
AKI_assessment_OVERCOME_07_22 <- read_excel(
        "~/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/colistin/data/AKI assessment OVERCOME 07 22.xlsx"
)
names(AKI_assessment_OVERCOME_07_22)[1]<-"Sample ID" 
dfAKI<- AKI_assessment_OVERCOME_07_22[!is.na(
        AKI_assessment_OVERCOME_07_22$`AKI Y/N`),]
#####
ggplot(data=dfAKI,aes(
        x=dfAKI$`AKI Y/N`,y=dfAKI$`Baseline Clcr`
))+
        geom_boxplot()+
        ggtitle("OVERCOME Study - Baseline CrCL Values")+
        xlab("Acute Kidney Injury (AKI) Status")+
        ylab("Creatinine Clearance (ml/min)")+
        theme_bw()
