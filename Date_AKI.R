=#Look at trends inside AKI group, and separate between 'Time to reach AKI'
#Import Renal Function Dataset 
AKI_assessment_OVERCOME_07_22 <- read_excel(
        "~/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/colistin/data/AKI assessment OVERCOME 07 22.xlsx"
)
names(AKI_assessment_OVERCOME_07_22)[1]<-"Sample ID" 
dfAKI<- AKI_assessment_OVERCOME_07_22[!is.na(
        AKI_assessment_OVERCOME_07_22$`AKI Y/N`),]
#Import PK Dataset
Copy_of_Colistin_PK_Data_Updated_7_6_22 <- read_excel(
        "~/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/colistin/data/Copy of Colistin_PK_Data_Updated_7.6.22.xlsx", 
        sheet = "Sheet1", skip = 2
)
dfPK<-Copy_of_Colistin_PK_Data_Updated_7_6_22[!is.na(
        Copy_of_Colistin_PK_Data_Updated_7_6_22$`ColA Area`),] #removed all data with NA values for ColA Area 
dfPK$`Sample ID`<-gsub("-1","",as.character(dfPK$`Sample ID`)) #remove ID with -1 at the end of sample ID 
dfPK$`Sample ID`<-gsub("-2","",as.character(dfPK$`Sample ID`))
dfPK$`Sample ID`<-gsub("-3","",as.character(dfPK$`Sample ID`))
dfPK$`Sample ID`<-gsub("-4","",as.character(dfPK$`Sample ID`))
#Common metric to merge DF's based on Sample ID should be acceptable now. 
#merge both df's into 1 based on sample ID 
df<-merge(dfPK,dfAKI,by='Sample ID')
prelimdf<-df #use different df to avoid downstream effects of data manip
prelimdf<-prelimdf %>% 
        filter(!str_detect(prelimdf$Time,'OR')) #deletes string, 'OR' within the column, 'Time.' 
prelimdf<-prelimdf %>%
        filter(!str_detect(prelimdf$Time,'0R'))
prelimdf$Time<-as.numeric(prelimdf$Time)

#####df subset#####
subgroupdf<-prelimdf[!is.na(prelimdf$`Day AKI`),]
subgroupdf$dDate<-ifelse(subgroupdf$'Day AKI'<= 6, 'A','B') #create new column to separate pt who experienced Day AKI 

#####plot Day AKI on x-axis versus ColA,B, and everything else mentioned, maybe peak Scr as well? 
#Peak Scr
ggplot(data=subgroupdf,
       aes(x=dDate,y=as.numeric(subgroupdf$`Peak Cr`)))+
        geom_boxplot()+
        xlab("Date of AKI")+
        ylab("Peak Scr levels during admission (mg/dL)")+
        ggtitle("Peak Scr and Date of AKI")+
        scale_x_discrete(labels=c("AKI achieved < 6 days","AKI achieved >= 6 days"))+
        scale_y_continuous(breaks=c(0,1,3,5,7,9))+
        theme_bw()

#####
               

