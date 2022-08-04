#Sphagetti Plots: Patient_ID x ColA
#Load in necessary packages
library(tidyverse)
library(readxl)


#Import Renal Function Dataset 
AKI_assessment_OVERCOME_07_22 <- read_excel(
        "~/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/colistin/data/AKI assessment OVERCOME 07 22.xlsx"
)
names(AKI_assessment_OVERCOME_07_22)[1]<-"Sample ID" 
dfAKI<- AKI_assessment_OVERCOME_07_22[!is.na(
        AKI_assessment_OVERCOME_07_22$`AKI Y/N`),]
#Windows import AKI_df 
#AKI_assessment_OVERCOME_07_22 <- read_excel("M:/DPET/Rao Lab/Colistin Overcome Trial/AKI Assessment/AKI assessment OVERCOME 07 22.xlsx", 
#col_types = c("text", "numeric", "numeric", 
             # "text", "numeric", "numeric", "text", 
              #"text"))
#Windows import PK_df 
#Copy_of_Colistin_PK_Data_Updated_7_6_22 <- read_excel("M:/DPET/Rao Lab/Colistin Overcome Trial/Raw Data/Copy of Colistin_PK_Data_Updated_7.6.22.xlsx", 
#sheet = "Sheet1", col_types = c("text", 
                                     # +         "text", "numeric", "numeric", "numeric", 
                                    #  +         "numeric", "numeric", "numeric", 
                                     # +         "numeric", "numeric", "numeric", 
                                     # +         "numeric", "numeric", "numeric", 
                                     # +         "numeric"), skip = 2)

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
#rename column headers bc ggplot won't read in symbols 
names(df)
df<-df %>% 
        rename(ColAConc = 'ColA Conc. (mcg/mL)') %>% 
        rename(Time='Time (h)') %>% 
        rename(AKI = 'AKI Y/N') %>% 
        rename(ColBConc = 'ColB Conc. (mcg.mL)')

#make copy of dataframe bc we will need to adjust later 
prelimdf<-df #use different df to avoid downstream effects of data manip
prelimdf<-prelimdf %>% 
        filter(!str_detect(prelimdf$Time,'OR')) #deletes string, 'OR' within the column, 'Time.' 
prelimdf<-prelimdf %>%
        filter(!str_detect(prelimdf$Time,'0R'))
prelimdf$Time<-as.numeric(prelimdf$Time)

#ColAxSampleID
p<-ggplot(data=prelimdf,aes(x=Time,y=ColAConc,color=factor(prelimdf$'Sample ID')))+
        geom_point()+
        geom_line()+
        scale_y_continuous(breaks=c(0,1,3,5,7,9),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,8))+
        ggtitle("[ColA] over Time for ICU Patients")+
        xlab("Time since last dose (hours)")+
        ylab("ColA Concentration (units)")+
        theme_bw()
p+theme(legend.position="none") #removes the legend after the lines are colored after each sample ID 

#ColBxSampleID
L<-ggplot(data=prelimdf,aes(x=Time,y=ColBConc,color=factor(prelimdf$'Sample ID')))+
        geom_point()+
        geom_line()+
        scale_y_continuous(breaks=c(0,1,3,5,7,9),limits=c(0,10))+
        scale_x_continuous(breaks=c(0,1,3,5,6),limits=c(0,8))+
        ggtitle("[ColB] over Time for ICU Patients")+
        xlab("Time since last dose (hours)")+
        ylab("ColB Concentration (units)")+
        theme_bw()
L+theme(legend.position="none")
#ISxSampleID
