#Load in necessary packages
library(tidyverse)
library(readxl)

#Import PK dataset 
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

#Import AKI dataset 
AKI_assessment_OVERCOME_07_22 <- read_excel(
        "~/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/colistin/data/AKI assessment OVERCOME 07 22.xlsx"
        )
names(AKI_assessment_OVERCOME_07_22)[1]<-"Sample ID" #uniformity in patient identification across the 2 df. 
dfAKI<- AKI_assessment_OVERCOME_07_22[!is.na(
        AKI_assessment_OVERCOME_07_22$`AKI Y/N`),] #omit all values within Y/N that are NA. Without !, then this filters ONLY NA values


#merge both df's into 1 based on sample ID 
df<-merge(dfPK,dfAKI,by='Sample ID')

#rename column headers bc ggplot won't read in symbols 
names(df)
df<-df %>% 
        rename(ColAConc = 'ColA Conc. (mcg/mL)') %>% 
        rename(Time='Time (h)') %>% 
        rename(AKI = 'AKI Y/N') %>% 
        rename(ColBConc = 'ColB Conc. (mcg.mL)')
#renamed columns bc ggplot won't read in symbols 

