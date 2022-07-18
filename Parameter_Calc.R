#Calculation of median, mean for values across AKI Y/N 
#Perform calculations based on original .csv bc upon merging the two dataframes, many values are omitted d/t NA presence 
#Import Renal Function Dataset 
AKI_assessment_OVERCOME_07_22 <- read_excel(
        "~/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/colistin/data/AKI assessment OVERCOME 07 22.xlsx"
)
names(AKI_assessment_OVERCOME_07_22)[1]<-"Sample ID" #uniformity in patient identification across the 2 df. 
dfAKI<- AKI_assessment_OVERCOME_07_22[!is.na(
        AKI_assessment_OVERCOME_07_22$`AKI Y/N`),] #omit all values within Y/N that are NA. Without !, then this filters ONLY NA values
#Extract renal function data for Yes - AKI by creating new dataframe 
AKI<- dfAKI %>% 
        filter(dfAKI$`AKI Y/N`=="Y")
#AKI characteristics; n = 184
#AKI renal function
median(AKI$`Baseline Clcr`) #57.5
mean(AKI$`Baseline Clcr`) #74.03804
median(AKI$`Peak Cr`) #2.255
mean(AKI$`Peak Cr`) #2.615598

#non-AKI data extraction; n = 183 d/t one row having NA values  
nonAKI<-dfAKI %>% 
        filter(dfAKI$'AKI Y/N' == "N")
#nonAKI renal function 
median(nonAKI$`Baseline Clcr`,na.rm=TRUE) #42
mean(nonAKI$`Baseline Clcr`,na.rm=TRUE) #66.28022
median(nonAKI$`Peak Cr`) #NA
mean(nonAKI$`Peak Cr`) #NA
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

#AKI PK data
median(dfPK$`ColA Area`) #1272958
mean(AKI$`ColA Area`) #1401150
median(AKI$`ColB Area`) #286455
mean(AKI$`ColB Area`) #310093
median(AKI$`IS Area`) #3818686
mean(AKI$`IS Area`) #4971739
############
#non-AKI: 
nonAKI<-prelimdf %>% 
        filter(prelimdf$AKI == "N")
#non-AKI characteristics; n = 13 
median(nonAKI$`ColA Area`) #1000404
mean(nonAKI$`ColA Area`) #2230010
median(nonAKI$`ColB Area`) #239103
mean(nonAKI$`ColB Area`) #581389.6
median(nonAKI$`IS Area`) #3818686
mean(nonAKI$`IS Area`) #4971739


