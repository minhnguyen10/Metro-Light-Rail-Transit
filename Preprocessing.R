# data input ---------------------
library(data.table)
library(tidyr)
library(dplyr)
library(stringr)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Constants
ALLDAY<-1:99
AMPEAK<-2:4 #0600-0900 
PMPEAK<-11:14 #1500-1830
PEAK<-union(AMPEAK,PMPEAK)
OFFPEAK<-c(1,5:10,15:18) #before 0600, 0900-1500, 1830-2100
LRT<-c("MMTBLUE","MMTGREEN")
LRT<-c(LRT,paste0(LRT,"00"),paste0(LRT,"01")) #codes associated with LRT
surveyname<-c('GREEN','BLUE')

#input
survey<-fread("Survey.csv")
survey$count<-1
GIS<-fread("GIS.csv")

#retrieving each passenger's used path from the survey, determine whether
#their first and last trip is LRT trip.
survey2LRT<-function(str){
    if(str==""|str=="Other"){
        return ("")
    }else{
        str2<-str_sub(str,1,str_locate(str," ")[[1,1]]-1)
        return (str2)
    }
}

chaining<-function(rawpath){
    rawpath<-sapply(rawpath,survey2LRT)
    nonzero<-min(which(sapply(rawpath,str_length)>0)):max(which(sapply(rawpath,str_length)>0))
    chain<-""
    for (r in nonzero){
        chain<-paste0(chain,rawpath[r],", ")
    }
    chain<-str_sub(chain,1,str_length(chain)-2)
    return (chain)
}

unlistedpath<-sapply(survey[,c("TRANSFERS_FROM_THIRD","TRANSFERS_FROM_SECOND","TRANSFERS_FROM_FIRST","ROUTE_SURVEYED","TRANSFERS_TO_FIRST","TRANSFERS_TO_SECOND","TRANSFERS_TO_THIRD")],unlist)
survey[,"path"]<-apply(unlistedpath,1,chaining)

survey$isFirstLRT<-unlist(lapply(str_split(survey$path,","),'[',1)) %in% surveyname
survey$isLastLRT<-unlist(lapply(lapply(str_split(survey$path,","),rev),'[',1)) %in% surveyname


#merged<-#it will be added after finishing the GIS task
merged<-survey #this line is temporary

merged$peak<-"Off_Peak"
merged[which(merged$TIME_PERIOD=="AM Peak"),"peak"]<-"AM Peak"
merged[which(merged$TIME_PERIOD=="PM Peak"),"peak"]<-"PM Peak"

merged$Access<-"Non-Motorized"
merged[which(!(merged$ACCESS_MODE_CODE %in% c(1,2,3,99))),"Access"]<-"Motorized"

merged$Egress<-"Non-Motorized"
merged[which(!(merged$EGRESS_MODE_CODE %in% c(1,2,3,99))),"Egress"]<-"Motorized"

merged$Vehicle<-"Available"
merged[which(merged$CAN_USE_VEH_TRIP_CODE==2|merged$HAS_DRIVE_LICENSE_CODE==2|merged$COUNT_VH_HH_CODE==0),"Vehicle"]<-"Not Available"


merged$pay<-"Free Ride\nor Pass Used"
merged[which(merged$PAYMENT_METHOD_CODE==1),"pay"]<-"Cash"
merged[which(merged$PAYMENT_METHOD_CODE %in% c(2,10,12)),"pay"]<-"Go-to Card\nor Mobile"


merged$trnsf<-"No"
merged[which(merged$TOTAL_TRANSFERS>0),"trnsf"]<-"Yes"

merged$income<-"Over $60K"
merged[which(merged$INCOME_CODE<5),"income"]<-"$15K~$60K"
merged[which(merged$INCOME_CODE==1),"income"]<-"Under $15K"
merged$income<-factor(merged$income,levels=c("Over $60K","$15K~$60K","Under $15K"))

#merged[which(merged$STATUS_EMP<3|merged$STUDENT_ST!=1),"status"]<-"Active"
merged$status<-"Others"
merged[which(merged$STATUS_EMPLOYMENT_CODE==1|merged$STATUS_EMPLOYMENT_CODE==2),"status"]<-"Employed"
merged[which(merged$STUDENT_STATUS_CODE!=1 & merged$STUDENT_STATUS_CODE!=9),"status"]<-"Student"
merged$status<-factor(merged$status,levels=c("Employed","Student","Others"))

merged$disable<-"Disabled"
merged[which(merged$DISABILITY_CODE==2),"disable"]<-"Not Disabled"

merged$ages<-"Over 44"
merged[which(merged$AGE_CODE %in% c("AG13-15","AG16-17","AG18-24")),"ages"]<-"Under 25"
merged[which(merged$AGE_CODE %in% c("AG25-34","AG35-44")),"ages"]<-"Age 25-44"
merged$ages<-factor(merged$ages,levels=c("Over 44","Age 25-44","Under 25"))

merged$eng<-"English"
merged[which(merged$HOME_LANG_OTHER_CODE==1),"eng"]<-"Others"

merged$HH<-"More than 2"
merged[which(merged$COUNT_MEMBER_HH_CODE==1),"HH"]<-1
merged[which(merged$COUNT_MEMBER_HH_CODE==2),"HH"]<-2

merged$subs<-"None"
merged[which(merged$FARE_SUBSIDY_CODE==1),"subs"]<-"Full"
merged[which(merged$FARE_SUBSIDY_CODE==2),"subs"]<-"Partial"       

merged$iswhite<-merged$WHITE

#This is our binary target label
merged$LRT<-"Bus"
merged[str_detect(merged$path,"BLUE|GREEN"),"LRT"]<-"LRT"


#Below code block will be edited after finishing GIS task
#merged$mile<-merged$TRIP_DISTA/1.60934
#merged$Dist_mile<-"Under 10mi"
#merged[which(merged$mile>10),"Dist_mile"]<-"Over 10mi"
#merged$

fwrite(merged,"processed.csv")
