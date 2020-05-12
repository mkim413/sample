################################################################
#Make sure connect to the Cisco AnyConnect before run the codes.
################################################################
exppid = data.frame(pid = c())
##########library##########
#If this is the first time to run the codes, please install the packages first (remove # below (line 7) and run. 
#install.packages(c("REDCapR","redcapAPI","dplyr"))
library(REDCapR)
library(redcapAPI)
library(dplyr)

###############Export Data from the redcap###############
#navigate to the project (url and token setting)
redcap_url = "https://redcap.stanford.edu/api/"
token1 = ""
#bring data from redcap
data=redcap_read_oneshot(redcap_uri = redcap_url, token = token1)$data
#set today's date
date = Sys.Date()
date=format(date, "%Y%m%d")
#data clean (pid)
df1 = subset(data, pid %in% 10000:99999)
########################################################

#1. Prescreen
#Set directory
if(.Platform$OS.type=="unix")
file_dir <- "" 
#set file's name
ps_filename = paste0("online_prescreen","_", date,".csv", sep="")
#save the file to the server
write.csv(df1, file = ps_filename)
#1-1 Check data existance
AllDatadf <- data.frame()
setwd(file_dir)
ps_list= list.files(pattern = date)
if(file.exists(ps_list)){print("The file is in the right place.")}else {"."}
#1-2 The latest date
filenames=list.files()
timeframe=file.info(filenames)$ctime
timeframe = as.POSIXct(timeframe,"America/San_Francisco")
timeframe = as.data.frame(timeframe)
ld= max(timeframe$timeframe)
ld2= format(ld, "%Y%m%d")
today=format(Sys.Date(),"%Y%m%d")
today2=format(Sys.Date(),"%Y-%m-%d")
ps_list2= list.files(pattern = today)
if(file.exists(ps_list2)){print(paste0("The newest file is saved in ",today))}else {print(paste0("The file is not saved ", today2, "."))}
#1-3 file name check
ps_name=paste0("online_prescreen","_",today)
ps_list3 = list.files(pattern = ps_name)
if(file.exists(ps_list3)){print("The file name is correct.")} else {print("The file name is incorrect. Please check.")}
#1-4 Missing any expected PID
exppid$pid %in% data$pid
exppid1=as.data.frame(exppid$pid %in% data$pid)
exppid1$pid = exppid
exppid1$TF= exppid1$`exppid$pid %in% data$pid`
if (all(exppid1$TF == "TRUE")) {print("There is no missing PID.")} else {print(paste("PID",exppid$pid[exppid1=="FALSE"], "is missing."))}
#1-5 Prescreen Consent
if (all(is.na(df1$onlineps_consent_yn)==TRUE)){print("All participants consent to participate in the prescreen.")} else {print(paste("PID", df1$pid[which(is.na(df1$onlineps_consent_yn))], "didn't agree to the consent."))}

#1-6 Prescreen Survey Complete
if (all(is.na(df1$online_prescreen_survey_complete)==TRUE)){print("All participants complete the prescreen survey.")} else {print(paste("PID",df1$pid[df1$online_prescreen_survey_complete == 0], "incomplete the presecreen survey"))}

if (any(is.na(df1$email) ==TRUE)){print("All participants are filled out email address.")} else {print(paste("PID",df1$pid[df1$email==""],"do not fill out the email address"))}

if (any(is.na(df1$onlineps_phone) ==TRUE)){print("All participants are filled out phone number.")} else {print(paste("PID",df1$pid[df1$onlineps_phone==""],"do not fill out the phone number."))}


#1-7 Phoescreen Survey complete
if (any(df1$phone_prescreen_survey_complete == 2)){print(paste("PID",df1$pid[df1$phone_prescreen_survey_complete==2],"is eligible."))}

if (any(df1$Phoneps_dropout == 1)){print(paste("PID",df1$pid[df1$Phoneps_dropout == 1],"is dropped out."))} else {print(paste("There's participant who no droppped out ."))}

#2. ID survey
#Set directory
file_dir <- "./redcap" 
#set file's name
id_filename = paste0("./redcap/inddiff_T1","_", date,".csv", sep="")
#save the file to the server
write.csv(df1, file = id_filename)
#2-1 Check data existance
AllDatadf <- data.frame()
setwd(file_dir)
ps_list= list.files(pattern = date)
if(file.exists(ps_list)){print("The file is in the right place.")}else {"."}
#2-2 The latest date
id_list2= list.files(pattern = today)
if(file.exists(id_list2)){print(paste0("The newest file is saved in ",today2))}else {print(paste0("The file is not saved ", today2, "."))}
#2-3 file name check
id_name=paste0("inddiff_T1","_",today)
id_list3 = list.files(pattern = id_name)
if(file.exists(id_list3)){print("The file name is correct.")} else {print(".")}
#2-4 Missing any expected PID
exppid$pid %in% data$pid
exppid1=as.data.frame(exppid$pid %in% data$pid)
exppid1$pid = exppid
exppid1$TF= exppid1$`exppid$pid %in% data$pid`
if (all(exppid1$TF == "TRUE")) {print("There is no missing PID.")} else {print(paste("PID",exppid$pid[exppid1=="FALSE"], "is missing."))}
#2-5 Individual Difference Consent
if (all(is.na(df1$indiff_consent)==TRUE)){print("All participants consent to participate in the prescreen.")} else {print(paste("PID", df1$pid[which(is.na(df1$oindiff_consent))], "didn't agree to the consent."))}
