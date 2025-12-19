rm(list = ls())
# file names from raw gorilla data folder
files <- list.files("pilot/data_exp_132936-v11/")



#### create userid by participant in row data base ####
gpt <- read.csv("pilot/gptstudydata_Sep15.csv")
options(scipen=999)
gpt$timestamp <- as.integer(substr(gpt$GPTtimestamp,1,10))
library("lubridate")
gpt$fulldate <- as_datetime(gpt$timestamp)
gpt$days <- as.Date(gpt$fulldate, format = "%Y-%m-%d")
gpt$hours <- format(gpt$fulldate, format = "%H:%M:%S")
# order by date
gpt <- gpt[order(gpt$fulldate),]

userid <- unique(gpt$userid)
fechas <- gpt[!duplicated(gpt$userid),c("userid","botpersonality","fulldate",
                                        "days","hours")]
fechas <- fechas[fechas$days >= "2023-08-25" & fechas$days < "2023-08-29",]

temp4 <- matrix(NA,nrow = length(userid),ncol=2)
colnames(temp4) <- c("chat1","chat2")
for (i in 1:length(userid)) {
  temp <- gpt[gpt$userid == userid[i],]
  temp2 <- mean(temp$fulldate)
  # get order of chats
  temp$idbot <- paste0(temp$userid,temp$botpersonality)
  temp$chatOrder <- rep(1:length(unique(temp$idbot)),table(temp$idbot))
  temp4[i,1:length(unique(temp$botpersonality))] <- unique(temp$botpersonality)
  if (i == 1) {
    temp3 <- temp2
    gpt3 <- temp
  } else {
    temp3 <- c(temp3,temp2)
    gpt3 <- rbind(gpt3,temp)
  }
}
userid <- data.frame(userid,fulldate = temp3,temp4)
userid$days <- as.Date(userid$fulldate, format = "%Y-%m-%d")
userid$hours <- format(userid$fulldate, format = "%H:%M:%OS")
# remove temp objects
remove(temp,temp2,temp3,temp4,gpt)



# experimental IDs only the ones in gorilla
expid <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("8v2c",files)]))
gorPubId <- unique(expid$Participant.Public.ID); gorPubId<-gorPubId[gorPubId!=""]
gorPriId <- unique(expid$Participant.Private.ID); gorPriId<-gorPriId[!is.na(gorPriId)]
age <- expid$Response[expid$Object.ID == "object-4"]
sex <- expid$Response[expid$Object.ID == "object-5"]; sex <- sex[seq(1,length(sex),by=2)]
# id <- expid$Response[expid$Object.ID == "object-10"]
# expid <- data.frame(id,age,sex)
expid <- data.frame(age,sex)
# expid <- expid[seq(1,nrow(expid),by=2),]
expid <- data.frame(gorPubId,gorPriId,expid)

# add gorilla ids to user id
userid$gorPubId <- userid$gorPriId <- userid$age <- userid$sex <- NA
for (i in 1:nrow(expid)) {
  if (sum(userid$userid == expid$id[i]) > 0) {
    userid$sex[userid$userid == expid$id[i]] <- expid$sex[i]
    userid$age[userid$userid == expid$id[i]] <- expid$age[i]
    userid$gorPriId[userid$userid == expid$id[i]] <- expid$gorPriId[i]
    userid$gorPubId[userid$userid == expid$id[i]] <- expid$gorPubId[i]
  }
}

## ## ## summary # ## ##
# userid is the ID record from oneReach matched with gorilla's
# expid is the ID record from gorilla experiment first node
# gpt3 are all the interactions with chat order



#### create chat by row data base ####
relCols <- c("UTC.Date.and.Time","Participant.Private.ID","Participant.Public.ID",
             "Spreadsheet","Object.ID","Response")

chat12_1 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("keg8",files)]))
chat12_1 <- chat12_1[,relCols]
chat12_1$order <- 1
chat12_1$group <- "first anxious"
chat12_2 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("mrfa",files)]))
chat12_2 <- chat12_2[,relCols]
chat12_2$order <- 2
chat12_2$group <- "first anxious"
chat21_1 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("xy27",files)]))
chat21_1 <- chat21_1[,relCols]
chat21_1$order <- 1
chat21_1$group <- "first normal"
chat21_2 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("vs28",files)]))
chat21_2 <- chat21_2[,relCols]
chat21_2$order <- 2
chat21_2$group <- "first normal"

# combine
chat <- rbind(chat12_1,chat12_2,chat21_1,chat21_2)

# clean rows
chat <- chat[grepl("object",chat$Object.ID),]
chat <- chat[chat$Response != "",]
chat$Response <- as.integer(chat$Response)

# to time format
chat$days <- as.Date(chat$UTC.Date.and.Time,format = "%d/%m/%Y")
chat$hours <- format(chat$UTC.Date.and.Time,format = "%H:%M:%S")
chat$fulldate <- as.POSIXct(chat$UTC.Date.and.Time, format = "%d/%m/%Y %H:%M:%OS")

# create sliders
chat$slider <- NA
chat$slider <- ifelse(grepl("16",chat$Object.ID) | grepl("47",chat$Object.ID),
                      "enjoy",chat$slider)
chat$slider <- ifelse(grepl("17",chat$Object.ID) | grepl("48",chat$Object.ID),
                      "understand",chat$slider)
chat$slider <- ifelse(grepl("21",chat$Object.ID) | grepl("52",chat$Object.ID),
                      "chatAgain",chat$slider)



#### visualization ####
library(ggplot2)
chat$Spreadsheet <- ifelse(chat$Spreadsheet == "chat1","anxious","happy")
ggplot(chat, aes(x=slider,y=Response,col=Spreadsheet)) + 
  labs(x="slider type", y="Slider value", col="bot type") +
  stat_summary(position = position_dodge(0.5),
               fun.data = "mean_cl_boot") + 
  geom_point(position = position_dodge(0.5), alpha=0.2) +
  # facet_grid(.~group) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = 1))


"object-16" # enjoy
"object-17" # understand
"object-21" # chatAgain

"object-47" # enjoy
"object-48" # understand 
"object-52" # chatAgain

relCols <- c("UTC.Date.and.Time","Participant.Private.ID","Participant.Public.ID",
             "Question.Key","Response")

chat1_1 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("mkoe",files)]))
chat1_1 <- chat1_1[,relCols]
chat1_1$order <- 1
chat1_1$botType <- "anxious"
chat1_1$group <- "first anxious"
chat2_1 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("4793",files)]))
chat2_1 <- chat2_1[,relCols]
chat2_1$order <- 1
chat2_1$botType <- "normal"
chat2_1$group <- "first anxious"
chat1_2 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("5yf1",files)]))
chat1_2 <- chat1_2[,relCols]
chat1_2$order <- 1
chat1_2$botType <- "normal"
chat1_2$group <- "first normal"
chat2_2 <- read.csv(paste0("pilot/data_exp_132936-v11/",files[grepl("a8jv",files)]))
chat2_2 <- chat2_2[,relCols]
chat2_2$order <- 1
chat2_2$botType <- "anxious"
chat2_2$group <- "first normal"

# combine
quest <- rbind(chat1_1,chat1_2,chat2_1,chat2_2)
# filter quantitative likert scales
quest <- quest[grepl("quantised",quest$Question.Key),]
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
quest$Question.Key <- recode(quest$Question.Key, 
                             "response-1-quantised"="I felt that we were similar",
                             "response-2-quantised"="I enjoy our conversation",
                             "response-5-quantised"="I felt distant from them",
                             "response-3-quantised"="I felt that they understood me",
                             "response-6-quantised"="I felt that we were different from each other")
quest$Response <- as.integer(quest$Response)

ggplot(quest, aes(x=Question.Key, y=Response ,col=botType)) + 
  labs(x="Item", y="Lickert value", col="Bot Type") +
  stat_summary(position = position_dodge(0.5),
               fun.data = "mean_cl_boot") + 
  scale_y_continuous(breaks = c(1,3,5), labels = c("Dissagree","Neutral","Agree")) +
  geom_point(position = position_dodge(0.5), alpha=0.2) +
  facet_grid(. ~ group) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.position = "top")
