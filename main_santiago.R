rm(list=ls(all=TRUE))
# data cleaned by Riddhi and Santiago check script cleaning.R

# print figures? if yes, then 1
print_fig <- 0

# interaction (chat1 and chat2) via One Reach (thanks Daniel!)
# behaviours in ratings
ratings1 <- read.csv("experiment1/cleaned/ratings.csv")
ratings2 <- read.csv("experiment2/cleaned/ratings.csv")
# file with gorilla IDs and OneReach IDs
task1 <- read.csv("experiment1/cleaned/task.csv")
task1 <- task1[order(task1$Participant.Private.ID),]
# task1$Response <- gsub("[^0-9]", "", task1$Response) 
task2 <- read.csv("experiment2/cleaned/task.csv")
task2 <- task2[order(task2$Participant.Private.ID),]
# task2$Response <- gsub("[^0-9]", "", task1$Response) 
# interaction via One Reach (thanks Daniel!)
if (!require(readxl)) {install.packages("readxl")}; library(readxl)
# chat1 <- read.csv("experiment1/cleaned/gptstudydata_cleaned_2024.csv")
chat1 <- read_excel("experiment1/cleaned/EXP1_data_jan2025.xlsx", sheet = 1)
chat1$botpersonality <- ifelse(chat1$botpersonality == "anxious","Anxious","Normal")
keep1 <- read_excel("experiment1/cleaned/EXP1_data_jan2025.xlsx", sheet = 2)
demo1 <- read.csv("experiment1/cleaned/demographics.csv")
# chat2 <- read.csv("experiment2/cleaned/gptstudydata_S2B_Dec2024.csv")
chat2 <- read_excel("experiment2/cleaned/EXP2_data_jan2025.xlsx", sheet = 1)
chat2$botpersonality <- ifelse(chat2$botpersonality == "extrovert","Extrovert","Introvert")
keep2 <- read_excel("experiment2/cleaned/EXP2_data_jan2025.xlsx", sheet = 2)
demo2 <- read.csv("experiment2/cleaned/demographics.csv")
# scl90
scl90 <- read.csv("experiment1/cleaned/scl90.csv")
# bfi10
bfi10 <- read.csv("experiment1/cleaned/bfi10.csv")
# bfi44
bfi44 <- read.csv("experiment2/cleaned/bfi44.csv")


# score questionnires
source("functions.R")
quest <- scoreQuestionnaires_e1(scl90, bfi10)
scl90 <- quest$scl
bfi10 <- quest$bfi

quest <- scoreQuestionnaires_e2(bfi44)
bfi44 <- quest$bfi

# add questionnaires to ratings
ratings1 <- addQuestionnaireToDataFrame_e1(ratings1, scl90, bfi10)
ratings2 <- addQuestionnaireToDataFrame_e2(ratings2, bfi44)

# add chat as factor with specific order
ratings1$chat <- as.factor(ratings1$chatType)
levels(ratings1$chat) <- c("Anxious","Non-Anxious")
ratings2$chat <- as.factor(ratings2$chatType)
levels(ratings2$chat) <- c("Extrovert","Introvert")

# change factor order
ratings1$quest <- factor(ratings1$question, levels = c("chat-again","different","similar",
                                                       "enjoy","distant","understood"))
ratings2$quest <- factor(ratings2$question, levels = c("chat-again","different","similar",
                                                       "enjoy","distant","understood"))

# REMOVE PARTICIPANTS IF THEY HAVE ONE CHAT WITH LESS THAN 8 INTERACTIONS
# FIGURE 4 WITH INFLUENCE, SAME WITH INTERACTIONS AND SLOPES. SAME FORMAT DIFFERENT SLOPES

# remove participants?
remove_participants <- T
if (remove_participants) {
  # remove or keep in influence and ratings
  task1$remove <- T
  ratings1$remove <- T
  chat1$remove <- T
  demo1$remove <- T
  for (i in 1:length(keep1$PID)) {
    task1$remove[task1$Participant.Private.ID == keep1$PID[i]] <- F
    ratings1$remove[ratings1$Participant.Private.ID == keep1$PID[i]] <- F
    chat1$remove[chat1$PID == keep1$PID[i]] <- F
    demo1$remove[demo1$Participant.Private.ID == keep1$PID[i]] <- F
  }
  task1 <- task1[task1$remove==F,]; task1$remove <- NULL
  ratings1 <- ratings1[ratings1$remove==F,]; ratings1$remove <- NULL
  chat1 <- chat1[chat1$remove==F,]; chat1$remove <- NULL
  demo1 <- demo1[demo1$remove==F,]; demo1$remove <- NULL
  
  task2$remove <- T
  ratings2$remove <- T
  chat2$remove <- T
  demo2$remove <- T
  for (i in 1:length(keep2$PID)) {
    task2$remove[task2$Participant.Private.ID == keep2$PID[i]] <- F
    ratings2$remove[ratings2$Participant.Private.ID == keep2$PID[i]] <- F
    chat2$remove[chat2$PID == keep2$PID[i]] <- F
    demo2$remove[demo2$Participant.Private.ID == keep2$PID[i]] <- F
  }
  task2 <- task2[task2$remove==F,]; task2$remove <- NULL
  ratings2 <- ratings2[ratings2$remove==F,]; ratings2$remove <- NULL
  chat2 <- chat2[chat2$remove==F,]; chat2$remove <- NULL
  demo2 <- demo2[demo2$remove==F,]; demo2$remove <- NULL
}
# how many participants?
length(unique(task1$Participant.Private.ID))
length(unique(task2$Participant.Private.ID))
length(unique(ratings1$Participant.Private.ID))
length(unique(ratings2$Participant.Private.ID))
length(unique(chat1$PID))
length(unique(chat2$PID))
length(unique(demo1$Participant.Private.ID))
length(unique(demo2$Participant.Private.ID))



source("functions.R")
# use cleaning function to extract summary information for the interactions
combine1 <- summariseChatInteraction_v2(task=task1, chat=chat1, ratings=ratings1)
combine2 <- summariseChatInteraction_v2(task=task2, chat=chat2, ratings=ratings2)

# rows are conditional probabilities of the sentiment analysis, thus cells from 
# the transition matrices
influence1 <- combine1$influence
influence2 <- combine2$influence

# rows are chats in long format. It also contains the descriptions of the chats 
# as well as ratings in wide format
combine1 <- combine1$combine
combine2 <- combine2$combine
sum(table(combine1$Participant.Private.ID)==2) 
length(table(combine1$Participant.Private.ID))
sum(table(combine2$Participant.Private.ID)==2)
length(table(combine2$Participant.Private.ID))

# add chat as factor with specific order
combine1$chat <- as.factor(combine1$chatType)
levels(combine1$chat) <- c("Anxious","Non-Anxious")
combine2$chat <- as.factor(combine2$chatType)
levels(combine2$chat) <- c("Extrovert","Introvert")

# add questionnaires to combine
combine1 <- addQuestionnaireToDataFrame_e1(combine1, scl90, bfi10)
combine2 <- addQuestionnaireToDataFrame_e2(combine2, bfi44)


# we need to use good_pids to remove not included PIDs
length(unique(ratings1$Participant.Private.ID))
length(unique(ratings2$Participant.Private.ID))
length(unique(task1$Participant.Private.ID))
length(unique(task2$Participant.Private.ID))
length(unique(chat1$PID))
length(unique(chat2$PID))
length(keep1$PID)
length(keep2$PID)

# age and sex per experiment
range(demo1$age,na.rm=T); mean(demo1$age,na.rm=T); sd(demo1$age,na.rm=T); nrow(demo1)
table(demo1$sex)
range(demo2$age,na.rm=T); mean(demo2$age,na.rm=T); sd(demo2$age,na.rm=T); nrow(demo2)
table(demo2$sex)

# age and sex overall
age <- c(demo1$age,demo2$age)
range(age,na.rm=T); mean(age,na.rm=T); sd(age,na.rm=T); length(age)
table(c(demo1$sex,demo2$sex))

# does anxiety and extroversion correlate?
temp <- ratings1[!duplicated(ratings1$Participant.Private.ID),]
if (!require(report)) {install.packages("report")}; library(report)
report_table(cor.test(temp$bfi10_extraversion,temp$scl90_anxiety, method = "spearman"))
report_table(cor.test(temp$bfi10_agreeableness,temp$scl90_anxiety, method = "spearman"))
report_table(cor.test(temp$bfi10_conscientiousness,temp$scl90_anxiety, method = "spearman"))
report_table(cor.test(temp$bfi10_neuroticism,temp$scl90_anxiety, method = "spearman"))
report_table(cor.test(temp$bfi10_openness,temp$scl90_anxiety, method = "spearman"))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Statistical Analysis: Ratings - Questionnaires# # # # # # # # # # #### 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# stats
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
chat <- stats_one_panel(ratings=ratings1,dep_var="chat-again",ind_var="scl90_anxiety",chats=c("Anxious","Non-Anxious"))
m.chat.int <- chat$regressions$m.int
m.chat.anx <- chat$regressions$m.chat1
m.chat.nan <- chat$regressions$m.chat2

diff <- stats_one_panel(ratings=ratings1,dep_var="different",ind_var="scl90_anxiety",chats=c("Anxious","Non-Anxious"))
m.diff.int <- diff$regressions$m.int
m.diff.anx <- diff$regressions$m.chat1
m.diff.nan <- diff$regressions$m.chat2

dist <- stats_one_panel(ratings=ratings1,dep_var="distant",ind_var="scl90_anxiety",chats=c("Anxious","Non-Anxious"))
m.dist.int <- dist$regressions$m.int
m.dist.anx <- dist$regressions$m.chat1
m.dist.nan <- dist$regressions$m.chat2

enjo <- stats_one_panel(ratings=ratings1,dep_var="enjoy",ind_var="scl90_anxiety",chats=c("Anxious","Non-Anxious"))
m.enjo.int <- enjo$regressions$m.int
m.enjo.anx <- enjo$regressions$m.chat1
m.enjo.nan <- enjo$regressions$m.chat2

simi <- stats_one_panel(ratings=ratings1,dep_var="similar",ind_var="scl90_anxiety",chats=c("Anxious","Non-Anxious"))
m.simi.int <- simi$regressions$m.int
m.simi.anx <- simi$regressions$m.chat1
m.simi.nan <- simi$regressions$m.chat2

unde <- stats_one_panel(ratings=ratings1,dep_var="understood",ind_var="scl90_anxiety",chats=c("Anxious","Non-Anxious"))
m.unde.int <- unde$regressions$m.int
m.unde.anx <- unde$regressions$m.chat1
m.unde.nan <- unde$regressions$m.chat2

# Correction Methods 
hb_corr <- data.frame(chat=m.chat.int$p[4],diff=m.diff.int$p[4],dist=m.dist.int$p[4],
                      enjo=m.enjo.int$p[4],simi=m.simi.int$p[4],unde=m.unde.int$p[4])
hb_corr <- hb_corr[order(hb_corr)]
# Holm-Bonferroni Method
hb_corr < .05/6:1
# Bonferroni Method
hb_corr < .05/6



# combine
exp1 <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
              data.frame(quest="chat-again",effect="Anxious",m.chat.anx[2,9:11]),
              data.frame(quest="chat-again",effect="Non-Anxious",m.chat.nan[2,9:11]),
              data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
              data.frame(quest="different",effect="Anxious",m.diff.anx[2,9:11]),
              data.frame(quest="different",effect="Non-Anxious",m.diff.nan[2,9:11]),
              data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
              data.frame(quest="distant",effect="Anxious",m.dist.anx[2,9:11]),
              data.frame(quest="distant",effect="Non-Anxious",m.dist.nan[2,9:11]),
              data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),
              data.frame(quest="enjoy",effect="Anxious",m.enjo.anx[2,9:11]),
              data.frame(quest="enjoy",effect="Non-Anxious",m.enjo.nan[2,9:11]),
              data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
              data.frame(quest="similar",effect="Anxious",m.simi.anx[2,9:11]),
              data.frame(quest="similar",effect="Non-Anxious",m.simi.nan[2,9:11]),
              data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
              data.frame(quest="understood",effect="Anxious",m.unde.anx[2,9:11]),
              data.frame(quest="understood",effect="Non-Anxious",m.unde.nan[2,9:11]))



chat <- stats_one_panel(ratings=ratings1,dep_var="chat-again",ind_var="bfi10_extraversion",chats=c("Anxious","Non-Anxious"))
m.chat.int <- chat$regressions$m.int
m.chat.anx <- chat$regressions$m.chat1
m.chat.nan <- chat$regressions$m.chat2

diff <- stats_one_panel(ratings=ratings1,dep_var="different",ind_var="bfi10_extraversion",chats=c("Anxious","Non-Anxious"))
m.diff.int <- diff$regressions$m.int
m.diff.anx <- diff$regressions$m.chat1
m.diff.nan <- diff$regressions$m.chat2

dist <- stats_one_panel(ratings=ratings1,dep_var="distant",ind_var="bfi10_extraversion",chats=c("Anxious","Non-Anxious"))
m.dist.int <- dist$regressions$m.int
m.dist.anx <- dist$regressions$m.chat1
m.dist.nan <- dist$regressions$m.chat2

enjo <- stats_one_panel(ratings=ratings1,dep_var="enjoy",ind_var="bfi10_extraversion",chats=c("Anxious","Non-Anxious"))
m.enjo.int <- enjo$regressions$m.int
m.enjo.anx <- enjo$regressions$m.chat1
m.enjo.nan <- enjo$regressions$m.chat2

simi <- stats_one_panel(ratings=ratings1,dep_var="similar",ind_var="bfi10_extraversion",chats=c("Anxious","Non-Anxious"))
m.simi.int <- simi$regressions$m.int
m.simi.anx <- simi$regressions$m.chat1
m.simi.nan <- simi$regressions$m.chat2

unde <- stats_one_panel(ratings=ratings1,dep_var="understood",ind_var="bfi10_extraversion",chats=c("Anxious","Non-Anxious"))
m.unde.int <- unde$regressions$m.int
m.unde.anx <- unde$regressions$m.chat1
m.unde.nan <- unde$regressions$m.chat2

# combine
exp1_sm <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
                 data.frame(quest="chat-again",effect="Anxious",m.chat.anx[2,9:11]),
                 data.frame(quest="chat-again",effect="Non-Anxious",m.chat.nan[2,9:11]),
                 data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
                 data.frame(quest="different",effect="Anxious",m.diff.anx[2,9:11]),
                 data.frame(quest="different",effect="Non-Anxious",m.diff.nan[2,9:11]),
                 data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
                 data.frame(quest="distant",effect="Anxious",m.dist.anx[2,9:11]),
                 data.frame(quest="distant",effect="Non-Anxious",m.dist.nan[2,9:11]),
                 data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),
                 data.frame(quest="enjoy",effect="Anxious",m.enjo.anx[2,9:11]),
                 data.frame(quest="enjoy",effect="Non-Anxious",m.enjo.nan[2,9:11]),
                 data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
                 data.frame(quest="similar",effect="Anxious",m.simi.anx[2,9:11]),
                 data.frame(quest="similar",effect="Non-Anxious",m.simi.nan[2,9:11]),
                 data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
                 data.frame(quest="understood",effect="Anxious",m.unde.anx[2,9:11]),
                 data.frame(quest="understood",effect="Non-Anxious",m.unde.nan[2,9:11]))



chat <- stats_one_panel(ratings=ratings2,dep_var="chat-again",ind_var="bfi44_extraversion",chats=c("Extrovert","Introvert"))
m.chat.dif <- chat$regressions$m.int
m.chat.ext <- chat$regressions$m.chat1
m.chat.int <- chat$regressions$m.chat2

diff <- stats_one_panel(ratings=ratings2,dep_var="different",ind_var="bfi44_extraversion",chats=c("Extrovert","Introvert"))
m.diff.dif <- diff$regressions$m.int
m.diff.ext <- diff$regressions$m.chat1
m.diff.int <- diff$regressions$m.chat2

dist <- stats_one_panel(ratings=ratings2,dep_var="distant",ind_var="bfi44_extraversion",chats=c("Extrovert","Introvert"))
m.dist.dif <- dist$regressions$m.int
m.dist.ext <- dist$regressions$m.chat1
m.dist.int <- dist$regressions$m.chat2

enjo <- stats_one_panel(ratings=ratings2,dep_var="enjoy",ind_var="bfi44_extraversion",chats=c("Extrovert","Introvert"))
m.enjo.dif <- enjo$regressions$m.int
m.enjo.ext <- enjo$regressions$m.chat1
m.enjo.int <- enjo$regressions$m.chat2

simi <- stats_one_panel(ratings=ratings2,dep_var="similar",ind_var="bfi44_extraversion",chats=c("Extrovert","Introvert"))
m.simi.dif <- simi$regressions$m.int
m.simi.ext <- simi$regressions$m.chat1
m.simi.int <- simi$regressions$m.chat2

unde <- stats_one_panel(ratings=ratings2,dep_var="understood",ind_var="bfi44_extraversion",chats=c("Extrovert","Introvert"))
m.unde.dif <- unde$regressions$m.int
m.unde.ext <- unde$regressions$m.chat1
m.unde.int <- unde$regressions$m.chat2

# Correction Methods 
hb_corr <- data.frame(chat=m.chat.dif$p[4],diff=m.diff.dif$p[4],dist=m.dist.dif$p[4],
                      enjo=m.enjo.dif$p[4],simi=m.simi.dif$p[4],unde=m.unde.dif$p[4])
hb_corr <- hb_corr[order(hb_corr)]
# Holm-Bonferroni Method
hb_corr < .05/6:1
# Bonferroni Method
hb_corr < .05/6

# combine
exp2 <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.dif[4,11:13]),
              data.frame(quest="chat-again",effect="Extrovert",m.chat.ext[2,9:11]),
              data.frame(quest="chat-again",effect="Introvert",m.chat.int[2,9:11]),
              data.frame(quest="different",effect="Interaction",m.diff.dif[4,11:13]),
              data.frame(quest="different",effect="Extrovert",m.diff.ext[2,9:11]),
              data.frame(quest="different",effect="Introvert",m.diff.int[2,9:11]),
              data.frame(quest="distant",effect="Interaction",m.dist.dif[4,11:13]),
              data.frame(quest="distant",effect="Extrovert",m.dist.ext[2,9:11]),
              data.frame(quest="distant",effect="Introvert",m.dist.int[2,9:11]),
              data.frame(quest="enjoy",effect="Interaction",m.enjo.dif[4,11:13]),
              data.frame(quest="enjoy",effect="Extrovert",m.enjo.ext[2,9:11]),
              data.frame(quest="enjoy",effect="Introvert",m.enjo.int[2,9:11]),
              data.frame(quest="similar",effect="Interaction",m.simi.dif[4,11:13]),
              data.frame(quest="similar",effect="Extrovert",m.simi.ext[2,9:11]),
              data.frame(quest="similar",effect="Introvert",m.simi.int[2,9:11]),
              data.frame(quest="understood",effect="Interaction",m.unde.dif[4,11:13]),
              data.frame(quest="understood",effect="Extrovert",m.unde.ext[2,9:11]),
              data.frame(quest="understood",effect="Introvert",m.unde.int[2,9:11]))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Statistical Analysis: Ratings - Influence # # # # # # # # # # # # #### 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Experiment 1
# user to bot
# m.chat.int <- report_table(lmer(`chat-again` ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine1))
# m.chat.anx <- report_table(lm(`chat-again` ~ user_gpt_mirror, combine1[combine1$chat == "Anxious",]))
# m.chat.nan <- report_table(lm(`chat-again` ~ user_gpt_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.diff.int <- report_table(lmer(different ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine1))
# m.diff.anx <- report_table(lm(different ~ user_gpt_mirror, combine1[combine1$chat == "Anxious",]))
# m.diff.nan <- report_table(lm(different ~ user_gpt_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.simi.int <- report_table(lmer(similar ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine1))
# m.simi.anx <- report_table(lm(similar ~ user_gpt_mirror, combine1[combine1$chat == "Anxious",]))
# m.simi.nan <- report_table(lm(similar ~ user_gpt_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.enjo.int <- report_table(lmer(enjoy ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine1))
# m.enjo.anx <- report_table(lm(enjoy ~ user_gpt_mirror, combine1[combine1$chat == "Anxious",]))
# m.enjo.nan <- report_table(lm(enjoy ~ user_gpt_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.dist.int <- report_table(lmer(distant ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine1))
# m.dist.anx <- report_table(lm(distant ~ user_gpt_mirror, combine1[combine1$chat == "Anxious",]))
# m.dist.nan <- report_table(lm(distant ~ user_gpt_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.unde.int <- report_table(lmer(understood ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine1))
# m.unde.anx <- report_table(lm(understood ~ user_gpt_mirror, combine1[combine1$chat == "Anxious",]))
# m.unde.nan <- report_table(lm(understood ~ user_gpt_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# exp1_user_gpt <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
#                        data.frame(quest="chat-again",effect="Anxious",m.chat.anx[2,9:11]),
#                        data.frame(quest="chat-again",effect="Non-Anxious",m.chat.nan[2,9:11]),
#                        data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
#                        data.frame(quest="different",effect="Anxious",m.diff.anx[2,9:11]),
#                        data.frame(quest="different",effect="Non-Anxious",m.diff.nan[2,9:11]), 
#                        data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
#                        data.frame(quest="distant",effect="Anxious",m.dist.anx[2,9:11]),          
#                        data.frame(quest="distant",effect="Non-Anxious",m.dist.nan[2,9:11]),
#                        data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),    
#                        data.frame(quest="enjoy",effect="Anxious",m.enjo.anx[2,9:11]),
#                        data.frame(quest="enjoy",effect="Non-Anxious",m.enjo.nan[2,9:11]),
#                        data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
#                        data.frame(quest="similar",effect="Anxious",m.simi.anx[2,9:11]),
#                        data.frame(quest="similar",effect="Non-Anxious",m.simi.nan[2,9:11]),
#                        data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
#                        data.frame(quest="understood",effect="Anxious",m.unde.anx[2,9:11]),
#                        data.frame(quest="understood",effect="Non-Anxious",m.unde.nan[2,9:11]))



# bot to user
# m.chat.int <- report_table(lmer(`chat-again` ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine1))
# m.chat.anx <- report_table(lm(`chat-again` ~ gpt_user_mirror, combine1[combine1$chat == "Anxious",]))
# m.chat.nan <- report_table(lm(`chat-again` ~ gpt_user_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.diff.int <- report_table(lmer(different ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine1))
# m.diff.anx <- report_table(lm(different ~ gpt_user_mirror, combine1[combine1$chat == "Anxious",]))
# m.diff.nan <- report_table(lm(different ~ gpt_user_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.simi.int <- report_table(lmer(similar ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine1))
# m.simi.anx <- report_table(lm(similar ~ gpt_user_mirror, combine1[combine1$chat == "Anxious",]))
# m.simi.nan <- report_table(lm(similar ~ gpt_user_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.enjo.int <- report_table(lmer(enjoy ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine1))
# m.enjo.anx <- report_table(lm(enjoy ~ gpt_user_mirror, combine1[combine1$chat == "Anxious",]))
# m.enjo.nan <- report_table(lm(enjoy ~ gpt_user_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.dist.int <- report_table(lmer(distant ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine1))
# m.dist.anx <- report_table(lm(distant ~ gpt_user_mirror, combine1[combine1$chat == "Anxious",]))
# m.dist.nan <- report_table(lm(distant ~ gpt_user_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# m.unde.int <- report_table(lmer(understood ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine1))
# m.unde.anx <- report_table(lm(understood ~ gpt_user_mirror, combine1[combine1$chat == "Anxious",]))
# m.unde.nan <- report_table(lm(understood ~ gpt_user_mirror, combine1[combine1$chat == "Non-Anxious",]))
# 
# exp1_gpt_user <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
#                        data.frame(quest="chat-again",effect="Anxious",m.chat.anx[2,9:11]),
#                        data.frame(quest="chat-again",effect="Non-Anxious",m.chat.nan[2,9:11]),
#                        data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
#                        data.frame(quest="different",effect="Anxious",m.diff.anx[2,9:11]),
#                        data.frame(quest="different",effect="Non-Anxious",m.diff.nan[2,9:11]), 
#                        data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
#                        data.frame(quest="distant",effect="Anxious",m.dist.anx[2,9:11]),          
#                        data.frame(quest="distant",effect="Non-Anxious",m.dist.nan[2,9:11]),
#                        data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),    
#                        data.frame(quest="enjoy",effect="Anxious",m.enjo.anx[2,9:11]),
#                        data.frame(quest="enjoy",effect="Non-Anxious",m.enjo.nan[2,9:11]),
#                        data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
#                        data.frame(quest="similar",effect="Anxious",m.simi.anx[2,9:11]),
#                        data.frame(quest="similar",effect="Non-Anxious",m.simi.nan[2,9:11]),
#                        data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
#                        data.frame(quest="understood",effect="Anxious",m.unde.anx[2,9:11]),
#                        data.frame(quest="understood",effect="Non-Anxious",m.unde.nan[2,9:11]))



# Experiment 2
# user to bot
# m.chat.int <- report_table(lmer(`chat-again` ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine2))
# m.chat.anx <- report_table(lm(`chat-again` ~ user_gpt_mirror, combine2[combine2$chat == "Extrovert",]))
# m.chat.nan <- report_table(lm(`chat-again` ~ user_gpt_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.diff.int <- report_table(lmer(different ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine2))
# m.diff.anx <- report_table(lm(different ~ user_gpt_mirror, combine2[combine2$chat == "Extrovert",]))
# m.diff.nan <- report_table(lm(different ~ user_gpt_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.simi.int <- report_table(lmer(similar ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine2))
# m.simi.anx <- report_table(lm(similar ~ user_gpt_mirror, combine2[combine2$chat == "Extrovert",]))
# m.simi.nan <- report_table(lm(similar ~ user_gpt_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.enjo.int <- report_table(lmer(enjoy ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine2))
# m.enjo.anx <- report_table(lm(enjoy ~ user_gpt_mirror, combine2[combine2$chat == "Extrovert",]))
# m.enjo.nan <- report_table(lm(enjoy ~ user_gpt_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.dist.int <- report_table(lmer(distant ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine2))
# m.dist.anx <- report_table(lm(distant ~ user_gpt_mirror, combine2[combine2$chat == "Extrovert",]))
# m.dist.nan <- report_table(lm(distant ~ user_gpt_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.unde.int <- report_table(lmer(understood ~ user_gpt_mirror * chat + (1|Participant.Private.ID), combine2))
# m.unde.anx <- report_table(lm(understood ~ user_gpt_mirror, combine2[combine2$chat == "Extrovert",]))
# m.unde.nan <- report_table(lm(understood ~ user_gpt_mirror, combine2[combine2$chat == "Introvert",]))
# 
# exp2_user_gpt <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
#                        data.frame(quest="chat-again",effect="Extrovert",m.chat.anx[2,9:11]),
#                        data.frame(quest="chat-again",effect="Introvert",m.chat.nan[2,9:11]),
#                        data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
#                        data.frame(quest="different",effect="Extrovert",m.diff.anx[2,9:11]),
#                        data.frame(quest="different",effect="Introvert",m.diff.nan[2,9:11]), 
#                        data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
#                        data.frame(quest="distant",effect="Extrovert",m.dist.anx[2,9:11]),          
#                        data.frame(quest="distant",effect="Introvert",m.dist.nan[2,9:11]),
#                        data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),    
#                        data.frame(quest="enjoy",effect="Extrovert",m.enjo.anx[2,9:11]),
#                        data.frame(quest="enjoy",effect="Introvert",m.enjo.nan[2,9:11]),
#                        data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
#                        data.frame(quest="similar",effect="Extrovert",m.simi.anx[2,9:11]),
#                        data.frame(quest="similar",effect="Introvert",m.simi.nan[2,9:11]),
#                        data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
#                        data.frame(quest="understood",effect="Extrovert",m.unde.anx[2,9:11]),
#                        data.frame(quest="understood",effect="Introvert",m.unde.nan[2,9:11]))



# bot to user
# m.chat.int <- report_table(lmer(`chat-again` ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine2))
# m.chat.anx <- report_table(lm(`chat-again` ~ gpt_user_mirror, combine2[combine2$chat == "Extrovert",]))
# m.chat.nan <- report_table(lm(`chat-again` ~ gpt_user_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.diff.int <- report_table(lmer(different ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine2))
# m.diff.anx <- report_table(lm(different ~ gpt_user_mirror, combine2[combine2$chat == "Extrovert",]))
# m.diff.nan <- report_table(lm(different ~ gpt_user_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.simi.int <- report_table(lmer(similar ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine2))
# m.simi.anx <- report_table(lm(similar ~ gpt_user_mirror, combine2[combine2$chat == "Extrovert",]))
# m.simi.nan <- report_table(lm(similar ~ gpt_user_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.enjo.int <- report_table(lmer(enjoy ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine2))
# m.enjo.anx <- report_table(lm(enjoy ~ gpt_user_mirror, combine2[combine2$chat == "Extrovert",]))
# m.enjo.nan <- report_table(lm(enjoy ~ gpt_user_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.dist.int <- report_table(lmer(distant ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine2))
# m.dist.anx <- report_table(lm(distant ~ gpt_user_mirror, combine2[combine2$chat == "Extrovert",]))
# m.dist.nan <- report_table(lm(distant ~ gpt_user_mirror, combine2[combine2$chat == "Introvert",]))
# 
# m.unde.int <- report_table(lmer(understood ~ gpt_user_mirror * chat + (1|Participant.Private.ID), combine2))
# m.unde.anx <- report_table(lm(understood ~ gpt_user_mirror, combine2[combine2$chat == "Extrovert",]))
# m.unde.nan <- report_table(lm(understood ~ gpt_user_mirror, combine2[combine2$chat == "Introvert",]))
# 
# exp2_gpt_user <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
#                        data.frame(quest="chat-again",effect="Extrovert",m.chat.anx[2,9:11]),
#                        data.frame(quest="chat-again",effect="Introvert",m.chat.nan[2,9:11]),
#                        data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
#                        data.frame(quest="different",effect="Extrovert",m.diff.anx[2,9:11]),
#                        data.frame(quest="different",effect="Introvert",m.diff.nan[2,9:11]), 
#                        data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
#                        data.frame(quest="distant",effect="Extrovert",m.dist.anx[2,9:11]),          
#                        data.frame(quest="distant",effect="Introvert",m.dist.nan[2,9:11]),
#                        data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),    
#                        data.frame(quest="enjoy",effect="Extrovert",m.enjo.anx[2,9:11]),
#                        data.frame(quest="enjoy",effect="Introvert",m.enjo.nan[2,9:11]),
#                        data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
#                        data.frame(quest="similar",effect="Extrovert",m.simi.anx[2,9:11]),
#                        data.frame(quest="similar",effect="Introvert",m.simi.nan[2,9:11]),
#                        data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
#                        data.frame(quest="understood",effect="Extrovert",m.unde.anx[2,9:11]),
#                        data.frame(quest="understood",effect="Introvert",m.unde.nan[2,9:11]))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Visualization # # # # # # # # # # # # # # # # # # # # # # # # # # #### 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
# # combine experiment 1 and experiment 2
# exps <- rbind(data.frame(exp="Exp. 1: GPT to User",exp1_gpt_user),
#               data.frame(exp="Exp. 1: User to GPT",exp1_user_gpt),
#               data.frame(exp="Exp. 2: GPT to User",exp2_gpt_user),
#               data.frame(exp="Exp. 2: User to GPT",exp2_user_gpt))
# # exps <- rbind(data.frame(exp="Exp. 1",exp1),data.frame(exp="Exp. 2",exp2))
# exps$effect <- factor(exps$effect, levels = rev(c("Interaction","Introvert","Extrovert","Non-Anxious","Anxious")))
# # exps <- exps[exps$exp != "E1-Ext.",]
# # exps$exp <- factor(as.character(exps$exp), levels = c("Exp. 2","Exp. 1"))
# exps$exp <- factor(as.character(exps$exp), levels = c("Exp. 2: GPT to User","Exp. 2: User to GPT",
#                                                       "Exp. 1: GPT to User","Exp. 1: User to GPT"))
# # levels(exps$exp) <- c(expression(Exp*`.`~1*`:`~p*`(`*user[t]==gpt[t-1]*`)`),expression(Exp*`.`~1*`:`~p*`(`*gpt[t]==user[t-1]*`)`),
# #                       expression(Exp*`.`~2*`:`~p*`(`*user[t]==gpt[t-1]*`)`),expression(Exp*`.`~2*`:`~p*`(`*gpt[t]==user[t-1]*`)`))
# # change factor order
# exps$quest <- factor(exps$quest, levels = c("chat-again","different","similar",
#                                             "enjoy","distant","understood"))
# 
# 
# (fig4 <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
#     labs(title = "Sentiment Influence predict Likert Scales",
#          subtitle = expression(Sentiment~Influence~`=`~p*`(`*User[t]==GPT[t-1]*`)`~OR~p*`(`*GPT[t]==User[t-1]*`)`), 
#          y = "Effect Size") +
#     geom_hline(yintercept = 0, col="grey") +
#     geom_point(size=2, position = position_dodge(.6)) +
#     geom_errorbar(aes(ymin=Std_Coefficient_CI_low, ymax=Std_Coefficient_CI_high), 
#                   width=.4, position = position_dodge(.6)) +
#     scale_shape_manual(values = c(17, 19, 17, 19, 15)) +
#     scale_colour_manual(values = c("#0072B2", "#D55E00","#009E73","#CC79A7","black")) + 
#     scale_y_continuous(breaks = c(-.5,0,.5)) +
#     scale_x_discrete(labels = c(expression(Exp*`.`~2*`:`~p*`(`*User[t]==GPT4[t-1]*`)`),
#                                 expression(Exp*`.`~2*`:`~p*`(`*GPT4[t]==User[t-1]*`)`),
#                                 expression(Exp*`.`~1*`:`~p*`(`*User[t]==GPT4[t-1]*`)`),
#                                 expression(Exp*`.`~1*`:`~p*`(`*GPT4[t]==User[t-1]*`)`))) +
#     coord_flip() +
#     facet_wrap(. ~ quest, ncol = 3, labeller = labeller(
#       # quest = c("chat-again" = "chat again",
#       #              "different" = "felt different",
#       #              "distant" = "felt distant",
#       #              "enjoy" = "enjoyed",
#       #              "similar" = "felt similar",
#       #              "understood" = "felt understood")
#       quest = c("chat-again" = "I would chat with\n them again",
#                 "different" = "I felt that they were\n different from me",
#                 "similar" = "I felt that we\n are similar",
#                 "enjoy" = "I enjoyed our\n conversation",
#                 "distant" = "I felt distant\n from them",
#                 "understood" = "I felt that they\n understood me"))) +
#     theme_classic() + 
#     theme(legend.position = "bottom",
#           legend.title = element_blank(),
#           axis.title.y = element_blank(),
#           legend.background = element_rect(colour='black',fill='white',linetype='solid'))
# )
# if (print_fig == 1) {
#   ggsave("figures/fig4_v3.pdf", fig4, dpi = 1200, scale = 1, units = "cm",
#          width = 16, height = 12, bg = "white")
# }






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # A Panels (Likert scales and questionnaires) # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

ann_text <- data.frame(lab="Text",scl90_anxiety = rep(.5,4), Response = rep(4.5,4),
                       quest = factor(c("different","distant","similar","understood"), 
                                      levels = c("chat-again","enjoy","different",
                                                 "distant","similar","understood")),
                       chat = c("Anxious","Non-Anxious"))
ratings1$quest <- factor(ratings1$question, levels = c("chat-again","enjoy","different",
                                                       "distant","similar","understood"))
ratings1$scl90_anxiety <- ratings1$scl90_anxiety/max(ratings1$scl90_anxiety) 
(figure2A <- ggplot(ratings1, aes(x=scl90_anxiety,y=Response,col=chat,shape=chat)) +
  labs(title = "Users' Judgements", 
       y="Likert Scale", x="Anxiety (SCL-90R)",
       col = "GPT-4 type:", shape = "GPT-4 type:") +
  geom_point(alpha = .1, stroke = 0, size = 1.5) +
  geom_smooth(method="lm", se = F, size = 1) +
  geom_text(data = ann_text,label = "*",col="black", size = 10) +
  scale_shape_manual(values = c(17, 19)) +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) + 
  scale_x_continuous(breaks = c(0, .5, 1), ) +
  scale_y_continuous(breaks = 1:5, labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
  coord_cartesian(ylim = c(1, 5)) +
  facet_wrap(. ~ quest, ncol = 2, labeller = labeller(
    quest = c("chat-again" = "I would chat with\n them again",
              "different" = "I felt that they were\n different from me",
               "similar" = "I felt that we\n are similar",
               "enjoy" = "I enjoyed our\n conversation",
               "distant" = "I felt distant\n from them",
               "understood" = "I felt that they\n understood me"))) +
  theme_classic() + 
  theme(legend.position = "bottom",
        legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)

ann_text <- data.frame(lab="Text",bfi44_extraversion = rep(.5,4), Response = rep(4.5,4),
                       quest = factor(c("similar"), 
                                      levels = c("chat-again","enjoy","different",
                                                 "distant","similar","understood")),
                       chat = c("Extrovert","Introvert"))
ratings2$quest <- factor(ratings2$question, levels = c("chat-again","enjoy","different",
                                                       "distant","similar","understood"))
ratings2$bfi44_extraversion <- ratings2$bfi44_extraversion/max(ratings2$bfi44_extraversion)
(figure3A <- ggplot(ratings2, aes(x=bfi44_extraversion,y=Response,col=chat,shape=chat)) +
  labs(title = "Users' Judgements", 
       y="Likert Scale", x="Extraversion (BFI-44)",
       col = "GPT-4 type:", shape = "GPT-4 type:") +
  geom_point(alpha = .1, stroke = 0, size = 1.5) +
  geom_smooth(method="lm", se = F, size = 1) +
  geom_text(data = ann_text,label = "*",col="black", size = 10) +
  scale_shape_manual(values = c(17, 19)) +
  scale_colour_manual(values = c("#009E73","#CC79A7")) + 
  scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
  scale_y_continuous(breaks = 1:5, labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
  coord_cartesian(ylim = c(1, 5)) +
  facet_wrap(. ~ quest, ncol = 2, labeller = labeller(
    quest = c("chat-again" = "I would chat with\n them again",
              "different" = "I felt that they were\n different from me",
              "similar" = "I felt that we\n are similar",
              "enjoy" = "I enjoyed our\n conversation",
              "distant" = "I felt distant\n from them",
              "understood" = "I felt that they\n understood me"))) +
  theme_classic() + 
    theme(legend.position = "bottom",
          legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # B Panel: Sentiment Analysis User and GPT-4# # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# options(scipen = 999) # in order to get non-scientific notation
length(unique(chat1$userid))
length(unique(task1$Response))
length(unique(chat2$userid))
length(unique(task2$Response))


# normalize sentiments by proportions
# experiment 1
combine1$user_total <- rowSums(combine1[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")])
combine1[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")] <- 
  combine1[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")]/combine1$user_total
combine1$bots_total <- rowSums(combine1[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")])
combine1[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")] <- 
  combine1[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")]/combine1$bots_total
# experiment 2
combine2$user_total <- rowSums(combine2[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")])
combine2[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")] <- 
  combine2[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")]/combine2$user_total
combine2$bots_total <- rowSums(combine2[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")])
combine2[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")] <- 
  combine2[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")]/combine2$bots_total


# normalize questionnaires between 0 and 1
combine1$scl90_anxiety <- combine1$scl90_anxiety/max(combine1$scl90_anxiety)
combine2$bfi44_extraversion <- combine2$bfi44_extraversion/max(combine2$bfi44_extraversion)



# reshape data frame so we can easy visualize (melt by the count sentiment analysis
# for each level (mixed, negative, neutral, and positive) for both user and bot
library(reshape2)
combine1.lf <- melt(combine1, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                               "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
combine2.lf <- melt(combine2, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                               "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
# change column name
colnames(combine1.lf)[ncol(combine1.lf)] <- c("prop") 
colnames(combine2.lf)[ncol(combine2.lf)] <- c("prop") 
# transform variable to character
combine1.lf$variable<- as.character(combine1.lf$variable)
combine2.lf$variable<- as.character(combine2.lf$variable)
# split string to get the first 4 character (user or bots)
combine1.lf$who <- factor(substr(combine1.lf$variable,1,4),levels = c("bots","user"))
combine2.lf$who <- factor(substr(combine2.lf$variable,1,4),levels = c("bots","user"))
# add a nicer names
levels(combine1.lf$who) <- c("GPT-4 Texts","Participants Texts")
levels(combine2.lf$who) <- c("GPT-4 Texts","Participants Texts")
# add the sentiment on column variable (created with the columns in melt)
combine1.lf$sentiment <- substr(combine1.lf$variable,6,nchar(combine1.lf$variable))
combine2.lf$sentiment <- substr(combine2.lf$variable,6,nchar(combine2.lf$variable))

# add chat as factor with specific order
# influence
influence1$chat <- as.factor(influence1$chatType)
levels(influence1$chat) <- c("Anxious","Non-Anxious")
influence2$chat <- as.factor(influence2$chatType)
levels(influence2$chat) <- c("Extrovert","Introvert")



# aim 1, GPT4 Texts are different between conditions (chatbots) 
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine1.lf[combine1.lf$who=="GPT-4 Texts",]))
# run LMER, LM, and aov as "sensitivity" analysis, so everything says the same we are more confident
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine1.lf[combine1.lf$who=="Participants Texts",]))
summary(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine1.lf[combine1.lf$who=="Participants Texts",]))

# aim 2, GPT4 Texts are different between conditions (chatbots)
report_table(t.test(user_Positive~chat,combine1[combine1.lf$who=="Participants Texts",], paired = TRUE))
report_table(t.test(user_Neutral~chat,combine1[combine1.lf$who=="Participants Texts",], paired = TRUE))
report_table(t.test(user_Negative~chat,combine1[combine1.lf$who=="Participants Texts",], paired = TRUE))
report_table(t.test(user_Mixed~chat,combine1[combine1.lf$who=="Participants Texts",], paired = TRUE))

ann_text <- data.frame(sentiment = c(2,4), prop = c(.25,.45),
                       lab = "Text", who = factor("Participants Texts",levels = c("GPT-4 Texts","Participants Texts")),
                       chat = c("Anxious","Non-Anxious"))
# visualize the average of count for each sentiment and for each chat personality
(figure2B <- ggplot(combine1.lf, aes(x=sentiment,y=prop,col=chat,shape=chat)) + 
    labs(title = "Sentiment Analysis",
         y="Prop. (Sentiment ea Condition)", x = "Text Sentiment Category",
         col = "GPT-4 type:", shape = "GPT-4 type:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0,1)) +
    # geom_violin(position = position_dodge(0.5)) +
    geom_boxplot(alpha=.3,position = position_dodge(0.5)) +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.5)) +
    # geom_text(data = ann_text,label = "*", col="black", size = 10) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    scale_shape_manual(values = c(17,19)) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    facet_wrap(who ~ ., ncol = 2) + 
    theme_classic() +
    # guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", #c(.3,.89),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
)



# aim 1, GPT4 Texts are different between conditions (chatbots) 
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine2.lf[combine2.lf$who=="GPT-4 Texts",]))
# run LMER, LM, and aov as "sensitivity" analysis, so everything says the same we are more confident
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine2.lf[combine2.lf$who=="Participants Texts",]))
summary(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine2.lf[combine2.lf$who=="Participants Texts",]))

# aim 2, GPT4 Texts are different between conditions (chatbots)
# report_table(t.test(user_Positive~chat,combine2[combine2.lf$who=="User Texts",], paired = TRUE))
# report_table(t.test(user_Neutral~chat,combine2[combine2.lf$who=="User Texts",], paired = TRUE))
# report_table(t.test(user_Negative~chat,combine2[combine2.lf$who=="User Texts",], paired = TRUE))
# report_table(t.test(user_Mixed~chat,combine2[combine2.lf$who=="User Texts",], paired = TRUE))

(figure3B <- ggplot(combine2.lf, aes(x=sentiment,y=prop,col=chat,shape=chat)) + 
    labs(title = "Sentiment Analysis",
         y="Prop. (Sentiment ea Condition)", x = "Text Sentiment Category",
         col = "GPT-4 type:", shape = "GPT-4 type:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0,1)) +
    # geom_violin(position = position_dodge(0.5)) +
    geom_boxplot(alpha=.3,position = position_dodge(0.5)) +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.5)) +
    scale_shape_manual(values = c(17,19)) +
    scale_colour_manual(values = c("#009E73","#CC79A7")) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    facet_wrap(who ~ ., ncol = 2) + 
    theme_classic() +
    # guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", #c(.3,.89),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # C Panel: Questionnaires and Sentiment Analysis# # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# difference between GPT-4 types, to simplify statistical analysis with triple order interaction
# experiment 1
combine1.wf <- combine1[combine1$chat == "Anxious",]
temp <- combine1[combine1$chat == "Non-Anxious",]
combine1.wf$mixed <- combine1.wf$user_Mixed - temp$user_Mixed
combine1.wf$negative <- combine1.wf$user_Negative - temp$user_Negative
combine1.wf$neutral <- combine1.wf$user_Neutral - temp$user_Neutral
combine1.wf$positive <- combine1.wf$user_Positive - temp$user_Positive

# experiment 2
combine2.wf <- combine2[combine2$chat == "Extrovert",]
temp <- combine2[combine2$chat == "Introvert",]
combine2.wf$mixed <- combine2.wf$user_Mixed - temp$user_Mixed
combine2.wf$negative <- combine2.wf$user_Negative - temp$user_Negative
combine2.wf$neutral <- combine2.wf$user_Neutral - temp$user_Neutral
combine2.wf$positive <- combine2.wf$user_Positive - temp$user_Positive

# test difference scores
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
combine1.lf <- melt(combine1.wf, measure.vars = c("mixed","negative","neutral","positive"))
combine2.lf <- melt(combine2.wf, measure.vars = c("mixed","negative","neutral","positive"))
# change column names
colnames(combine1.lf)[50:51] <- c("sentiment","diff")
colnames(combine2.lf)[41:42] <- c("sentiment","diff")

# stats experiment 1
anova(lmer(diff ~ sentiment * scl90_anxiety + (1|Participant.Private.ID), combine1.lf))
# combine1.lf$sentiment <- factor(combine1.lf$sentiment, levels = c("neutral","mixed","negative","positive"))
# summary(lmer(diff ~ sentiment * scl90_anxiety + (1|Participant.Private.ID), combine1.lf))
# combine1.lf$sentiment <- factor(combine1.lf$sentiment, levels = c("mixed","neutral","negative","positive"))
# summary(lmer(diff ~ sentiment * scl90_anxiety + (1|Participant.Private.ID), combine1.lf))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="mixed",]))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="negative",]))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="neutral",]))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="positive",]))

levels(combine1.lf$sentiment) <- c("Mixed","Negative","Neutral","Positive")
# (figure2C <- ggplot(combine1.lf, aes(x=scl90_anxiety,y=diff,col=sentiment)) + 
#     labs(title = "Exp. 1: Anxiety and Sentiment",
#          y=expression(Prop.[Anxious]-Prop.[Non*`-`*Anxious]), x = "Anxiety (SCL-90R)",
#          col = "Sentiment:") +
#     geom_hline(yintercept = 0) +
#     coord_cartesian(ylim = c(-.5,.5)) +
#     geom_point(alpha=.2) +
#     geom_smooth(method = "lm", se=F) +
#     scale_colour_manual(values = c("grey","red","black","green")) +
#     scale_y_continuous(breaks = c(-.5,0,.5)) +
#     scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
#     annotate("text",x=.8,y=.3,label="*",col="red",size=10) +
#     theme_classic() +
#     # guides(color = guide_legend(nrow = 2)) +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
# )
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
(figure2C <- ggplot(combine1.lf, aes(x=scl90_anxiety,y=diff,fill=diff)) + 
    labs(title = "Anxiety and Sentiment",
         y=expression(Prop.[Anxious]-Prop.[Non*`-`*Anxious]), x = "Anxiety (SCL-90R)",
         col = "Sentiment:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(-.5,.8)) +
    geom_point(alpha=.4, shape=21, col="black") +
    geom_smooth(method = "lm", col="black", se=F) +
    scale_fill_gradient2(low="#D55E00",mid="white",high="#0072B2",midpoint=0,
                         limits=range(combine1.lf$diff), 
                         breaks=seq(min(combine1.lf$diff),max(combine1.lf$diff), by=.1)) +
    scale_y_continuous(breaks = c(-.5,0,.8)) +
    scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
    stat_cor(col="black",label.y=.75) +
    theme_classic() +
    facet_wrap(sentiment~., ncol=2) +
    theme(legend.position = "none")
)



# stats experiment 2
anova(lmer(diff ~ sentiment * bfi44_extraversion + (1|Participant.Private.ID), combine2.lf))
# summary(lmer(diff ~ sentiment * bfi44_extraversion + (1|Participant.Private.ID), combine2.lf))

levels(combine2.lf$sentiment) <- c("Mixed","Negative","Neutral","Positive")
# (figure3C <- ggplot(combine2.lf, aes(x=bfi44_extraversion,y=diff,col=sentiment)) +
#     labs(title = "Exp. 2: Extraversion and Sentiment",
#          y=expression(Prop.[Extraversion]-Prop.[Intraversion]), x = "Extraversion (BFI-44)",
#          col = "Sentiment:") +
#     geom_hline(yintercept = 0) +
#     coord_cartesian(ylim = c(-.5,.5)) +
#     geom_point(alpha=.2) +
#     geom_smooth(method = "lm", se=F) +
#     scale_colour_manual(values = c("grey","red","black","green")) +
#     scale_y_continuous(breaks = c(-.5,0,.5)) +
#     scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
#     # annotate("text",x=.8,y=.3,label="*",col="red",size=10) +
#     theme_classic() +
#     # guides(color = guide_legend(nrow = 2)) +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
# )
(figure3C <- ggplot(combine2.lf, aes(x=bfi44_extraversion,y=diff,fill=diff)) + 
    labs(title = "Extraversion and Sentiment",
         y=expression(Prop.[Extraversion]-Prop.[Intraversion]), x = "Extraversion (BFI-44)",
         col = "Sentiment:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(-.4,.55)) +
    geom_point(alpha=.4, shape=21, col="black") +
    geom_smooth(method = "lm", col="black", se=F) +
    scale_fill_gradient2(low="#CC79A7",mid="white",high="#009E73",midpoint=0,
                         limits=range(combine2.lf$diff), 
                         breaks=seq(min(combine2.lf$diff),max(combine2.lf$diff), by=.1)) +
    scale_y_continuous(breaks = c(-.4,0,.5)) +
    scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
    stat_cor(col="black",label.y=.5) +
    theme_classic() +
    facet_wrap(sentiment~., ncol=2) +
    theme(legend.position = "none")
)



# influence1 <- influence1[influence1$direction != "",]
# influence1$direction <- as.factor(influence1$direction)
# levels(influence1$direction) <- c("User(t-1) --> Bot(t)","Bot(t-1) --> User(t)")
# (p_inf_1 <- ggplot(influence1, aes(x=target,y=value,col=direction)) + stat_summary() +
#   labs(title = "Exp. 1: GPT4 Influence",
#        x = "Transition", y = "Probability", col = "Direction") +
#   scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1)) + 
#   facet_grid(.~chat) + theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = c(0.2,0.8)))
# 
# influence2 <- influence2[influence2$direction != "",]
# influence2$direction <- as.factor(influence2$direction)
# levels(influence2$direction) <- c("User(t-1) --> Bot(t)","Bot(t-1) --> User(t)")
# (p_inf_2 <- ggplot(influence2, aes(x=target,y=value,col=direction)) + stat_summary() +
#   labs(title = "Exp. 2: GPT4 Influence",
#        x = "Transition", y = "Probability", col = "Direction") +
#   scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1)) + 
#   facet_grid(.~chat) + theme_classic() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = c(0.2,0.8)))

# library(dplyr)
# tild_1 <- data.frame(influence1 %>% group_by(from,to,chatType,direction,target) %>% 
#                        summarise(mean=mean(value)))
# tild_2 <- data.frame(influence2 %>% group_by(from,to,chatType,direction,target) %>%
#                        summarise(mean=mean(value)))
# # tild_1 <- influence1[influence1$chatId=="6886",]
# ggplot(tild_1, 
#        aes(x=from,y=to,fill=mean,label=round(mean,2))) + 
#   labs(title="Exp. 1: Probability of Current given Previous",
#        x="Previous(t-1)",y="Current(t)",fill="P(x)",label="P(x)") + 
#   geom_tile() + geom_text(col="red") +
#   facet_grid(chatType~direction) + theme_classic()
# ggplot(tild_2, 
#        aes(x=from,y=to,fill=mean,label=round(mean,2))) + 
#   labs(title="Exp. 2: Probability of Current given Previous",
#        x="Previous(t-1)",y="Current(t)",fill="P(x)",label="P(x)") + 
#   geom_tile() + geom_text(col="red") +
#   facet_grid(chatType~direction) + theme_classic()



# # melt combine experiments 1 and 2
# combine1.lf <- melt(combine1, measure.vars = c("chat-again","different","similar","enjoy","distant","understood"))
# combine2.lf <- melt(combine2, measure.vars = c("chat-again","different","similar","enjoy","distant","understood"))
# # change column name
# colnames(combine1.lf)[ncol(combine1.lf)] <- c("rating") 
# colnames(combine2.lf)[ncol(combine2.lf)] <- c("rating") 
# 
# plotWhatMakesLikert <- function (combine.lf, title, x_var, x_label) {
#   combine.lf$x_axis <- combine.lf[,x_var]
#   library(ggpubr)
#   return(ggplot(combine.lf, aes(x=x_axis,y=rating,col=chatType)) + 
#            labs(title=title,x = x_label, y = "Likert Scale") +
#            geom_point(alpha=0.2,col="grey") +
#            geom_smooth(method = "lm",se=F) + stat_cor() + 
#            scale_y_continuous(breaks = 1:5, limits = c(1, 5),
#                               labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
#            facet_wrap(variable~., labeller = labeller(
#              variable = c("chat-again" = "I would chat with\n them again",
#                           "different" = "I felt that they were\n different from me",
#                           "similar" = "I felt that we\n are similar",
#                           "enjoy" = "I enjoyed our\n conversation",
#                           "distant" = "I felt distant\n from them",
#                           "understood" = "I felt that they\n understood me"))) + 
#            theme_classic())
# }
# 
# plotWhatMakesLikert(combine1.lf,title="Exp. 1: Num. Interactions",
#                     x_var="num_interactions",x_label="Number of Intearactions")
# plotWhatMakesLikert(combine1.lf,title="Exp. 1: User mirror Sentiment",
#                     x_var="gpt_user_mirror",x_label="p(Bot --> User)")
# plotWhatMakesLikert(combine1.lf,title="Exp. 1: Bot mirror Sentiment",
#                     x_var="user_gpt_mirror",x_label="p(User --> Bot)")
# plotWhatMakesLikert(combine1.lf,title="Exp. 1: GPT Average Word",
#                     x_var="bots_mean_words",x_label="Bot Mean Words")
# plotWhatMakesLikert(combine1.lf,title="Exp. 1: User Average Word",
#                     x_var="user_mean_words",x_label="User Mean Words")
# 
# plotWhatMakesLikert(combine2.lf,title="Exp. 2: Num. Interactions",
#                     x_var="num_interactions",x_label="Number of Intearactions")
# plotWhatMakesLikert(combine2.lf,title="Exp. 2: User mirror Sentiment",
#                     x_var="gpt_user_mirror",x_label="p(Bot --> User)")
# plotWhatMakesLikert(combine2.lf,title="Exp. 2: Bot mirror Sentiment",
#                     x_var="user_gpt_mirror",x_label="p(User --> Bot)")
# plotWhatMakesLikert(combine2.lf,title="Exp. 2: GPT Average Word",
#                     x_var="bots_mean_words",x_label="Bot Mean Words")
# plotWhatMakesLikert(combine2.lf,title="Exp. 2: User Average Word",
#                     x_var="user_mean_words",x_label="User Mean Words")
# 
# 
# 
# summary(lm(`chat-again` ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine1))
# summary(lm(different ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine1))
# summary(lm(similar ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine1))
# summary(lm(enjoy ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine1))
# summary(lm(distant ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine1))
# summary(lm(understood ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine1))
# 
# summary(lm(`chat-again` ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine2))
# summary(lm(different ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine2))
# summary(lm(similar ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine2))
# summary(lm(enjoy ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine2))
# summary(lm(distant ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine2))
# summary(lm(understood ~ num_interactions + gpt_user_mirror + user_gpt_mirror + 
#              bots_mean_words + user_mean_words, combine2))
# 
# m <- lm(understood ~ bots_Mixed + bots_Negative + bots_Neutral + bots_Positive +
#              user_Mixed + user_Negative + user_Neutral + user_Positive, combine2)
# summary(m)
# step(m)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Combine Figures # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# fig2 <- ggarrange(ggarrange(figure2A,figure2B, ncol=2, widths = c(2,1),
#                             labels = c("A","B")))
fig2 <- annotate_figure(ggarrange(figure2A,
                  ggarrange(figure2B,figure2C, nrow=2, heights = c(1, 1.2),
                            labels = c("B","C")),
                  ncol=2, widths = c(1, 1), labels = c("A","")),
                  top = text_grob("Experiment 1", color = "black", face = "bold", size = 14))
# fig2
# fig3 <- ggarrange(ggarrange(figure3A,figure3B, ncol=2, widths = c(2,1),
#                             labels = c("A","B")))
fig3 <- annotate_figure(ggarrange(figure3A,
                  ggarrange(figure3B,figure3C, nrow=2, heights = c(1, 1.2),
                            labels = c("B","C")),
                  ncol=2, widths = c(1, 1), labels = c("A","")),
                  top = text_grob("Experiment 2", color = "black", face = "bold", size = 14))
# fig3
if (print_fig == 1) {
  # ggsave("figures/fig2_v4.pdf", fig2, dpi = 2400, scale = .9, units = "cm",
  #        width = 24, height = 16, bg = "white")
  ggsave("figures/fig2_v6.pdf", fig2, dpi = 2400, scale = .85, units = "cm",
         width = 24, height = 24, bg = "white")
  
  # ggsave("figures/fig3_v4.pdf", fig3, dpi = 2400, scale = .9, units = "cm",
  #        width = 24, height = 16, bg = "white")
  ggsave("figures/fig3_v6.pdf", fig3, dpi = 2400, scale = .85, units = "cm",
         width = 24, height = 24, bg = "white")
}





# combine experiment 1 and experiment 2
exps <- rbind(data.frame(exp="Exp. 1",exp1),data.frame(exp="Exp. 2",exp2))
exps$effect <- factor(exps$effect, levels = rev(c("Interaction","Introvert","Extrovert","Non-Anxious","Anxious")))
# exps <- exps[exps$exp != "E1-Ext.",]
exps$exp <- factor(as.character(exps$exp), levels = c("Exp. 2","Exp. 1"))
# change factor order
exps$quest <- factor(exps$quest, levels = c("chat-again","different","similar",
                                            "enjoy","distant","understood"))

library(ggplot2)
(figS1 <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
    labs(title = "Experiments Summary and Statistics",
         y = "Effect Size") +
    geom_hline(yintercept = 0, col="grey") +
    geom_point(size=2, position = position_dodge(.6)) +
    geom_errorbar(aes(ymin=Std_Coefficient_CI_low, ymax=Std_Coefficient_CI_high), 
                  width=.4, position = position_dodge(.6)) +
    scale_shape_manual(values = c(17, 19, 17, 19, 15)) +
    scale_colour_manual(values = c("#0072B2", "#D55E00","#009E73","#CC79A7","black")) + 
    coord_flip() +
    facet_wrap(. ~ quest, ncol = 3, labeller = labeller(
      quest = c("chat-again" = "I would chat with\n them again",
                "different" = "I felt that they were\n different from me",
                "similar" = "I felt that we\n are similar",
                "enjoy" = "I enjoyed our\n conversation",
                "distant" = "I felt distant\n from them",
                "understood" = "I felt that they\n understood me"))) +
    theme_classic() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)
if (print_fig == 1) {
  ggsave("figures/figS1.pdf", figS1, dpi = 1200, scale = 1, units = "cm",
         width = 16, height = 12, bg = "white")
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ratings1$exp <- "Experiment 1"
# ratings2$exp <- "Experiment 2"

(figureA <- ggplot(ratings1, aes(x=scl90_anxiety,y=Response,col=chat,shape=chat)) +
    labs(title = "Exp. 1", 
         y="Likert Scale", x="Anxiety",
         col = "Chatbot", shape = "Chatbot") +
    geom_point(alpha = .1, stroke = 0, size = 1.5) +
    geom_smooth(method="lm", se = F, size = 1) +
    scale_shape_manual(values = c(17, 19)) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) + 
    scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                       labels = c("Disagree","","","","Agree")) +
    facet_wrap(. ~ question, ncol = 3, labeller = labeller(
      question = c("chat-again" = "chat again",
                   "different" = "felt different",
                   "distant" = "felt distant",
                   "enjoy" = "enjoyed",
                   "similar" = "felt similar",
                   "understood" = "felt understood"))) +
    theme_classic() #+ theme(legend.position = "none", axis.title.y = element_blank())
)
ratings1$bfi10_extraversion <- ratings1$bfi10_extraversion/max(ratings1$bfi10_extraversion)
(figureB <- ggplot(ratings1, aes(x=bfi10_extraversion ,y=Response,col=chat,shape=chat)) +
    labs(title = "Exp. 1", 
         y="Likert Scale", x="Extraversion",
         col = "Chatbot", shape = "Chatbot") +
    geom_point(alpha = .1, stroke = 0, size = 1.5) +
    geom_smooth(method="lm", se = F, size = 1) +
    scale_shape_manual(values = c(17, 19)) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) + 
    scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                       labels = c("Disagree","","","","Agree")) +
    facet_wrap(. ~ question, ncol = 3, labeller = labeller(
      question = c("chat-again" = "chat again",
                   "different" = "felt different",
                   "distant" = "felt distant",
                   "enjoy" = "enjoyed",
                   "similar" = "felt similar",
                   "understood" = "felt understood"))) +
    theme_classic() #+ theme(legend.position = "none", axis.title.y = element_blank())
)
(figureC <- ggplot(ratings2, aes(x=bfi44_extraversion,y=Response,col=chat,shape=chat)) +
    labs(title = "Exp. 2", 
         y="Likert Scale", x="Extraversion",
         col = "Chatbot", shape = "Chatbot") +
    geom_point(alpha = .1, stroke = 0, size = 1.5) +
    geom_smooth(method="lm", se = F, size = 1) +
    scale_shape_manual(values = c(17, 19)) +
    scale_colour_manual(values = c("#009E73","#CC79A7")) + 
    scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                       labels = c("Disagree","","","","Agree")) +
    facet_wrap(. ~ question, ncol = 3, labeller = labeller(
      question = c("chat-again" = "chat again",
                   "different" = "felt different",
                   "distant" = "felt distant",
                   "enjoy" = "enjoyed",
                   "similar" = "felt similar",
                   "understood" = "felt understood"))) +
    theme_classic() #+ theme(legend.position = "none", axis.title.y = element_blank())
)

exps <- rbind(data.frame(exp="E1-Anx.",exp1),
              data.frame(exp="E1-Ext.",exp1_sm),
              data.frame(exp="E2-Ext.",exp2))
exps$effect <- factor(exps$effect, levels = rev(c("Interaction","Introvert","Extrovert",
                                                  "Non-Anxious","Anxious")))

exps$exp <- factor(exps$exp, levels = c("E2-Ext.","E1-Ext.","E1-Anx.")) 
(figureD <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
    labs(title = "Stats", y = "Effect Size") +
    geom_hline(yintercept = 0, col="grey") +
    geom_point(size=2, position = position_dodge(.6)) +
    geom_errorbar(aes(ymin=Std_Coefficient_CI_low, ymax=Std_Coefficient_CI_high), 
                  width=.4, position = position_dodge(.6)) +
    scale_shape_manual(values = c(17, 19, 17, 19, 15)) +
    scale_colour_manual(values = c("#0072B2", "#D55E00","#009E73","#CC79A7","black")) + 
    coord_flip() +
    facet_wrap(. ~ quest, ncol = 3, labeller = labeller(
      # quest = c("chat-again" = "chat again",
      #              "different" = "felt different",
      #              "distant" = "felt distant",
      #              "enjoy" = "enjoyed",
      #              "similar" = "felt similar",
      #              "understood" = "felt understood")
      quest = c("chat-again" = "I would chat with\n them again",
                "different" = "I felt that they were\n different from me",
                "distant" = "I felt distant\n from them",
                "enjoy" = "I enjoyed our\n conversation",
                "similar" = "I felt that we\n are similar",
                "understood" = "I felt that they\n understood me"))) +
    theme_classic() + theme(legend.position = "bottom",
                            legend.title = element_blank(),
                            axis.title.y = element_blank())
)

# ggsave("figures/figureA.pdf", figureA, dpi = 1200, scale = 1, units = "cm",
#        width = 16, height = 12, bg = "white")
# ggsave("figures/figureB.pdf", figureB, dpi = 1200, scale = 1, units = "cm",
#        width = 16, height = 12, bg = "white")
# ggsave("figures/figureC.pdf", figureC, dpi = 1200, scale = 1, units = "cm",
#        width = 16, height = 12, bg = "white")
# ggsave("figures/figureD.pdf", figureD, dpi = 1200, scale = 1, units = "cm",
#        width = 16, height = 12, bg = "white")

library(ggpubr)
legend <- get_legend(figureD)
figureD <- figureD + theme(legend.position = "none")



figure <- ggarrange(ggarrange(figureA,figureB,figureC,figureD, ncol=4,widths = c(1,1,1,1.2)),
                    legend,nrow=2,heights = c(10,1))

# ggsave("figures/figure.pdf", figure, dpi = 2400, scale = 1, units = "cm",
#        width = 20, height = 20, bg = "white")


(figureD <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
    labs(title = "Stats", y = "Effect Size") +
    geom_hline(yintercept = 0, col="grey") +
    geom_point(size=2, position = position_dodge(.6)) +
    geom_errorbar(aes(ymin=Std_Coefficient_CI_low, ymax=Std_Coefficient_CI_high), 
                  width=.4, position = position_dodge(.5)) +
    scale_shape_manual(values = c(17, 19, 17, 19, 15)) +
    scale_colour_manual(values = c("#0072B2", "#D55E00","#009E73","#CC79A7","black")) + 
    coord_flip() +
    facet_wrap(. ~ quest, ncol = 3, labeller = labeller(
      quest = c("chat-again" = "chat again",
                "different" = "felt different",
                "distant" = "felt distant",
                "enjoy" = "enjoyed",
                "similar" = "felt similar",
                "understood" = "felt understood"))) +
    theme_classic() + theme(legend.position = "bottom",
                            legend.title = element_blank(),
                            axis.title.y = element_blank())
)

# # # How can the sentiment analysis moderate anxiety and understanding? # # #

# More negatives messages from users impact in more negative messages from the bot?

# longer user_mean_words





# 17/12/2024
# Effect sizes Positive versus Negative
# Stats for interactions extro vs introvert in addition to the correlations
# accounting for multiple comparisons

# Same script for the 2 experiments
# Exp1 extroversion

# omnibus test, reversing likerts to test an overall interaction