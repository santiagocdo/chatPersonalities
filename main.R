rm(list=ls(all=TRUE))
# data cleaned by Riddhi and Santiago check script cleaning.R

# print figures? if yes, then 1
print_fig <- 0

# interaction (chat1 and chat2) via One Reach (thanks Daniel!)
# behaviours in ratings
ratings1 <- read.csv("experiment1/cleaned/ratings.csv")
qual1 <- read.csv("experiment1/cleaned/qualitative.csv")
ratings2 <- read.csv("experiment2/cleaned/ratings.csv")
qual2 <- read.csv("experiment2/cleaned/qualitative.csv")
# file with gorilla IDs and OneReach IDs
task1 <- read.csv("experiment1/cleaned/task.csv")
task1 <- task1[order(task1$Participant.Private.ID),]
task2 <- read.csv("experiment2/cleaned/task.csv")
task2 <- task2[order(task2$Participant.Private.ID),]

# interaction via One Reach (thanks Daniel!)
if (!require(readxl)) {install.packages("readxl")}; library(readxl)
chat1 <- read_excel("experiment1/cleaned/EXP1_data_jan2025.xlsx", sheet = 1)
chat1$botpersonality <- ifelse(chat1$botpersonality == "anxious","Anxious","Normal")
keep1 <- read_excel("experiment1/cleaned/EXP1_data_jan2025.xlsx", sheet = 2)
demo1 <- read.csv("experiment1/cleaned/demographics.csv")
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

# call our functiosn
source("functions.R")

# score questionnires
quest <- scoreQuestionnaires_e1(scl90, bfi10)
scl90 <- quest$scl
bfi10 <- quest$bfi

quest <- scoreQuestionnaires_e2(bfi44)
bfi44 <- quest$bfi

# add questionnaires to ratings
ratings1 <- addQuestionnaireToDataFrame_e1(ratings1, scl90, bfi10)
qual1 <- addQuestionnaireToDataFrame_e1(qual1, scl90, bfi10)
ratings2 <- addQuestionnaireToDataFrame_e2(ratings2, bfi44)
qual2 <- addQuestionnaireToDataFrame_e2(qual2, bfi44)

# add chat as factor with specific order
ratings1$chat <- as.factor(ratings1$chatType)
levels(ratings1$chat) <- c("Anxious","Nonanxious")
ratings2$chat <- as.factor(ratings2$chatType)
levels(ratings2$chat) <- c("Extrovert","Introvert")

# change factor order
ratings1$quest <- factor(ratings1$question, levels = c("chat-again","different","similar",
                                                       "enjoy","distant","understood"))
ratings2$quest <- factor(ratings2$question, levels = c("chat-again","different","similar",
                                                       "enjoy","distant","understood"))

# REMOVE PARTICIPANTS IF THEY HAVE ONE CHAT WITH LESS THAN 8 INTERACTIONS 
# (see keep1 and keep2, conducted by D.R.L.)
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

# use cleaning function to extract summary information for the interactions
combine1 <- summariseChatInteraction(task=task1, chat=chat1, ratings=ratings1)
combine2 <- summariseChatInteraction(task=task2, chat=chat2, ratings=ratings2)

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
levels(combine1$chat) <- c("Anxious","Nonanxious")
combine2$chat <- as.factor(combine2$chatType)
levels(combine2$chat) <- c("Extrovert","Introvert")

# add questionnaires to interactions (combine)
combine1 <- addQuestionnaireToDataFrame_e1(combine1, scl90, bfi10)
combine2 <- addQuestionnaireToDataFrame_e2(combine2, bfi44)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Socio-Demographics and Sample Sizes # # # # # # # # # # # # # # # #### 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
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
# create affiliation score
ratings1$aff_score <- ratings1$Response - 3
ratings1$aff_score <- ifelse(ratings1$quest=="different"|ratings1$quest=="distant",
                             -1*ratings1$aff_score,ratings1$aff_score) 
# quick visualization of the affiliation score 
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
ggplot(ratings1, aes(x=scl90_anxiety,y=aff_score,col=chat)) + 
  geom_smooth(method="lm",se=F) +
  facet_wrap(.~quest)
ggplot(ratings1, aes(x=scl90_anxiety,y=aff_score,col=chat)) + 
  geom_smooth(method="lm",se=F)

# affiliation score average (primary outcome)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
# ratings1$sex <- ratings1$age <- NA
# for (i in 1:nrow(demo1)) {
#   ratings1$sex[ratings1$Participant.Private.ID==demo1$Participant.Private.ID[i]] <- demo1$sex[i]
#   ratings1$age[ratings1$Participant.Private.ID==demo1$Participant.Private.ID[i]] <- demo1$age[i]
# }
# ratings1_wf <- ratings1 %>% group_by(Participant.Private.ID,sex,age,chat,order,
#                                      scl90_anxiety,scl90_OCD,scl90_depression,scl90_interSens,scl90_psychotic,scl90_paranoidId,scl90_angerHost,scl90_phobic,
#                                      bfi10_extraversion,bfi10_agreeableness,bfi10_conscientiousness,bfi10_neuroticism,bfi10_openness) %>%
#   summarise(aff_score=mean(aff_score))
# write.csv(ratings1_wf,"ejercicio1.csv", row.names = F)
# write.csv(ratings1, "ejercicio2.csv", row.names = F)
ratings1_wf <- ratings1 %>% group_by(Participant.Private.ID,scl90_anxiety,chat) %>%
  summarise(aff_score=mean(aff_score))
ratings1_wf$scl90_anxiety <- ratings1_wf$scl90_anxiety/max(ratings1_wf$scl90_anxiety)

if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
# interaction questionnaire with chat-type
m.int <- report_table(lmer(aff_score ~ scl90_anxiety * chat+(1|Participant.Private.ID), ratings1_wf[,]))
# effect chat 1
m.chat1 <- report_table(lm(aff_score ~ scl90_anxiety, ratings1_wf[ratings1_wf$chat == "Anxious",]))
# effect chat 2
m.chat2 <- report_table(lm(aff_score ~ scl90_anxiety, ratings1_wf[ratings1_wf$chat == "Nonanxious",]))



# individual items analysis
chat <- stats_one_panel(ratings=ratings1,dep_var="chat-again",ind_var="scl90_anxiety",chats=c("Anxious","Nonanxious"))
m.chat.int <- chat$regressions$m.int
m.chat.anx <- chat$regressions$m.chat1
m.chat.nan <- chat$regressions$m.chat2

diff <- stats_one_panel(ratings=ratings1,dep_var="different",ind_var="scl90_anxiety",chats=c("Anxious","Nonanxious"))
m.diff.int <- diff$regressions$m.int
m.diff.anx <- diff$regressions$m.chat1
m.diff.nan <- diff$regressions$m.chat2

dist <- stats_one_panel(ratings=ratings1,dep_var="distant",ind_var="scl90_anxiety",chats=c("Anxious","Nonanxious"))
m.dist.int <- dist$regressions$m.int
m.dist.anx <- dist$regressions$m.chat1
m.dist.nan <- dist$regressions$m.chat2

enjo <- stats_one_panel(ratings=ratings1,dep_var="enjoy",ind_var="scl90_anxiety",chats=c("Anxious","Nonanxious"))
m.enjo.int <- enjo$regressions$m.int
m.enjo.anx <- enjo$regressions$m.chat1
m.enjo.nan <- enjo$regressions$m.chat2

simi <- stats_one_panel(ratings=ratings1,dep_var="similar",ind_var="scl90_anxiety",chats=c("Anxious","Nonanxious"))
m.simi.int <- simi$regressions$m.int
m.simi.anx <- simi$regressions$m.chat1
m.simi.nan <- simi$regressions$m.chat2

unde <- stats_one_panel(ratings=ratings1,dep_var="understood",ind_var="scl90_anxiety",chats=c("Anxious","Nonanxious"))
m.unde.int <- unde$regressions$m.int
m.unde.anx <- unde$regressions$m.chat1
m.unde.nan <- unde$regressions$m.chat2

# Correction Methods 
hb_corr_1 <- data.frame(chat=m.chat.int$p[4],diff=m.diff.int$p[4],dist=m.dist.int$p[4],
                        enjo=m.enjo.int$p[4],simi=m.simi.int$p[4],unde=m.unde.int$p[4])
hb_corr_1 <- hb_corr_1[order(hb_corr_1)]
# Holm-Bonferroni Method
hb_corr_1 < .05/6:1
# Bonferroni Method
hb_corr_1 < .05/6



# combine
exp1 <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,c(11:13,8)]),
              data.frame(quest="chat-again",effect="Anxious",m.chat.anx[2,c(9:11,8)]),
              data.frame(quest="chat-again",effect="Nonanxious",m.chat.nan[2,c(9:11,8)]),
              data.frame(quest="different",effect="Interaction",m.diff.int[4,c(11:13,8)]),
              data.frame(quest="different",effect="Anxious",m.diff.anx[2,c(9:11,8)]),
              data.frame(quest="different",effect="Nonanxious",m.diff.nan[2,c(9:11,8)]),
              data.frame(quest="distant",effect="Interaction",m.dist.int[4,c(11:13,8)]),
              data.frame(quest="distant",effect="Anxious",m.dist.anx[2,c(9:11,8)]),
              data.frame(quest="distant",effect="Nonanxious",m.dist.nan[2,c(9:11,8)]),
              data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,c(11:13,8)]),
              data.frame(quest="enjoy",effect="Anxious",m.enjo.anx[2,c(9:11,8)]),
              data.frame(quest="enjoy",effect="Nonanxious",m.enjo.nan[2,c(9:11,8)]),
              data.frame(quest="similar",effect="Interaction",m.simi.int[4,c(11:13,8)]),
              data.frame(quest="similar",effect="Anxious",m.simi.anx[2,c(9:11,8)]),
              data.frame(quest="similar",effect="Nonanxious",m.simi.nan[2,c(9:11,8)]),
              data.frame(quest="understood",effect="Interaction",m.unde.int[4,c(11:13,8)]),
              data.frame(quest="understood",effect="Anxious",m.unde.anx[2,c(9:11,8)]),
              data.frame(quest="understood",effect="Nonanxious",m.unde.nan[2,c(9:11,8)]))
p_vals_1 <- c("p < .001", "p = .003", "p < .001", "p < .001")
exp1$p <- pValuesCategories(exp1$p)
# write.csv(exp1, "figures/stats_exp1.csv", row.names = F)






# create affiliation score
ratings2$aff_score <- ratings2$Response - 3
ratings2$aff_score <- ifelse(ratings2$quest=="different"|ratings2$quest=="distant",
                             -1*ratings2$aff_score,ratings2$aff_score) 
# quick visualization of the affiliation score 
ggplot(ratings2, aes(x=bfi44_extraversion,y=aff_score,col=chat)) + 
  geom_smooth(method="lm",se=F) +
  facet_wrap(.~quest)
ggplot(ratings2, aes(x=bfi44_extraversion,y=aff_score,col=chat)) + 
  geom_smooth(method="lm",se=F)

# affiliation score average (primary outcome)
ratings2_wf <- ratings2 %>% group_by(Participant.Private.ID,bfi44_extraversion,chat) %>%
  summarise(aff_score=mean(aff_score))
ratings2_wf$bfi44_extraversion <- ratings2_wf$bfi44_extraversion/max(ratings2_wf$bfi44_extraversion)

# interaction questionnaire with chat-type
m.int <- report_table(lmer(aff_score ~ bfi44_extraversion * chat+(1|Participant.Private.ID), ratings2_wf[,]))
# effect chat 1
m.chat1 <- report_table(lm(aff_score ~ bfi44_extraversion, ratings2_wf[ratings2_wf$chat == "Extrovert",]))
# effect chat 2
m.chat2 <- report_table(lm(aff_score ~ bfi44_extraversion, ratings2_wf[ratings2_wf$chat == "Introvert",]))



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
hb_corr_2 <- data.frame(chat=m.chat.dif$p[4],diff=m.diff.dif$p[4],dist=m.dist.dif$p[4],
                        enjo=m.enjo.dif$p[4],simi=m.simi.dif$p[4],unde=m.unde.dif$p[4])
hb_corr_2 <- hb_corr_2[order(hb_corr_2)]
# Holm-Bonferroni Method
hb_corr_2 < .05/6:1
# Bonferroni Method
hb_corr_2 < .05/6

# combine
exp2 <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.dif[4,c(11:13,8)]),
              data.frame(quest="chat-again",effect="Extrovert",m.chat.ext[2,c(9:11,8)]),
              data.frame(quest="chat-again",effect="Introvert",m.chat.int[2,c(9:11,8)]),
              data.frame(quest="different",effect="Interaction",m.diff.dif[4,c(11:13,8)]),
              data.frame(quest="different",effect="Extrovert",m.diff.ext[2,c(9:11,8)]),
              data.frame(quest="different",effect="Introvert",m.diff.int[2,c(9:11,8)]),
              data.frame(quest="distant",effect="Interaction",m.dist.dif[4,c(11:13,8)]),
              data.frame(quest="distant",effect="Extrovert",m.dist.ext[2,c(9:11,8)]),
              data.frame(quest="distant",effect="Introvert",m.dist.int[2,c(9:11,8)]),
              data.frame(quest="enjoy",effect="Interaction",m.enjo.dif[4,c(11:13,8)]),
              data.frame(quest="enjoy",effect="Extrovert",m.enjo.ext[2,c(9:11,8)]),
              data.frame(quest="enjoy",effect="Introvert",m.enjo.int[2,c(9:11,8)]),
              data.frame(quest="similar",effect="Interaction",m.simi.dif[4,c(11:13,8)]),
              data.frame(quest="similar",effect="Extrovert",m.simi.ext[2,c(9:11,8)]),
              data.frame(quest="similar",effect="Introvert",m.simi.int[2,c(9:11,8)]),
              data.frame(quest="understood",effect="Interaction",m.unde.dif[4,c(11:13,8)]),
              data.frame(quest="understood",effect="Extrovert",m.unde.ext[2,c(9:11,8)]),
              data.frame(quest="understood",effect="Introvert",m.unde.int[2,c(9:11,8)]))
p_vals_2 <- c("p = .004")
exp2$p <- pValuesCategories(exp2$p)
# write.csv(exp2, "figures/stats_exp2.csv", row.names = F)






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # A Panels (Likert scales and questionnaires) # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # main outcome Experiment 1 # # # 
(figure2A <- ggplot(ratings1_wf, aes(x=scl90_anxiety,y=aff_score,col=chat,shape=chat,
                                  linetype=chat)) +
    labs(title = "Participants' Judgements", 
         y="Affiliation Score", x="Anxiety (SCL-90R)",
         col = "LLM type:", shape = "LLM type:",
         linetype = "LLM type:") +
    # geom_jitter(height = .05, alpha = .1, stroke = 0, size = 1.5) +
    geom_point(alpha = .1, stroke = 0, size = 1.5) +
    geom_smooth(method="lm", se = F, size = 1) +
    annotate("text",x=c(.5,.95),y=c(1.5,1.6),label=c("**","**"),col=c("black","#0072B2"),
             size=c(10,7)) +
    scale_shape_manual(values = c(17, 19)) +
    # scale_linetype_manual(values = c("solid","dashed")) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) + 
    scale_x_continuous(breaks = c(0, .5, 1), ) +
    # scale_y_continuous(breaks = c(-2:2), labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
    scale_y_continuous(breaks = c(-2:2), labels = c("-2","","0","","+2")) +
    coord_cartesian(ylim = c(-2, 2)) +
    theme_classic() + 
    theme(legend.position = c(.8,.15),
          legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)



# # # individual items Experiment 1 # # #
ratings1$quest <- factor(ratings1$question, levels = c("similar","understood","chat-again",
                                                       "enjoy","different","distant"))
ratings1$scl90_anxiety <- ratings1$scl90_anxiety/max(ratings1$scl90_anxiety)

# ann_text <- data.frame(lab="Text",scl90_anxiety = rep(.5,4), Response = c(4.5,4),
#                        quest = factor(c("similar","understood","different","distant"), 
#                                       levels = c("similar","understood","chat-again",
#                                                  "enjoy","different","distant")),
#                        chat = c("Anxious","Nonanxious"))
# ann_text_anx <- data.frame(lab="Text",scl90_anxiety = c(.94,.95,.94,.95), Response = c(4.5,4.3,1.5,1.5),
#                            quest = factor(c("similar","understood","different","distant"),
#                                           levels = c("similar","understood","chat-again",
#                                                      "enjoy","different","distant")),
#                            chat = c("Anxious","Nonanxious"))
# ann_text_non <- data.frame(lab="Text",scl90_anxiety = c(.95), Response = c(4.4),
#                            quest = factor(c("different"),
#                                           levels = c("similar","understood","chat-again",
#                                                      "enjoy","different","distant")),
#                            chat = c("Anxious","Nonanxious"))
# (figure2A <- ggplot(ratings1, aes(x=scl90_anxiety,y=Response,col=chat,shape=chat,
#                                   linetype=chat)) +
#   labs(title = "Participants' Judgements",
#        y="Likert Scale", x="Anxiety (SCL-90R)",
#        col = "LLM type:", shape = "LLM type:",
#        linetype = "LLM type:") +
#   geom_point(alpha = .1, stroke = 0, size = 1.5) +
#   geom_smooth(method="lm", se = F, size = 1) +
#   geom_text(data = ann_text,label = c("***","**","***","***"),col="black", size = 10) +
#   # geom_text(data = ann_text, label = p_vals_1, col="black", size = 3) +
#   geom_text(data=ann_text_anx, label =  c("***","**","***","**"),col="#0072B2", size = 7) +
#   geom_text(data=ann_text_non, label =  c("*"),col="#D55E00", size = 7) +
#   scale_shape_manual(values = c(17, 19)) +
#   # scale_linetype_manual(values = c("solid","dashed")) +
#   scale_colour_manual(values = c("#0072B2", "#D55E00")) +
#   scale_x_continuous(breaks = c(0, .5, 1), ) +
#   scale_y_continuous(breaks = 1:5, labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
#   coord_cartesian(ylim = c(1, 5)) +
#   facet_wrap(. ~ quest, ncol = 2, labeller = labeller(
#     quest = c("chat-again" = "I would chat with\n them again",
#               "different" = "I felt that they were\n different from me",
#                "similar" = "I felt that we\n are similar",
#                "enjoy" = "I enjoyed our\n conversation",
#                "distant" = "I felt distant\n from them",
#                "understood" = "I felt that they\n understood me"))) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         legend.background = element_rect(colour='black',fill='white',linetype='solid'))
# )



# # # main outcome Experiment 2 # # # 
(figure3A <- ggplot(ratings2_wf, aes(x=bfi44_extraversion,y=aff_score,col=chat,shape=chat,
                                  linetype=chat)) +
    labs(title = "Participants' Judgements", 
         y="Affiliation Score", x="Extroversion (BFI-44)",
         col = "LLM type:", shape = "LLM type:",
         linetype = "LLM type:") +
    # geom_jitter(height = .05, alpha = .1, stroke = 0, size = 1.5) +
    geom_point(alpha = .1, stroke = 0, size = 1.5) +
    geom_smooth(method="lm", se = F, size = 1) +
    annotate("text",x=c(.5,.95),y=c(1.5,1.3),label=c("*","*"),col=c("black","#009E73"),
             size=c(10,7)) +
    scale_shape_manual(values = c(17, 19)) +
    # scale_linetype_manual(values = c("solid","dashed")) +
    scale_colour_manual(values = c("#009E73","#CC79A7")) + 
    scale_x_continuous(breaks = c(0, .5, 1), ) +
    # scale_y_continuous(breaks = c(-2:2), labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
    scale_y_continuous(breaks = c(-2:2), labels = c("-2","","0","","+2")) +
    coord_cartesian(ylim = c(-2, 2),xlim = c(0,1)) +
    theme_classic() + 
    theme(legend.position = c(.2,.15),
          legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)



# # # individual items Experiment 2 # # #
ratings2$quest <- factor(ratings2$question, levels = c("similar","understood","chat-again",
                                                       "enjoy","different","distant"))
ratings2$bfi44_extraversion <- ratings2$bfi44_extraversion/max(ratings2$bfi44_extraversion)

# ann_text <- data.frame(lab="Text",bfi44_extraversion = rep(.5,4), Response = rep(4.5,4),
#                        quest = factor(c("similar"), 
#                                       levels = c("similar","understood","chat-again",
#                                                  "enjoy","different","distant")),
#                        chat = c("Extrovert","Introvert"))
# ann_text_ext <- data.frame(lab="Text",bfi44_extraversion = c(.95,.95,.95,.95), Response = c(4.5,4.5,4.5,1.5),
#                            quest = factor(c("similar","chat-again","enjoy","different"),
#                                           levels = c("similar","understood","chat-again",
#                                                      "enjoy","different","distant")),
#                            chat = c("Extrovert","Introvert"))
# (figure3A <- ggplot(ratings2, aes(x=bfi44_extraversion,y=Response,col=chat,shape=chat,
#                                   linetype=chat)) +
#   labs(title = "Participants' Judgements",
#        y="Likert Scale", x="Extroversion (BFI-44)",
#        col = "LLM type:", shape = "LLM type:",
#        linetype = "LLM type:",) +
#   geom_point(alpha = .1, stroke = 0, size = 1.5) +
#   geom_smooth(method="lm", se = F, size = 1) +
#   geom_text(data = ann_text,label = c("**"),col="black", size = 10) +
#   # geom_text(data = ann_text, label = p_vals_2, col="black", size = 3) +
#   geom_text(data = ann_text_ext,label = c("**","*","*","*"),col="#009E73", size = 7) +
#   scale_shape_manual(values = c(17, 19)) +
#   scale_colour_manual(values = c("#009E73","#CC79A7")) +
#   scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
#   scale_y_continuous(breaks = 1:5, labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
#   coord_cartesian(ylim = c(1, 5)) +
#   facet_wrap(. ~ quest, ncol = 2, labeller = labeller(
#     quest = c("chat-again" = "I would chat with\n them again",
#               "different" = "I felt that they were\n different from me",
#               "similar" = "I felt that we\n are similar",
#               "enjoy" = "I enjoyed our\n conversation",
#               "distant" = "I felt distant\n from them",
#               "understood" = "I felt that they\n understood me"))) +
#   theme_classic() +
#     theme(legend.position = "bottom",
#           legend.background = element_rect(colour='black',fill='white',linetype='solid'))
# )


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
if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
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
levels(influence1$chat) <- c("Anxious","Nonanxious")
influence2$chat <- as.factor(influence2$chatType)
levels(influence2$chat) <- c("Extrovert","Introvert")



# aim 1, GPT4 Texts are different between conditions (chatbots) 
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine1.lf[combine1.lf$who=="GPT-4 Texts",]))
report_table(t.test(bots_Positive~chat,combine1[,], paired = TRUE))
report_table(t.test(bots_Neutral~chat,combine1[,], paired = TRUE))
report_table(t.test(bots_Negative~chat,combine1[,], paired = TRUE))
report_table(t.test(bots_Mixed~chat,combine1[,], paired = TRUE))

# run LMER, LM, and aov as "sensitivity" analysis, so everything says the same we are more confident
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine1.lf[combine1.lf$who=="Participants Texts",]))
summary(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine1.lf[combine1.lf$who=="Participants Texts",]))

# aim 2, GPT4 Texts are different between conditions (chatbots)
report_table(t.test(user_Positive~chat,combine1[,], paired = TRUE))
report_table(t.test(user_Neutral~chat,combine1[,], paired = TRUE))
report_table(t.test(user_Negative~chat,combine1[,], paired = TRUE))
report_table(t.test(user_Mixed~chat,combine1[,], paired = TRUE))



ann_text <- data.frame(sentiment = c(1,2,3,4,2,4), prop = c(.7,.7,.5,.95,.8,.95),
                       who = factor(c(rep("GPT-4 Texts",4),rep("Participants Texts",2)),
                                                    levels = c("GPT-4 Texts","Participants Texts")),
                       lab = "Text", chat = c("Anxious","Nonanxious"))
# visualize the average of count for each sentiment and for each chat personality
(figure2B <- ggplot(combine1.lf, aes(x=sentiment,y=prop,col=chat,shape=chat)) + 
    labs(title = "Sentiment Analysis",
         y="Prop. (Sentiment ea Condition)", x = "Text Sentiment Category",
         col = "LLM type:", shape = "LLM type:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0,1)) +
    # geom_violin(position = position_dodge(0.5)) +
    geom_boxplot(alpha=.3,position = position_dodge(0.5)) +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.5)) +
    geom_text(data = ann_text,col="black", size = 7,
              label = c("***","**",rep("***",2),"**","***")) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    scale_shape_manual(values = c(17,19)) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    facet_wrap(. ~ who, nrow = 2) + 
    theme_classic() +
    # guides(color = guide_legend(nrow = 2)) +
    guides(shape = guide_legend(nrow = 2),
           color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", legend.box = "vertical", #c(.3,.89),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
  
)



# aim 1, GPT4 Texts are different between conditions (chatbots) 
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine2.lf[combine2.lf$who=="GPT-4 Texts",]))
report_table(t.test(bots_Positive~chat,combine2[,], paired = TRUE))
report_table(t.test(bots_Neutral~chat,combine2[,], paired = TRUE))
report_table(t.test(bots_Negative~chat,combine2[,], paired = TRUE))
report_table(t.test(bots_Mixed~chat,combine2[,], paired = TRUE))

# run LMER, LM, and aov as "sensitivity" analysis, so everything says the same we are more confident
anova(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine2.lf[combine2.lf$who=="Participants Texts",]))
summary(lmer(prop ~ sentiment * chat + (1|Participant.Private.ID), combine2.lf[combine2.lf$who=="Participants Texts",]))

# aim 2, GPT4 Texts are different between conditions (chatbots)
report_table(t.test(user_Positive~chat,combine2[,], paired = TRUE))
report_table(t.test(user_Neutral~chat,combine2[,], paired = TRUE))
report_table(t.test(user_Negative~chat,combine2[,], paired = TRUE))
report_table(t.test(user_Mixed~chat,combine2[,], paired = TRUE))



ann_text <- data.frame(sentiment = c(2,3,4), prop = c(.2,.7,.95),
                       who = factor(rep("GPT-4 Texts",3),
                                    levels = c("GPT-4 Texts","Participants Texts")),
                       lab = "Text", chat = c("Extrovert","Introvert","Introvert"))
(figure3B <- ggplot(combine2.lf, aes(x=sentiment,y=prop,col=chat,shape=chat)) + 
    labs(title = "Sentiment Analysis",
         y="Prop. (Sentiment ea Condition)", x = "Text Sentiment Category",
         col = "LLM type:", shape = "LLM type:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0,1)) +
    # geom_violin(position = position_dodge(0.5)) +
    geom_boxplot(alpha=.3,position = position_dodge(0.5)) +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.5)) +
    geom_text(data = ann_text,col="black", size = 7,
              label = rep("***",3)) +
    scale_shape_manual(values = c(17,19)) +
    scale_colour_manual(values = c("#009E73","#CC79A7")) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    facet_wrap(. ~ who, nrow = 2) + 
    theme_classic() +
    # guides(color = guide_legend(nrow = 2)) +
    guides(shape = guide_legend(nrow = 2),
           color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", legend.box = "vertical", #c(.3,.89),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # C Panel: Questionnaires and Sentiment Analysis# # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# difference between GPT-4 types, to simplify statistical analysis with triple order interaction
# experiment 1
combine1.wf <- combine1[combine1$chat == "Anxious",]
temp <- combine1[combine1$chat == "Nonanxious",]
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
combine1.lf <- melt(combine1.wf, measure.vars = c("mixed","negative","neutral","positive"))
combine2.lf <- melt(combine2.wf, measure.vars = c("mixed","negative","neutral","positive"))
# change column names
colnames(combine1.lf)[(ncol(combine1.lf)-1):ncol(combine1.lf)] <- c("sentiment","diff")
colnames(combine2.lf)[(ncol(combine2.lf)-1):ncol(combine2.lf)] <- c("sentiment","diff")

# stats experiment 1
anova(lmer(diff ~ sentiment * scl90_anxiety + (1|Participant.Private.ID), combine1.lf))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="mixed",]))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="negative",]))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="neutral",]))
report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine1.lf$sentiment=="positive",]))

levels(combine1.lf$sentiment) <- c("Mixed","Negative","Neutral","Positive")
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
    stat_cor(col="black",label.y=.75,method="pearson",r.accuracy=.01,p.accuracy=.001) +
    theme_classic() +
    facet_wrap(sentiment~., ncol=2) +
    theme(legend.position = "none")
)
cor.test(combine1.lf$diff[combine1.lf$sentiment=="Negative"],
         combine1.lf$scl90_anxiety[combine1.lf$sentiment=="Negative"],method="spearman")



# stats experiment 2
anova(lmer(diff ~ sentiment * bfi44_extraversion + (1|Participant.Private.ID), combine2.lf))
# summary(lmer(diff ~ sentiment * bfi44_extraversion + (1|Participant.Private.ID), combine2.lf))

levels(combine2.lf$sentiment) <- c("Mixed","Negative","Neutral","Positive")
(figure3C <- ggplot(combine2.lf, aes(x=bfi44_extraversion,y=diff,fill=diff)) + 
    labs(title = "Extroversion and Sentiment",
         y=expression(Prop.[Extroversion]-Prop.[Intraversion]), x = "Extroversion (BFI-44)",
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
    stat_cor(col="black",label.y=.5,method="pearson",r.accuracy=.01,p.accuracy=.001) +
    theme_classic() +
    facet_wrap(sentiment~., ncol=2) +
    theme(legend.position = "none")
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Combine Figures # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
fig2 <- annotate_figure(ggarrange(figure2A,figure2B,figure2C,
                                  ncol=3, widths = c(3, 2, 3), labels = c("A","B","C")),
                        top = text_grob("Experiment 1", color = "black", face = "bold", size = 14))
fig2
fig3 <- annotate_figure(ggarrange(figure3A,figure3B,figure3C,
                                  ncol=3, widths = c(3, 2, 3), labels = c("A","B","C")),
                        top = text_grob("Experiment 2", color = "black", face = "bold", size = 14))
fig3
if (print_fig == 1) {
  ggsave("figures/fig2.pdf", fig2, dpi = 2400, scale = .85, units = "cm",
         width = 30, height = 15, bg = "white")
  ggsave("figures/fig3.pdf", fig3, dpi = 2400, scale = .85, units = "cm",
         width = 30, height = 15, bg = "white")
}





# # combine experiment 1 and experiment 2
# exps <- rbind(data.frame(exp="Exp. 1",exp1),data.frame(exp="Exp. 2",exp2))
# exps$effect <- factor(exps$effect, levels = rev(c("Interaction","Introvert","Extrovert","Nonanxious","Anxious")))
# # exps <- exps[exps$exp != "E1-Ext.",]
# exps$exp <- factor(as.character(exps$exp), levels = c("Exp. 2","Exp. 1"))
# # change factor order
# exps$quest <- factor(exps$quest, levels = c("chat-again","different","similar",
#                                             "enjoy","distant","understood"))
# (figS1 <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
#     labs(title = "Experiments Summary and Statistics",
#          y = "Effect Size") +
#     geom_hline(yintercept = 0, col="grey") +
#     geom_point(size=2, position = position_dodge(.6)) +
#     geom_errorbar(aes(ymin=Std_Coefficient_CI_low, ymax=Std_Coefficient_CI_high), 
#                   width=.4, position = position_dodge(.6)) +
#     scale_shape_manual(values = c(17, 19, 17, 19, 15)) +
#     scale_colour_manual(values = c("#0072B2", "#D55E00","#009E73","#CC79A7","black")) + 
#     coord_flip() +
#     facet_wrap(. ~ quest, ncol = 3, labeller = labeller(
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
#   ggsave("figures/figS1.pdf", figS1, dpi = 1200, scale = 1, units = "cm",
#          width = 16, height = 12, bg = "white")
# }



# read effect sizes csvs for all experiments (including the one from script main_e3.R)
exp1 <- read.csv("figures/stats_exp1.csv")
exp2 <- read.csv("figures/stats_exp2.csv")
exp3 <- read.csv("figures/stats_exp3.csv")
# combine experiment 1, 2, and 3
exps <- rbind(data.frame(exp="Exp. 1",exp1),
              data.frame(exp="Exp. 2",exp2),
              data.frame(exp="Exp. 3",exp3))
exps$effect <- factor(exps$effect, levels = c("Interaction","Anxious","Nonanxious",
                                              "Extrovert","Introvert","Self-Antiself"))
exps$exp <- factor(as.character(exps$exp), levels = c("Exp. 3","Exp. 2","Exp. 1"))
# change factor order
exps$quest <- factor(exps$quest, levels = c("chat-again","different","similar",
                                            "enjoy","distant","understood"))
(figS1 <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
    labs(title = "Experiments Summary and Effect Sizes",
         y = "Effect Size") +
    geom_hline(yintercept = 0, col="grey") +
    geom_point(size=2, position = position_dodge(.6)) +
    geom_errorbar(aes(ymin=Std_Coefficient_CI_low, ymax=Std_Coefficient_CI_high), 
                  width=.4, position = position_dodge(.6)) +
    scale_y_continuous(breaks = c(-.6,0,.6)) +
    scale_shape_manual(values = c(15, 17, 19, 17, 19, 17)) +
    scale_colour_manual(values = c("black","#0072B2", "#D55E00",
                                   "#009E73","#CC79A7","grey")) + 
    coord_flip() +
    facet_wrap(. ~ quest, ncol = 3, labeller = labeller(
      quest = c("chat-again" = "I would chat with\n them again",
                "different" = "I felt that they were\n different from me",
                "similar" = "I felt that we\n are similar",
                "enjoy" = "I enjoyed our\n conversation",
                "distant" = "I felt distant\n from them",
                "understood" = "I felt that they\n understood me"))) +
    theme_classic() + 
    guides(color = guide_legend(nrow = 1, byrow = TRUE),
           shape = guide_legend(nrow = 1, byrow = TRUE)) + 
    theme(legend.position = "bottom",
          # legend.box = "horizontal",
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          legend.background = element_rect(colour='black',fill='white',linetype='solid')))
if (print_fig == 1) {
  ggsave("figures/figS1.pdf", figS1, dpi = 1200, scale = 1, units = "cm",
         width = 16, height = 12, bg = "white")
}


