rm(list=ls(all=TRUE))
# data cleaned by Riddhi and Santiago check script cleaning.R

# interaction via One Reach (thanks Daniel!)
# behaviours in ratings
ratings1 <- read.csv("experiment1/cleaned/ratings.csv")
ratings2 <- read.csv("experiment2/cleaned/ratings.csv")
# file with gorilla IDs and OneReach IDs
task1 <- read.csv("experiment1/cleaned/task.csv")
task1 <- task1[order(task1$Participant.Private.ID),]
task2 <- read.csv("experiment2/cleaned/task.csv")
task2 <- task2[order(task2$Participant.Private.ID),]
# interaction via One Reach (thanks Daniel!)
chat1 <- read.csv("experiment1/cleaned/gptstudydata_cleaned_2024.csv")
chat2 <- read.csv("experiment2/cleaned/gptstudydata_S2B_Dec2024.csv")

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
ratings1 <- addQuestionnaireToRating_e1(ratings1, scl90, bfi10)
ratings2 <- addQuestionnaireToRating_e2(ratings2, bfi44)

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

source("functions.R")
# use cleaning function to extract summary information for the interactions
combine1 <- summariseChatInteraction(task1, chat1, ratings1)
combine2 <- summariseChatInteraction(task2, chat2, ratings2)

# rows are conditional probabilities of the sentiment analysis, thus cells from 
# the transition matrices
influence1 <- combine1$influence
influence2 <- combine2$influence

# rows are chats in long format. It also contains the descriptions of the chats 
# as well as ratings in wide format
combine1 <- combine1$combine
combine2 <- combine2$combine



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Statistical Analysis# # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# stats
library(lmerTest)
library(report)
m.chat.int <- report_table(lmer(Response ~ scl90_anxiety * chat + (1|Participant.Private.ID),
                   ratings1[ratings1$question == "chat-again",]))
m.chat.anx <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "chat-again" & ratings1$chat == "Anxious",]))
m.chat.nan <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "chat-again" & ratings1$chat == "Non-Anxious",]))

m.diff.int <- report_table(lmer(Response ~ scl90_anxiety * chat + (1|Participant.Private.ID), 
                   ratings1[ratings1$question == "different",]))
m.diff.anx <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "different" & ratings1$chat == "Anxious",]))
m.diff.nan <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "different" & ratings1$chat == "Non-Anxious",]))

m.dist.int <- report_table(lmer(Response ~ scl90_anxiety * chat + (1|Participant.Private.ID), 
                   ratings1[ratings1$question == "distant",]))
m.dist.anx <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "distant" & ratings1$chat == "Anxious",]))
m.dist.nan <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "distant" & ratings1$chat == "Non-Anxious",]))

m.enjo.int <- report_table(lmer(Response ~ scl90_anxiety * chat + (1|Participant.Private.ID), 
                   ratings1[ratings1$question == "enjoy",]))
m.enjo.anx <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "enjoy" & ratings1$chat == "Anxious",]))
m.enjo.nan <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "enjoy" & ratings1$chat == "Non-Anxious",]))

m.simi.int <- report_table(lmer(Response ~ scl90_anxiety * chat + (1|Participant.Private.ID), 
                   ratings1[ratings1$question == "similar",]))
m.simi.anx <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "similar" & ratings1$chat == "Anxious",]))
m.simi.nan <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "similar" & ratings1$chat == "Non-Anxious",]))

m.unde.int <- report_table(lmer(Response ~ scl90_anxiety * chat + (1|Participant.Private.ID), 
                   ratings1[ratings1$question == "understood",]))
m.unde.anx <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "understood" & ratings1$chat == "Anxious",]))
m.unde.nan <- report_table(lm(Response ~ scl90_anxiety,
                 ratings1[ratings1$question == "understood" & ratings1$chat == "Non-Anxious",]))

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



m.chat.int <- report_table(lmer(Response ~ bfi10_extraversion * chat + (1|Participant.Private.ID),
                                ratings1[ratings1$question == "chat-again",]))
m.chat.anx <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "chat-again" & ratings1$chat == "Anxious",]))
m.chat.nan <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "chat-again" & ratings1$chat == "Non-Anxious",]))

m.diff.int <- report_table(lmer(Response ~ bfi10_extraversion * chat + (1|Participant.Private.ID), 
                                ratings1[ratings1$question == "different",]))
m.diff.anx <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "different" & ratings1$chat == "Anxious",]))
m.diff.nan <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "different" & ratings1$chat == "Non-Anxious",]))

m.dist.int <- report_table(lmer(Response ~ bfi10_extraversion * chat + (1|Participant.Private.ID), 
                                ratings1[ratings1$question == "distant",]))
m.dist.anx <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "distant" & ratings1$chat == "Anxious",]))
m.dist.nan <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "distant" & ratings1$chat == "Non-Anxious",]))

m.enjo.int <- report_table(lmer(Response ~ bfi10_extraversion * chat + (1|Participant.Private.ID), 
                                ratings1[ratings1$question == "enjoy",]))
m.enjo.anx <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "enjoy" & ratings1$chat == "Anxious",]))
m.enjo.nan <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "enjoy" & ratings1$chat == "Non-Anxious",]))

m.simi.int <- report_table(lmer(Response ~ bfi10_extraversion * chat + (1|Participant.Private.ID), 
                                ratings1[ratings1$question == "similar",]))
m.simi.anx <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "similar" & ratings1$chat == "Anxious",]))
m.simi.nan <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "similar" & ratings1$chat == "Non-Anxious",]))

m.unde.int <- report_table(lmer(Response ~ bfi10_extraversion * chat + (1|Participant.Private.ID), 
                                ratings1[ratings1$question == "understood",]))
m.unde.anx <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "understood" & ratings1$chat == "Anxious",]))
m.unde.nan <- report_table(lm(Response ~ bfi10_extraversion,
                              ratings1[ratings1$question == "understood" & ratings1$chat == "Non-Anxious",]))

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



m.chat.int <- report_table(lmer(Response ~ bfi44_extraversion * chat + (1|Participant.Private.ID),
                                ratings2[ratings2$question == "chat-again",]))
m.chat.anx <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "chat-again" & ratings2$chat == "Extrovert",]))
m.chat.nan <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "chat-again" & ratings2$chat == "Introvert",]))

m.diff.int <- report_table(lmer(Response ~ bfi44_extraversion * chat + (1|Participant.Private.ID), 
                                ratings2[ratings2$question == "different",]))
m.diff.anx <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "different" & ratings2$chat == "Extrovert",]))
m.diff.nan <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "different" & ratings2$chat == "Introvert",]))

m.dist.int <- report_table(lmer(Response ~ bfi44_extraversion * chat + (1|Participant.Private.ID), 
                                ratings2[ratings2$question == "distant",]))
m.dist.anx <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "distant" & ratings2$chat == "Extrovert",]))
m.dist.nan <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "distant" & ratings2$chat == "Introvert",]))

m.enjo.int <- report_table(lmer(Response ~ bfi44_extraversion * chat + (1|Participant.Private.ID), 
                                ratings2[ratings2$question == "enjoy",]))
m.enjo.anx <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "enjoy" & ratings2$chat == "Extrovert",]))
m.enjo.nan <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "enjoy" & ratings2$chat == "Introvert",]))

m.simi.int <- report_table(lmer(Response ~ bfi44_extraversion * chat + (1|Participant.Private.ID), 
                                ratings2[ratings2$question == "similar",]))
m.simi.anx <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "similar" & ratings2$chat == "Extrovert",]))
m.simi.nan <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "similar" & ratings2$chat == "Introvert",]))

m.unde.int <- report_table(lmer(Response ~ bfi44_extraversion * chat + (1|Participant.Private.ID), 
                                ratings2[ratings2$question == "understood",]))
m.unde.anx <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "understood" & ratings2$chat == "Extrovert",]))
m.unde.nan <- report_table(lm(Response ~ bfi44_extraversion,
                              ratings2[ratings2$question == "understood" & ratings2$chat == "Introvert",]))

exp2 <- rbind(data.frame(quest="chat-again",effect="Interaction",m.chat.int[4,11:13]),
              data.frame(quest="chat-again",effect="Extrovert",m.chat.anx[2,9:11]),
              data.frame(quest="chat-again",effect="Introvert",m.chat.nan[2,9:11]),
              data.frame(quest="different",effect="Interaction",m.diff.int[4,11:13]),
              data.frame(quest="different",effect="Extrovert",m.diff.anx[2,9:11]),
              data.frame(quest="different",effect="Introvert",m.diff.nan[2,9:11]),
              data.frame(quest="distant",effect="Interaction",m.dist.int[4,11:13]),
              data.frame(quest="distant",effect="Extrovert",m.dist.anx[2,9:11]),
              data.frame(quest="distant",effect="Introvert",m.dist.nan[2,9:11]),
              data.frame(quest="enjoy",effect="Interaction",m.enjo.int[4,11:13]),
              data.frame(quest="enjoy",effect="Extrovert",m.enjo.anx[2,9:11]),
              data.frame(quest="enjoy",effect="Introvert",m.enjo.nan[2,9:11]),
              data.frame(quest="similar",effect="Interaction",m.simi.int[4,11:13]),
              data.frame(quest="similar",effect="Extrovert",m.simi.anx[2,9:11]),
              data.frame(quest="similar",effect="Introvert",m.simi.nan[2,9:11]),
              data.frame(quest="understood",effect="Interaction",m.unde.int[4,11:13]),
              data.frame(quest="understood",effect="Extrovert",m.unde.anx[2,9:11]),
              data.frame(quest="understood",effect="Introvert",m.unde.nan[2,9:11]))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Visualization # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# combine experiment 1 and experiment 2
exps <- rbind(data.frame(exp="Exp. 1",exp1),data.frame(exp="Exp. 2",exp2))
exps$effect <- factor(exps$effect, levels = rev(c("Interaction","Introvert","Extrovert","Non-Anxious","Anxious")))
# exps <- exps[exps$exp != "E1-Ext.",]
exps$exp <- factor(as.character(exps$exp), levels = c("Exp. 2","Exp. 1"))
# levels(exps$exp) <- c("Exp. 1", "Exp. 2")
# change factor order
exps$quest <- factor(exps$quest, levels = c("chat-again","different","similar",
                                            "enjoy","distant","understood"))

library(ggplot2)
(fig4 <- ggplot(exps, aes(x=exp,y=Std_Coefficient,col=effect,shape=effect)) +
    labs(title = "Both Experiments, Statistics and Summary", y = "Effect Size") +
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
ggsave("figures/fig4.pdf", fig4, dpi = 1200, scale = 1, units = "cm",
       width = 16, height = 12, bg = "white")






# visualize normal behavior
ratings1$scl90_anxiety <- ratings1$scl90_anxiety/max(ratings1$scl90_anxiety) 
(figure2A <- ggplot(ratings1, aes(x=scl90_anxiety,y=Response,col=chat,shape=chat)) +
  labs(title = "Exp. 1: Chatbots' Judgements", 
       y="Likert Scale", x="Anxiety (SCL-90R)",
       col = "Chatbot", shape = "Chatbot") +
  geom_point(alpha = .1, stroke = 0, size = 1.5) +
  geom_smooth(method="lm", se = F, size = 1) +
  scale_shape_manual(values = c(17, 19)) +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) + 
  scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                     labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
  facet_wrap(. ~ quest, labeller = labeller(
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

ratings2$bfi44_extraversion <- ratings2$bfi44_extraversion/max(ratings2$bfi44_extraversion)
(figure3A <- ggplot(ratings2, aes(x=bfi44_extraversion,y=Response,col=chat,shape=chat)) +
  labs(title = "Exp. 2: Chatbots' Judgements", 
       y="Likert Scale", x="Extraversion (BFI-44)",
       col = "Chatbot", shape = "Chatbot") +
  geom_point(alpha = .1, stroke = 0, size = 1.5) +
  geom_smooth(method="lm", se = F, size = 1) +
  scale_shape_manual(values = c(17, 19)) +
  scale_colour_manual(values = c("#009E73","#CC79A7")) + 
  scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                     labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
  facet_wrap(. ~ quest, labeller = labeller(
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
# # # # # Sentiment Analysis and Influence# # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# options(scipen = 999) # in order to get non-scientific notation
length(unique(chat1$userid))
length(unique(task1$Response))

length(unique(chat2$userid))
length(unique(task2$Response))

# reshape data frame so we can easy visualize (melt by the count sentiment analysis
# for each level (mixed, negative, neutral, and positive) for both user and bot
library(reshape2)
combine1.lf <- melt(combine1, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                               "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
combine2.lf <- melt(combine2, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                               "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
# change column name
colnames(combine1.lf)[ncol(combine1.lf)] <- c("count") 
colnames(combine2.lf)[ncol(combine2.lf)] <- c("count") 
# transform variable to character
combine1.lf$variable<- as.character(combine1.lf$variable)
combine2.lf$variable<- as.character(combine2.lf$variable)
# split string to get the first 4 character (user or bots)
combine1.lf$who <- factor(substr(combine1.lf$variable,1,4),levels = c("user","bots"))
combine2.lf$who <- factor(substr(combine2.lf$variable,1,4),levels = c("user","bots"))
# add a nicer names
levels(combine1.lf$who) <- c("User Texts","Bot Texts")
levels(combine2.lf$who) <- c("User Texts","Bot Texts")
# add the sentiment on column variable (created with the columns in melt)
combine1.lf$sentiment <- substr(combine1.lf$variable,6,nchar(combine1.lf$variable))
combine2.lf$sentiment <- substr(combine2.lf$variable,6,nchar(combine2.lf$variable))

# add chat as factor with specific order
# combines
combine1.lf$chat <- as.factor(combine1.lf$chatType)
levels(combine1.lf$chat) <- c("Anxious","Non-Anxious")
combine2.lf$chat <- as.factor(combine2.lf$chatType)
levels(combine2.lf$chat) <- c("Extrovert","Introvert")
# influence
influence1$chat <- as.factor(influence1$chatType)
levels(influence1$chat) <- c("Anxious","Non-Anxious")
influence2$chat <- as.factor(influence2$chatType)
levels(influence2$chat) <- c("Extrovert","Introvert")



# univariate statistical analysis
report_table(lm(user_Positive~chat,combine1))
report_table(lm(user_Neutral~chat,combine1))
report_table(lm(user_Negative~chat,combine1))
report_table(lm(user_Mixed~chat,combine1))

ann_text <- data.frame(sentiment = c(2,4), count = c(5,10),
                       lab = "Text", who = factor("User Texts",levels = c("User Texts","Bot Texts")),
                       chat = c("Anxious","Non-Anxious"))
# visualize the average of count for each sentiment and for each chat personality
(figure2B <- ggplot(combine1.lf, aes(x=sentiment,y=count,col=chat,shape=chat)) + 
    labs(title = "Exp. 1: Sentiment Analysis",
         y="Average Count", x = "Text Sentiment Category",
         col = "Chatbot", shape = "Chatbot") +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.3)) +
    geom_text(data = ann_text,label = "*", col="black", size = 10) +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) + 
    scale_shape_manual(values = c(17,19)) +
    facet_grid(. ~ who) + 
    theme_classic() +
    theme(legend.position = "bottom", #legend.position = c(.7,.7),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)

report_table(lm(user_Positive~chat,combine2))
report_table(lm(user_Neutral~chat,combine2))
report_table(lm(user_Negative~chat,combine2))
report_table(lm(user_Mixed~chat,combine2))

(figure3B <- ggplot(combine2.lf, aes(x=sentiment,y=count,col=chat,shape=chat)) + 
    labs(title = "Exp. 2: Sentiment Analysis",
         y="Average Count", x = "Text Sentiment Category",
         col = "Chatbot", shape = "Chatbot") +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.3)) +
    scale_shape_manual(values = c(17,19)) +
    scale_colour_manual(values = c("#009E73","#CC79A7")) + 
    facet_grid(. ~ who) + 
    theme_classic() +
    theme(legend.position = "bottom", #legend.position = c(.7,.7),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.background = element_rect(colour='black',fill='white',linetype='solid'))
)



influence1 <- influence1[influence1$direction != "",]
influence1$direction <- as.factor(influence1$direction)
levels(influence1$direction) <- c("User(t-1) --> Bot(t)","Bot(t-1) --> User(t)")
(p_inf_1 <- ggplot(influence1, aes(x=target,y=value,col=direction)) + stat_summary() +
  labs(title = "Exp. 1: Chat Influence",
       x = "Transition", y = "Probability", col = "Direction") +
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1)) + 
  facet_grid(.~chat) + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8)))



influence2 <- influence2[influence2$direction != "",]
influence2$direction <- as.factor(influence2$direction)
levels(influence2$direction) <- c("User(t-1) --> Bot(t)","Bot(t-1) --> User(t)")
(p_inf_2 <- ggplot(influence2, aes(x=target,y=value,col=direction)) + stat_summary() +
  labs(title = "Exp. 2: Chat Influence",
       x = "Transition", y = "Probability", col = "Direction") +
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1)) + 
  facet_grid(.~chat) + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8)))





library(dplyr)
tild_1 <- data.frame(influence1 %>% group_by(from,to,chatType,direction,target) %>% 
                       summarise(mean=mean(value)))
tild_2 <- data.frame(influence2 %>% group_by(from,to,chatType,direction,target) %>%
                       summarise(mean=mean(value)))
# tild_1 <- influence1[influence1$chatId=="6886",]
ggplot(tild_1, 
       aes(x=from,y=to,fill=mean,label=round(mean,2))) + 
  labs(title="Exp. 1: Probability of Current given Previous",
       x="Previous(t-1)",y="Current(t)",fill="P(x)",label="P(x)") + 
  geom_tile() + geom_text(col="red") +
  facet_grid(chatType~direction) + theme_classic()
ggplot(tild_2, 
       aes(x=from,y=to,fill=mean,label=round(mean,2))) + 
  labs(title="Exp. 2: Probability of Current given Previous",
       x="Previous(t-1)",y="Current(t)",fill="P(x)",label="P(x)") + 
  geom_tile() + geom_text(col="red") +
  facet_grid(chatType~direction) + theme_classic()






combine1.lf <- melt(combine1, measure.vars = c("chat-again","different","similar","enjoy","distant","understood"))
combine2.lf <- melt(combine2, measure.vars = c("chat-again","different","similar","enjoy","distant","understood"))
# change column name
colnames(combine1.lf)[ncol(combine1.lf)] <- c("rating") 
colnames(combine2.lf)[ncol(combine2.lf)] <- c("rating") 

plotWhatMakesLikert <- function (combine.lf, title, x_var, x_label) {
  combine.lf$x_axis <- combine.lf[,x_var]
  return(ggplot(combine.lf, aes(x=x_axis,y=rating)) + 
           labs(title=title,x = x_label, y = "Likert Scale") +
           geom_point(alpha=0.2,col="grey") +
           geom_smooth(method = "lm",se=F) + stat_cor() + 
           scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                              labels = c("Strongly\n Disagree","","Neutral","","Strongly\n Agree")) +
           facet_wrap(variable~., labeller = labeller(
             variable = c("chat-again" = "I would chat with\n them again",
                          "different" = "I felt that they were\n different from me",
                          "similar" = "I felt that we\n are similar",
                          "enjoy" = "I enjoyed our\n conversation",
                          "distant" = "I felt distant\n from them",
                          "understood" = "I felt that they\n understood me"))) + 
           theme_classic())
}

plotWhatMakesLikert(combine1.lf,title="Exp. 1: Num. Interactions",
                    x_var="num_interactions",x_label="Number of Intearactions")
plotWhatMakesLikert(combine1.lf,title="Exp. 1: Same Sentiment",
                    x_var="perc_mirror",x_label="p(Mirrored Interactions)")
plotWhatMakesLikert(combine1.lf,title="Exp. 1: GPT Average Word",
                    x_var="bots_mean_words",x_label="Bot Mean Words")
plotWhatMakesLikert(combine1.lf,title="Exp. 1: User Average Word",
                    x_var="user_mean_words",x_label="User Mean Words")

plotWhatMakesLikert(combine2.lf,title="Exp. 2: Num. Interactions",
                    x_var="num_interactions",x_label="Number of Intearactions")
plotWhatMakesLikert(combine2.lf,title="Exp. 2: Same Sentiment",
                    x_var="perc_mirror",x_label="p(Mirrored Interactions)")
plotWhatMakesLikert(combine2.lf,title="Exp. 2: GPT Average Word",
                    x_var="bots_mean_words",x_label="Bot Mean Words")
plotWhatMakesLikert(combine2.lf,title="Exp. 2: User Average Word",
                    x_var="user_mean_words",x_label="User Mean Words")






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Combine Figures # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
library(ggpubr)
fig2 <- ggarrange(ggarrange(figure2A,figure2B, ncol=2, widths = c(2,1),
                            labels = c("A","B")))
# fig2
ggsave("figures/fig2.pdf", fig2, dpi = 2400, scale = 1, units = "cm",
       width = 24, height = 16, bg = "white")
fig3 <- ggarrange(ggarrange(figure3A,figure3B, ncol=2, widths = c(2,1),
                            labels = c("A","B")))
# fig3
ggsave("figures/fig3.pdf", fig3, dpi = 2400, scale = 1, units = "cm",
       width = 24, height = 16, bg = "white")




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