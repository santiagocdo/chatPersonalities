rm(list=ls(all=TRUE))
# data cleaned by Riddhi and Santiago check script cleaning.R

# interaction via One Reach (thanks Daniel!)
# behaviours in ratings
# ratings1 <- read.csv("experiment1/cleaned/ratings.csv")
ratings2 <- read.csv("experiment2/cleaned/ratings.csv")
# file with gorilla IDs and OneReach IDs
# task1 <- read.csv("experiment1/cleaned/task.csv")
# task1 <- task1[order(task1$Participant.Private.ID),]
task2 <- read.csv("experiment2/cleaned/task.csv")
task2 <- task2[order(task2$Participant.Private.ID),]
# interaction via One Reach (thanks Daniel!)
# chat1 <- read.csv("experiment1/cleaned/gptstudydata_cleaned_2024.csv")
chat2 <- read.csv("experiment2/gptstudydata_S2B_Dec2024.csv")

# bfi44
bfi44 <- read.csv("experiment2/cleaned/bfi44.csv")


source("functions.R")
quest <- scoreQuestionnaires_e2(bfi44)
bfi44 <- quest$bfi

# add questionnaires to ratings
ratings <- addQuestionnaireToRating_e2(ratings2,bfi44)

# add chat as factor with specific order
ratings$chat <- as.factor(ratings$chatType)
levels(ratings$chat) <- c("Extrovert","Introvert")


# visualize normal behavior
library(ggplot2)
(fig2A <- ggplot(ratings,aes(x=question,y=Response,col=chat,shape=chat)) + 
    geom_hline(yintercept = 3, col="grey50") +
    labs(y="Likert Scale", x="Question",
         col = "Bot \nPersonality",shape = "Bot \nPersonality") +
    stat_summary() + 
    scale_shape_manual(values = c(17,19)) +
    scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
                                                "Neutral","Agree","Strongly Agree")) +
    coord_cartesian(ylim = c(1.5,4.5)) +
    # facet_grid(.~order) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
)



ggplot(ratings, aes(x=bfi44_extraversion,y=Response,col=chat)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  facet_wrap(.~question) +
  theme_classic()

library(lmerTest)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

length(unique(chat2$userid))
length(unique(task2$Response))

source("functions.R")
# use cleaning function to extract summary information for the interactions
combine <- summariseChatInteraction(task2, chat2)

# chat-again  different    distant      enjoy    similar understood 
outcome <- addRatingsToInteractions(combine, ratings, likert = "understood")

summary(lm(likert~chatType*bfi44_extraversion, outcome))
library(lmerTest)
summary(lmer(likert~chatType*bfi44_extraversion+(1|Participant.Private.ID),REML=F,outcome))
ggplot(outcome, aes(x=bfi44_extraversion,y=likert,col=chatType)) + 
  labs(y = "Undersood") +
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  scale_shape_manual(values = c(17,19)) +
  scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
                                              "Neutral","Agree","Strongly Agree")) + 
  coord_cartesian(ylim = c(1,5)) +
  theme_classic()

m <- lm(likert~bfi44_extraversion*chatType*(bots_Negative+bots_Positive+user_Negative+user_Positive), outcome)
summary(m)



# reshape data frame so we can easy visualize (melt by the count sentiment analysis
# for each level (mixed, negative, neutral, and positive) for both user and bot)
library(reshape2)
combine2 <- melt(combine, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                           "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
# change column name
colnames(combine2)[ncol(combine2)] <- c("count") 
# transform variable to character
combine2$variable<- as.character(combine2$variable)
# split string to get the first 4 character (user or bots)
combine2$who <- factor(substr(combine2$variable,1,4),levels = c("user","bots"))
# add a nicer names
levels(combine2$who) <- c("User Texts","Bot Texts")
# add the sentiment on column variable (created with the columns in melt)
combine2$sentiment <- substr(combine2$variable,6,nchar(combine2$variable))

# univariate statistical analysis
summary(lm(user_Positive~chatType,combine))
summary(lm(user_Neutral~chatType,combine))
summary(lm(user_Negative~chatType,combine))
summary(lm(user_Mixed~chatType,combine))

ann_text <- data.frame(sentiment = c(2,4), count = c(5,10), 
                       lab = "Text", who = factor("User Texts",levels = c("User Texts","Bot Texts")),
                       chatType = c("Anxious","Normal"))
# visualize the average of count for each sentiment and for each chat personality
(fig2B <- ggplot(combine2, aes(x=sentiment,y=count,col=chatType,shape=chatType)) + 
    labs(y="Average Count", x = "Text Sentiment Category",
         col = "Bot \nPersonality",shape = "Bot \nPersonality") +
    stat_summary(position = position_dodge(0.3)) +
    # geom_text(data = ann_text,label = "*", col="black", size = 10) +
    scale_shape_manual(values = c(17,19)) +
    facet_grid(. ~ who) + 
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
)
summary(lm(bots_Positive~chatType,combine))
summary(lm(bots_Neutral~chatType,combine))
summary(lm(bots_Negative~chatType,combine))
summary(lm(bots_Mixed~chatType,combine))




outcome2 <- melt(outcome, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                           "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
# change column name
colnames(outcome2)[ncol(outcome2)] <- c("count") 
# transform variable to character
outcome2$variable <- as.character(outcome2$variable)
# split string to get the first 4 character (user or bots)
outcome2$who <- factor(substr(outcome2$variable,1,4),levels = c("user","bots"))
# add a nicer names
levels(outcome2$who) <- c("User Texts","Bot Texts")
# add the sentiment ased on column variable (created with the columns in melt)
outcome2$sentiment <- substr(outcome2$variable,6,nchar(outcome2$variable))

library(ggpubr)
(fig2C <- ggplot(outcome2, aes(x=bfi44_extraversion,y=count,col=chatType,shape=chatType)) + 
    labs(y="Average Count", x = "Extraversion",
         col = "Bot \nPersonality",shape = "Bot \nPersonality") +
    geom_smooth(method="lm", se=F) +
    # stat_summary(position = position_dodge(0.3)) +
    # geom_text(data = ann_text,label = "*", col="black", size = 10) +
    scale_shape_manual(values = c(17,19)) +
    facet_grid(sentiment ~ who) + 
    # stat_cor() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
)





# # # How can the sentiment analysis moderate anxiety and understanding? # # #

# More negatives messages from users impact in more negative messages from the bot?

# longer user_mean_words
