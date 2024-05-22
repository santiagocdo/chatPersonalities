# data cleaned by Riddhi and located in cleaned/
# behaviours in ratings
ratings <- read.csv("experiment1/cleaned/cleaned_data.csv")
# file with gorilla IDs and OneReach IDs
task <- read.csv("experiment1/cleaned/task_data.csv")
task <- task[order(task$Participant.Private.ID),]
# interaction via One Reach (thanks Daniel!)
chat <- read.csv("experiment1/cleaned/gptstudydata_cleaned_2024.csv")



# create variable bot_type encoding both personality. Remember depends on order and chat
# Bot is anxious if, chat=1 and order=Anxious first, but also when chat=2 and order=Normal first
# Bot is normal if, chat=1 and order=Normal first, but also when chat=2 and order=Anxious first
# ratings$bot_type <- ifelse((ratings$chat == 1 & ratings$order == "Anxious first") |
#                              (ratings$chat == 2 & ratings$order == "Normal first"),
#                            "Anxious", ifelse((ratings$chat == 1 & ratings$order == "Normal first") |
#                                                (ratings$chat == 2 & ratings$order == "Anxious first"),
#                                              "Normal",NA))
ratings$chat <- as.factor(ratings$chat)
levels(ratings$chat) <- c("Anxious","Normal")

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
  coord_cartesian(ylim = c(2,4)) +
  facet_grid(.~order) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
)



ggplot(ratings, aes(x=q_scl_anxiety,y=Response,col=chat)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  facet_wrap(.~question) +
  theme_classic()



ratings$q_scl_anxiety_2 <- factor(ifelse(ratings$q_scl_anxiety > quantile(ratings$q_scl_anxiety,0.75),
                                  "high","low"),levels = c("low","high")) 
ann_text <- data.frame(Response = 4, 
                       question = c("chat-again","different","distant","enjoy","similar","understood"),
                       chat = c("Anxious","Normal"),
                       q_scl_anxiety_2 = factor("high",levels = c("low","high")))
ggplot(ratings, aes(x=q_scl_anxiety_2,y=Response,col=chat,shape=chat)) + 
  stat_summary() +
  geom_text(data = ann_text, label = c("*","*","*","*","*","*"), col="black", size = 10) +
  scale_shape_manual(values = c(17,19)) +
  facet_wrap(.~question) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

summary(lm(Response~chat*q_scl_anxiety,ratings[ratings$question=="chat-again",]))
summary(lm(Response~chat*q_scl_anxiety,ratings[ratings$question=="different",]))
summary(lm(Response~chat*q_scl_anxiety,ratings[ratings$question=="distant",]))
summary(lm(Response~chat*q_scl_anxiety,ratings[ratings$question=="enjoy",]))
summary(lm(Response~chat*q_scl_anxiety,ratings[ratings$question=="similar",]))
summary(lm(Response~chat*q_scl_anxiety,ratings[ratings$question=="understood",]))

summary(lm(Response~q_scl_anxiety,ratings[ratings$question=="understood" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~q_scl_anxiety,ratings[ratings$question=="understood" &
                                            ratings$chat == "Normal",]))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

length(unique(chat$userid))
length(unique(task$Response))

source("experiment1/functions.R")
# use cleaning function to extract summary information for the interactions
combine <- summariseChatInteraction(task, chat)

outcome <- addRatingsToInteractions(combine, ratings, likert = "understood")

summary(lm(likert~chat_name*q_scl_anxiety, outcome))
library(lmerTest)
summary(lmer(likert~chat_name*q_scl_anxiety+(1|Participant.Private.ID),REML=F,outcome))
ggplot(outcome, aes(x=q_scl_anxiety,y=likert,col=chat_name)) + 
  labs(y = "Undersood") +
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  scale_shape_manual(values = c(17,19)) +
  scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
                                              "Neutral","Agree","Strongly Agree")) + 
  coord_cartesian(ylim = c(1,5)) +
  theme_classic()

m <- lm(likert~q_scl_anxiety*chat_name*(bots_Negative+bots_Positive+user_Negative+user_Positive), outcome)
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
# add the sentiment ased on column variable (created with the columns in melt)
combine2$sentiment <- substr(combine2$variable,6,nchar(combine2$variable))

# univariate statistical analysis
summary(lm(user_Positive~chat_name,combine))
summary(lm(user_Neutral~chat_name,combine))
summary(lm(user_Negative~chat_name,combine))
summary(lm(user_Mixed~chat_name,combine))

ann_text <- data.frame(sentiment = c(2,4), count = c(5,10), 
                       lab = "Text", who = factor("User Texts",levels = c("User Texts","Bot Texts")),
                       chat_name = c("Anxious","Normal"))
# visualize the average of count for each sentiment and for each chat personality
(fig2B <- ggplot(combine2, aes(x=sentiment,y=count,col=chat_name,shape=chat_name)) + 
  labs(y="Average Count", x = "Text Sentiment Category",
       col = "Bot \nPersonality",shape = "Bot \nPersonality") +
  stat_summary(position = position_dodge(0.3)) +
  geom_text(data = ann_text,label = "*", col="black", size = 10) +
  scale_shape_manual(values = c(17,19)) +
  facet_grid(. ~ who) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
)




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
(fig2C <- ggplot(outcome2, aes(x=q_scl_anxiety,y=count,col=chat_name,shape=chat_name)) + 
    labs(y="Average Count", x = "Anxiety",
         col = "Bot \nPersonality",shape = "Bot \nPersonality") +
    geom_smooth(method="lm", se=F) +
    # stat_summary(position = position_dodge(0.3)) +
    # geom_text(data = ann_text,label = "*", col="black", size = 10) +
    scale_shape_manual(values = c(17,19)) +
    facet_grid(sentiment ~ who) + 
    stat_cor() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
)


summary(lm(bots_Positive~chat_name,combine))
summary(lm(bots_Neutral~chat_name,combine))
summary(lm(bots_Negative~chat_name,combine))
summary(lm(bots_Mixed~chat_name,combine))



# # # How can the sentiment analysis moderate anxiety and understanding? # # #
