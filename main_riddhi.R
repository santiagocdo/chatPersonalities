# rm(list=ls(all=TRUE))
save_plots = 0
# data cleaned by Riddhi and Santiago check script cleaning.R
# demographics
demo <- read.csv("experiment1/cleaned/demographics.csv")
# behaviours in ratings
ratings <- read.csv("experiment1/cleaned/ratings.csv")
# file with gorilla IDs and OneReach IDs
task <- read.csv("experiment1/cleaned/task.csv")
task <- task[order(task$Participant.Private.ID),]
# interaction via One Reach (thanks Daniel!)
chat <- read.csv("experiment1/cleaned/gptstudydata_cleaned_2024.csv")

# scl90
scl90 <- read.csv("experiment1/cleaned/scl90.csv")
# bfi10
bfi10 <- read.csv("experiment1/cleaned/bfi10.csv")

source("experiment1/functions.R")
quest <- scoreQuestionnaires(scl90,bfi10)
scl90 <- quest$scl
bfi10 <- quest$bfi

# add questionnaires to ratings
ratings <- addQuestionnaireToRating(ratings,scl90,bfi10)

# add chat as factor with specific order
ratings$chat <- as.factor(ratings$chat)
levels(ratings$chat) <- c("Anxious","Normal")

full_data <- ratings
write.csv(full_data,"experiment1/cleaned/full_data.csv",row.names = F)

# summarising number of participants in each counterbalance and gorillaexp
library(dplyr)
summary_counterbalance_df <- ratings %>%
  group_by(counterbalance) %>%
  summarise(Number_of_Participants = n_distinct(Participant.Private.ID)) %>%
  ungroup()

summary_gorillaExp_df <- ratings %>%
  group_by(gorillaExp) %>%
  summarise(Number_of_Participants = n_distinct(Participant.Private.ID)) %>%
  ungroup()

############# VISUALISATIONS #############
library(ggplot2)

# scale_x_discrete(name = "Question", 
#                  labels = c("I would chat with them again",
#                             "I felt that they were different from me",
#                             "I felt distant from them",
#                             "I enjoyed our conversation", 
#                             "I felt that we are similar",
#                             "I felt that they understood me")) +

# likert ratings by order
(plot_likert_byorder <- ggplot(full_data,aes(x=question,
                                   y=Response,
                                   col=chat,
                                   shape=chat)) + 
    geom_hline(yintercept = 3, col="grey50") +
    labs(title = "Ratings regarding chatbots (by order)", 
         y="Likert Scale", x="Question",
         col = "Bot \nPersonality",
         shape = "Bot \nPersonality") +
    stat_summary(size = 1) + 
    scale_shape_manual(values = c(17,19)) +
    scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
                                              "Neutral","Agree","Strongly Agree")) +
    coord_cartesian(ylim = c(1.5,4.5)) +
    scale_color_manual(name = "Bot \nPersonality", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5)) +
    facet_grid(.~order)
  )
summary(lm(Response~chat*order,full_data[ratings$question=="chat-again",]))
summary(lm(Response~chat*order,full_data[ratings$question=="different",]))
summary(lm(Response~chat*order,full_data[ratings$question=="distant",]))
summary(lm(Response~chat*order,full_data[ratings$question=="enjoy",]))
summary(lm(Response~chat*order,full_data[ratings$question=="similar",]))
summary(lm(Response~chat*order,full_data[ratings$question=="understood",]))



# likert ratings
(plot_likert <- ggplot(full_data,aes(x=question,
                                   y=Response,
                                   col=chat,
                                   shape=chat)) + 
    geom_hline(yintercept = 3, col="grey50") +
    labs(title = "Ratings regarding chatbots", 
         y="Likert Scale", x="Question",
         col = "Bot \nPersonality",
         shape = "Bot \nPersonality") +
    stat_summary(size = 1) + 
    scale_shape_manual(values = c(17,19)) +
    scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
                                                "Neutral","Agree","Strongly Agree")) +
    coord_cartesian(ylim = c(1.5,4.5)) +
    scale_color_manual(name = "Bot \nPersonality", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)
ggsave("figures/figure2A.pdf", fig2A, dpi = 2400, scale = 1.1, units = "cm",
       width = 12, height = 8, bg = "white")

# plotting anxiety scores 
psychques_data <- full_data[!duplicated(full_data$Participant.Private.ID), ]

(plot_anxietyscores <- ggplot(psychques_data, aes(x = scl90_anxiety)) +
    geom_histogram(binwidth = 1, fill = "#6D7275", alpha = 0.7) +
    labs(title = "Histogram of Anxiety Scores",
         x = "Anxiety Scores",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)

fig2C <- ggplot(ratings, aes(x=scl90_anxiety,y=Response,col=chat)) +
  labs(y="Likert Scale", x="Anxiey Score (SCL90-R)",
       col = "Bot \nPersonality") +
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  scale_y_continuous(breaks = c(1,3,5), 
                     labels = c("Strongly Disagree","Neutral","Strongly Agree")) +
  scale_x_continuous(breaks = c(0,20,40)) +
  facet_wrap(.~question) +
  theme_classic()
ggsave("figures/figure2C.pdf", fig2C, dpi = 2400, scale = 1.1, units = "cm",
       width = 12, height = 8, bg = "white")
ggplot(ratings, aes(x=bfi10_extraversion,y=Response,col=chat)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  facet_wrap(.~question) +
  theme_classic()
=======
# plotting depression scores
(plot_depscores <- ggplot(psychques_data, aes(x = scl90_depression)) +
    geom_histogram(binwidth = 1, fill = "#6D7275", alpha = 0.7) +
    labs(title = "Histogram of Depression Scores",
         x = "Depression Scores",
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)

# plotting likert ratings by anxiety score
sig_3_int <- data.frame(
  scl90_anxiety = c(35, 35), 
  Response = c(2, 2), 
  question = c("different", "distant") 
)

sig_2_int <- data.frame(
  scl90_anxiety = c(35, 35), 
  Response = c(4, 4), 
  question = c("similar", "understood") 
)

sig_3_red <- data.frame(
  scl90_anxiety = c(20, 20, 20, 20), 
  Response = c(1.5, 1.5, 4.5, 4.5), 
  question = c("different", "distant", "similar", "understood" ) 
)

sig_1_black <- data.frame(
  scl90_anxiety = c(20), 
  Response = c(3.5), 
  question = c("different") 
)

(plot_likert_byanxiety <- ggplot(full_data, 
                                 aes(x=scl90_anxiety,
                                     y=Response,col=chat)) +
    geom_point(alpha = 0.01) +
    geom_smooth(method="lm", se = F) +
    geom_hline(yintercept = 3, color = "#6D7275", linetype = "dashed") +
    labs(title = "Ratings by anxiety",
         x = "Anxiety Scores",
         y = "Response") +
    facet_wrap(~question, labeller = labeller(
      question = c(
        "chat-again" = "I would chat with them again", 
        "different" = "I felt that they were different from me", 
        "distant" = "I felt distant from them", 
        "enjoy" = "I enjoyed our conversation", 
        "similar" = "I felt that we are similar", 
        "understood" = "I felt that they understood me"
      )
    )) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                       labels = c("Strongly Disagree", "Disagree", 
                                  "Neutral", "Agree", "Strongly Agree")) +
    scale_color_manual(name = "Bot \nPersonality",
                       values = c('#C42021', '#25283D'), 
                       labels = c("Anxious", "Normal")) +
    theme_classic() + 
    geom_text(data = sig_3_int, aes(x = scl90_anxiety, y = Response, label = "***"), 
              color = "#135E90", size = 6) +
    geom_text(data = sig_2_int, aes(x = scl90_anxiety, y = Response, label = "**"), 
              color = "#135E90", size = 6) +
    geom_text(data = sig_3_red, aes(x = scl90_anxiety, y = Response, label = "***"), 
              color = "#C42021", size = 6) +
    geom_text(data = sig_1_black, aes(x = scl90_anxiety, y = Response, label = "*"), 
              color = "#25283D", size = 6) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)
summary(lm(Response~chat*scl90_anxiety,ratings[ratings$question=="chat-again",]))
>>>>>>> d2c91d8eb462cdb5b75d9ec82253244e31f5c498



library(lmerTest)
# linear mixed model (assuming that likert is continious)
summary(lmer(Response ~ chat * scl90_anxiety + (1|Participant.Private.ID), REML = T, 
             ratings[ratings$question=="chat-again",])) 

summary(lmer(Response ~ chat * scl90_anxiety + (1|Participant.Private.ID), REML = T, 
             ratings[ratings$question=="different",])) 
summary(lm(Response ~ scl90_anxiety,ratings[ratings$question=="different" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response ~ scl90_anxiety,ratings[ratings$question=="different" &
                                            ratings$chat == "Normal",]))

summary(lmer(Response~chat*scl90_anxiety+(1|Participant.Private.ID), REML = T, 
             ratings[ratings$question=="distant",])) 
summary(lm(Response~scl90_anxiety,ratings[ratings$question=="distant" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_anxiety,ratings[ratings$question=="distant" &
                                            ratings$chat == "Normal",]))

summary(lmer(Response~chat*scl90_anxiety+(1|Participant.Private.ID), REML = T, 
             ratings[ratings$question=="enjoy",])) 

summary(lmer(Response~chat*scl90_anxiety+(1|Participant.Private.ID), REML = T, 
             ratings[ratings$question=="similar",])) 
summary(lm(Response~scl90_anxiety,ratings[ratings$question=="similar" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_anxiety,ratings[ratings$question=="similar" &
                                            ratings$chat == "Normal",]))

summary(lm(Response~chat*scl90_anxiety,ratings[ratings$question=="understood",]))
summary(lm(Response~scl90_anxiety,ratings[ratings$question=="understood" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_anxiety,ratings[ratings$question=="understood" &
                                            ratings$chat == "Normal",]))

# plotting likert ratings by anxiety score by order
(plot_likert_byanxiety_byorder <- ggplot(full_data, 
                                 aes(x=scl90_anxiety,
                                     y=Response,col=chat)) +
    geom_point(alpha = 0.01) +
    geom_smooth(method="lm", se = F) +
    geom_hline(yintercept = 3, color = "#6D7275", linetype = "dashed") +
    labs(title = "Ratings by anxiety (by order)",
         x = "Anxiety Scores",
         y = "Response") +
    facet_grid(order~question, labeller = labeller(
      question = c(
        "chat-again" = "I would chat \nwith them again", 
        "different" = "I felt that \nthey were \ndifferent from me", 
        "distant" = "I felt distant \nfrom them", 
        "enjoy" = "I enjoyed our \nconversation", 
        "similar" = "I felt that we \nare similar", 
        "understood" = "I felt that they \nunderstood me"
      )
    )) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                       labels = c("Strongly Disagree", "Disagree", 
                                  "Neutral", "Agree", "Strongly Agree")) +
    scale_color_manual(name = "Bot \nPersonality",
                       values = c('#C42021', '#25283D'), 
                       labels = c("Anxious", "Normal")) +
    theme_classic() + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5)) 
)
summary(lm(Response~chat*scl90_anxiety*order,ratings[ratings$question=="chat-again",]))

summary(lm(Response~chat*scl90_anxiety*order,ratings[ratings$question=="enjoy",]))


# plotting likert ratings by depression score
sig_3_int_dep <- data.frame(
  scl90_depression = c(45, 45, 45), 
  Response = c(2.5, 2.3, 3.8), 
  question = c("different", "distant", "similar") 
)

sig_1_int_dep <- data.frame(
  scl90_depression = c(45), 
  Response = c(3.8), 
  question = c("understood") 
)

sig_3_red_dep <- data.frame(
  scl90_depression = c(25, 25), 
  Response = c(1.5, 4.2), 
  question = c("different", "similar") 
)

sig_2_red_dep <- data.frame(
  scl90_depression = c(25, 25), 
  Response = c(2, 4), 
  question = c("distant", "understood") 
)

sig_1_red_dep <- data.frame(
  scl90_depression = c(25), 
  Response = c(2.7), 
  question = c("chat-again") 
)

sig_1_black_dep <- data.frame(
  scl90_depression = c(25), 
  Response = c(3.5), 
  question = c("different") 
)

(plot_likert_bydep <- ggplot(full_data, 
                                 aes(x=scl90_depression,
                                     y=Response,col=chat)) +
    geom_point(alpha = 0.01) +
    geom_smooth(method="lm", se = F) +
    geom_hline(yintercept = 3, color = "#6D7275", linetype = "dashed") +
    labs(title = "Ratings by depression",
         x = "Depression Scores",
         y = "Response") +
    facet_wrap(~question, labeller = labeller(
      question = c(
        "chat-again" = "I would chat with them again", 
        "different" = "I felt that they were different from me", 
        "distant" = "I felt distant from them", 
        "enjoy" = "I enjoyed our conversation", 
        "similar" = "I felt that we are similar", 
        "understood" = "I felt that they understood me"
      )
    )) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                       labels = c("Strongly Disagree", "Disagree", 
                                  "Neutral", "Agree", "Strongly Agree")) +
    scale_color_manual(name = "Bot \nPersonality",
                       values = c('#C42021', '#25283D'), 
                       labels = c("Anxious", "Normal")) +
    theme_classic() + 
    geom_text(data = sig_3_int_dep, aes(x = scl90_depression, y = Response, label = "***"), 
              color = "#135E90", size = 6) +
    geom_text(data = sig_1_int_dep, aes(x = scl90_depression, y = Response, label = "*"), 
              color = "#135E90", size = 6) +
    geom_text(data = sig_3_red_dep, aes(x = scl90_depression, y = Response, label = "***"), 
              color = "#C42021", size = 6) +
    geom_text(data = sig_2_red_dep, aes(x = scl90_depression, y = Response, label = "**"), 
              color = "#C42021", size = 6) +
    geom_text(data = sig_1_red_dep, aes(x = scl90_depression, y = Response, label = "*"), 
              color = "#C42021", size = 6) +
    geom_text(data = sig_1_black_dep, aes(x = scl90_depression, y = Response, label = "*"), 
              color = "#25283D", size = 6) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)
summary(lm(Response~chat*scl90_depression,ratings[ratings$question=="chat-again",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="chat-again" &
                                               ratings$chat == "Anxious",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="chat-again" &
                                               ratings$chat == "Normal",]))

summary(lm(Response~chat*scl90_depression,ratings[ratings$question=="different",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="different" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="different" &
                                            ratings$chat == "Normal",]))

summary(lm(Response~chat*scl90_depression,ratings[ratings$question=="distant",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="distant" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="distant" &
                                            ratings$chat == "Normal",]))

summary(lm(Response~chat*scl90_depression,ratings[ratings$question=="enjoy",]))

summary(lm(Response~chat*scl90_depression,ratings[ratings$question=="similar",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="similar" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="similar" &
                                            ratings$chat == "Normal",]))

summary(lm(Response~chat*scl90_depression,ratings[ratings$question=="understood",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="understood" &
                                            ratings$chat == "Anxious",]))
summary(lm(Response~scl90_depression,ratings[ratings$question=="understood" &
                                            ratings$chat == "Normal",]))

# plotting likert ratings by depression score by order
(plot_likert_bydep_byorder <- ggplot(full_data, 
                             aes(x=scl90_depression,
                                 y=Response,col=chat)) +
    geom_point(alpha = 0.01) +
    geom_smooth(method="lm", se = F) +
    geom_hline(yintercept = 3, color = "#6D7275", linetype = "dashed") +
    labs(title = "Ratings by depression (by order)",
         x = "Depression Scores",
         y = "Response") +
    facet_grid(order~question, labeller = labeller(
      question = c(
        "chat-again" = "I would chat \nwith them again", 
        "different" = "I felt that \nthey were \ndifferent from me", 
        "distant" = "I felt distant \nfrom them", 
        "enjoy" = "I enjoyed our \nconversation", 
        "similar" = "I felt that we \nare similar", 
        "understood" = "I felt that they \nunderstood me"
      )
    )) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                       labels = c("Strongly Disagree", "Disagree", 
                                  "Neutral", "Agree", "Strongly Agree")) +
    scale_color_manual(name = "Bot \nPersonality",
                       values = c('#C42021', '#25283D'), 
                       labels = c("Anxious", "Normal")) +
    theme_classic() + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)
summary(lm(Response~chat*scl90_depression*order,ratings[ratings$question=="chat-again",]))

summary(lm(Response~chat*scl90_depression*order,ratings[ratings$question=="enjoy",]))

if (save_plots == 1) {
  ggsave('experiment1/figures/likert.jpeg', plot = plot_likert, 
         width = 20, height = 15, units = "cm")
  ggsave('experiment1/figures/likert_byorder.jpeg', plot = plot_likert_byorder, 
         width = 25, height = 15, units = "cm")
  ggsave('experiment1/figures/anxietyscores.jpeg', plot = plot_anxietyscores, 
         width = 20, height = 15, units = "cm")
  ggsave('experiment1/figures/depscores.jpeg', plot = plot_depscores, 
         width = 20, height = 15, units = "cm")
  ggsave('experiment1/figures/likert_byanxiety.jpeg', plot = plot_likert_byanxiety, 
         width = 30, height = 15, units = "cm")
  ggsave('experiment1/figures/likert_byanxiety_byorder.jpeg', plot = plot_likert_byanxiety_byorder, 
         width = 30, height = 15, units = "cm")
  ggsave('experiment1/figures/likert_bydep.jpeg', plot = plot_likert_bydep, 
         width = 30, height = 15, units = "cm")
  ggsave('experiment1/figures/likert_bydep_byorder.jpeg', plot = plot_likert_bydep_byorder, 
         width = 30, height = 15, units = "cm")
}

# creating a mega-plot with mini-plots, one for each subscale
library(gridExtra)

NAME_LIST <- c("scl90_somatization", "scl90_anxiety", "scl90_OCD",
               "scl90_depression", "scl90_interSens", "scl90_psychotic",
               "scl90_paranoidId", "scl90_angerHost", "scl90_phobic",
               "scl90_addItems", "bfi10_extraversion", "bfi10_agreeableness",
               "bfi10_conscientiousness", "bfi10_neuroticism", "bfi10_openness"
               )
plot_list <- list()

for (NAME in NAME_LIST) {
  plot <- ggplot(full_data, 
                 aes(x = !!sym(NAME), y = Response, col = chat)) +
    geom_point(alpha = 0.01) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_hline(yintercept = 3, color = "#6D7275", linetype = "dashed") +
    labs(title = paste("Ratings by", NAME),
         x = paste(NAME, "Scores"),
         y = "Response") +
    facet_wrap(~question, labeller = labeller(
      question = c(
        "chat-again" = "I would chat with them again", 
        "different" = "I felt that they were different from me", 
        "distant" = "I felt distant from them", 
        "enjoy" = "I enjoyed our conversation", 
        "similar" = "I felt that we are similar", 
        "understood" = "I felt that they understood me"
      )
    )) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                       labels = c("Strongly Disagree", "Disagree", 
                                  "Neutral", "Agree", "Strongly Agree")) +
    scale_color_manual(name = "Bot \nPersonality",
                       values = c('#C42021', '#25283D'), 
                       labels = c("Anxious", "Normal")) +
    theme_classic() + 
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5))
  
  # Append the plot to the list
  plot_list[[NAME]] <- plot
}

grid_plot <- marrangeGrob(plot_list, nrow=5, ncol=3)

ggsave("experiment1/mega_plot.pdf", grid_plot, width = 20, height = 25)

# # Display the grid plot
# grid::grid.newpage()
# grid::grid.draw(grid_plot)
 


########


# ratings$scl90_anxiety_2 <- factor(ifelse(ratings$scl90_anxiety > quantile(ratings$scl90_anxiety,0.75),
#                                          "high","low"),levels = c("low","high"))
# 
# ann_text <- data.frame(Response = 4,
#                        question = c("chat-again","different","distant","enjoy","similar","understood"),
#                        chat = c("Anxious","Normal"),
#                        scl90_anxiety_2 = factor("high",levels = c("low","high")))
# ggplot(ratings, aes(x=scl90_anxiety_2,y=Response,col=chat,shape=chat)) +
#   stat_summary() +
#   geom_text(data = ann_text, label = c("","*","*","","*","*"), 
#             col="black", size = 10) +
#   scale_shape_manual(values = c(17,19)) +
#   scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
#                                               "Neutral","Agree","Strongly Agree")) + 
#   coord_cartesian(ylim = c(1.5,4.5)) +
#   facet_wrap(.~question) +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

length(unique(chat$userid))
length(unique(task$Response))

source("experiment1/functions.R")
# use cleaning function to extract summary information for the interactions
combine <- summariseChatInteraction(task, chat)

outcome <- addRatingsToInteractions(combine, ratings, likert = "understood")

summary(lm(likert~chatType*scl90_anxiety, outcome))
library(lmerTest)
summary(lmer(likert~chatType*scl90_anxiety+(1|Participant.Private.ID),REML=F,outcome))
ggplot(outcome, aes(x=scl90_anxiety,y=likert,col=chatType)) + 
  labs(y = "Undersood") +
  geom_point(alpha = 0.1) +
  geom_smooth(method="lm", se = F) +
  scale_shape_manual(values = c(17,19)) +
  scale_y_continuous(breaks = 1:5, labels = c("Strongly Disagree","Disagree",
                                              "Neutral","Agree","Strongly Agree")) + 
  coord_cartesian(ylim = c(1,5)) +
  theme_classic()

m <- lm(likert~scl90_anxiety*chatType*(bots_Negative+bots_Positive+user_Negative+user_Positive), outcome)
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
  geom_text(data = ann_text,label = "*", col="black", size = 10) +
  scale_shape_manual(values = c(17,19)) +
  facet_grid(. ~ who) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
)
ggsave("figures/figure2B.pdf", fig2B, dpi = 2400, scale = 1.1, units = "cm",
       width = 12, height = 8, bg = "white")
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
(fig2C <- ggplot(outcome2, aes(x=scl90_anxiety,y=count,col=chatType,shape=chatType)) + 
    labs(y="Average Count", x = "Anxiety",
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

summary(lm(user_Mixed~scl90_anxiety*chatType, outcome[,]))
summary(lm(user_Negative~scl90_anxiety*chatType, outcome[,]))
summary(lm(user_Neutral~scl90_anxiety*chatType, outcome[,]))
summary(lm(user_Positive~scl90_anxiety*chatType, outcome[,]))

summary(lm(bots_Mixed~scl90_anxiety*chatType, outcome[,]))
summary(lm(bots_Negative~scl90_anxiety*chatType, outcome[,]))
summary(lm(bots_Neutral~scl90_anxiety*chatType, outcome[,]))
summary(lm(bots_Positive~scl90_anxiety*chatType, outcome[,]))

# # # How can the sentiment analysis moderate anxiety and understanding? # # #

# More negatives messages from users impact in more negative messages from the bot?

# longer user_mean_words
