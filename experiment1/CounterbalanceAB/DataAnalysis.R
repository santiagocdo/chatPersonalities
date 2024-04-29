# CHATGPT STUDY: OXFORD X ONEREACH; RJP Nov 2023

rm(list = ls()) # clear environment

# CHANGE THIS TO YOUR DIRECTORY
# path = '/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/CounterbalanceAB/'
# setwd(path)

# installs packages this script uses if not already installed; then loads the package
if (!require(plyr)) {install.packages("plyr")}; library(plyr);
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr);
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2);
if (!require(lme4)) {install.packages("lme4")}; library(lme4);
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr);
if (!require(tidyr)) {install.packages("tidyr")}; library(tidyr);
if (!require(stringr)) {install.packages("stringr")}; library(stringr);
if (!require(readr)) {install.packages("readr")}; library(readr);
if (!require(plotrix)) {install.packages("plotrix")}; library(plotrix);
if (!require(ggtext)) {install.packages("ggtext")}; library(ggtext);
if (!require(ggrepel)) {install.packages("ggrepel")}; library(ggrepel);


source("summarySE.R")
source("summarySEwithin.R")
source("normDataWithin.R")

##############################  COMPILING RAW TASK DATA ##############################

# finding .csv files in the TASK DATA folder directory # CHANGE THIS TO YOUR DIRECTORY
# setwd(path)

# obtaining all the csv files
indiv_files <- list.files("task data/",pattern = ".csv")

# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Tree.Node.Key", "Display", "Response")

for (i in 1:length(indiv_files)) {
  temp_data <- read.csv(paste0("task data/",indiv_files[i]))
  temp_data <- temp_data[,cols]
  temp_data <- temp_data[temp_data$Display == "completionCode1" | temp_data$Display == "completionCode2",]
  
  if (i == 1) {
    task_data <- temp_data
  } else {
    task_data <- rbind(task_data, temp_data)
  }
}

task_data <- na.omit(task_data)
task_data <- task_data[task_data$Participant.Status == "complete",]

task_data$chat[grepl("ncut", task_data$Tree.Node.Key)] <- 1
task_data$chat[grepl("hu1g", task_data$Tree.Node.Key)] <- 1
task_data$chat[grepl("4uru", task_data$Tree.Node.Key)] <- 1
task_data$chat[grepl("mghz", task_data$Tree.Node.Key)] <- 1
task_data$chat[grepl("vjth", task_data$Tree.Node.Key)] <- 2
task_data$chat[grepl("4nrv", task_data$Tree.Node.Key)] <- 2
task_data$chat[grepl("ppql", task_data$Tree.Node.Key)] <- 2
task_data$chat[grepl("tvu5", task_data$Tree.Node.Key)] <- 2

task_data$chat_name[grepl("1", task_data$chat)] <- "Anxious"
task_data$chat_name[grepl("2", task_data$chat)] <- "Normal"

task_data$counterbalance[grepl("ncut", task_data$Tree.Node.Key)] <- "A"
task_data$counterbalance[grepl("hu1g", task_data$Tree.Node.Key)] <- "A"
task_data$counterbalance[grepl("vjth", task_data$Tree.Node.Key)] <- "A"
task_data$counterbalance[grepl("4nrv", task_data$Tree.Node.Key)] <- "A"
task_data$counterbalance[grepl("4uru", task_data$Tree.Node.Key)] <- "B"
task_data$counterbalance[grepl("mghz", task_data$Tree.Node.Key)] <- "B"
task_data$counterbalance[grepl("ppql", task_data$Tree.Node.Key)] <- "B"
task_data$counterbalance[grepl("tvu5", task_data$Tree.Node.Key)] <- "B"
task_data$counterbalance <- as.factor(task_data$counterbalance)

task_data$chat1_first[grepl("4uru", task_data$Tree.Node.Key)] <- "yes"
task_data$chat1_first[grepl("tvu5", task_data$Tree.Node.Key)] <- "yes"
task_data$chat1_first[grepl("ncut", task_data$Tree.Node.Key)] <- "yes"
task_data$chat1_first[grepl("vjth", task_data$Tree.Node.Key)] <- "yes"
task_data$chat1_first[grepl("ppql", task_data$Tree.Node.Key)] <- "no"
task_data$chat1_first[grepl("mghz", task_data$Tree.Node.Key)] <- "no"
task_data$chat1_first[grepl("4nrv", task_data$Tree.Node.Key)] <- "no"
task_data$chat1_first[grepl("hu1g", task_data$Tree.Node.Key)] <- "no"

task_data$X <- task_data$Participant.Status <- task_data$Tree.Node.Key <- task_data$Display <- NULL # removing irrelevant columns

# write.csv(task_data,"/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/CounterbalanceAB/task_data.csv", row.names = TRUE)
write.csv(task_data,"task_data.csv", row.names = F)

##############################  IMPORTING ALL FILES ##############################

# set this as the main folder path now
# path = '/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/CounterbalanceAB/'
# setwd(path)

# loading raw data (csv files)
demographics <- read.csv("demographics.csv")
demographics_A <- read.csv("demographics_A.csv")
demographics_B <- read.csv("demographics_B.csv")

names(demographics_A) <- names(demographics_B) <- names(demographics)
demographics <- rbind(demographics, demographics_A, demographics_B)

botques_data <- rbind(read.csv("botques12_A.csv"), read.csv("botques21_A.csv"),
                      read.csv("botques12_B.csv"), read.csv("botques21_B.csv"))
botques_data_A <- rbind(read.csv("botques12_A_A.csv"), read.csv("botques21_A_A.csv"))
botques_data_B <- rbind(read.csv("botques12_B_B.csv"), read.csv("botques21_B_B.csv"))
names(botques_data_B) <- names(botques_data_A) <- names(botques_data)
botques_data <- rbind(botques_data, botques_data_A, botques_data_B)

bfi10_data <- read.csv("bfi10.csv")
bfi10_data_A <- read.csv("bfi10_A.csv")
bfi10_data_B <- read.csv("bfi10_B.csv")
names(bfi10_data_B) <- names(bfi10_data_A) <- names(bfi10_data)
bfi10_data <- rbind(bfi10_data, bfi10_data_A, bfi10_data_B)

scl90_data <- read.csv("scl90.csv")
scl90_data_A <- read.csv("scl90_A.csv")
scl90_data_B <- read.csv("scl90_B.csv")
names(scl90_data_B) <- names(scl90_data_A) <- names(scl90_data)
scl90_data <- rbind(scl90_data, scl90_data_A, scl90_data_B)


##############################  EXTRACTING DEMOGRAPHICS INFORMATION ##############################

# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Question", "Key", "Response")

# extracting relevant columns and renaming them
demographics <- demographics[,cols]
demographics <- demographics[demographics$Participant.Status == "complete" & 
                               demographics$Key == "value" | demographics$Key == "quantised",]

demographics$question[grepl("age", demographics$Question)] <- "age"
demographics$question[grepl("gender", demographics$Question)] <- "gender"


demographics_gender <- demographics[demographics$question == "gender" & demographics$Key == "quantised", ]
demographics_age <- demographics[demographics$question == "age", ]

# creating a summary dataframe
summary_demographics <- data.frame(matrix(nrow = 6, ncol = 2))
colnames(summary_demographics)[1] <- "Property"
colnames(summary_demographics)[2] <- "Value"
summary_demographics[,1] <- c("N", "num_M", "num_F", "num_other", "mean_Age", "sd_Age") 

gender_distribution <- table(demographics_gender$Response)
demographics_age$Response <- as.integer(demographics_age$Response)

# filling in demographic information into the summary sheet
summary_demographics$Value[summary_demographics$Property == "N"] <- 
  length(unique(demographics_gender$Participant.Private.ID))
summary_demographics$Value[summary_demographics$Property == "num_M"] <- 
  gender_distribution[names(gender_distribution) == 1]
summary_demographics$Value[summary_demographics$Property == "num_F"] <- 
  gender_distribution[names(gender_distribution) == 2]
summary_demographics$Value[summary_demographics$Property == "num_other"] <-
  gender_distribution[names(gender_distribution) == 3]
summary_demographics$Value[summary_demographics$Property == "mean_Age"] <- 
  mean(demographics_age$Response)
summary_demographics$Value[summary_demographics$Property == "sd_Age"] <- 
  sd(demographics_age$Response)

############################## BOT QUESTIONNAIRE DATA ANALYSIS ##############################

# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Tree.Node.Key",
          "Question.Key", "Response")

botques_data <- botques_data[,cols]

botques_data <- botques_data[botques_data$Participant.Status == "complete",]
botques_data <- botques_data[!botques_data$Question.Key == "BEGIN QUESTIONNAIRE" & !botques_data$Question.Key == "END QUESTIONNAIRE" ,]
botques_data_qualitative <- botques_data[grepl("response", botques_data$Question.Key), ] 
botques_data <- botques_data[grepl("quantised", botques_data$Question.Key), ]
botques_data$Response <- as.integer(botques_data$Response)

botques_data$counterbalance[grepl("7ewr", botques_data$Tree.Node.Key)] <- "A"
botques_data$counterbalance[grepl("6eoj", botques_data$Tree.Node.Key)] <- "A"
botques_data$counterbalance[grepl("4a4q", botques_data$Tree.Node.Key)] <- "A"
botques_data$counterbalance[grepl("ewzl", botques_data$Tree.Node.Key)] <- "A"
botques_data$counterbalance[grepl("wcll", botques_data$Tree.Node.Key)] <- "B"
botques_data$counterbalance[grepl("eupm", botques_data$Tree.Node.Key)] <- "B"
botques_data$counterbalance[grepl("gjc4", botques_data$Tree.Node.Key)] <- "B"
botques_data$counterbalance[grepl("gbf9", botques_data$Tree.Node.Key)] <- "B"
botques_data$counterbalance <- as.factor(botques_data$counterbalance)

botques_data$order[grepl("7ewr", botques_data$Tree.Node.Key)] <- "Anxious first"
botques_data$order[grepl("6eoj", botques_data$Tree.Node.Key)] <- "Normal first"
botques_data$order[grepl("wcll", botques_data$Tree.Node.Key)] <- "Anxious first"
botques_data$order[grepl("eupm", botques_data$Tree.Node.Key)] <- "Normal first"
botques_data$order[grepl("4a4q", botques_data$Tree.Node.Key)] <- "Anxious first"
botques_data$order[grepl("ewzl", botques_data$Tree.Node.Key)] <- "Normal first"
botques_data$order[grepl("gjc4", botques_data$Tree.Node.Key)] <- "Anxious first"
botques_data$order[grepl("gbf9", botques_data$Tree.Node.Key)] <- "Normal first"
botques_data$order <- as.factor(botques_data$order)

botques_data$chat[grepl("pat", botques_data$Question.Key) & botques_data$counterbalance == "A"] <- "1"
botques_data$chat[grepl("pat", botques_data$Question.Key) & botques_data$counterbalance == "B"] <- "2"
botques_data$chat[grepl("alex", botques_data$Question.Key) & botques_data$counterbalance == "A"] <- "2"
botques_data$chat[grepl("alex", botques_data$Question.Key) & botques_data$counterbalance == "B"] <- "1"
botques_data$chat <- as.factor(botques_data$chat)

botques_data$question[grepl("similar", botques_data$Question.Key)] <- "similar"
botques_data$question[grepl("enjoy", botques_data$Question.Key)] <- "enjoy"
botques_data$question[grepl("distant", botques_data$Question.Key)] <- "distant"
botques_data$question[grepl("understood", botques_data$Question.Key)] <- "understood"
botques_data$question[grepl("different", botques_data$Question.Key)] <- "different"
botques_data$question[grepl("chat-again", botques_data$Question.Key)] <- "chat-again"
botques_data$question <- as.factor(botques_data$question)

botques_data$Participant.Status <- botques_data$Tree.Node.Key <- botques_data$Question.Key <- NULL

# santiago 26/04/2024
# mean(botques_data$Response)
# ggplot(botques_data,(aes(x=question,y=Response,col=chat))) +
#   stat_summary() +
#   theme_classic() +
#   facet_grid(.~order) +
  # theme(axis.text.x = element_text(angle = 30, hjust = 1))

summary_botques_data <- summarySEwithin(data=botques_data, measurevar = "Response", betweenvars = "order", 
                               withinvars = c("chat", "question"), idvar = "Participant.Private.ID")

anxious_first_N <- summary_botques_data[summary_botques_data$order == "Anxious first", "N"][1]
normal_first_N <- summary_botques_data[summary_botques_data$order == "Normal first", "N"][1]
default_text <- "Bot Questionnaire (by block order: N_anxious_first = %s, N_normal_first = %s)"
plot_title <- sprintf(default_text, anxious_first_N, normal_first_N)

# santiago 26/04/2024
# ggplot(summary_botques_data, aes(x=question,y=Response,col=order)) + 
#   stat_summary(geom="point") + 
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1))

(plot_bot_ques_data <- ggplot(summary_botques_data,
                              aes(x = question, y = Response, 
                                  color = order, group = order)) +
  geom_point(position=position_dodge(width=0.5), 
             aes(x = question, y = Response, color = chat), 
             size = 3)  +
  geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
  geom_errorbar(position=position_dodge(width=0.5), 
                aes(x = question, ymin=Response-se, ymax=Response+se), 
                width=.3, alpha = 1.0, colour = "black") + 
  labs(title = plot_title, x = "Question", y = "Response") + 
  scale_x_discrete(name = "Question", 
                   labels = c("I would chat with them again",
                              "I felt that they were different from me",
                              "I felt distant from them",
                              "I enjoyed our conversation", 
                              "I felt that we are similar",
                              "I felt that they understood me")) +
  scale_y_continuous(name = "Response", 
                     breaks = c(1, 2, 3, 4, 5),
                   labels = c("Strongly Disagree",
                              "Disagree",
                              "Neutral", 
                              "Agree",
                              "Strongly Agree"), 
                   limits = c(1.8, 4.2)) +
  scale_color_manual(name = "Chat", 
                     values=c('#C42021','#25283D'),
                     labels=c("Anxious","Normal")) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 60),
        plot.title = element_text(size = 15), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        strip.text.x = element_text(size = 10)) + 
  facet_grid(.~order))

ggsave('bot questionnaire.jpeg', plot = plot_bot_ques_data, 
       width = 30, height = 15, units = "cm")

summarising_bot_ques_data_collapsed <- summarySEwithin(data=botques_data, measurevar = "Response", 
                                                   withinvars = c("chat", "question"), idvar = "Participant.Private.ID")

(plot_bot_ques_data_sum <- ggplot(summarising_bot_ques_data_collapsed, 
                                    aes(x = question, y = Response, 
                                        color = chat, group = chat)) +
    geom_point(position=position_dodge(width=0.5), 
               aes(x = question, y = Response, color = chat), 
               size = 3)  +
    geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
    geom_errorbar(position=position_dodge(width=0.5), 
                  aes(x = question, ymin=Response-se, ymax=Response+se), 
                  width=.3, alpha = 1.0, colour = "black") + 
    labs(title = "Bot Questionnaire (collapsed across block order)", x = "Question", y = "Response") + 
    scale_x_discrete(name = "Question", 
                     labels = c("I would chat with them again",
                                "I felt that they were different from me",
                                "I felt distant from them",
                                "I enjoyed our conversation", 
                                "I felt that we are similar",
                                "I felt that they understood me")) +
    scale_y_continuous(name = "Response", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral", 
                                  "Agree",
                                  "Strongly Agree"), 
                       limits = c(1.8, 4.2)) +
    scale_color_manual(name = "Chat", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = element_text(size = 10)))

ggsave('bot questionnaire sum.jpeg', plot = plot_bot_ques_data_sum, 
       width = 20, height = 15, units = "cm")

(plot_bot_ques_data_sum <- ggplot(summarising_bot_ques_data_collapsed, 
                                  aes(x = question, y = Response, 
                                      color = chat, group = chat)) +
    geom_point(position=position_dodge(width=0.5), 
               aes(x = question, y = Response, color = chat), 
               size = 3)  +
    geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
    geom_errorbar(position=position_dodge(width=0.5), 
                  aes(x = question, ymin=Response-se, ymax=Response+se), 
                  width=.3, alpha = 1.0, colour = "black") + 
    labs(title = "Bot Questionnaire (collapsed across block order)", x = "Question", y = "Response") + 
    scale_x_discrete(name = "Question", 
                     labels = c("I would chat with them again",
                                "I felt that they were different from me",
                                "I felt distant from them",
                                "I enjoyed our conversation", 
                                "I felt that we are similar",
                                "I felt that they understood me")) +
    scale_y_continuous(name = "Response", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral", 
                                  "Agree",
                                  "Strongly Agree"), 
                       limits = c(1.8, 4.2)) +
    scale_color_manual(name = "Chat", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = element_text(size = 10)))

ggsave('bot questionnaire sum.jpeg', plot = plot_bot_ques_data_sum, 
       width = 20, height = 15, units = "cm")

##############################  GETTING SCORES FROM QUESTIONNAIRES ##############################

pooling_questionnaires <- data.frame(matrix(nrow = length(unique(demographics$Participant.Private.ID)), 
                                                ncol = 1))
colnames(pooling_questionnaires)[1] <- "PID"
pooling_questionnaires$PID <- unique(demographics$Participant.Private.ID)

# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Question.Key", "Response")

# extracting relevant columns and renaming them
bfi10_data <- bfi10_data[,cols]
bfi10_data <- bfi10_data[bfi10_data$Participant.Status == "complete",]

bfi10_data <- subset(bfi10_data, grepl("quantised", Question.Key))

bfi10_data$Response <- as.integer(bfi10_data$Response)

bfi10_data$Response[bfi10_data$Question.Key == "response-1-quantised" | 
                      bfi10_data$Question.Key == "response-3-quantised" |
                      bfi10_data$Question.Key == "response-4-quantised" |
                      bfi10_data$Question.Key == "response-5-quantised" |
                      bfi10_data$Question.Key == "response-7-quantised"] <- 
  7- bfi10_data$Response[bfi10_data$Question.Key == "response-1-quantised" | 
                                       bfi10_data$Question.Key == "response-3-quantised" |
                                       bfi10_data$Question.Key == "response-4-quantised" |
                                       bfi10_data$Question.Key == "response-5-quantised" |
                                       bfi10_data$Question.Key == "response-7-quantised"]
             
pooling_questionnaires$q_bfi_extraversion <- 
  bfi10_data$Response[bfi10_data$Question.Key == "response-1-quantised"] + 
  bfi10_data$Response[bfi10_data$Question.Key == "response-6-quantised"]

pooling_questionnaires$q_bfi_agreeableness <- 
  bfi10_data$Response[bfi10_data$Question.Key == "response-2-quantised"] + 
  bfi10_data$Response[bfi10_data$Question.Key == "response-7-quantised"]

pooling_questionnaires$q_bfi_conscientiousness <- 
  bfi10_data$Response[bfi10_data$Question.Key == "response-3-quantised"] + 
  bfi10_data$Response[bfi10_data$Question.Key == "response-8-quantised"]

pooling_questionnaires$q_bfi_neuroticism <- 
  bfi10_data$Response[bfi10_data$Question.Key == "response-4-quantised"] + 
  bfi10_data$Response[bfi10_data$Question.Key == "response-9-quantised"]

pooling_questionnaires$q_bfi_openness_to_experience <- 
  bfi10_data$Response[bfi10_data$Question.Key == "response-5-quantised"] + 
  bfi10_data$Response[bfi10_data$Question.Key == "response-10-quantised"]


# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Question.Key", "Response")

# extracting relevant columns and renaming them
scl90_data <- scl90_data[,cols]
scl90_data <- scl90_data[scl90_data$Participant.Status == "complete",]

scl90_data <- subset(scl90_data, grepl("quantised", Question.Key))

scl90_data$Response <- as.integer(scl90_data$Response)

pooling_questionnaires$q_scl_somatisation <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-1-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-4-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-12-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-27-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-40-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-42-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-48-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-49-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-52-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-53-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-56-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-58-quantised"] 

pooling_questionnaires$q_scl_ocd <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-3-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-9-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-10-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-28-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-38-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-45-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-46-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-51-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-55-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-65-quantised"] 

pooling_questionnaires$q_scl_interpersonal_sensibility <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-6-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-21-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-34-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-36-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-37-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-41-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-61-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-69-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-73-quantised"] 

pooling_questionnaires$q_scl_depression <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-5-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-14-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-20-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-22-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-26-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-29-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-30-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-31-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-32-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-54-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-71-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-79-quantised"] 

pooling_questionnaires$q_scl_anxiety <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-17-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-23-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-33-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-39-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-57-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-72-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-78-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-80-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-86-quantised"] 

pooling_questionnaires$q_scl_anger <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-11-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-24-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-63-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-67-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-74-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-81-quantised"]

pooling_questionnaires$q_scl_phobic_anxiety <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-13-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-25-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-47-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-50-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-70-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-75-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-82-quantised"]

pooling_questionnaires$q_scl_paranoid <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-8-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-18-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-43-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-68-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-76-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-83-quantised"] 

pooling_questionnaires$q_scl_psychoticism <- 
  scl90_data$Response[scl90_data$Question.Key == "response-1-7-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-16-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-35-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-62-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-77-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-84-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-85-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-87-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-88-quantised"] + 
  scl90_data$Response[scl90_data$Question.Key == "response-1-90-quantised"] 

pooling_questionnaires_shortform <- pooling_questionnaires

pooling_questionnaires <- pooling_questionnaires %>%
  pivot_longer(col = q_bfi_extraversion:q_scl_psychoticism,
               names_to = "question",
               names_prefix = "q_",
               values_to = "response")

participants_unique <- unique(demographics$Participant.Private.ID)
pooling_questionnaires$order <- NA
for (p in 1:length(participants_unique)) {
  pooling_questionnaires[pooling_questionnaires$PID == participants_unique[p], "order"] <- botques_data[botques_data$Participant.Private.ID == participants_unique[p], "order"][1]
}

summarising_questionnaires_order <- summarySEwithin(data=pooling_questionnaires, betweenvars = "order",
                                        measurevar = "response", withinvars = "question",
                                        idvar = "PID")

summarising_questionnaires <- summarySEwithin(data=pooling_questionnaires, 
                                              measurevar = "response", withinvars = "question",
                                              idvar = "PID")

default_text_q <- "Questionnaire (by block order: N_anxious_first = %s, N_normal_first = %s)"
plot_title_q <- sprintf(default_text_q, anxious_first_N, normal_first_N)

(plot_questionnaires_order <- ggplot(summarising_questionnaires_order, 
                              aes(x = question, y = response)) +
  geom_point(aes(x = question, y = response), 
             size = 3)  +
  geom_errorbar(aes(x = question, ymin=response-se, ymax=response+se), 
                width=.3, alpha = 1.0, colour = "black") + 
  labs(title = plot_title_q, x = "Subscale", y = "Score") + 
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(angle = 60),
        plot.title = element_text(size = 15), 
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10), 
        strip.text.x = element_text(size = 10)) + 
    facet_grid(.~order))

ggsave('questionnaires_byOrder.jpeg', plot = plot_questionnaires_order, 
       width = 30, height = 15, units = "cm")

subset_pooling_questionnaires <- pooling_questionnaires[pooling_questionnaires$question == "scl_depression" |
                                                          pooling_questionnaires$question == "scl_anxiety",]
(plot_questionnaires_order_2 <- ggplot(subset_pooling_questionnaires, 
                                     aes(x = question, y = response)) +
    geom_point(aes(x = question, y = response), 
               size = 3, position = "jitter")  +
    geom_label_repel(aes(label = PID), size = 2) +
    labs(title = plot_title_q, x = "Subscale", y = "Score") + 
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = element_text(size = 10)) + 
    facet_grid(.~order))

ggsave('questionnaires_byOrder_2.jpeg', plot = plot_questionnaires_order_2, 
       width = 30, height = 15, units = "cm")

##############################  bot + questionnaire plots ##############################

merged_data <- botques_data

PID_list <- unique(merged_data$Participant.Private.ID)
merged_data$q_bfi_extraversion <- merged_data$q_bfi_agreeableness <- merged_data$q_bfi_conscientiousness <- 
  merged_data$q_bfi_neuroticism <- merged_data$q_bfi_openness_to_experience <- merged_data$q_scl_somatisation <- 
  merged_data$q_scl_ocd <- merged_data$q_scl_interpersonal_sensibility <- merged_data$q_scl_depression <- 
  merged_data$q_scl_anxiety <- merged_data$q_scl_anger <- merged_data$q_scl_phobic_anxiety <- 
  merged_data$q_scl_paranoid <- merged_data$q_scl_psychoticism <- NA


for (p in 1:length(PID_list)) {
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_bfi_extraversion"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_bfi_extraversion"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_bfi_agreeableness"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_bfi_agreeableness"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_bfi_conscientiousness"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_bfi_conscientiousness"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_bfi_neuroticism"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_bfi_neuroticism"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_bfi_openness_to_experience"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_bfi_openness_to_experience"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_somatisation"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_somatisation"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_ocd"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_ocd"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_interpersonal_sensibility"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_interpersonal_sensibility"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_depression"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_depression"]

  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_anxiety"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_anxiety"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_anger"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_anger"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_phobic_anxiety"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_phobic_anxiety"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_paranoid"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_paranoid"]
  
  merged_data[merged_data$Participant.Private.ID == PID_list[p], "q_scl_psychoticism"] <- 
    pooling_questionnaires_shortform[pooling_questionnaires_shortform$PID == PID_list[p], "q_scl_psychoticism"]
}

merged_data$chat <- as.factor(merged_data$chat)

merged_data$question_full[grepl("similar", merged_data$question)] <- "I felt that <br>we are similar"
merged_data$question_full[grepl("enjoy", merged_data$question)] <- "I enjoyed our <br>conversation"
merged_data$question_full[grepl("distant", merged_data$question)] <- "I felt distant <br>from them"
merged_data$question_full[grepl("understood", merged_data$question)] <- "I felt that <br>they understood me"
merged_data$question_full[grepl("different", merged_data$question)] <- "I felt that they were <br>different from me"
merged_data$question_full[grepl("chat-again", merged_data$question)] <- "I would chat <br>with them again"
merged_data$question_full <- as.factor(merged_data$question_full)

(plot_bot_ques_anxiety <- ggplot(merged_data, aes(x = q_scl_anxiety, y = Response, 
                                                  color = chat, group = chat)) +
    geom_smooth()  +
    geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
    labs(title = "Bot Questionnaire ~ Anxiety Scores", x = "Anxiety Scores", y = "Response") + 
    scale_y_continuous(name = "Response", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral", 
                                  "Agree",
                                  "Strongly Agree")) +
    scale_color_manual(name = "Chat", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = ggtext::element_markdown(size = 10, hjust = 0.5)) + 
    facet_grid(~question_full))

ggsave('bot ques_anxiety.jpeg', plot = plot_bot_ques_anxiety, 
       width = 30, height = 15, units = "cm")

default_text_q_anx <- "Bot Questionnaire ~ Anxiety Scores (by block order: N_anxious_first = %s, N_normal_first = %s)"
plot_title_q_anx <- sprintf(default_text_q_anx, anxious_first_N, normal_first_N)
(plot_bot_ques_anxiety_order <- ggplot(merged_data, aes(x = q_scl_anxiety, y = Response, 
                                                        color = chat, group = chat)) +
    geom_smooth()  +
    geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
    labs(title = plot_title_q_anx, x = "Anxiety Scores", y = "Response") + 
    scale_y_continuous(name = "Response", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral", 
                                  "Agree",
                                  "Strongly Agree")) +
    scale_color_manual(name = "Chat", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = ggtext::element_markdown(size = 10, hjust = 0.5)) + 
    facet_grid(order~question_full))

ggsave('bot ques_anxiety_order.jpeg', plot = plot_bot_ques_anxiety_order, 
       width = 30, height = 15, units = "cm")

hist_data_anx <- merged_data[merged_data$order == "Anxious first", "q_scl_anxiety"]
hist_data_anx <- hist_data_anx[seq(1, length(hist_data_anx), 12)]
hist_data_nor <- merged_data[merged_data$order == "Normal first", "q_scl_anxiety"]
hist_data_nor <- hist_data_nor[seq(1, length(hist_data_nor), 12)]
hist(hist_data_anx, xlim=c(0,50), 
     main = "histogram of anxiety scores \n from anxious-first group", 
     xlab = "anxiety scores")
hist(hist_data_nor, xlim=c(0,50),
     main = "histogram of anxiety scores \n from normal-first group", 
     xlab = "anxiety scores")

(plot_bot_ques_dep <- ggplot(merged_data, aes(x = q_scl_depression, y = Response, 
                                              color = chat, group = chat)) +
    geom_smooth()  +
    geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
    labs(title = "Bot Questionnaire ~ Depression Scores", x = "Depression Scores", y = "Response") + 
    scale_y_continuous(name = "Response", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral", 
                                  "Agree",
                                  "Strongly Agree")) +
    scale_color_manual(name = "Chat", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = ggtext::element_markdown(size = 10, hjust = 0.5)) + 
    facet_grid(~question_full))

ggsave('bot ques_dep.jpeg', plot = plot_bot_ques_dep, 
       width = 30, height = 15, units = "cm")

default_text_q_dep <- "Bot Questionnaire ~ Depression Scores (by block order: N_anxious_first = %s, N_normal_first = %s)"
plot_title_q_dep <- sprintf(default_text_q_dep, anxious_first_N, normal_first_N)
(plot_bot_ques_dep_order <- ggplot(merged_data, aes(x = q_scl_depression, y = Response, 
                                                    color = chat, group = chat)) +
    geom_smooth()  +
    geom_hline(yintercept=3, linetype = "dotted", col = "grey") +
    labs(title = plot_title_q_dep, x = "Depression Scores", y = "Response") + 
    scale_y_continuous(name = "Response", 
                       breaks = c(1, 2, 3, 4, 5),
                       labels = c("Strongly Disagree",
                                  "Disagree",
                                  "Neutral", 
                                  "Agree",
                                  "Strongly Agree")) +
    scale_color_manual(name = "Chat", 
                       values=c('#C42021','#25283D'),
                       labels=c("Anxious","Normal")) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          axis.text.x = element_text(angle = 60),
          plot.title = element_text(size = 15), 
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10), 
          strip.text.x = ggtext::element_markdown(size = 10, hjust = 0.5)) + 
    facet_grid(order~question_full))

ggsave('bot ques_dep_order.jpeg', plot = plot_bot_ques_dep_order, 
       width = 30, height = 15, units = "cm")

hist_data_anx_dep <- merged_data[merged_data$order == "Anxious first", "q_scl_depression"]
hist_data_anx_dep <- hist_data_anx_dep[seq(1, length(hist_data_anx_dep), 12)]
hist_data_nor_dep <- merged_data[merged_data$order == "Normal first", "q_scl_depression"]
hist_data_nor_dep <- hist_data_nor_dep[seq(1, length(hist_data_nor_dep), 12)]
hist(hist_data_anx_dep, xlim=c(0,60), 
     main = "histogram of depression scores \n from anxious-first group", 
     xlab = "depression scores")
hist(hist_data_nor_dep, xlim=c(0,60), 
     main = "histogram of depression scores \n from normal-first group", 
     xlab = "depression scores")

