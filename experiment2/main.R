rm(list=ls(all=TRUE))

if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)

ratings <- read.csv("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/cleaned/ratings.csv")
task <- read.csv("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/cleaned/task.csv")
task <- task[order(task$Participant.Private.ID),]
bfi <- read.csv("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/cleaned/bfi44.csv")

source("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/functions.R")
quest <- scoreQuestionnaires(bfi)

bfi <- quest$bfi

# add questionnaires to ratings
# ratings <- addQuestionnaireToRating(ratings,bfi)
nsubj <- unique(ratings$Participant.Private.ID)
for (p in 1:length(nsubj)) {
    ratings[ratings$Participant.Private.ID == nsubj[p], "bfi_extraversion"] <-
      bfi[bfi$Participant.Private.ID == nsubj[p],"bfi_extraversion"]
    
    ratings[ratings$Participant.Private.ID == nsubj[p], "bfi_agreeableness"] <-
      bfi[bfi$Participant.Private.ID == nsubj[p],"bfi_agreeableness"]
    
    ratings[ratings$Participant.Private.ID == nsubj[p], "bfi_conscientiousness"] <-
      bfi[bfi$Participant.Private.ID == nsubj[p],"bfi_conscientiousness"]
    
    ratings[ratings$Participant.Private.ID == nsubj[p], "bfi_neuroticism"] <-
      bfi[bfi$Participant.Private.ID == nsubj[p],"bfi_neuroticism"]
    
    ratings[ratings$Participant.Private.ID == nsubj[p], "bfi_openness"] <-
      bfi[bfi$Participant.Private.ID == nsubj[p],"bfi_openness"]
}

# add chat as factor with specific order
ratings$chat <- as.factor(ratings$chat)
# levels(ratings$chat) <- c("Anxious","Normal")

full_data <- ratings
write.csv(full_data,"/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/cleaned/full_data.csv",row.names = F)

# removing PIDs after qualitative elimination
elim_PIDs <- c("11566059", "11566066", "11566067", "11566069", "11566073",
               "11566074", "11566081", "11566142", "11566218", "11580653", 
               "11580672", "11580907", "11580923", "11580940", "11581134",
               "11581555", "11581620", "11602380", "11650702", "11651220",
               "11651466", "11651689", "11652569", "11652658", "11652702",
               "11652900", "11653598", "11665444", "11665450", "11665465",
               "11665605", "11665637", "11665717", "11665781", "11665846",
               "11666017", "11666076", "11666153", "11666462", "11666481",
               "11666498", "11666541", "11666650", "11666969", "11667145",
               "11667383", "11667572", "11667573", "11667632", "11667763",
               "11667966", "11668005", "11668041", '11668218', "11668279", 
               "11668368", "11671634", "11671697", "11671818", "11672647", 
               "11674611", "11674631", "11675287", "11913392", "11914120",
               "11931829", "11931864", "11932074", "11932189", "11932374",
               "11932375", "11932406", "11932489", "11932513", "11932694",
               '11934473', '11934547', '11934574', '11934591', '11935138',
               '11935599', '11935789', '11936703', '11940020', '11940898',
               '11941590', '12164951', '12164954', '12165018', '12165146',
               '12165229', '12165240', '12166182', '12207478', '12218235',
               '12219352', '12219557', '12219667', '12220463')

full_data <- full_data[full_data$Participant.Private.ID %in% 
                                  elim_PIDs==TRUE, ]

########## DEMOGRAPHICS ####################

demographics <- read.csv("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/cleaned/demographics.csv")

for (p in 1:length(nsubj)) {
  demographics[demographics$Participant.Private.ID == nsubj[p], "gorillaExp"] <-
    full_data[full_data$Participant.Private.ID == nsubj[p],"gorillaExp"][1]
}

# removing participant status column 
demographics$Participant.Status <- NULL

# renaming the columns
demographics <- demographics %>% 
  rename(PID = Participant.Private.ID) # if this line does not work, restart the R session

# converting data structure of PID column to be factorial
demographics$PID <- as.factor(demographics$PID)

# adding columns and filling them out based on Response
demographics$sex[demographics$sex == "Male"] <- "1"
demographics$sex[demographics$sex == "Female"] <- "2"
demographics$sex[demographics$sex == "Non-Binary"] <- "3"


participants_unique <- unique(demographics$PID)


demographics$age <- as.integer(demographics$age)
demographics$sex <- as.factor(demographics$sex)
demographics$PID <- as.factor(demographics$PID)

genderDist <- table(demographics$sex)
genderDist_cat <- table(demographics$sex, demographics$gorillaExp)


# creating empty dataframe for demographic information
summary_demographics <- data.frame(matrix(nrow = 18, ncol = 2))
summary_demographics <- summary_demographics %>% rename(Property = X1, Value = X2)
summary_demographics[,1] <- c("N", "number of males", "number of females", 
                                 "number of other genders", 
                                 "mean age", "sd age", 
                                 
                                 "number of participants in counterbalanceA", 
                                 "number of males in counterbalanceA",
                                 "number of females in counterbalanceA", 
                                 "number of other genders in counterbalanceA",
                                 "mean age in counterbalanceA", 
                                 "sd age in counterbalanceA", 
                                 
                                 "number of participants in counterbalanceB", 
                                 "number of males in counterbalanceB",
                                 "number of females in counterbalanceB",
                                 "number of other genders in counterbalanceB",
                                 "mean age in counterbalanceB", "sd age in counterbalanceB") 

demographics <- na.omit(demographics)

# filling in demographic information
summary_demographics$Value[summary_demographics$Property == "N"] <- length(unique(demographics$PID))
summary_demographics$Value[summary_demographics$Property == "number of males"] <- genderDist[names(genderDist) == 1]
summary_demographics$Value[summary_demographics$Property == "number of females"] <- genderDist[names(genderDist) == 2]
summary_demographics$Value[summary_demographics$Property == "number of other genders"] <- genderDist[names(genderDist) == 3]
summary_demographics$Value[summary_demographics$Property == "mean age"] <- mean(demographics$age, na.rm = TRUE)
summary_demographics$Value[summary_demographics$Property == "sd age"] <- sd(demographics$age, na.rm = TRUE)

summary_demographics$Value[summary_demographics$Property == "number of participants in counterbalanceA"] <- length(unique(demographics$PID[demographics$gorillaExp == "A"]))
summary_demographics$Value[summary_demographics$Property == "number of males in counterbalanceA"] <- genderDist_cat[names(genderDist) == 1, "A"]
summary_demographics$Value[summary_demographics$Property == "number of females in counterbalanceA"] <- genderDist_cat[names(genderDist) == 2, "A"]
summary_demographics$Value[summary_demographics$Property == "number of other genders in counterbalanceA"] <- genderDist_cat[names(genderDist) == 3, "A"]
summary_demographics$Value[summary_demographics$Property == "mean age in counterbalanceA"] <- mean(demographics$age[demographics$gorillaExp == "A"], na.rm = TRUE)
summary_demographics$Value[summary_demographics$Property == "sd age in counterbalanceA"] <- sd(demographics$age[demographics$gorillaExp == "A"], na.rm = TRUE)

summary_demographics$Value[summary_demographics$Property == "number of participants in counterbalanceB"] <- length(unique(demographics$PID[demographics$gorillaExp == "B"]))
summary_demographics$Value[summary_demographics$Property == "number of males in counterbalanceB"] <- genderDist_cat[names(genderDist) == 1, "B"]
summary_demographics$Value[summary_demographics$Property == "number of females in counterbalanceB"] <- genderDist_cat[names(genderDist) == 2, "B"]
summary_demographics$Value[summary_demographics$Property == "number of other genders in counterbalanceB"] <- genderDist_cat[names(genderDist) == 3, "B"]
summary_demographics$Value[summary_demographics$Property == "mean age in counterbalanceB"] <- mean(demographics$age[demographics$gorillaExp == "B"], na.rm = TRUE)
summary_demographics$Value[summary_demographics$Property == "sd age in counterbalanceB"] <- sd(demographics$age[demographics$gorillaExp == "B"], na.rm = TRUE)

# rounding up all values to 3 decimal places
summary_demographics$Value <- as.numeric(summary_demographics$Value)
summary_demographics$Value <- format(round(summary_demographics$Value, 3), nsmall = 2)
View(summary_demographics)

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
    coord_cartesian(ylim = c(2.0,4.2)) +
    scale_color_manual(name = "Bot \nPersonality", 
                       values=c('#C42021','#25283D'),
                       labels=c("Extrovert","Introvert")) +
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
    coord_cartesian(ylim = c(2.0,4.0)) +
    scale_color_manual(name = "Bot \nPersonality", 
                       values=c('#C42021','#25283D'),
                       labels=c("Extrovert","Introvert")) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12), 
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size = 16, hjust = 0.5))
)

summary(lm(Response~chat,full_data[ratings$question=="chat-again",]))
summary(lm(Response~chat,full_data[ratings$question=="different",]))
summary(lm(Response~chat,full_data[ratings$question=="distant",]))
summary(lm(Response~chat,full_data[ratings$question=="enjoy",]))
summary(lm(Response~chat,full_data[ratings$question=="similar",]))
summary(lm(Response~chat,full_data[ratings$question=="understood",]))

ggsave("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/figures/likert.jpeg", 
       plot_likert, width = 10, height = 7)

ggsave("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/figures/likert_byorder.jpeg", 
       plot_likert_byorder, width = 10, height = 8)

# creating a mega-plot with mini-plots, one for each subscale
library(gridExtra)

NAME_LIST <- c("bfi_extraversion", "bfi_agreeableness",
               "bfi_conscientiousness", "bfi_neuroticism", "bfi_openness"
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
        "chat-again" = "I would chat \nwith them again", 
        "different" = "I felt that they \nwere different from me", 
        "distant" = "I felt distant \nfrom them", 
        "enjoy" = "I enjoyed our \nconversation", 
        "similar" = "I felt that we \nare similar", 
        "understood" = "I felt that \nthey understood me"
      )
    )) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                       labels = c("Strongly Disagree", "Disagree", 
                                  "Neutral", "Agree", "Strongly Agree")) +
    scale_color_manual(name = "Bot \nPersonality",
                       values = c('#C42021', '#25283D'), 
                       labels = c("Extrovert", "Introvert")) +
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

grid_plot <- marrangeGrob(plot_list, nrow=2, ncol=3)

ggsave("/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/mega_plot.pdf", 
       grid_plot, width = 20, height = 15)

# statisitcal analysis
summary(lm(Response~chat*bfi_extraversion,full_data[ratings$question=="chat-again",]))
summary(lm(Response~chat*bfi_agreeableness,full_data[ratings$question=="chat-again",]))
summary(lm(Response~chat*bfi_conscientiousness,full_data[ratings$question=="chat-again",]))
summary(lm(Response~chat*bfi_neuroticism,full_data[ratings$question=="chat-again",]))
summary(lm(Response~chat*bfi_openness,full_data[ratings$question=="chat-again",]))

summary(lm(Response~chat*bfi_extraversion,full_data[ratings$question=="different",]))
summary(lm(Response~chat*bfi_agreeableness,full_data[ratings$question=="different",]))
summary(lm(Response~chat*bfi_conscientiousness,full_data[ratings$question=="different",]))
summary(lm(Response~chat*bfi_neuroticism,full_data[ratings$question=="different",]))
summary(lm(Response~chat*bfi_openness,full_data[ratings$question=="different",]))

summary(lm(Response~chat*bfi_extraversion,full_data[ratings$question=="distant",]))
summary(lm(Response~chat*bfi_agreeableness,full_data[ratings$question=="distant",]))
summary(lm(Response~chat*bfi_conscientiousness,full_data[ratings$question=="distant",]))
summary(lm(Response~chat*bfi_neuroticism,full_data[ratings$question=="distant",]))
summary(lm(Response~chat*bfi_openness,full_data[ratings$question=="distant",]))

summary(lm(Response~chat*bfi_extraversion,full_data[ratings$question=="enjoy",]))
summary(lm(Response~chat*bfi_agreeableness,full_data[ratings$question=="enjoy",]))
summary(lm(Response~chat*bfi_conscientiousness,full_data[ratings$question=="enjoy",]))
summary(lm(Response~chat*bfi_neuroticism,full_data[ratings$question=="enjoy",]))
summary(lm(Response~chat*bfi_openness,full_data[ratings$question=="enjoy",]))

summary(lm(Response~chat*bfi_extraversion,full_data[ratings$question=="similar",]))
summary(lm(Response~chat*bfi_agreeableness,full_data[ratings$question=="similar",]))
summary(lm(Response~chat*bfi_conscientiousness,full_data[ratings$question=="similar",]))
summary(lm(Response~chat*bfi_neuroticism,full_data[ratings$question=="similar",]))
summary(lm(Response~chat*bfi_openness,full_data[ratings$question=="similar",]))

summary(lm(Response~chat*bfi_extraversion,full_data[ratings$question=="understood",]))
summary(lm(Response~chat*bfi_agreeableness,full_data[ratings$question=="understood",]))
summary(lm(Response~chat*bfi_conscientiousness,full_data[ratings$question=="understood",]))
summary(lm(Response~chat*bfi_neuroticism,full_data[ratings$question=="understood",]))
summary(lm(Response~chat*bfi_openness,full_data[ratings$question=="understood",]))

# Create an empty data frame to store results
results <- data.frame(
  Question = character(),
  BFI_Factor = character(),
  Predictor = character(),
  Estimate = numeric(),
  Std_Error = numeric(),
  t_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Define questions and BFI factors
questions <- c("chat-again", "different", "distant", "enjoy", "similar", "understood")
bfi_factors <- c("bfi_extraversion", "bfi_agreeableness", "bfi_conscientiousness", "bfi_neuroticism", "bfi_openness")

# Loop through each question and BFI factor
for (question in questions) {
  for (bfi in bfi_factors) {
    # Fit the model
    model <- summary(lm(Response ~ chat * get(bfi), data = full_data[ratings$question == question, ]))
    
    # Extract coefficients
    coefficients <- as.data.frame(model$coefficients)
    
    # Add rows to results
    for (i in 1:nrow(coefficients)) {
      results <- rbind(
        results,
        data.frame(
          Question = question,
          BFI_Factor = gsub("bfi_", "", bfi),
          Predictor = rownames(coefficients)[i],
          Estimate = round(coefficients[i, "Estimate"], 3),
          Std_Error = round(coefficients[i, "Std. Error"], 3),
          t_value = round(coefficients[i, "t value"], 3),
          p_value = round(coefficients[i, "Pr(>|t|)"], 3)
        )
      )
    }
  }
}

# Save results to CSV
write.csv(results, "/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/results_bdi.csv", row.names = FALSE)

############
# histograms for the bfi factors

output_dir <- "/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/figures/histograms/"
for (bfi in bfi_factors) {
  p <- ggplot(full_data, aes_string(x = bfi)) +
    geom_histogram(
      bins = 30,                   
      fill = "lightblue",          
      color = "black"              
    ) +
    labs(
      title = paste("Histogram of", gsub("_", " ", bfi)),  
      x = gsub("_", " ", bfi),                             
      y = "Frequency"                                      
    ) +
    theme_minimal()                                        
  
  # Define the file name and path
  file_name <- paste0(output_dir, bfi, ".png")
  
  # Save the plot as a PNG file
  ggsave(file_name, plot = p, width = 6, height = 4, dpi = 300)
}

  


# Create an empty data frame to store results
results <- data.frame(
  Question = character(),
  Order = character(),
  Predictor = character(),
  Estimate = numeric(),
  Std_Error = numeric(),
  t_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Define questions and BFI factors
questions <- c("chat-again", "different", "distant", "enjoy", "similar", "understood")
order_factors <- c("Extrovert first", "Introvert first")
# Loop through each question and chat order
for (question in questions) {
  for (order in order_factors) {
    # Fit the model
    model <- summary(lm(Response ~ chat * order, data = full_data[ratings$question == question, ]))
    
    # Extract coefficients
    coefficients <- as.data.frame(model$coefficients)
    
    # Add rows to results
    for (i in 1:nrow(coefficients)) {
      results <- rbind(
        results,
        data.frame(
          Question = question,
          Order = order,
          Predictor = rownames(coefficients)[i],
          Estimate = round(coefficients[i, "Estimate"], 3),
          Std_Error = round(coefficients[i, "Std. Error"], 3),
          t_value = round(coefficients[i, "t value"], 3),
          p_value = round(coefficients[i, "Pr(>|t|)"], 3)
        )
      )
    }
  }
}

# Save results to CSV
write.csv(results, "/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/results_blockorder.csv", row.names = FALSE)




# Create an empty data frame to store results
results <- data.frame(
  Question = character(),
  Order = character(),
  Predictor = character(),
  Estimate = numeric(),
  Std_Error = numeric(),
  t_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Define questions and BFI factors
questions <- c("chat-again", "different", "distant", "enjoy", "similar", "understood")
# Loop through each question and chat order
for (question in questions) {
    # Fit the model
    model <- summary(lm(Response ~ chat, data = full_data[ratings$question == question, ]))
    
    # Extract coefficients
    coefficients <- as.data.frame(model$coefficients)
    
    # Add rows to results
    for (i in 1:nrow(coefficients)) {
      results <- rbind(
        results,
        data.frame(
          Question = question,
          Predictor = rownames(coefficients)[i],
          Estimate = round(coefficients[i, "Estimate"], 3),
          Std_Error = round(coefficients[i, "Std. Error"], 3),
          t_value = round(coefficients[i, "t value"], 3),
          p_value = round(coefficients[i, "Pr(>|t|)"], 3)
        )
      )
    }
  }


# Save results to CSV
write.csv(results, "/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_OneReach/1_Data&Analysis/FinalAnalysis_SCdOandRJP/chatPersonalities/experiment2/results_chatbots.csv", row.names = FALSE)
