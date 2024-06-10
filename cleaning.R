rm(list=ls(all=TRUE))

# print csv?
print_csv <- 1



# create vectors with file names
AB_files <- list.files("experiment1/rawGorilla/data_exp_163181-v2_A&B/")
A_files <- list.files("experiment1/rawGorilla/data_exp_132936-v24_A/")
B_files <- list.files("experiment1/rawGorilla/data_exp_154938-v3_B/")



# # # # # # # # # # # # demographics # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
AB_demo <- read.csv(paste0("experiment1/rawGorilla/data_exp_163181-v2_A&B/",
                           AB_files[grepl("2qbv",AB_files)]))
A_demo <- read.csv(paste0("experiment1/rawGorilla/data_exp_132936-v24_A/",
                           A_files[grepl("8v2c",A_files)]))
B_demo <- read.csv(paste0("experiment1/rawGorilla/data_exp_154938-v3_B/",
                           B_files[grepl("8v2c",B_files)]))

# combine
# relevant columns
cols <- c("Participant.Public.ID","Participant.Private.ID",
          "Participant.Status", "Question", "Key", "Response")

# combine by rows
demographics <- rbind(AB_demo[,cols],A_demo[,cols],B_demo[,cols])

# extracting relevant columns and renaming them
demographics <- demographics[demographics$Participant.Status == "complete" & 
                               demographics$Key == "value" | demographics$Key == "quantised",]
demographics$Question[grepl("age", demographics$Question)] <- "age"
demographics$Question[grepl("gender", demographics$Question)] <- "gender"

demographics_gender <- demographics[demographics$Question == "gender" & demographics$Key == "quantised", ]
demographics_sex <- demographics[demographics$Question == "gender" & demographics$Key == "value", ]
demographics_age <- demographics[demographics$Question == "age", ]

# combine in raw database
if (sum(demographics_sex$Participant.Public.ID == 
        demographics_age$Participant.Public.ID)== nrow(demographics_age)) {
  demo <- cbind(demographics_sex[,c("Participant.Public.ID","Participant.Private.ID")],
    sex=demographics_sex$Response,age=demographics_age$Response)
}






# # creating a summary dataframe
# # manual removal
# demo <- demo[demo$Participant.Private.ID != "9810928",] # removing our team's pilot data
# summary_demographics <- data.frame(matrix(nrow = 6, ncol = 2))
# colnames(summary_demographics)[1] <- "Property"
# colnames(summary_demographics)[2] <- "Value"
# summary_demographics[,1] <- c("N", "num_M", "num_F", "num_other", "mean_Age", "sd_Age")
# 
# gender_distribution <- table(demographics_gender$Response)
# demographics_age$Response <- as.integer(demographics_age$Response)
# # filling in demographic information into the summary sheet
# summary_demographics$Value[summary_demographics$Property == "N"] <-
#   length(unique(demographics_gender$Participant.Private.ID))
# summary_demographics$Value[summary_demographics$Property == "num_M"] <-
#   gender_distribution[names(gender_distribution) == 1]
# summary_demographics$Value[summary_demographics$Property == "num_F"] <-
#   gender_distribution[names(gender_distribution) == 2]
# summary_demographics$Value[summary_demographics$Property == "num_other"] <-
#   gender_distribution[names(gender_distribution) == 3]
# summary_demographics$Value[summary_demographics$Property == "mean_Age"] <-
#   mean(demographics_age$Response)
# summary_demographics$Value[summary_demographics$Property == "sd_Age"] <-
#   sd(demographics_age$Response)







# # # # # # # # # # # # psychological questionnaires# # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BFI10
AB_bfi10 <- read.csv(paste0("experiment1/rawGorilla/data_exp_163181-v2_A&B/",
                           AB_files[grepl("7spi",AB_files)]))
A_bfi10 <- read.csv(paste0("experiment1/rawGorilla/data_exp_132936-v24_A/",
                          A_files[grepl("mcfk",A_files)]))
B_bfi10 <- read.csv(paste0("experiment1/rawGorilla/data_exp_154938-v3_B/",
                          B_files[grepl("mcfk",B_files)]))
# combine
# relevant columns
cols <- c("Participant.Public.ID","Participant.Private.ID",
          "Participant.Status", "Question.Key", "Response")

# combine by rows
bfi10 <- rbind(AB_bfi10[,cols],A_bfi10[,cols],B_bfi10[,cols])
# get responses
bfi10 <- bfi10[grepl("quantised",bfi10$Question.Key),]
# extract item number
bfi10$Question.Key <- gsub(".*-(.*?)-.*", "\\1", bfi10$Question.Key)
# matrix with items numbers (just to check)
mat <- t(matrix(as.matrix(bfi10$Question.Key),ncol=nrow(demo)))
# matrix with individual ID responses
mat <- t(matrix(as.matrix(as.integer(bfi10$Response)),ncol=nrow(demo)))
colnames(mat) <- paste0("item.",1:ncol(mat))
# raw BFI10 
bfi10 <- data.frame(Participant.Public.ID=unique(bfi10$Participant.Public.ID),
                    Participant.Private.ID=unique(bfi10$Participant.Private.ID), mat)






# SCL90
AB_scl90 <- read.csv(paste0("experiment1/rawGorilla/data_exp_163181-v2_A&B/",
                            AB_files[grepl("83ca",AB_files)]))
A_scl90 <- read.csv(paste0("experiment1/rawGorilla/data_exp_132936-v24_A/",
                           A_files[grepl("2bh6",A_files)]))
B_scl90 <- read.csv(paste0("experiment1/rawGorilla/data_exp_154938-v3_B/",
                           B_files[grepl("2bh6",B_files)]))
# combine
# relevant columns
cols <- c("Participant.Public.ID","Participant.Private.ID",
          "Participant.Status", "Question.Key", "Response")

# combine by rows
scl90 <- rbind(AB_scl90[,cols],A_scl90[,cols],B_scl90[,cols])
# get responses
scl90 <- scl90[grepl("quantised",scl90$Question.Key),]
# extract item number
scl90$Question.Key <- gsub("response-1-","",scl90$Question.Key)
scl90$Question.Key <- gsub("-quantised","",scl90$Question.Key)
# matrix with items numbers (just to check)
mat <- t(matrix(as.matrix(scl90$Question.Key),ncol=nrow(demo)))
# matrix with individual ID responses
mat <- t(matrix(as.matrix(as.integer(scl90$Response)),ncol=nrow(demo)))
mat <- mat - 1
colnames(mat) <- paste0("item.",1:ncol(mat))
# raw BFI10 
scl90 <- data.frame(Participant.Public.ID=unique(scl90$Participant.Public.ID),
                    Participant.Private.ID=unique(scl90$Participant.Private.ID), mat)






# # # # # # # # # # # # chatbot task # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# counterbalance AB
AB_tasks <- c("ncut","vjth","4nrv","hu1g","4uru","tvu5","ppql","mghz")
for (i in 1:length(AB_tasks)) {
  temp <- read.csv(paste0("experiment1/rawGorilla/data_exp_163181-v2_A&B/",
                          AB_files[grepl(AB_tasks[i],AB_files)]))
  if (i == 1) {
    AB_dat <- temp
  } else {
    AB_dat <- rbind(AB_dat,temp)
  }
}
# add chat column
AB_dat$chat <- NA
# add chat label
AB_dat$chat[grepl("ncut", AB_dat$Tree.Node.Key)] <- 1
AB_dat$chat[grepl("hu1g", AB_dat$Tree.Node.Key)] <- 1
AB_dat$chat[grepl("4uru", AB_dat$Tree.Node.Key)] <- 1
AB_dat$chat[grepl("mghz", AB_dat$Tree.Node.Key)] <- 1
AB_dat$chat[grepl("vjth", AB_dat$Tree.Node.Key)] <- 2
AB_dat$chat[grepl("4nrv", AB_dat$Tree.Node.Key)] <- 2
AB_dat$chat[grepl("ppql", AB_dat$Tree.Node.Key)] <- 2
AB_dat$chat[grepl("tvu5", AB_dat$Tree.Node.Key)] <- 2

# add chatType column
AB_dat$chatType <- NA
# add chatType label
AB_dat$chatType[grepl("1", AB_dat$chat)] <- "Anxious"
AB_dat$chatType[grepl("2", AB_dat$chat)] <- "Normal"

# add counterbalance column
AB_dat$counterbalance <- NA
# add counterbalance label
AB_dat$counterbalance[grepl("ncut", AB_dat$Tree.Node.Key)] <- "A"
AB_dat$counterbalance[grepl("hu1g", AB_dat$Tree.Node.Key)] <- "A"
AB_dat$counterbalance[grepl("vjth", AB_dat$Tree.Node.Key)] <- "A"
AB_dat$counterbalance[grepl("4nrv", AB_dat$Tree.Node.Key)] <- "A"
AB_dat$counterbalance[grepl("4uru", AB_dat$Tree.Node.Key)] <- "B"
AB_dat$counterbalance[grepl("mghz", AB_dat$Tree.Node.Key)] <- "B"
AB_dat$counterbalance[grepl("ppql", AB_dat$Tree.Node.Key)] <- "B"
AB_dat$counterbalance[grepl("tvu5", AB_dat$Tree.Node.Key)] <- "B"
AB_dat$counterbalance <- as.factor(AB_dat$counterbalance)

# add chat1_first column
AB_dat$chat1_first <- NA
# add chat1_first label
AB_dat$chat1_first[grepl("4uru", AB_dat$Tree.Node.Key)] <- "yes"
AB_dat$chat1_first[grepl("tvu5", AB_dat$Tree.Node.Key)] <- "yes"
AB_dat$chat1_first[grepl("ncut", AB_dat$Tree.Node.Key)] <- "yes"
AB_dat$chat1_first[grepl("vjth", AB_dat$Tree.Node.Key)] <- "yes"
AB_dat$chat1_first[grepl("ppql", AB_dat$Tree.Node.Key)] <- "no"
AB_dat$chat1_first[grepl("mghz", AB_dat$Tree.Node.Key)] <- "no"
AB_dat$chat1_first[grepl("4nrv", AB_dat$Tree.Node.Key)] <- "no"
AB_dat$chat1_first[grepl("hu1g", AB_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
AB_dat$gorillaExp <- "AB"






# counterbalance A
A_tasks <- c("keg8","mrfa","xy27","vs28")
for (i in 1:length(A_tasks)) {
  temp <- read.csv(paste0("experiment1/rawGorilla/data_exp_132936-v24_A/",
                          A_files[grepl(A_tasks[i],A_files)]))
  if (i == 1) {
    A_dat <- temp
  } else {
    A_dat <- rbind(A_dat,temp)
  }
}
# add chat column
A_dat$chat <- NA
# add chat label
A_dat$chat[grepl("keg8", A_dat$Tree.Node.Key)] <- 1
A_dat$chat[grepl("vs28", A_dat$Tree.Node.Key)] <- 1
A_dat$chat[grepl("mrfa", A_dat$Tree.Node.Key)] <- 2
A_dat$chat[grepl("xy27", A_dat$Tree.Node.Key)] <- 2

# add chatType column
A_dat$chatType <- NA
# add chatType label
A_dat$chatType[grepl("1", A_dat$chat)] <- "Anxious"
A_dat$chatType[grepl("2", A_dat$chat)] <- "Normal"

# add counterbalance column
A_dat$counterbalance <- NA
# add counterbalance label
A_dat$counterbalance <- "A"

# add chat1_first column
A_dat$chat1_first <- NA
# add chat1_first label
A_dat$chat1_first[grepl("keg8", A_dat$Tree.Node.Key)] <- "yes"
A_dat$chat1_first[grepl("vs28", A_dat$Tree.Node.Key)] <- "no"
A_dat$chat1_first[grepl("mrfa", A_dat$Tree.Node.Key)] <- "yes"
A_dat$chat1_first[grepl("xy27", A_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
A_dat$gorillaExp <- "A"






# counterbalance B
B_tasks <- c("p8dx","ocuq","19sb","ses1")
for (i in 1:length(B_tasks)) {
  temp <- read.csv(paste0("experiment1/rawGorilla/data_exp_154938-v3_B/",
                          B_files[grepl(B_tasks[i],B_files)]))
  if (i == 1) {
    B_dat <- temp
  } else {
    B_dat <- rbind(B_dat,temp)
  }
}
# add chat column
B_dat$chat <- NA
# add chat label
B_dat$chat[grepl("p8dx", B_dat$Tree.Node.Key)] <- 1
B_dat$chat[grepl("ocuq", B_dat$Tree.Node.Key)] <- 1
B_dat$chat[grepl("19sb", B_dat$Tree.Node.Key)] <- 2
B_dat$chat[grepl("ses1", B_dat$Tree.Node.Key)] <- 2

# add chatType column
B_dat$chatType <- NA
# add chatType label
B_dat$chatType[grepl("1", B_dat$chat)] <- "Anxious"
B_dat$chatType[grepl("2", B_dat$chat)] <- "Normal"

# add counterbalance column
B_dat$counterbalance <- NA
# add counterbalance label
B_dat$counterbalance <- "B"

# add chat1_first column
B_dat$chat1_first <- NA
# add chat1_first label
B_dat$chat1_first[grepl("p8dx", B_dat$Tree.Node.Key)] <- "yes"
B_dat$chat1_first[grepl("ocuq", B_dat$Tree.Node.Key)] <- "no"
B_dat$chat1_first[grepl("19sb", B_dat$Tree.Node.Key)] <- "yes"
B_dat$chat1_first[grepl("ses1", B_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
B_dat$gorillaExp <- "B"





# # # combine # # # ####
# rename shared column with arm
AB_dat$arm <- AB_dat$randomiser.czhu
A_dat$arm <- A_dat$randomiser.bl99
B_dat$arm <- B_dat$randomiser.bl99
# relevant columns
relCols <- c("Participant.Public.ID","Participant.Private.ID","Participant.Status",
             "arm","Response","chat","chatType","counterbalance","chat1_first",
             "gorillaExp")
# combine
dat <- rbind(AB_dat[,relCols],A_dat[,relCols],B_dat[,relCols])
# remove irrelevant rows
dat <- dat[dat$Response != "BEGIN" & dat$Response != "END" & dat$Response != "",]

# length(unique(AB_dat$Participant.Private.ID))
# length(unique(A_dat$Participant.Private.ID))
# length(unique(B_dat$Participant.Private.ID))



# # # # # # # # # # # # chatbot questionnaires# # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# counterbalance AB
AB_tasks <- c("7ewr","6eoj","wcll","eupm")
for (i in 1:length(AB_tasks)) {
  temp <- read.csv(paste0("experiment1/rawGorilla/data_exp_163181-v2_A&B/",
                          AB_files[grepl(AB_tasks[i],AB_files)]))
  if (i == 1) {
    AB_dat <- temp
  } else {
    AB_dat <- rbind(AB_dat,temp)
  }
}

# counterbalance A
A_tasks <- c("4a4q","ewzl")
for (i in 1:length(A_tasks)) {
  temp <- read.csv(paste0("experiment1/rawGorilla/data_exp_132936-v24_A/",
                          A_files[grepl(A_tasks[i],A_files)]))
  if (i == 1) {
    A_dat <- temp
  } else {
    A_dat <- rbind(A_dat,temp)
  }
}

# counterbalance B
B_tasks <- c("gjc4","gbf9")
for (i in 1:length(B_tasks)) {
  temp <- read.csv(paste0("experiment1/rawGorilla/data_exp_154938-v3_B/",
                          B_files[grepl(B_tasks[i],B_files)]))
  if (i == 1) {
    B_dat <- temp
  } else {
    B_dat <- rbind(B_dat,temp)
  }
}

# gorilla experiment
AB_dat$gorillaExp <- "AB"
A_dat$gorillaExp <- "A"
B_dat$gorillaExp <- "B"

# # # combine # # # ####
# rename shared column with arm
AB_dat$arm <- AB_dat$randomiser.czhu
A_dat$arm <- A_dat$randomiser.bl99
B_dat$arm <- B_dat$randomiser.bl99
# relevant columns
relCols <- c("Participant.Public.ID","Participant.Private.ID","Participant.Status",
             "Tree.Node.Key","gorillaExp","arm","Question.Key","Response")
# combine
likert <- rbind(AB_dat[,relCols],A_dat[,relCols],B_dat[,relCols])

# length(unique(AB_dat$Participant.Private.ID))
# length(unique(A_dat$Participant.Private.ID))
# length(unique(B_dat$Participant.Private.ID))

# add counterbalance
likert$counterbalance <- NA
likert$counterbalance[grepl("7ewr", likert$Tree.Node.Key)] <- "A"
likert$counterbalance[grepl("6eoj", likert$Tree.Node.Key)] <- "A"
likert$counterbalance[grepl("4a4q", likert$Tree.Node.Key)] <- "A"
likert$counterbalance[grepl("ewzl", likert$Tree.Node.Key)] <- "A"
likert$counterbalance[grepl("wcll", likert$Tree.Node.Key)] <- "B"
likert$counterbalance[grepl("eupm", likert$Tree.Node.Key)] <- "B"
likert$counterbalance[grepl("gjc4", likert$Tree.Node.Key)] <- "B"
likert$counterbalance[grepl("gbf9", likert$Tree.Node.Key)] <- "B"
likert$counterbalance <- as.factor(likert$counterbalance)

# add order
likert$order <- NA
likert$order[grepl("7ewr", likert$Tree.Node.Key)] <- "Anxious first"
likert$order[grepl("6eoj", likert$Tree.Node.Key)] <- "Normal first"
likert$order[grepl("wcll", likert$Tree.Node.Key)] <- "Anxious first"
likert$order[grepl("eupm", likert$Tree.Node.Key)] <- "Normal first"
likert$order[grepl("4a4q", likert$Tree.Node.Key)] <- "Anxious first"
likert$order[grepl("ewzl", likert$Tree.Node.Key)] <- "Normal first"
likert$order[grepl("gjc4", likert$Tree.Node.Key)] <- "Anxious first"
likert$order[grepl("gbf9", likert$Tree.Node.Key)] <- "Normal first"
likert$order <- as.factor(likert$order)

# add chat
likert$chatType <- NA
likert$chatType[grepl("pat", likert$Question.Key) & likert$counterbalance == "A"] <- "Anxious"
likert$chatType[grepl("pat", likert$Question.Key) & likert$counterbalance == "B"] <- "Normal"
likert$chatType[grepl("alex", likert$Question.Key) & likert$counterbalance == "A"] <- "Normal"
likert$chatType[grepl("alex", likert$Question.Key) & likert$counterbalance == "B"] <- "Anxious"
likert$chatType <- as.factor(likert$chatType)

# add question
likert$question <- NA
likert$question[grepl("similar", likert$Question.Key)] <- "similar"
likert$question[grepl("enjoy", likert$Question.Key)] <- "enjoy"
likert$question[grepl("distant", likert$Question.Key)] <- "distant"
likert$question[grepl("understood", likert$Question.Key)] <- "understood"
likert$question[grepl("different", likert$Question.Key)] <- "different"
likert$question[grepl("chat-again", likert$Question.Key)] <- "chat-again"
likert$question <- as.factor(likert$question)

# remove last irrelevant column
likert$Tree.Node.Key <- NULL

# create qualitative data
qualitative <- likert[grepl("response-",likert$Question.Key),]
# remove columns
qualitative$chatType <- qualitative$question <- NULL

# get quantitative rows
likert <- likert[grepl("quantised",likert$Question.Key),]

# add full question
likert$questionFull <- NA
likert$questionFull[grepl("similar", likert$question)] <- "I felt that <br>we are similar"
likert$questionFull[grepl("enjoy", likert$question)] <- "I enjoyed our <br>conversation"
likert$questionFull[grepl("distant", likert$question)] <- "I felt distant <br>from them"
likert$questionFull[grepl("understood", likert$question)] <- "I felt that <br>they understood me"
likert$questionFull[grepl("different", likert$question)] <- "I felt that they were <br>different from me"
likert$questionFull[grepl("chat-again", likert$question)] <- "I would chat <br>with them again"



# manual removal
demo <- demo[demo$Participant.Private.ID != "9810928",] # removing our team's pilot data
bfi10 <- bfi10[bfi10$Participant.Private.ID != "9810928",]
scl90 <- scl90[scl90$Participant.Private.ID != "9810928",]
dat <- dat[dat$Participant.Private.ID != "9810928",]
qualitative <- qualitative[qualitative$Participant.Private.ID != "9810928",]
likert <- likert[likert$Participant.Private.ID != "9810928",]

# order participant ID
demo <- demo[order(demo$Participant.Private.ID),]
bfi10 <- bfi10[order(bfi10$Participant.Private.ID),]
scl90 <- scl90[order(scl90$Participant.Private.ID),]
dat <- dat[order(dat$Participant.Private.ID),]
qualitative <- qualitative[order(qualitative$Participant.Private.ID),]
likert <- likert[order(likert$Participant.Private.ID),]

demo$Participant.Public.ID <- NULL
bfi10$Participant.Public.ID <- NULL
scl90$Participant.Public.ID <- NULL
dat$Participant.Public.ID <- NULL
qualitative$Participant.Public.ID <- NULL
likert$Participant.Public.ID <- NULL

# print?
if (print_csv == 1) {
  write.csv(demo,"experiment1/cleaned/demographics.csv",row.names = F)
  # write.csv(summary_demographics,"experiment1/cleaned/demographics_summarised.csv",row.names = F)
  write.csv(bfi10,"experiment1/cleaned/bfi10.csv",row.names = F)
  write.csv(scl90,"experiment1/cleaned/scl90.csv",row.names = F)
  write.csv(dat,"experiment1/cleaned/task.csv",row.names = F)
  write.csv(qualitative,"experiment1/cleaned/qualitative.csv",row.names = F)
  write.csv(likert,"experiment1/cleaned/ratings.csv",row.names = F)
}



