rm(list=ls(all=TRUE))

# print csv?
print_csv <- 0



# create vectors with file names
A_files <- list.files("experiment2/rawGorilla/data_exp_183710-v1-A")
A_files2 <- list.files("experiment2/rawGorilla/data_exp_183710-v2-A")
B_files <- list.files("experiment2/rawGorilla/data_exp_183710-v1-B")
B_files2 <- list.files("experiment2/rawGorilla/data_exp_183710-v2-B")



# # # # # # # # # # # # demographics # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
A_demo <- read.csv("experiment2/rawGorilla/data_exp_183710-v1-A/data_exp_183710-v5_questionnaire-2qbv.csv")
A_demo2 <- read.csv("experiment2/rawGorilla/data_exp_183710-v2-A/data_exp_183710-v7_questionnaire-2qbv.csv")
B_demo <- read.csv("experiment2/rawGorilla/data_exp_183710-v1-B/data_exp_183710-v6_questionnaire-2qbv.csv")
B_demo2 <- read.csv("experiment2/rawGorilla/data_exp_183710-v2-B/data_exp_183710-v9_questionnaire-2qbv.csv")

# combine
# relevant columns
cols <- c("Participant.Public.ID","Participant.Private.ID",
          "Participant.Status", "Question", "Key", "Response")

# combine by rows
demographics <- rbind(A_demo[,cols],B_demo[,cols],A_demo2[,cols], B_demo2[,cols])

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




# # # # # # # # # # # # psychological questionnaires# # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# BFI10
A_bfi <- read.csv("experiment2/rawGorilla/data_exp_183710-v1-A/data_exp_183710-v5_questionnaire-cq39.csv")
A_bfi2 <- read.csv("experiment2/rawGorilla/data_exp_183710-v2-A/data_exp_183710-v7_questionnaire-cq39.csv")
B_bfi <- read.csv("experiment2/rawGorilla/data_exp_183710-v1-B/data_exp_183710-v6_questionnaire-cq39.csv")
B_bfi2 <- read.csv("experiment2/rawGorilla/data_exp_183710-v2-B/data_exp_183710-v9_questionnaire-cq39.csv")

# combine
# relevant columns
cols <- c("Participant.Public.ID","Participant.Private.ID",
          "Participant.Status", "Question.Key", "Response")

# combine by rows
bfi <- rbind(A_bfi[,cols],B_bfi[,cols], A_bfi2[,cols], B_bfi2[,cols])
# get responses
bfi <- bfi[grepl("quantised",bfi$Question.Key),]
# extract item number
bfi$Question.Key <- gsub(".*-(.*?)-.*", "\\1", bfi$Question.Key)
# matrix with items numbers (just to check)
mat <- t(matrix(as.matrix(bfi$Question.Key),ncol=nrow(demo)))
# matrix with individual ID responses
mat <- t(matrix(as.matrix(as.integer(bfi$Response)),ncol=nrow(demo)))
colnames(mat) <- paste0("item.",1:ncol(mat))
# raw BFI10 
# bfi <- data.frame(Participant.Public.ID=unique(bfi$Participant.Public.ID),
#                   Participant.Private.ID=unique(bfi$Participant.Private.ID), mat)
bfi <- data.frame(Participant.Private.ID=unique(bfi$Participant.Private.ID), mat)





# # # # # # # # # # # # chatbot task # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# counterbalance A
A_tasks <- c("iehl","pman","ufqh","wzw3")
for (i in 1:length(A_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v1-A/",
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
A_dat$chat[grepl("iehl", A_dat$Tree.Node.Key)] <- 1
A_dat$chat[grepl("ufqh", A_dat$Tree.Node.Key)] <- 1
A_dat$chat[grepl("pman", A_dat$Tree.Node.Key)] <- 2
A_dat$chat[grepl("wzw3", A_dat$Tree.Node.Key)] <- 2

# add chatType column
A_dat$chatType <- NA
# add chatType label
A_dat$chatType[grepl("1", A_dat$chat)] <- "Extrovert"
A_dat$chatType[grepl("2", A_dat$chat)] <- "Introvert"

# add counterbalance column
A_dat$counterbalance <- NA
# add counterbalance label
A_dat$counterbalance <- "A"

# add chat1_first column
A_dat$chat1_first <- NA
# add chat1_first label
A_dat$chat1_first[grepl("ufqh", A_dat$Tree.Node.Key)] <- "yes"
A_dat$chat1_first[grepl("iehl", A_dat$Tree.Node.Key)] <- "no"
A_dat$chat1_first[grepl("wzw3", A_dat$Tree.Node.Key)] <- "yes"
A_dat$chat1_first[grepl("pman", A_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
A_dat$gorillaExp <- "A"



# counterbalance A
A_tasks <- c("iehl","pman","ufqh","wzw3")
for (i in 1:length(A_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v2-A/",
                          A_files2[grepl(A_tasks[i],A_files)]))
  if (i == 1) {
    A2_dat <- temp
  } else {
    A2_dat <- rbind(A2_dat,temp)
  }
}
# add chat column
A2_dat$chat <- NA
# add chat label
A2_dat$chat[grepl("iehl", A2_dat$Tree.Node.Key)] <- 1
A2_dat$chat[grepl("ufqh", A2_dat$Tree.Node.Key)] <- 1
A2_dat$chat[grepl("pman", A2_dat$Tree.Node.Key)] <- 2
A2_dat$chat[grepl("wzw3", A2_dat$Tree.Node.Key)] <- 2

# add chatType column
A2_dat$chatType <- NA
# add chatType label
A2_dat$chatType[grepl("1", A2_dat$chat)] <- "Extrovert"
A2_dat$chatType[grepl("2", A2_dat$chat)] <- "Introvert"

# add counterbalance column
A2_dat$counterbalance <- NA
# add counterbalance label
A2_dat$counterbalance <- "A"

# add chat1_first column
A2_dat$chat1_first <- NA
# add chat1_first label
A2_dat$chat1_first[grepl("ufqh", A2_dat$Tree.Node.Key)] <- "yes"
A2_dat$chat1_first[grepl("iehl", A2_dat$Tree.Node.Key)] <- "no"
A2_dat$chat1_first[grepl("wzw3", A2_dat$Tree.Node.Key)] <- "yes"
A2_dat$chat1_first[grepl("pman", A2_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
A2_dat$gorillaExp <- "A"



# counterbalance B
B_tasks <- c("71pp","n6lp","p97z","xsqk")
for (i in 1:length(B_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v1-B/",
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
B_dat$chat[grepl("p97z", B_dat$Tree.Node.Key)] <- 1
B_dat$chat[grepl("xsqk", B_dat$Tree.Node.Key)] <- 1
B_dat$chat[grepl("71pp", B_dat$Tree.Node.Key)] <- 2
B_dat$chat[grepl("n6lp", B_dat$Tree.Node.Key)] <- 2

# add chatType column
B_dat$chatType <- NA
# add chatType label
B_dat$chatType[grepl("1", B_dat$chat)] <- "Extrovert"
B_dat$chatType[grepl("2", B_dat$chat)] <- "Introvert"

# add counterbalance column
B_dat$counterbalance <- NA
# add counterbalance label
B_dat$counterbalance <- "B"

# add chat1_first column
B_dat$chat1_first <- NA
# add chat1_first label
B_dat$chat1_first[grepl("xsqk", B_dat$Tree.Node.Key)] <- "yes"
B_dat$chat1_first[grepl("71pp", B_dat$Tree.Node.Key)] <- "no"
B_dat$chat1_first[grepl("n6lp", B_dat$Tree.Node.Key)] <- "yes"
B_dat$chat1_first[grepl("p97z", B_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
B_dat$gorillaExp <- "B"



# counterbalance B
B2_tasks <- c("71pp","n6lp","p97z","xsqk")
for (i in 1:length(B2_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v2-B/",
                          B_files2[grepl(B2_tasks[i],B_files2)]))
  if (i == 1) {
    B2_dat <- temp
  } else {
    B2_dat <- rbind(B2_dat,temp)
  }
}
# add chat column
B2_dat$chat <- NA
# add chat laB2el
B2_dat$chat[grepl("p97z", B2_dat$Tree.Node.Key)] <- 1
B2_dat$chat[grepl("xsqk", B2_dat$Tree.Node.Key)] <- 1
B2_dat$chat[grepl("71pp", B2_dat$Tree.Node.Key)] <- 2
B2_dat$chat[grepl("n6lp", B2_dat$Tree.Node.Key)] <- 2

# add chatType column
B2_dat$chatType <- NA
# add chatType laB2el
B2_dat$chatType[grepl("1", B2_dat$chat)] <- "Extrovert"
B2_dat$chatType[grepl("2", B2_dat$chat)] <- "Introvert"

# add counterB2alance column
B2_dat$counterbalance <- NA
# add counterB2alance laB2el
B2_dat$counterbalance <- "B2"

# add chat1_first column
B2_dat$chat1_first <- NA
# add chat1_first laB2el
B2_dat$chat1_first[grepl("xsqk", B2_dat$Tree.Node.Key)] <- "yes"
B2_dat$chat1_first[grepl("71pp", B2_dat$Tree.Node.Key)] <- "no"
B2_dat$chat1_first[grepl("n6lp", B2_dat$Tree.Node.Key)] <- "yes"
B2_dat$chat1_first[grepl("p97z", B2_dat$Tree.Node.Key)] <- "no"
# gorilla experiment
B2_dat$gorillaExp <- "B2"



# # # combine # # # ####
# rename shared column with arm
A_dat$arm <- A_dat$randomiser.czhu
A2_dat$arm <- A2_dat$randomiser.czhu
B_dat$arm <- B_dat$randomiser.czhu
B2_dat$arm <- B2_dat$randomiser.czhu
# relevant columns
relCols <- c("Participant.Public.ID","Participant.Private.ID","Participant.Status", "Display",
             "arm","Response","chat","chatType","counterbalance","chat1_first",
             "gorillaExp")
# combine
dat <- rbind(A_dat[,relCols],A2_dat[,relCols],B_dat[,relCols], B2_dat[,relCols])
# remove irrelevant rows
dat <- dat[dat$Response != "BEGIN" & dat$Response != "END" & dat$Response != "",]

# length(unique(AB_dat$Participant.Private.ID))
# length(unique(A_dat$Participant.Private.ID))
# length(unique(B_dat$Participant.Private.ID))



# # # # # # # # # # # # chatbot questionnaires# # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# counterbalance A
A_tasks <- c("eupm","wcll")
for (i in 1:length(A_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v1-A/",
                          A_files[grepl(A_tasks[i],A_files)]))
  if (i == 1) {
    A_dat <- temp
  } else {
    A_dat <- rbind(A_dat,temp)
  }
}

for (i in 1:length(A_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v2-A/",
                          A_files2[grepl(A_tasks[i],A_files)]))
  if (i == 1) {
    A2_dat <- temp
  } else {
    A2_dat <- rbind(A2_dat,temp)
  }
}



# counterbalance B
B_tasks <- c("eupm","wcll")
for (i in 1:length(B_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v1-B/",
                          B_files[grepl(B_tasks[i],B_files)]))
  if (i == 1) {
    B_dat <- temp
  } else {
    B_dat <- rbind(B_dat,temp)
  }
}

B_tasks <- c("eupm","wcll")
for (i in 1:length(B_tasks)) {
  temp <- read.csv(paste0("experiment2/rawGorilla/data_exp_183710-v2-B/",
                          B_files2[grepl(B_tasks[i],B_files2)]))
  if (i == 1) {
    B2_dat <- temp
  } else {
    B2_dat <- rbind(B2_dat,temp)
  }
}

# gorilla experiment
A_dat$gorillaExp <- "A"
A2_dat$gorillaExp <- "A"
B_dat$gorillaExp <- "B"
B2_dat$gorillaExp <- "B"



# # # combine # # # ####
# rename shared column with arm
A_dat$arm <- A_dat$randomiser.czhu
A2_dat$arm <- A2_dat$randomiser.czhu
B_dat$arm <- B_dat$randomiser.czhu
B2_dat$arm <- B2_dat$randomiser.czhu

# relevant columns
relCols <- c("Participant.Public.ID","Participant.Private.ID","Participant.Status",
             "Tree.Node.Key","gorillaExp","arm","Question.Key","Response")
# combine
likert <- rbind(A_dat[,relCols], A2_dat[,relCols], B_dat[,relCols], B2_dat[,relCols])



# add order
likert$order <- NA
likert$order[grepl("wcll", likert$Tree.Node.Key)] <- "Extrovert first"
likert$order[grepl("eupm", likert$Tree.Node.Key)] <- "Introvert first"
likert$order <- as.factor(likert$order)

# add chat
likert$chatType <- NA
likert$chatType[grepl("pat", likert$Question.Key) & likert$gorillaExp == "A"] <- "Introvert"
likert$chatType[grepl("pat", likert$Question.Key) & likert$gorillaExp == "B"] <- "Extrovert"
likert$chatType[grepl("alex", likert$Question.Key) & likert$gorillaExp == "A"] <- "Extrovert"
likert$chatType[grepl("alex", likert$Question.Key) & likert$gorillaExp == "B"] <- "Introvert"
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

# add chat condition
likert$chat_condition <- NA
likert$chat_condition[grepl("A_1AlexExt_2PatInt", likert$arm)] <- "alex_ext"
likert$chat_condition[grepl("A_2PatInt_1AlexExt", likert$arm)] <- "pat_int"
likert$chat_condition[grepl("B_1PatExt_2AlexInt", likert$arm)] <- "pat_ext"
likert$chat_condition[grepl("B_2AlexInt_1PatExt", likert$arm)] <- "alex_int"

# remove last irrelevant column
likert$Tree.Node.Key <- NULL

# create qualitative data
qualitative <- likert[grepl("response-",likert$Question.Key),]
# remove columns
qualitative$chatType <- qualitative$question <- NULL
# all to lower
qualitative$Response[grepl("response-1",qualitative$Question.Key)] <- 
  tolower(qualitative$Response[grepl("response-1",qualitative$Question.Key)])
# name of anxious bot
qualitative$anx_bot_name <- dplyr::recode(qualitative$chat_condition,"alex_ext"="alex",
                                          "pat_int"="pat","pat_ext"="pat","alex_int"="alex")
# prefer anxious?
qualitative$pref_ext <- NA
qualitative$pref_ext[grepl("response-1-1",qualitative$Question.Key)] <- 
  qualitative$Response[grepl("response-1-1",qualitative$Question.Key)] == qualitative$anx_bot_name[grepl("response-1-1",qualitative$Question.Key)]


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



# order participant ID
demo <- demo[order(demo$Participant.Private.ID),]
bfi <- bfi[order(bfi$Participant.Private.ID),]
# scl90 <- scl90[order(scl90$Participant.Private.ID),]
dat <- dat[order(dat$Participant.Private.ID),]
qualitative <- qualitative[order(qualitative$Participant.Private.ID),]
likert <- likert[order(likert$Participant.Private.ID),]

demo$Participant.Public.ID <- NULL
bfi$Participant.Public.ID <- NULL
# scl90$Participant.Public.ID <- NULL
dat$Participant.Public.ID <- NULL
qualitative$Participant.Public.ID <- NULL
likert$Participant.Public.ID <- NULL

# print?
if (print_csv == 1) {
  write.csv(demo,"experiment2/cleaned/demographics.csv",row.names = F)
  # write.csv(summary_demographics,"experiment1/cleaned/demographics_summarised.csv",row.names = F)
  write.csv(bfi,"experiment2/cleaned/bfi44.csv",row.names = F)
  # write.csv(scl90,"experiment2/cleaned/scl90.csv",row.names = F)
  write.csv(dat,"experiment2/cleaned/task.csv",row.names = F)
  write.csv(qualitative,"experiment2/cleaned/qualitative.csv",row.names = F)
  write.csv(likert,"experiment2/cleaned/ratings.csv",row.names = F)
}


