# score questionnaires
scoreQuestionnaires <- function(scl90,bfi10) {
  # # # # # # # # # # # # # # # SCL90-R # # # # # # # # # # # # # # #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # read scoring csv
  subsco <- read.csv("experiment1/scoringQuestionnaires/SCL90-R_scoring.csv")
  
  # vector of subscales
  subscales <- unique(subsco$subScale)
  
  # create empty matrix
  scl90_total <- data.frame(matrix(NA,nrow=nrow(scl90),ncol=length(subscales)))
  colnames(scl90_total) <- paste0("scl90_",subscales)
  
  for (i in 1:length(subscales)) {
    tempItems <- paste0("item.",subsco$item[subsco$subScale == subscales[i]])
    scl90_total[,i] <- rowSums(scl90[,tempItems])
  }
  scl90_total <- cbind(Participant.Private.ID=scl90$Participant.Private.ID,
                       scl90_total)
  
  
  
  # # # # # # # # # # # # # # # BFI-10# # # # # # # # # # # # # # # #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # read scoring csv
  subsco <- read.csv("experiment1/scoringQuestionnaires/BFI10_scoring.csv")
  
  # vector of subscales
  subscales <- unique(subsco$subScale)
  
  # create empty matrix
  bfi10_total <- data.frame(matrix(NA,nrow=nrow(bfi10),ncol=length(subscales)))
  colnames(bfi10_total) <- paste0("bfi10_",subscales)
  
  for (i in 1:length(subscales)) {
    tempItems <- paste0("item.",subsco$item[subsco$subScale == subscales[i]])
    bfi10_total[,i] <- rowSums(bfi10[,tempItems])
  }
  bfi10_total <- cbind(Participant.Private.ID=bfi10$Participant.Private.ID,
                       bfi10_total)
  
  
  
  # return
  return(list(scl=scl90_total,bfi=bfi10_total))
}


# add questionnaires to ratings
addQuestionnaireToRating <- function (ratings,scl90,bfi10) {
  # create an empty matrix
  temp <- matrix(NA,nrow=nrow(ratings),ncol=sum(ncol(scl90)-1,ncol(bfi10)-1))
  colnames(temp) <- c(colnames(scl90)[-1],colnames(bfi10)[-1])
  # add temp to ratings
  ratings <- cbind(ratings,temp)
  
  # combine questionnaires
  quests <- cbind(scl90,bfi10[-1])
  
  # vector with participants public id
  Participant.Private.ID <- unique(ratings$Participant.Private.ID)

  for (p in 1:length(Participant.Private.ID)) {
    for (q in 1:ncol(temp)) {
      ratings[ratings$Participant.Private.ID == Participant.Private.ID[p],colnames(temp)[q]] <-
        quests[quests$Participant.Private.ID == Participant.Private.ID[p],colnames(temp)[q]]
    }
  }
 
  # return ratings with questionnaires
  return(ratings)
}


# function used to extract overall information from chat interaction
summariseChatInteraction <- function(task, chat) {
  # get intersect between chat ids and what participants wrote as their ids.
  # remember: one element is not one participant but one chat
  good_ids <- intersect(unique(task$Response),unique(chat$userid))
  
  # sentiment labels are:
  sentiment_label <- c("Mixed","Negative","Neutral","Positive")
  
  # for loop for each element within good_ids
  for (i in 1:length(good_ids)) {
    # extract one chat (each Participant.Private.ID has two chats)
    temp1 <- chat[chat$userid == good_ids[i],]
    temp2 <- task[task$Response == good_ids[i],]
    
    # # # sentiment analysis # # #
    sentiment <- t(matrix(rep(sentiment_label,nrow(temp1)),nrow=4))
    # remove punctuation
    temp1$GPTsentiment <- gsub("[[:punct:]]", "", temp1$GPTsentiment)
    temp1$usersentiment <- gsub("[[:punct:]]", "", temp1$usersentiment)
    # frequency of sentiment_label
    sent_bots <- data.frame(t(colSums(temp1$GPTsentiment==sentiment)))
    sent_user <- data.frame(t(colSums(temp1$usersentiment==sentiment)))
    # label those columns
    colnames(sent_bots) <- paste0("bots_",sentiment_label)
    colnames(sent_user) <- paste0("user_",sentiment_label)
    # user mean words
    user_mean_words <- mean(temp1$userwordnums)
    # user mean reaction time
    user_mean_rt <- mean(temp1$RT)
    # chat mean words
    bots_mean_words <- mean(temp1$GPTwordnums)
    # sentiment mirror %
    perc_mirror <- sum(temp1$GPTsentiment==temp1$usersentiment)/nrow(temp1)
    
    # combine all good_ids
    if (i == 1) {
      combine <- data.frame(temp2,botpersonality=temp1$botpersonality[1],
                            sent_bots,bots_mean_words,
                            sent_user,user_mean_words,user_mean_rt,
                            perc_mirror)
    } else {
      combine <- rbind(combine,
                       data.frame(temp2,botpersonality=temp1$botpersonality[1],
                                  sent_bots,bots_mean_words,
                                  sent_user,user_mean_words,user_mean_rt,
                                  perc_mirror))
    }
  }
  # output
  return(combine)
}

# chat$GPTsentiment <- gsub("[[:punct:]]", "", chat$GPTsentiment)
# chat$usersentiment <- gsub("[[:punct:]]", "", chat$usersentiment)
# table(chat$GPTsentiment,chat$usersentiment)


addRatingsToInteractions <- function (combine, ratings, likert="understood") {
  # remember: one element is not one participant but one chat
  good_ids <- unique(combine$Participant.Private.ID)
  
  for (i in 1:length(good_ids)) {
    temp1 <- combine[combine$Participant.Private.ID == good_ids[i],]
    temp2 <- ratings[ratings$Participant.Private.ID == good_ids[i],]
    temp2 <- temp2[temp2$question == likert,]
    
    # same order
    temp1 <- temp1[order(temp1$chat_name),] 
    temp2 <- temp2[order(temp2$chat),]
    if (sum(temp1$chat_name == temp2$chat) != 2) {
      temp2 <- temp2[temp2$chat == temp1$chat_name,]
    }
   
    
    # combine ratings with interactions
    if (i == 1) {
      output <- data.frame(temp1, likert=temp2$Response,
                           temp2[,grepl("q_scl",colnames(temp2)) | 
                                   grepl("q_bfi",colnames(temp2))])
    } else {
      output <- rbind(output,data.frame(temp1, likert=temp2$Response,
                                        temp2[,grepl("q_scl",colnames(temp2)) | 
                                                grepl("q_bfi",colnames(temp2))]))
    }
  }
  
  # output
  return(output)
}


