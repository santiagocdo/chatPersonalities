# function used to extract overall information from chat ineraction
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
