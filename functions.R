# opposite to intersect
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

# score questionnaires Experiment 1
scoreQuestionnaires_e1 <- function(scl90, bfi10) {
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

# score questionnaires Experiment 2
scoreQuestionnaires_e2 <- function(bfi44) {
  # # # # # # # # # # # # # # # BFI-10# # # # # # # # # # # # # # # #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # read scoring csv
  subsco <- read.csv("experiment2/scoringQuestionnaires/BFI44_scoring.csv")
  
  # vector of subscales
  subscales <- unique(subsco$subScale)
  
  # create empty matrix
  bfi44_total <- data.frame(matrix(NA,nrow=nrow(bfi44),ncol=length(subscales)))
  colnames(bfi44_total) <- paste0("bfi44_",subscales)
  
  for (i in 1:length(subscales)) {
    tempItems <- paste0("item.",subsco$item[subsco$subScale == subscales[i]])
    bfi44_total[,i] <- rowSums(bfi44[,tempItems])
  }
  bfi44_total <- cbind(Participant.Private.ID=bfi44$Participant.Private.ID,
                       bfi44_total)
  
  # return
  return(list(bfi=bfi44_total))
}

# add questionnaires to ratings Experiment 1
addQuestionnaireToDataFrame_e1 <- function (dataframe,scl90,bfi10) {
  # create an empty matrix
  temp <- matrix(NA,nrow=nrow(dataframe),ncol=sum(ncol(scl90)-1,ncol(bfi10)-1))
  colnames(temp) <- c(colnames(scl90)[-1],colnames(bfi10)[-1])
  # add temp to ratings
  dataframe <- cbind(dataframe,temp)
  
  # combine questionnaires
  quests <- cbind(scl90,bfi10[-1])
  
  # vector with participants public id
  Participant.Private.ID <- unique(dataframe$Participant.Private.ID)

  for (p in 1:length(Participant.Private.ID)) {
    for (q in 1:ncol(temp)) {
      dataframe[dataframe$Participant.Private.ID == Participant.Private.ID[p],colnames(temp)[q]] <-
        quests[quests$Participant.Private.ID == Participant.Private.ID[p],colnames(temp)[q]]
    }
  }
 
  # return ratings with questionnaires
  return(dataframe)
}

# add questionnaires to ratings Experiment 2
addQuestionnaireToDataFrame_e2 <- function (dataframe,bfi44) {
  # create an empty matrix
  temp <- matrix(NA,nrow=nrow(dataframe),ncol=sum(ncol(bfi44)-1))
  colnames(temp) <- colnames(bfi44)[-1]
  # add temp to ratings
  dataframe <- cbind(dataframe,temp)
  
  # combine questionnaires (add here another quest, if needed)
  quests <- cbind(bfi44)
  
  # vector with participants public id
  Participant.Private.ID <- unique(dataframe$Participant.Private.ID)
  
  for (p in 1:length(Participant.Private.ID)) {
    for (q in 1:ncol(temp)) {
      dataframe[dataframe$Participant.Private.ID == Participant.Private.ID[p],colnames(temp)[q]] <-
        quests[quests$Participant.Private.ID == Participant.Private.ID[p],colnames(temp)[q]]
    }
  }
  
  # return ratings with questionnaires
  return(dataframe)
}


# function used to extract overall information from chat interaction
summariseChatInteraction_v2 <- function(task, chat, ratings) {
  # get intersect between chat ids and what participants wrote as their ids.
  # remember: one element is not one participant but one chat
  task$good_ids <- paste0(task$Participant.Private.ID,task$chatType)
  chat$good_ids <- paste0(chat$PID,chat$botpersonality)

  # intersection
  good_ids <- intersect(chat$good_ids,task$good_ids)
  Participant.Private.ID <- unique(ratings$Participant.Private.ID)
  
  # sentiment labels are:
  sentiment_label <- c("Mixed","Negative","Neutral","Positive")
  
  # for loop for each element within good_ids
  for (i in 1:length(good_ids)) {
    # extract one chat (each Participant.Private.ID has two chats)
    temp1 <- chat[chat$good_ids == good_ids[i],]
    temp2 <- task[task$good_ids == good_ids[i],]
    temp3 <- ratings[ratings$Participant.Private.ID == temp2$Participant.Private.ID,]
    
    # # # sentiment analysis # # #
    sentiment <- t(matrix(rep(sentiment_label,nrow(temp1)),nrow=4))
    # remove punctuation
    temp1$GPTsentiment <- gsub("[[:punct:]]", "", temp1$GPTsentiment)
    temp1$GPTsentiment <- gsub(" ", "", temp1$GPTsentiment)
    # remove empty spaces
    temp1$usersentiment <- gsub("[[:punct:]]", "", temp1$usersentiment)
    temp1$usersentiment <- gsub(" ", "", temp1$usersentiment)
    
    # all possible relations (use permutations)
    targets <- gtools::permutations(4,2,sentiment_label,repeats.allowed = T)
    targets <- paste(targets[,1],"-->",targets[,2])
    
    # transition p(bot_sentiment(t)|user_sentiment(t-1))
    freq <- table(temp1$GPTsentiment[2:nrow(temp1)],temp1$usersentiment[1:(nrow(temp1)-1)])
    chToUs <- reshape2::melt(freq/rowSums(freq))
    chToUs$direction <- "p(bot|user)"
    chToUs$target<-paste(chToUs$Var2,"-->",chToUs$Var1)
    colnames(chToUs)[1:2] <- c("to","from")
    # Control metrics:
    # (1) If there is an odd value in Var1 OR Var2
    all_interactions <- as.character(unique(c(chToUs$to,chToUs$from)))
    for (j in 1:length(all_interactions)) {
      if (j == 1) {
        temp <- any(sentiment_label == all_interactions[j])
      } else {
        temp <- c(temp,any(sentiment_label == all_interactions[j]))
      }
    }
    # (2) any is false means that one element in all_interactions is wrong
    if (any(temp==F)) {
      message(paste(i,"id:",good_ids[i]))
      chToUs <- data.frame(to=NA,from=NA,value=0,direction="",target=NA)
    }
    # (3) if the rows in chToUs are less than 16, add the extra interactions with 0
    if (nrow(chToUs) < 16 & !any(temp==F)) {
      temp <- outersect(targets,chToUs$target)
      vars <- unlist(strsplit(temp, split = " "))
      chToUs <- rbind(data.frame(chToUs),
                      data.frame(to=vars[seq(3,length(vars),by=3)],
                                 from=vars[seq(1,length(vars),by=3)],
                                 value=0,direction="p(bot|user)",target=temp))
    }
    remove(temp)
    
    
    
    # # transition p(user_sentiment(t)|bot_sentiment(t-1))
    freq <- table(temp1$usersentiment,temp1$GPTsentiment)
    usToCh <- reshape2::melt(freq/rowSums(freq))
    usToCh$direction <- "p(user|bot)"
    usToCh$target<-paste(usToCh$Var2,"-->",usToCh$Var1)
    colnames(usToCh)[1:2] <- c("to","from")
    # Control metrics:
    # (1) If there is an odd value in Var1 OR Var2
    all_interactions <- as.character(unique(c(usToCh$to,usToCh$from)))
    for (j in 1:length(all_interactions)) {
      if (j == 1) {
        temp <- any(sentiment_label == all_interactions[j])
      } else {
        temp <- c(temp,any(sentiment_label == all_interactions[j]))
      }
    }
    # (2) any is false means that one element in all_interactions is wrong
    if (any(temp==F)) {
      # message(paste(i,"id:",good_ids[i]))
      usToCh <- data.frame(to=NA,from=NA,value=0,direction="",target=NA)
    }
    # (3) if the rows in chToUs are less than 16, add the extra interactions with 0
    if (nrow(usToCh) < 16 & !any(temp==F)) {
      temp <- outersect(targets,usToCh$target)
      vars <- unlist(strsplit(temp, split = " "))
      usToCh <- rbind(data.frame(usToCh),
                      data.frame(to=vars[seq(3,length(vars),by=3)],
                                 from=vars[seq(1,length(vars),by=3)],
                                 value=0,direction="p(bot|user)",target=temp))
    }
    remove(temp)
    
    # frequency of sentiment_label
    sent_bots <- data.frame(t(colSums(temp1$GPTsentiment==sentiment)))
    sent_user <- data.frame(t(colSums(temp1$usersentiment==sentiment)))
    # label those columns
    colnames(sent_bots) <- paste0("bots_",sentiment_label)
    colnames(sent_user) <- paste0("user_",sentiment_label)
    # user mean words
    user_mean_words <- mean(temp1$userwordnums)
    # user mean reaction time
    if (is.null(temp1$RT)) {
      user_mean_rt <- NA 
    } else {
      user_mean_rt <- mean(temp1$RT)
    }
    # chat mean words
    bots_mean_words <- mean(temp1$GPTwordnums)
    # total of interactions
    num_interactions <- nrow(temp1)
    # sentiment mirror %
    # gpt --> user
    gpt_user_mirror <- sum(temp1$GPTsentiment==temp1$usersentiment)/nrow(temp1)
    # user --> gpt
    user_gpt_mirror <- sum(temp1$usersentiment[1:(nrow(temp1)-1)]==
                             temp1$GPTsentiment[2:nrow(temp1)])/(nrow(temp1)-1)
    
    # combine all good_ids
    if (i == 1) {
      combine <- data.frame(temp2,chatId=temp1$userid[1],
                            botpersonality=temp1$botpersonality[1],
                            sent_bots, bots_mean_words,
                            sent_user, user_mean_words, user_mean_rt,
                            num_interactions, gpt_user_mirror, user_gpt_mirror)
      influence <- data.frame(Participant.Private.ID=temp2$Participant.Private.ID,
                              arm = temp2$arm,chatId=good_ids[i],
                              chatType = temp2$chatType,
                              rbind(chToUs,usToCh))
    } else {
      combine <- rbind(combine,
                       data.frame(temp2,chatId=temp1$userid[1],
                                  botpersonality=temp1$botpersonality[1],
                                  sent_bots, bots_mean_words,
                                  sent_user, user_mean_words, user_mean_rt,
                                  num_interactions, gpt_user_mirror, user_gpt_mirror))
      influence <- rbind(influence,data.frame(Participant.Private.ID=temp2$Participant.Private.ID,
                                              arm = temp2$arm, chatId=good_ids[i],
                                              chatType = temp2$chatType,
                                              rbind(chToUs,usToCh)))
    }
  }
  # remove irrelevant variables
  combine$Response <- NULL
  combine$Response <- NULL
  # from and to should be in the correct factor order
  influence$from <- factor(influence$from, levels = sentiment_label)
  influence$to <- factor(influence$to, levels = sentiment_label)
  
  if (!is.null(ratings)) {
    # create chat ids
    ratings$chatCode <- paste0(ratings$Participant.Private.ID,ratings$chatType)
    combine$chatCode <- paste0(combine$Participant.Private.ID,combine$chatType)
    # in one vector
    chatCode <- intersect(ratings$chatCode,combine$chatCode)
    # matrix to fill
    toFill <- matrix(NA,nrow=nrow(combine),ncol=6)
    # correct columns
    colnames(toFill) <- levels(ratings$quest)
    # combine
    combine <- cbind(combine,toFill); remove(toFill)
    for (k in 1:length(chatCode)) {
      temp <- ratings[ratings$chatCode == chatCode[k],c("quest","Response")]
      rownames(temp) <- temp$quest
      temp <- temp[levels(ratings$quest),]
      combine[combine$chatCode == chatCode[k],levels(ratings$quest)] <- temp$Response
    }
  } 
  
  # output
  return(list(combine=combine,influence=influence,ratings=ratings))
}



# function used to extract overall information from chat interaction
summariseChatInteraction <- function(task, chat, ratings) {
  # get intersect between chat ids and what participants wrote as their ids.
  # remember: one element is not one participant but one chat
  good_ids <- intersect(unique(task$Response), unique(chat$userid))
  outersect(unique(task$Response), unique(chat$userid))
  
  # sentiment labels are:
  sentiment_label <- c("Mixed","Negative","Neutral","Positive")
  
  # for loop for each element within good_ids
  for (i in 1:length(good_ids)) {
    # extract one chat (each Participant.Private.ID has two chats)
    temp1 <- chat[chat$userid == good_ids[i],]
    # write.csv(temp1,"daniel.csv",row.names = F)
    # temp1 <- temp1[order(temp1$GPTtimestamp),]
    temp2 <- task[task$Response == good_ids[i],]
    
    # # # sentiment analysis # # #
    sentiment <- t(matrix(rep(sentiment_label,nrow(temp1)),nrow=4))
    # remove punctuation
    temp1$GPTsentiment <- gsub("[[:punct:]]", "", temp1$GPTsentiment)
    temp1$GPTsentiment <- gsub(" ", "", temp1$GPTsentiment)
    # remove empty spaces
    temp1$usersentiment <- gsub("[[:punct:]]", "", temp1$usersentiment)
    temp1$usersentiment <- gsub(" ", "", temp1$usersentiment)
    
    # all possible relations (use permutations)
    targets <- gtools::permutations(4,2,sentiment_label,repeats.allowed = T)
    targets <- paste(targets[,1],"-->",targets[,2])
    
    # transition p(bot_sentiment(t)|user_sentiment(t-1))
    freq <- table(temp1$GPTsentiment[2:nrow(temp1)],temp1$usersentiment[1:(nrow(temp1)-1)])
    chToUs <- reshape2::melt(freq/rowSums(freq))
    chToUs$direction <- "p(bot|user)"
    chToUs$target<-paste(chToUs$Var2,"-->",chToUs$Var1)
    colnames(chToUs)[1:2] <- c("to","from")
    # Control metrics:
    # (1) If there is an odd value in Var1 OR Var2
    all_interactions <- as.character(unique(c(chToUs$to,chToUs$from)))
    for (j in 1:length(all_interactions)) {
      if (j == 1) {
        temp <- any(sentiment_label == all_interactions[j])
      } else {
        temp <- c(temp,any(sentiment_label == all_interactions[j]))
      }
    }
    # (2) any is false means that one element in all_interactions is wrong
    if (any(temp==F)) {
      message(paste(i,"id:",good_ids[i]))
      chToUs <- data.frame(to=NA,from=NA,value=0,direction="",target=NA)
    }
    # (3) if the rows in chToUs are less than 16, add the extra interactions with 0
    if (nrow(chToUs) < 16 & !any(temp==F)) {
      temp <- outersect(targets,chToUs$target)
      vars <- unlist(strsplit(temp, split = " "))
      chToUs <- rbind(data.frame(chToUs),
                      data.frame(to=vars[seq(3,length(vars),by=3)],
                                 from=vars[seq(1,length(vars),by=3)],
                                 value=0,direction="p(bot|user)",target=temp))
    }
    remove(temp)

    
    
    # # transition p(user_sentiment(t)|bot_sentiment(t-1))
    freq <- table(temp1$usersentiment,temp1$GPTsentiment)
    usToCh <- reshape2::melt(freq/rowSums(freq))
    usToCh$direction <- "p(user|bot)"
    usToCh$target<-paste(usToCh$Var2,"-->",usToCh$Var1)
    colnames(usToCh)[1:2] <- c("to","from")
    # Control metrics:
    # (1) If there is an odd value in Var1 OR Var2
    all_interactions <- as.character(unique(c(usToCh$to,usToCh$from)))
    for (j in 1:length(all_interactions)) {
      if (j == 1) {
        temp <- any(sentiment_label == all_interactions[j])
      } else {
        temp <- c(temp,any(sentiment_label == all_interactions[j]))
      }
    }
    # (2) any is false means that one element in all_interactions is wrong
    if (any(temp==F)) {
      # message(paste(i,"id:",good_ids[i]))
      usToCh <- data.frame(to=NA,from=NA,value=0,direction="",target=NA)
    }
    # (3) if the rows in chToUs are less than 16, add the extra interactions with 0
    if (nrow(usToCh) < 16 & !any(temp==F)) {
      temp <- outersect(targets,usToCh$target)
      vars <- unlist(strsplit(temp, split = " "))
      usToCh <- rbind(data.frame(usToCh),
                      data.frame(to=vars[seq(3,length(vars),by=3)],
                                 from=vars[seq(1,length(vars),by=3)],
                                 value=0,direction="p(bot|user)",target=temp))
    }
    remove(temp)
    
    # frequency of sentiment_label
    sent_bots <- data.frame(t(colSums(temp1$GPTsentiment==sentiment)))
    sent_user <- data.frame(t(colSums(temp1$usersentiment==sentiment)))
    # label those columns
    colnames(sent_bots) <- paste0("bots_",sentiment_label)
    colnames(sent_user) <- paste0("user_",sentiment_label)
    # user mean words
    user_mean_words <- mean(temp1$userwordnums)
    # user mean reaction time
    if (is.null(temp1$RT)) {
      user_mean_rt <- NA 
    } else {
      user_mean_rt <- mean(temp1$RT)
    }
    # chat mean words
    bots_mean_words <- mean(temp1$GPTwordnums)
    # total of interactions
    num_interactions <- nrow(temp1)
    # sentiment mirror %
    # gpt --> user
    gpt_user_mirror <- sum(temp1$GPTsentiment==temp1$usersentiment)/nrow(temp1)
    # user --> gpt
    user_gpt_mirror <- sum(temp1$usersentiment[1:(nrow(temp1)-1)]==
                             temp1$GPTsentiment[2:nrow(temp1)])/nrow(temp1)
    
    # combine all good_ids
    if (i == 1) {
      combine <- data.frame(temp2,chatId=temp1$userid[1],
                            botpersonality=temp1$botpersonality[1],
                            sent_bots, bots_mean_words,
                            sent_user, user_mean_words, user_mean_rt,
                            num_interactions, gpt_user_mirror, user_gpt_mirror)
      influence <- data.frame(Participant.Private.ID=temp2$Participant.Private.ID,
                              arm = temp2$arm,chatId=good_ids[i],
                              chatType = temp2$chatType,
                              rbind(chToUs,usToCh))
    } else {
      combine <- rbind(combine,
                       data.frame(temp2,chatId=temp1$userid[1],
                                  botpersonality=temp1$botpersonality[1],
                                  sent_bots, bots_mean_words,
                                  sent_user, user_mean_words, user_mean_rt,
                                  num_interactions, gpt_user_mirror, user_gpt_mirror))
      influence <- rbind(influence,data.frame(Participant.Private.ID=temp2$Participant.Private.ID,
                                              arm = temp2$arm, chatId=good_ids[i],
                                              chatType = temp2$chatType,
                                              rbind(chToUs,usToCh)))
    }
  }
  # remove irrelevant variables
  combine$Response <- NULL
  combine$Response <- NULL
  # from and to should be in the correct factor order
  influence$from <- factor(influence$from, levels = sentiment_label)
  influence$to <- factor(influence$to, levels = sentiment_label)
  
  if (!is.null(ratings)) {
    # create chat ids
    ratings$chatCode <- paste0(ratings$Participant.Private.ID,ratings$chatType)
    combine$chatCode <- paste0(combine$Participant.Private.ID,combine$chatType)
    # in one vector
    chatCode <- intersect(ratings$chatCode,combine$chatCode)
    # matrix to fill
    toFill <- matrix(NA,nrow=nrow(combine),ncol=6)
    # correct columns
    colnames(toFill) <- levels(ratings$quest)
    # combine
    combine <- cbind(combine,toFill); remove(toFill)
    for (k in 1:length(chatCode)) {
      temp <- ratings[ratings$chatCode == chatCode[k],c("quest","Response")]
      rownames(temp) <- temp$quest
      temp <- temp[levels(ratings$quest),]
      combine[combine$chatCode == chatCode[k],levels(ratings$quest)] <- temp$Response
    }
  } 
  
  # output
  return(list(combine=combine,influence=influence,ratings=ratings))
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
    temp1 <- temp1[order(temp1$chatType),] 
    temp2 <- temp2[order(temp2$chatType),]
    if (sum(temp1$chatType == temp2$chatType) != 2) {
      temp2 <- temp2[temp2$chatType == temp1$chatType,]
    }
   
    
    # combine ratings with interactions
    if (i == 1) {
      output <- data.frame(temp1, likert=temp2$Response,
                           temp2[,grepl("scl90",colnames(temp2)) |
                                   grepl("bfi10",colnames(temp2)) |
                                   grepl("bfi44",colnames(temp2))])
    } else {
      output <- rbind(output,data.frame(temp1, likert=temp2$Response,
                                        temp2[,grepl("scl90",colnames(temp2)) |
                                                grepl("bfi10",colnames(temp2)) |
                                                grepl("bfi44",colnames(temp2))]))
    }
  }
  
  # output
  return(output)
}


# differences in correlations
spearman_diff_test <- function(r1, n1, r2, n2) {
  # Fisher r-to-z transformation
  z1 <- 0.5 * log((1 + r1) / (1 - r1))
  z2 <- 0.5 * log((1 + r2) / (1 - r2))
  
  # Standard errors
  SE1 <- 1 / sqrt(n1 - 3)
  SE2 <- 1 / sqrt(n2 - 3)
  
  # Difference and its standard error
  z_diff <- z1 - z2
  SE_diff <- sqrt(SE1^2 + SE2^2)
  
  # z-test statistic
  z <- z_diff / SE_diff
  
  # Two-tailed p-value
  p_value <- 2 * (1 - pnorm(abs(z)))
  
  # 95% confidence intervals
  ci95 <- paste0("dif: ",round(z_diff,4), ", from ", 
                 round(z_diff + SE_diff*1.96,4), " to ", 
                 round(z_diff - SE_diff*1.96,4))
  
  return(list(z_score = z, p_value = p_value, ci95 = ci95))
}

stats_one_panel <- function(ratings, dep_var, ind_var, chats) {
  # regression models
  ratings$questionnaire <- ratings[,ind_var]
  # interaction questionnaire with chat-type
  m.int <- report_table(lmer(Response ~ questionnaire * chat + (1|Participant.Private.ID), ratings[ratings$question == dep_var,]))
  # effect chat 1
  m.chat1 <- report_table(lm(Response ~ questionnaire, ratings[ratings$question == dep_var & ratings$chat == chats[1],]))
  # effect chat 2
  m.chat2 <- report_table(lm(Response ~ questionnaire, ratings[ratings$question == dep_var & ratings$chat == chats[2],]))
  
  # chat 1 type vectors (e.g., Anxious)
  rat.chat1 <- ratings$Response[ratings$question == dep_var & ratings$chat == chats[1]]
  quest.chat1 <- ratings$questionnaire[ratings$question == dep_var & ratings$chat == chats[1]]
  # chat 2 type vectors (e.g., Non-Anxious)
  rat.chat2 <- ratings$Response[ratings$question == dep_var & ratings$chat == chats[2]]
  quest.chat2 <- ratings$questionnaire[ratings$question == dep_var & ratings$chat == chats[2]]
  # correlations for both chats
  rho.chat1 <- cor.test(rat.chat1, quest.chat1, method = "spearman")
  rho.chat2 <- cor.test(rat.chat2, quest.chat2, method = "spearman")
  # difference in correlations
  rho_diff <- spearman_diff_test(r1=rho.chat1$estimate,n1=length(rat.chat1),
                                 r2=rho.chat2$estimate,n2=length(rat.chat2))
  
  # output
  return(list(regressions=list(m.int=m.int,m.chat1=m.chat1,m.chat2=m.chat2),
              correlations=list(rho.chat1=rho.chat1,rho.chat2=rho.chat2,rho_diff=rho_diff)))
}
