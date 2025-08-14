# remove everything in the environment
rm(list = ls())



# # # # # # # # # # Questionnaire # # # # # # # # # # # # # # # # # # # # # ####
# read
bfi <- read.csv("experiment3/2025_gpt_bfi_scores.csv")

# from likert 1 to 5, to likert -2 to 2 (see inverse scores)
bfi$extraversion_score <- bfi$extraversion_score - 3
bfi$agreeableness_score <- bfi$agreeableness_score - 3
bfi$conscientiousness_score <- bfi$conscientiousness_score - 3
bfi$openness_score <- bfi$openness_score - 3
bfi$neuroticism_score <- bfi$neuroticism_score - 3
# inverse scores by multiplying by -1 (note: 0 does not have inverse)
bfi$extraversion_score_inv <- -1 * bfi$extraversion_score
bfi$agreeableness_score_inv <- -1 * bfi$agreeableness_score
bfi$conscientiousness_score_inv <- -1 * bfi$conscientiousness_score
bfi$openness_score_inv <- -1 * bfi$openness_score
bfi$neuroticism_score_inv <- -1 * bfi$neuroticism_score

# vectors with column names
scores <- c("extraversion_score","agreeableness_score","conscientiousness_score",
            "openness_score","neuroticism_score")
inv_scores <- c("extraversion_score_inv","agreeableness_score_inv","conscientiousness_score_inv",
                "openness_score_inv","neuroticism_score_inv")

# examples of distances
c(2,2,2,2,2) %*% c(-2,-2,-2,-2,-2)
c(0,0,0,0,0) %*% c(0,0,0,0,0)
c(2,-2,2,-2,2) %*% c(-2,2,-2,2,-2)
c(1,1,1,1,1) %*% c(-1,-1,-1,-1,-1)
c(-2,-2,-2,-2,-2) %*% c(2,2,2,2,2)

# calculate personality (condition) distances 
bfi$pers_distance <- NA
for (i in 1:nrow(bfi)) {
  bfi$pers_distance[i] <- -1 * (unlist(bfi[i,scores]) %*% unlist(bfi[i,inv_scores]))
}
hist(bfi$pers_distance)



# # # # # # # # # # Prolific# # # # # # # # # # # # # # # # # # # # # # # # ####
inter <- read.csv("experiment3/2025_gptstudyusers_bfi.csv")
participant_ID <- inter$participant_ID[!is.na(inter$participant_end_time)]
prolific_ID <- inter$prolific_ID[!is.na(inter$participant_end_time)]
inter <- inter[!is.na(inter$participant_end_time),]
table(inter$participant_gender); prop.table(table(inter$participant_gender))
mean(inter$participant_age); sd(inter$participant_age); range(inter$participant_age)



# # # # # # # # # # Ratings # # # # # # # # # # # # # # # # # # # # # # # # ####
# ratings to conditions
ratings3 <- read.csv("experiment3/2025_gpt_pcq_bfi.csv")
# good participants, or participants to be included
participant_ID <- unique(ratings3$participant_ID)

# relevant columns (personalities and anti-personalities)
rating_cols <- c("chat_again_mirror","chat_again_inverse","different_mirror",
                 "different_inverse","enjoy_mirror","enjoy_inverse","similar_mirror",
                 "similar_inverse","understood_mirror","understood_inverse",
                 "distant_mirror","distant_inverse")

if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
ratings3 <- melt(ratings3, measure.vars = rating_cols)
# standardized dependent variable (so 3 is 0)
ratings3$Response <- ratings3$value - 3
ratings3$value <- NULL

# add condition and rating type (6 likerts)
ratings3$chat <- ifelse(grepl("mirror",ratings3$variable),"mirror","inverse")
ratings3$rating <- ifelse(grepl("chat_again",ratings3$variable),"chat-again",
                     ifelse(grepl("different",ratings3$variable),"different",
                            ifelse(grepl("enjoy",ratings3$variable),"enjoy",
                                   ifelse(grepl("similar",ratings3$variable),"similar",
                                          ifelse(grepl("understood",ratings3$variable),"understood","distant")))))
# how many ratings per condition?
table(ratings3$chat,ratings3$rating)

# visualize each rating
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
ggplot(ratings3, aes(x=chat,y=Response)) + 
  labs(x="Condition",y="Normalized Likert",title="N=100") + 
  stat_summary() +
  facet_wrap(rating~.)
# invert different and distant
ratings3$likerts <- ifelse(grepl("different",ratings3$rating) |
                             grepl("distant",ratings3$rating),
                           -1*ratings3$Response,ratings3$Response)



# add personality, demographics, and distances to the wide format data.frame
ratings3$pers_distance <- ratings3$age <- ratings3$sex <- 
  ratings3$extraversion_score <- ratings3$agreeableness_score <- 
  ratings3$conscientiousness_score  <- ratings3$openness_score <- 
  ratings3$neuroticism_score <- NA
for (i in 1:length(participant_ID)) {
  # condition distance
  ratings3$pers_distance[ratings3$participant_ID == participant_ID[i]] <- 
    bfi$pers_distance[bfi$participant_ID==participant_ID[i]]
  # demographics
  ratings3$age[ratings3$participant_ID == participant_ID[i]] <- 
    inter$participant_age[inter$participant_ID==participant_ID[i]]
  ratings3$sex[ratings3$participant_ID == participant_ID[i]] <- 
    inter$participant_gender[inter$participant_ID==participant_ID[i]]
  # big five
  ratings3$extraversion_score[ratings3$participant_ID == participant_ID[i]] <- 
    bfi$extraversion_score[bfi$participant_ID==participant_ID[i]]
  ratings3$agreeableness_score[ratings3$participant_ID == participant_ID[i]] <- 
    bfi$agreeableness_score[bfi$participant_ID==participant_ID[i]]
  ratings3$conscientiousness_score[ratings3$participant_ID == participant_ID[i]] <- 
    bfi$conscientiousness_score[bfi$participant_ID==participant_ID[i]]
  ratings3$openness_score[ratings3$participant_ID == participant_ID[i]] <- 
    bfi$openness_score[bfi$participant_ID==participant_ID[i]]
  ratings3$neuroticism_score[ratings3$participant_ID == participant_ID[i]] <- 
    bfi$neuroticism_score[bfi$participant_ID==participant_ID[i]]
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # Statistical Analysis: Ratings - Questionnaires# # # # # # # # # # #### 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# individual items analysis
if (!require(report)) {install.packages("report")}; library(report)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
m.chat <- report_table(lmer(Response ~ chat + (1|participant_ID), ratings3[ratings3$rating=="chat-again",]))
m.diff <- report_table(lmer(Response ~ chat + (1|participant_ID), ratings3[ratings3$rating=="different",]))
m.dist <- report_table(lmer(Response ~ chat + (1|participant_ID), ratings3[ratings3$rating=="distant",]))
m.enjo <- report_table(lmer(Response ~ chat + (1|participant_ID), ratings3[ratings3$rating=="enjoy",]))
m.simi <- report_table(lmer(Response ~ chat + (1|participant_ID), ratings3[ratings3$rating=="similar",]))
m.unde <- report_table(lmer(Response ~ chat + (1|participant_ID), ratings3[ratings3$rating=="understood",]))

# combine
exp3 <- rbind(data.frame(quest="chat-again",effect="Mirror-Inverse",m.chat[2,c(11:13,8)]),
              data.frame(quest="different",effect="Mirror-Inverse",m.diff[2,c(11:13,8)]),
              data.frame(quest="distant",effect="Mirror-Inverse",m.dist[2,c(11:13,8)]),
              data.frame(quest="enjoy",effect="Mirror-Inverse",m.enjo[2,c(11:13,8)]),
              data.frame(quest="similar",effect="Mirror-Inverse",m.simi[2,c(11:13,8)]),
              data.frame(quest="understood",effect="Mirror-Inverse",m.unde[2,c(11:13,8)]))
# write.csv(exp3, "figures/stats_exp3.csv", row.names = F)



# score overall affiliation score
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
ratings3_wf <- as.data.frame(
  ratings3 %>% group_by(participant_ID, chat, neuroticism_score, openness_score,
                        conscientiousness_score, agreeableness_score, extraversion_score, 
                        sex, age, pers_distance) %>% summarise(likerts=mean(likerts))
)
m.aff <- report_table(lmer(likerts ~ chat + (1|participant_ID), ratings3_wf))



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # Figure 4# # # # # # # # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# # # # Figure 4A # # # #
# preregistered analysis
report_table(t.test(likerts~chat,ratings3_wf,paired=T))
summary(lm(likerts ~ chat, ratings3_wf))
report_table(lmer(likerts ~ chat + (1|participant_ID), ratings3_wf))
if (!require(ggsignif)) {install.packages("ggsignif")}; library(ggsignif)
ratings3_wf$condition2 <- factor(ifelse(ratings3_wf$chat=="mirror","Mirror","Inverse"),
                            levels = c("Mirror","Inverse"))
(fig4A <- ggplot(ratings3_wf, aes(x=condition2,y=likerts, col=condition2, shape=condition2)) + 
    labs(title="Participants' Judgements",x="Condition", y="Affiliation Score") + 
    geom_boxplot() + stat_summary() + 
    scale_shape_manual(values = c(17, 19)) +
    scale_colour_manual(values = c("black", "grey50")) + 
    scale_y_continuous(breaks = c(-2,-1,0,1,2), limits = c(-2,2.5),
                       labels = c("-2","","0","","+2")) +
    geom_signif(comparisons = list(c("Mirror", "Inverse")),
                map_signif_level = TRUE, col="black",textsize = 5) +
    theme_classic() + theme(legend.position = "none"))



# # # # Figure 4B # # # #
summary(lm(likerts ~ chat*pers_distance, ratings3_wf))
report_table(lmer(likerts ~ chat*pers_distance + sex+age+agreeableness_score + (1|participant_ID), ratings3_wf))
report_table(lmer(likerts ~ chat*pers_distance + (1|participant_ID), ratings3_wf))
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
(fig4B <- ggplot(ratings3_wf, aes(x=pers_distance, y=likerts, col=condition2)) + 
    labs(title="",x="Condition Distance",y="Affiliation Score") + 
    # geom_point(alpha=.2) + 
    scale_y_continuous(breaks = c(-2,-1,0,1,2), labels = c("-2","","0","","+2")) +
    scale_colour_manual(values = c("black", "grey50")) + 
    stat_cor(size=3, p.accuracy = .001, r.accuracy = .001) + theme_classic() +
    geom_smooth(method = "lm", se=T, fill="grey80") + 
    theme(legend.position = "none"))
summary(lm(likerts~pers_distance*chat, ratings3_wf))



# # # # Figure 4C # # # #
# difference between  mirror and inverse (used in regression)
wf_m <- ratings3_wf[ratings3_wf$chat=="mirror",]
wf_i <- ratings3_wf[ratings3_wf$chat=="inverse",]
wf_m$likert_inv <- wf_i$likerts
# affiliation for mirror minus affiliation for inverse
wf_m$dif_mir_inv <- wf_m$likerts - wf_m$likert_inv
wide_format <- wf_m; rm(wf_m, wf_i)

# make it long format
tmp <- melt(wide_format, measure.vars = scores)
tmp$variable <- factor(tmp$variable, levels = scores)
summary(aov(value~variable+Error(participant_ID/variable),tmp))
(fig4C <- ggplot(tmp, aes(x=value,y=variable)) + 
    labs(subtitle = "Distributions", x = "Score", y = "Personalities") + 
    scale_y_discrete(labels=c("Extraversion","Agreeableness","Conscientiousness",
                              "Openness","Neuroticism")) +
    geom_violin() + theme_classic() +#geom_boxplot(alpha=.1) + 
    stat_summary())

# wide_format$dif_mir_inv <- wide_format$dif_mir_inv*-1
# run univariate models
tm1 <- as.data.frame(report_table(lm(dif_mir_inv ~ neuroticism_score, wide_format)))
tm1 <- tm1[!is.na(tm1$Coefficient),]; tm1 <- tm1[-1,]
tm2 <- as.data.frame(report_table(lm(dif_mir_inv ~ openness_score, wide_format)))
tm2 <- tm2[!is.na(tm2$Coefficient),]; tm2 <- tm2[-1,]
tm3 <- as.data.frame(report_table(lm(dif_mir_inv ~ conscientiousness_score, wide_format)))
tm3 <- tm3[!is.na(tm3$Coefficient),]; tm3 <- tm3[-1,]
tm4 <- as.data.frame(report_table(lm(dif_mir_inv ~ agreeableness_score, wide_format)))
tm4 <- tm4[!is.na(tm4$Coefficient),]; tm4 <- tm4[-1,]
tm5 <- as.data.frame(report_table(lm(dif_mir_inv ~ extraversion_score, wide_format)))
tm5 <- tm5[!is.na(tm5$Coefficient),]; tm5 <- tm5[-1,]
# combine simple models
tm0 <- rbind(tm1,tm2,tm3,tm4,tm5)
tm0$Parameter <- rev(scores)

# run multivariate model
m <- lm(dif_mir_inv ~ neuroticism_score+openness_score+conscientiousness_score+
          agreeableness_score+extraversion_score, wide_format)
tm <- as.data.frame(report_table(m))
tm <- tm[!is.na(tm$Coefficient),]; tm <- tm[-1,]
tm$Parameter <- rev(scores)#factor(tm$Parameter, levels = )

summary(step(m))
m0 <- lm(dif_mir_inv ~ neuroticism_score+agreeableness_score+extraversion_score, wide_format)
tm0 <- as.data.frame(report_table(m0))
tm0 <- tm0[!is.na(tm0$Coefficient),]; tm0 <- tm0[-1,]
tm0$Parameter <- c("neuroticism_score","agreeableness_score","extraversion_score")
# mA <- lm(dif_mir_inv ~ agreeableness_score, wide_format)
# mAE <- lm(dif_mir_inv ~ agreeableness_score+extraversion_score, wide_format)
# mNAE <- lm(dif_mir_inv ~ neuroticism_score+agreeableness_score+extraversion_score, wide_format)
# anova(mA,mAE,mNAE); BIC(mA,mAE,mNAE)


tm <- rbind(data.frame(type="full",tm),
            data.frame(type="reduced",tm0))
tm$significance <- ifelse(tm$p < .05/5, "p<.01", "ns")
tm$Parameter <- factor(tm$Parameter, levels = scores)
# second part of figure 4C
# tm$condition <- ifelse(tm$Std_Coefficient > 0, "mirror","inverse")
(fig4D <- ggplot(tm, aes(x=Std_Coefficient,y=Parameter,shape=type)) + #col=condition
    labs(subtitle = "    Mirror - Inverse", x = "Std. Coef.", shape="Model type:") +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin = Std_Coefficient_CI_low,
                      xmax=Std_Coefficient_CI_high), width=.4, 
                  position = position_dodge(.8)) + #col=c("black","black","black","#00BFC4","black")
    scale_x_continuous(breaks = c(-.3,0,.3,.6)) +
    scale_shape_manual(values = c(21,19)) +
    geom_point(size=3, fill="white", position = position_dodge(.8)) + 
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(.17,.15),
          # legend.title = element_blank(),
          legend.title = element_text(size = 7),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key.size = unit(.2, "cm"), # Adjust size of legend keys
          legend.text = element_text(size = 7)))# Adjust size of legend tex



# # # # # # # # # # Sentiment Analysis # # # # # # # # # #
# read
interactions <- read.csv("experiment3/gpt_data_all_bfi.csv")

# participant id
participant_ID <- intersect(participant_ID, interactions$userid)
source("functions.R")
tmp <- summariseChatInteraction_e3(interactions, participant_ID)
combine <- tmp$combine
influence <- tmp$influence

# normalize sentiments by proportions
# experiment 1
combine$user_total <- rowSums(combine[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")])
combine[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")] <- 
  combine[,c("user_Mixed","user_Negative","user_Neutral","user_Positive")]/combine$user_total
combine$bots_total <- rowSums(combine[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")])
combine[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")] <- 
  combine[,c("bots_Mixed","bots_Negative","bots_Neutral","bots_Positive")]/combine$bots_total

# obtain chat id
ratings3_wf$chat_id <- paste0(ratings3_wf$participant_ID, ratings3_wf$chat)
combine$chat_id <- paste0(combine$participant_ID, combine$botcondition)
# order rows based on chat_id, so data.frames can be added together
ratings3_wf <- ratings3_wf[order(ratings3_wf$chat_id),]
combine <- combine[order(combine$chat_id),]
sum(ratings3_wf$chat_id == combine$chat_id)==nrow(ratings3_wf)

# combine interactions summary (combine) and gptaff(iliation)
combine <- data.frame(combine, aff_score=ratings3_wf$likerts)

# long format
combine.lf <- melt(combine, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                             "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))

# change column name
colnames(combine.lf)[ncol(combine.lf)] <- c("prop") 
# transform variable to character
combine.lf$variable<- as.character(combine.lf$variable)
# split string to get the first 4 character (user or bots)
combine.lf$who <- factor(substr(combine.lf$variable,1,4),levels = c("bots","user"))
# add a nicer names
levels(combine.lf$who) <- c("GPT-4.1 Texts","Participants Texts")
# add the sentiment on column variable (created with the columns in melt)
combine.lf$sentiment <- substr(combine.lf$variable,6,nchar(combine.lf$variable))



# # # # Figure 4D # # # #
# aim 1, GPT4 Texts are different between conditions (chatbots) 
report_table(anova(lmer(prop ~ sentiment * botcondition + (1|participant_ID), 
           combine.lf[combine.lf$who=="GPT-4.1 Texts",])))
aov(prop ~ sentiment * botcondition + Error(participant_ID/(sentiment*botcondition)),
    combine.lf[combine.lf$who=="GPT-4.1 Texts",])
report_table(t.test(bots_Positive~botcondition, combine[,], paired = TRUE))
report_table(t.test(bots_Neutral~botcondition, combine[,], paired = TRUE))
report_table(t.test(bots_Negative~botcondition, combine[,], paired = TRUE))
report_table(t.test(bots_Mixed~botcondition, combine[,], paired = TRUE))

# aim 2, GPT4 Texts are different between conditions (chatbots)
report_table(anova(lmer(prop ~ sentiment * botcondition + (1|participant_ID),
                        combine.lf[combine.lf$who=="Participants Texts",])))
aov(prop ~ sentiment * botcondition + Error(participant_ID/(sentiment*botcondition)),
    combine.lf[combine.lf$who=="Participants Texts",])
report_table(t.test(user_Positive~botcondition, combine[,], paired = TRUE))
report_table(t.test(user_Neutral~botcondition, combine[,], paired = TRUE))
report_table(t.test(user_Negative~botcondition, combine[,], paired = TRUE))
report_table(t.test(user_Mixed~botcondition, combine[,], paired = TRUE))



ann_text <- data.frame(sentiment = c(1,2,4,1,2,4), prop = c(.9,.7,.9,.6,.7,.8),
                       who = factor(c(rep("GPT-4.1 Texts",3),rep("Participants Texts",3)),
                                    levels = c("GPT-4.1 Texts","Participants Texts")),
                       lab = "Text", condition2 = c("Mirror","Inverse"))
# visualize the average of count for each sentiment and for each chat personality
combine.lf$condition2 <- factor(ifelse(combine.lf$botcondition=="mirror","Mirror","Inverse"),
                                levels = c("Mirror","Inverse"))
(fig4E <- ggplot(combine.lf, aes(x=sentiment,y=prop,col=condition2,shape=condition2)) + 
    labs(title = "Sentiment Analysis",
         y="Prop. (Sentiment ea Condition)", x = "Text Sentiment Category",
         col = "LLM type:", shape = "LLM type:") +
    geom_hline(yintercept = 0) +
    coord_cartesian(ylim = c(0,1)) +
    geom_boxplot(alpha=.3,position = position_dodge(0.5)) +
    stat_summary(fun.data="mean_cl_normal",position = position_dodge(0.5)) +
    geom_text(data = ann_text,col="black", size = 7,
              label = c(rep("***",3),"**","***","**")) +
    scale_colour_manual(values = c("black", "grey50")) +
    scale_shape_manual(values = c(17,19)) +
    scale_y_continuous(breaks = c(0,.5,1)) +
    facet_wrap(. ~ who, nrow = 2) + 
    theme_classic() +
    # guides(color = guide_legend(nrow = 2)) +
    guides(shape = guide_legend(nrow = 2),
           color = guide_legend(nrow = 2)) +
    theme(legend.position = "bottom", legend.box = "vertical", #c(.3,.89),
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.background = element_rect(colour='black',fill=alpha("white", 0.5),linetype='solid'))
)



# # # # # Figure 4 # # # # #
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
fig4AB <- ggarrange(fig4A, fig4B,ncol=2, labels = c("A","B"))
fig4CD <- ggarrange(fig4C, fig4D,ncol=2, labels=c("C","D"), align = "h", widths = c(5, 3))

(fig4 <- annotate_figure(ggarrange(
  ggarrange(fig4AB,fig4CD,nrow=2),
  fig4E, ncol=2, widths = c(2,1), labels=c("","E")),
  top = text_grob("Experiment 3", color = "black", face = "bold", size = 14)))

if (print_fig == 1) {
  ggsave("figures/fig4_v2.pdf", fig4, dpi = 2400, scale = .85, units = "cm",
         width = 24, height = 16, bg = "white")
}






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # Supplementary Figure# # # # # # # # # # # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# relevant columns (personalities and anti-personalities)
wide_format <- wide_format[order(wide_format$dif_mir_inv),]
wide_format$participant_ID <- factor(wide_format$participant_ID,
                                     levels = wide_format$participant_ID)
(figS3 <-ggplot(wide_format, aes(x=dif_mir_inv, y=as.factor(participant_ID))) + 
    labs(x="Mirror - Inverse\n(0 > more affiliation for mirror)",
         y="Participant ID (n=100)") +
    geom_bar(stat = "identity") + 
    theme(axis.text.y = element_blank()))

# sentiments with affiliation only inverse
comb_inv <- combine[combine$botcondition == "inverse",]
# sentiments with affiliation only mirror
comb_mir <- combine[combine$botcondition == "mirror",]

# if participant Ids are the same then differences in conditions
if (sum(comb_inv$participant_ID == comb_mir$participant_ID)==length(participant_ID)) {
  comb_mir$botcondition <- NULL
  comb_mir$aff_score <- comb_mir$aff_score - comb_inv$aff_score
  comb_mir$bots_Mixed <- comb_mir$bots_Mixed - comb_inv$bots_Mixed
  comb_mir$bots_Mixed <- comb_mir$bots_Negative - comb_inv$bots_Negative
  comb_mir$bots_Mixed <- comb_mir$bots_Neutral - comb_inv$bots_Neutral
  comb_mir$bots_Mixed <- comb_mir$bots_Positive - comb_inv$bots_Positive
  comb_mir$user_Mixed <- comb_mir$user_Mixed - comb_inv$user_Mixed
  comb_mir$user_Mixed <- comb_mir$user_Negative - comb_inv$user_Negative
  comb_mir$user_Mixed <- comb_mir$user_Neutral - comb_inv$user_Neutral
  comb_mir$user_Mixed <- comb_mir$user_Positive - comb_inv$user_Positive
  comb_dif <- comb_mir; rm(comb_inv, comb_mir)
}
# long format
comb_dif <- melt(comb_dif, measure.vars = c("user_Mixed","user_Negative","user_Neutral","user_Positive",
                                            "bots_Mixed","bots_Negative","bots_Neutral","bots_Positive"))
# change column name
colnames(comb_dif)[ncol(comb_dif)] <- c("prop_dif") 
# transform variable to character
comb_dif$variable<- as.character(comb_dif$variable)
# split string to get the first 4 character (user or bots)
comb_dif$who <- factor(substr(comb_dif$variable, 1,4), levels = c("bots","user"))
# add a nicer names
levels(comb_dif$who) <- c("GPT-4 Texts","Participants Texts")
# add the sentiment on column variable (created with the columns in melt)
comb_dif$sentiment <- substr(comb_dif$variable,6,nchar(comb_dif$variable))

ggplot(comb_dif, aes(x=prop_dif,y=aff_score)) +
  labs(x="Sentiment Prop. (Self-Antiself)",y="Affilation (Self-Mirror)") +
  geom_point() + geom_smooth(method="lm") + stat_cor() +
  facet_grid(who~sentiment, scales = "free")

# add sentiment to wide_format with difference scores:
wide_format$sent
for (i in 1:length(participant_ID)) {
  participant_ID[i]
}



# difference between GPT-4 types, to simplify statistical analysis with triple order interaction
# experiment 1
combine.wf <- combine[combine$botcondition == "mirror",]
temp <- combine[combine$botcondition == "inverse",]
# combine.wf$participant_ID == temp$participant_ID
combine.wf$mixed <- combine.wf$user_Mixed - temp$user_Mixed
combine.wf$negative <- combine.wf$user_Negative - temp$user_Negative
combine.wf$neutral <- combine.wf$user_Neutral - temp$user_Neutral
combine.wf$positive <- combine.wf$user_Positive - temp$user_Positive

# test difference scores
combine.lf <- melt(combine.wf, measure.vars = c("mixed","negative","neutral","positive"))
# change column names
colnames(combine.lf)[(ncol(combine.lf)-1):ncol(combine.lf)] <- c("sentiment","diff")

# # stats experiment 1
# anova(lmer(diff ~ sentiment * scl90_anxiety + (1|participant_ID), combine.lf))
# report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine.lf$sentiment=="mixed",]))
# report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine.lf$sentiment=="negative",]))
# report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine.lf$sentiment=="neutral",]))
# report_table(lm(diff ~ scl90_anxiety, combine1.lf[combine.lf$sentiment=="positive",]))
# 
# levels(combine1.lf$sentiment) <- c("Mixed","Negative","Neutral","Positive")
# if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
# (figure2C <- ggplot(combine1.lf, aes(x=scl90_anxiety,y=diff,fill=diff)) + 
#     labs(title = "Anxiety and Sentiment",
#          y=expression(Prop.[Anxious]-Prop.[Non*`-`*Anxious]), x = "Anxiety (SCL-90R)",
#          col = "Sentiment:") +
#     geom_hline(yintercept = 0) +
#     coord_cartesian(ylim = c(-.5,.8)) +
#     geom_point(alpha=.4, shape=21, col="black") +
#     geom_smooth(method = "lm", col="black", se=F) +
#     scale_fill_gradient2(low="#D55E00",mid="white",high="#0072B2",midpoint=0,
#                          limits=range(combine1.lf$diff), 
#                          breaks=seq(min(combine1.lf$diff),max(combine1.lf$diff), by=.1)) +
#     scale_y_continuous(breaks = c(-.5,0,.8)) +
#     scale_x_continuous(breaks = c(0, .5, 1), limits = c(0, 1)) +
#     stat_cor(col="black",label.y=.75,method="pearson",r.accuracy=.01,p.accuracy=.001) +
#     theme_classic() +
#     facet_wrap(sentiment~., ncol=2) +
#     theme(legend.position = "none")
# )
# cor.test(combine1.lf$diff[combine1.lf$sentiment=="Negative"],
#          combine1.lf$scl90_anxiety[combine1.lf$sentiment=="Negative"],method="spearman")











# # # # Sample Size Calculation # # # #
vec_mirror <- ratings3_wf$likerts[ratings3_wf$chat=="mirror"]
vec_inverse <- ratings3_wf$likerts[ratings3_wf$chat!="mirror"]
var.test(vec_mirror, vec_inverse, alternative = "two.sided")
# If p>.05. We can conclude that there is no differences between variances.
t.test(vec_mirror, vec_inverse, var.equal = F)
cor.test(vec_mirror, vec_inverse)

library(pwr)
sd_pooled <- sqrt(((sd(vec_mirror)^2) + (sd(vec_inverse)^2)) / 2)
# Cohen's d
effect_size <- (mean(vec_mirror) - mean(vec_inverse)) / sd_pooled
pwr.t.test(d = .3, power = .8, sig.level = .05, type = "paired")

library(pwrss)
pwrss.t.2means(mu1 = mean(vec_mirror), mu2 = mean(vec_inverse), 
               sd1 = sd(vec_mirror), sd2 = sd(vec_inverse), 
               paired = TRUE, paired.r = cor(vec_mirror,vec_inverse),
               power = .8, alpha = .05,
               alternative = "not equal")

m <- lm(value~condition, gptaff)
summary(m)
library(effectsize)
standardize_parameters(m)

r_squared <- .04374
f2 <- r_squared / (1 - r_squared)

pwr.f2.test(u = 1, f2 = f2, sig.level = .05, power = .8)
171.5365+1+1
