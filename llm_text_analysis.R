rm(list=ls(all=TRUE))

library(reshape2)
library(ggplot2)
library(ggpubr)
library(viridisLite)



# # # # # # # # # # # Personality # # # # # # # # # # # # # # # # # # # # # ####
bfi_e1 <- read.csv("llm_infer_personalities/exp1_n89_bfi.csv")
participant_ID1 <- bfi_e1$Participant.Private.ID
bfi_e2 <- read.csv("llm_infer_personalities/exp2_n97_bfi.csv")
participant_ID2 <- bfi_e2$Participant.Private.ID
bfi_e3 <- read.csv("experiment3/2025_gptstudyusers_bfi.csv")
participant_ID3 <- bfi_e3$participant_ID[!is.na(bfi_e3$participant_end_time)]

# clean exp 3 bfi
bfi_e3 <- read.csv("experiment3/2025_gpt_bfi_scores.csv"); bfi_e3$remove <- T
bfi_e3 <- bfi_e3[order(bfi_e3$participant_ID),]
for (i in 1:length(participant_ID3)) {
  if (any(bfi_e3$participant_ID == participant_ID3[i])) {
    bfi_e3$remove[bfi_e3$participant_ID == participant_ID3[i]] <- F
  }
}; bfi_e3 <- bfi_e3[bfi_e3$remove==F,]; bfi_e3$remove <- NULL



bfi_e3 <- bfi_e3[,c("participant_ID","extraversion_score","agreeableness_score",
                    "conscientiousness_score","neuroticism_score","openness_score")]
colnames(bfi_e1)[1:6] <- colnames(bfi_e2)[1:6] <- colnames(bfi_e3)[1:6] <- 
  c("participant_ID","Extroversion","Agreeableness","Conscientiousness","Neuroticism","Openness")

# same scale all scores
bfi_e1[,-1] <- bfi_e1[,-1] / 7
bfi_e2[,-1] <- bfi_e2[,-1] / 5
bfi_e3[,-1] <- bfi_e3[,-1] / 5

# same variance coverance matrix?
melt_bfi1 <- melt(cor(bfi_e1[,-1]))
melt_bfi1$value <- round(melt_bfi1$value, 2)
melt_bfi1$exp <- "Expt. 1"

melt_bfi2 <- melt(cor(bfi_e2[,-1]))
melt_bfi2$value <- round(melt_bfi2$value, 2)
melt_bfi2$exp <- "Expt. 2"

melt_bfi3 <- melt(cor(bfi_e3[,-1]))
melt_bfi3$value <- round(melt_bfi3$value, 2)
melt_bfi3$exp <- "Expt. 3"

melt_bfi <- melt(cor(rbind(bfi_e1[,-1],bfi_e2[,-1],bfi_e3[,-1])))
melt_bfi$value <- round(melt_bfi$value, 2)
melt_bfi$exp <- "All"

tmp <- rbind(melt_bfi1, melt_bfi2, melt_bfi3, melt_bfi)
(p <- ggplot(tmp, aes(x = Var1, y = Var2, fill = value)) +
  labs(x = NULL, y = NULL) +
  geom_tile(color = "white") + # Adds a white border to tiles
  geom_text(aes(label = value), color = "black", size = 3) + # Adds correlation values as text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", # Custom color scale
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "r") +
  theme_minimal() +
  coord_fixed() + # Ensures square tiles
  facet_wrap(exp ~ .) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "right"))


p1 <- ggplot(melt_bfi1, aes(x = Var1, y = Var2, fill = value)) +
  labs(title="Experiment 1",x = NULL, y = NULL) +
  geom_tile(color = "white") + # Adds a white border to tiles
  geom_text(aes(label = value), color = "black", size = 3) + # Adds correlation values as text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", # Custom color scale
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "r") +
  theme_minimal() +
  coord_fixed() + # Ensures square tiles
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "right")

p2 <- ggplot(melt_bfi2, aes(x = Var1, y = Var2, fill = value)) +
  labs(title="Experiment 2",x = NULL, y = NULL) +
  geom_tile(color = "white") + # Adds a white border to tiles
  geom_text(aes(label = value), color = "black", size = 3) + # Adds correlation values as text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", # Custom color scale
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "r") +
  theme_minimal() +
  coord_fixed() + # Ensures square tiles
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "right")

p3 <- ggplot(melt_bfi3, aes(x = Var1, y = Var2, fill = value)) +
  labs(title="Experiment 3",x = NULL, y = NULL) +
  geom_tile(color = "white") + # Adds a white border to tiles
  geom_text(aes(label = value), color = "black", size = 3) + # Adds correlation values as text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", # Custom color scale
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "r") +
  theme_minimal() +
  coord_fixed() + # Ensures square tiles
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "right")

p4 <- ggplot(melt_bfi, aes(x = Var1, y = Var2, fill = value)) +
  labs(title="All Experiments",x = NULL, y = NULL) +
  geom_tile(color = "white") + # Adds a white border to tiles
  geom_text(aes(label = value), color = "black", size = 3) + # Adds correlation values as text
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", # Custom color scale
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "r") +
  theme_minimal() +
  coord_fixed() + # Ensures square tiles
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.position = "right")

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = T)



# combine, all have same columns
bfi <- rbind(bfi_e1, bfi_e2, bfi_e3)
bfi <- bfi[order(bfi$participant_ID),]



# # # # # # # # # # Sentiment Analysis # # # # # # # # # #
# read
interactions1 <- read.csv("llm_infer_personalities/exp1_n89_interactions.csv")
interactions1$userid <- interactions1$PID
interactions2 <- read.csv("llm_infer_personalities/exp2_n97_interactions.csv")
interactions2$userid <- interactions2$PID
# interactions3 <- read.csv("experiment3/gpt_data_all_bfi.csv")
interactions3 <- read.csv("llm_infer_personalities/exp3_n100_interactions.csv")

builtDataLLM <- function (interactions, participant_ID) {
  for (i in 1:length(participant_ID)) {
    tmp <- interactions[interactions$userid == participant_ID[i],]
    tmp$text <- paste0("GPT: ",tmp$GPTmessage,"; User: ", tmp$usermessage)
    text <- paste(tmp$text, collapse = "; ")
    # text <- paste(tmp$usermessage, collapse = "; ")
    if (i == 1) {
      for_gemini <- data.frame(participant_ID = participant_ID[i], text)
    } else {
      for_gemini <- rbind(for_gemini,data.frame(participant_ID = participant_ID[i], text))
    }
  }
  return(for_gemini)
}

for_gemini1 <- builtDataLLM(interactions = interactions1, participant_ID = participant_ID1)
for_gemini1 <- for_gemini1[order(for_gemini1$participant_ID),]

for_gemini2 <- builtDataLLM(interactions2, participant_ID2)
for_gemini2 <- for_gemini2[order(for_gemini2$participant_ID),]

for_gemini3 <- builtDataLLM(interactions3, participant_ID3)
for_gemini3 <- for_gemini3[order(for_gemini3$participant_ID),]

# combine
for_gemini <- rbind(for_gemini1,for_gemini2,for_gemini3)
for_gemini <- for_gemini[order(for_gemini$participant_ID),]

# write.table(for_gemini, "llm_infer_personalities/for_gemini.txt", row.names = F)
# write.table(for_gemini1, "llm_infer_personalities/for_gemini_e1.txt", row.names = F)
# write.table(for_gemini2, "llm_infer_personalities/for_gemini_e2.txt", row.names = F)
# write.table(for_gemini3, "llm_infer_personalities/for_gemini_e3.txt", row.names = F)
# write.csv(for_gemini, "llm_infer_personalities/for_gemini.csv", row.names = F)
# write.csv(for_gemini1, "llm_infer_personalities/for_gemini_e1.csv", row.names = F)
# write.csv(for_gemini2, "llm_infer_personalities/for_gemini_e2.csv", row.names = F)
# write.csv(for_gemini3, "llm_infer_personalities/for_gemini_e3.csv", row.names = F)


# functions
getCorrels <- function (bfi, bfi_llm, cols = c("Openness","Conscientiousness",
                                               "Extroversion","Agreeableness",
                                               "Neuroticism")) {
  o <- cor.test(bfi$Openness, bfi_llm[,cols[1]])
  c <- cor.test(bfi$Conscientiousness, bfi_llm[,cols[2]])
  e <- cor.test(bfi$Extroversion, bfi_llm[,cols[3]])
  a <- cor.test(bfi$Agreeableness, bfi_llm[,cols[4]])
  n <- cor.test(bfi$Neuroticism, bfi_llm[,cols[5]])
  correls <- data.frame(dimension=cols,
                        cor=c(o$estimate,c$estimate,e$estimate,a$estimate,n$estimate), 
                        ci_low=c(o$conf.int[1],c$conf.int[1],e$conf.int[1],a$conf.int[1],n$conf.int[1]), 
                        ci_high=c(o$conf.int[2],c$conf.int[2],e$conf.int[2],a$conf.int[2],n$conf.int[2]))
  return(correls)
}
getAllCorrels <- function (bfi, bfi_llm) {
  bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
  bfi_llm$Openness <- bfi_llm$Open + -1*(bfi_llm$Conventional-4)+4
  bfi_llm$Conscientiousness <- bfi_llm$Dependable + -1*(bfi_llm$Disorganized-4)+4
  bfi_llm$Extroversion <- bfi_llm$Extroverted + -1*(bfi_llm$Reserved-4)+4
  bfi_llm$Agreeableness <- bfi_llm$Sympathetic + -1*(bfi_llm$Critical-4)+4
  bfi_llm$Neuroticism <- bfi_llm$Anxious + -1*(bfi_llm$Calm-4)+4
  if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
    prompt <- getCorrels(bfi, bfi_llm)
  }
  return(prompt)
}



# # # # # Experiment 1 # # # # #
bfi <- bfi_e1
# prompt 1
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt1_exp1.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt1_exp1 <- getCorrels(bfi, bfi_llm)
}
# prompt 2
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt2_exp1.csv")
prompt2_exp1 <- getAllCorrels(bfi, bfi_llm)
# prompt 3
# bfi_llm <- read.csv("llm_infer_personalities/prompt3_exp1.csv")
# unique_id <- bfi_llm$Participant_ID[!is.na(bfi_llm$Participant_ID)]
# bfi_llm$Participant_ID <- rep(unique_id,each=5)
# colnames(bfi_llm)[3] <- c("score")
# bfi_llm <- data.frame(Participant_ID=unique_id, 
#                       Extroversion=bfi_llm$score[bfi_llm$Trait == "Extraversion"],
#                       Agreeableness=bfi_llm$score[bfi_llm$Trait == "Agreeableness"],
#                       Conscientiousness=bfi_llm$score[bfi_llm$Trait == "Conscientiousness"],
#                       Neuroticism=bfi_llm$score[bfi_llm$Trait == "Neuroticism"],
#                       Openness=bfi_llm$score[bfi_llm$Trait == "Openness"])
# bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
# if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
#   prompt3_exp1 <- getCorrels(bfi, bfi_llm)
# }
# prompt 5
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt5_exp1.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt5_exp1 <- getCorrels(bfi, bfi_llm)
}
# prompt 6
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt6_exp1.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt6_exp1 <- getCorrels(bfi, bfi_llm)
}



# # # # # Experiment 2 # # # # #
bfi <- bfi_e2
# prompt 1
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt1_exp2.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt1_exp2 <- getCorrels(bfi, bfi_llm)
}
# prompt 2
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt2_exp2.csv")
prompt2_exp2 <- getAllCorrels(bfi, bfi_llm)
# prompt 5
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt5_exp2.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt5_exp2 <- getCorrels(bfi, bfi_llm)
}
# prompt 6
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt6_exp2.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt6_exp2 <- getCorrels(bfi, bfi_llm)
}



# # # # # Experiment 3 # # # # #
bfi <- bfi_e3
# prompt 1 # bfi_llm <- read.csv("llm_infer_personalities/prompt1_exp3.csv")
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt1_exp3_v2.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt1_exp3 <- getCorrels(bfi, bfi_llm)
}
# prompt 2
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt2_exp3.csv")
prompt2_exp3 <- getAllCorrels(bfi, bfi_llm)
# prompt 3
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt3_exp3.csv")
unique_id <- bfi_llm$Participant_ID[!is.na(bfi_llm$Participant_ID)]
bfi_llm$Participant_ID <- rep(unique_id,each=5)
colnames(bfi_llm)[3] <- c("score")
bfi_llm <- data.frame(Participant_ID=unique_id, 
           Extroversion=bfi_llm$score[bfi_llm$Trait == "Extraversion"],
           Agreeableness=bfi_llm$score[bfi_llm$Trait == "Agreeableness"],
           Conscientiousness=bfi_llm$score[bfi_llm$Trait == "Conscientiousness"],
           Neuroticism=bfi_llm$score[bfi_llm$Trait == "Neuroticism"],
           Openness=bfi_llm$score[bfi_llm$Trait == "Openness"])
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt3_exp3 <- getCorrels(bfi, bfi_llm)
}
# prompt 5
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt5_exp3.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt5_exp3 <- getCorrels(bfi, bfi_llm)
}
# prompt 6
bfi_llm <- read.csv("llm_infer_personalities/llm_outputs/prompt6_exp3.csv")
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),] 
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt6_exp3 <- getCorrels(bfi, bfi_llm)
}


bfi <- rbind(bfi_e1, bfi_e2, bfi_e3)
bfi <- bfi[order(bfi$participant_ID),]
bfi_llm <- rbind(read.csv("llm_infer_personalities/llm_outputs/prompt1_exp1.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt1_exp2.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt1_exp3.csv"))
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),]
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt1 <- getCorrels(bfi, bfi_llm)
}
bfi_llm <- rbind(read.csv("llm_infer_personalities/llm_outputs/prompt2_exp1.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt2_exp2.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt2_exp3.csv"))
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),]
prompt2 <- getAllCorrels(bfi, bfi_llm)
bfi_llm <- rbind(read.csv("llm_infer_personalities/llm_outputs/prompt5_exp1.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt5_exp2.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt5_exp3.csv"))
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),]
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt5 <- getCorrels(bfi, bfi_llm)
}
bfi_llm <- rbind(read.csv("llm_infer_personalities/llm_outputs/prompt6_exp1.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt6_exp2.csv"),
                 read.csv("llm_infer_personalities/llm_outputs/prompt6_exp3.csv"))
bfi_llm <- bfi_llm[order(bfi_llm$Participant_ID),]
if (sum(bfi$participant_ID == bfi_llm$Participant_ID)==nrow(bfi)) {
  prompt6 <- getCorrels(bfi, bfi_llm)
}





correls <- rbind(data.frame(prompt="1", exp="All Expts (n=286)", prompt1),
                 data.frame(prompt="1", exp="Expt. 1 (n=89)", prompt1_exp1),
                 data.frame(prompt="1", exp="Expt. 2 (n=97)", prompt1_exp2),
                 data.frame(prompt="1", exp="Expt. 3 (n=100)", prompt1_exp3),
                 data.frame(prompt="2", exp="All Expts (n=286)", prompt2),
                 data.frame(prompt="2", exp="Expt. 1 (n=89)", prompt2_exp1),
                 data.frame(prompt="2", exp="Expt. 2 (n=97)", prompt2_exp2),
                 data.frame(prompt="2", exp="Expt. 3 (n=100)", prompt2_exp3),
                 data.frame(prompt="3", exp="Expt. 3 (n=100)", prompt3_exp3),
                 data.frame(prompt="5", exp="Expt. 1 (n=89)", prompt5_exp1),
                 data.frame(prompt="5", exp="Expt. 2 (n=97)", prompt5_exp2),
                 data.frame(prompt="5", exp="Expt. 3 (n=100)", prompt5_exp3),
                 data.frame(prompt="5", exp="All Expts (n=286)", prompt5),
                 data.frame(prompt="6", exp="Expt. 1 (n=89)", prompt6_exp1),
                 data.frame(prompt="6", exp="Expt. 2 (n=97)", prompt6_exp2),
                 data.frame(prompt="6", exp="Expt. 3 (n=100)", prompt6_exp3),
                 data.frame(prompt="6", exp="All Expts (n=286)", prompt6))

correls$sig <- ifelse(correls$ci_low < 0 & correls$ci_high > 0, "ns", "sig")

ggplot(correls, aes(x=cor,y=dimension,col=prompt,shape=sig)) +
  labs(title="AI moderately predict user's personality\n from text-based interactions",
       y="Personality Dimension", x="Correlation with 95% Confidence Intervals", col="Prompt:") +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = c(viridis(5)[1:5])) + #,"black")) +
  scale_shape_manual(values = c(21, 19)) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), position = position_dodge(.5), width = .2) +
  geom_point(fill="white",size = 3, position = position_dodge(.5)) + 
  scale_x_continuous(breaks = seq(-1,1,by=.5)) +
  facet_grid(. ~ exp) +
  theme_bw()




# respond reviewer
# Prompt to absorbent anxiety personality and then respond anxiety questionnaire.
