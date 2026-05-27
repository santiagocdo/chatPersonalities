# # # Affiliation # # #
aff <- read.csv("pilot4/2025_gpt_cdcs_ctrl.csv")
rel_cols <- c("Q11","Q12","Q13","Q14")
for (i in 1:length(rel_cols)) {
  aff[,rel_cols[i]] <- -1*(aff[,rel_cols[i]] - 4) + 4
}
aff$cdcs <- rowMeans(aff[,-1])
aff <- aff[order(aff$participant_ID),]



# # # Ratings # # #
rat <- read.csv("pilot4/2025_gpt_pcq_ctrl.csv")
rel_cols <- c("distant","different")
for (i in 1:length(rel_cols)) {
  rat[,rel_cols[i]] <- -1*(rat[,rel_cols[i]] - 3) + 3
}
rat$aff_score <- rowMeans(rat[,-1] - 3)
rat <- rat[order(rat$participant_ID),]



# # # Big-Five # # #
bfi <- read.csv("pilot4/2025_gpt_bfi_scores_ctrl.csv")
# bfi <- read.csv("pilot4/2026_gpt_bfi_scores_ctrl.csv")
rel_cols <- colnames(bfi)[-1]
bfi[,rel_cols] <- (bfi[,rel_cols] - 3)^2
# bfi[,rel_cols] <- abs(bfi[,rel_cols] - 3)
bfi$pers_distance <- rowSums(bfi[,rel_cols])

bfi$remove <- T
for (i in 1:nrow(rat)) {
  bfi$remove[bfi$participant_ID == rat$participant_ID[i]] <- F
}
bfi <- bfi[bfi$remove==F,]; bfi$remove <- NULL
bfi <- bfi[order(bfi$participant_ID),]



# # # Combine # # #
sum(rat$participant_ID==aff$participant_ID)
wf <- data.frame(rat, cdcs=aff$cdcs)
sum(wf$participant_ID==bfi$participant_ID)
wf <- data.frame(wf, bfi[,-1])

report::report_table(cor.test(wf$aff_score,wf$cdcs))

library(ggplot2)
library(ggpubr)
ggplot(wf, aes(x=cdcs,y=aff_score)) + geom_point() + stat_cor() + geom_smooth(method="lm")

ggplot(wf, aes(x=pers_distance,y=aff_score)) + geom_point() + stat_cor() + geom_smooth(method="lm")
ggplot(wf, aes(x=pers_distance,y=cdcs)) + geom_point() + stat_cor() + geom_smooth(method="lm")





# # # # # prepare interactions for LLM personality estimation (same as main_e3.R)

# ratings to conditions
tmp <- read.csv("pilot4/2026_gptstudyusers_bfi_ctrl.csv")
tmp <- tmp[tmp$complete_study == "Yes",]
participant_ID <- unique(tmp$participant_ID)
# add chat order

# read
interactions <- read.csv("pilot4/2026_gpt_data_all_bfi_ctrl.csv")
# participant id
participant_ID <- intersect(participant_ID, interactions$userid)
source("functions.R")
tmp <- summariseChatInteraction_e4(interactions, participant_ID)

# save interactions already order by timeline
# write.csv(tmp$inters, "llm_infer_personalities/exp4_n104_interactions.csv", row.names = F)
# write.csv(tmp$combine, "llm_infer_personalities/exp4_n104_inter_summary.csv", row.names = F)
