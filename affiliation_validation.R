# The Connection During Conversations Scale (CDCS)
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0286408#sec044

# read data from experiments
ratings1 <- read.csv("experiment1/cleaned/ratings_final.csv")
ratings2 <- read.csv("experiment2/cleaned/ratings_final.csv")
ratings3 <- read.csv("experiment3/2025_gpt_pcq_bfi.csv")



# experiment 1
id1 <- unique(ratings1$Participant.Private.ID)
for (i in 1:length(id1)) {
  tmp <- ratings1[ratings1$Participant.Private.ID == id1[i],]
  tmp$likert <- tolower(paste0(tmp$question,"_",tmp$chat))
  tmp <- tmp[order(tmp$likert),]
  tmp2 <- data.frame(t(tmp$Response))
  colnames(tmp2) <- tmp$likert
  tmp2 <- data.frame(participant_ID=tmp$Participant.Private.ID[1],tmp2)
  if (i == 1) {
    wf1 <- tmp2
  } else {
    wf1 <- rbind(wf1, tmp2)
  }
}; rm(tmp, tmp2)

# experiment 2
id2 <- unique(ratings2$Participant.Private.ID)
for (i in 1:length(id2)) {
  tmp <- ratings2[ratings2$Participant.Private.ID == id2[i],]
  tmp$likert <- tolower(paste0(tmp$question,"_",tmp$chat))
  tmp <- tmp[order(tmp$likert),]
  tmp2 <- data.frame(t(tmp$Response))
  colnames(tmp2) <- tmp$likert
  tmp2 <- data.frame(participant_ID=tmp$Participant.Private.ID[1],tmp2)
  if (i == 1) {
    wf2 <- tmp2
  } else {
    wf2 <- rbind(wf2, tmp2)
  }
}; rm(tmp, tmp2)

# experiment 3
colnames(ratings3)[2:3] <- c("chat.again_mirror","chat.again_inverse")
wf3 <- ratings3


wf1$chat.again <- (wf1$chat.again_anxious + wf1$chat.again_nonanxious)/2
wf1$different <- (wf1$different_anxious + wf1$different_nonanxious)/2
wf1$distant <- (wf1$distant_anxious + wf1$distant_nonanxious)/2
wf1$enjoy <- (wf1$enjoy_anxious + wf1$enjoy_nonanxious)/2
wf1$similar <- (wf1$similar_anxious + wf1$similar_nonanxious)/2
wf1$understood <- (wf1$understood_anxious + wf1$understood_nonanxious)/2

wf2$chat.again <- (wf2$chat.again_extrovert + wf2$chat.again_introvert)/2
wf2$different <- (wf2$different_extrovert + wf2$different_introvert)/2
wf2$distant <- (wf2$distant_extrovert + wf2$distant_introvert)/2
wf2$enjoy <- (wf2$enjoy_extrovert + wf2$enjoy_introvert)/2
wf2$similar <- (wf2$similar_extrovert + wf2$similar_introvert)/2
wf2$understood <- (wf2$understood_extrovert + wf2$understood_introvert)/2

wf3$chat.again <- (wf3$chat.again_mirror + wf3$chat.again_inverse)/2
wf3$different <- (wf3$different_mirror + wf3$different_inverse)/2
wf3$distant <- (wf3$distant_mirror + wf3$distant_inverse)/2
wf3$enjoy <- (wf3$enjoy_mirror + wf3$enjoy_inverse)/2
wf3$similar <- (wf3$similar_mirror + wf3$similar_inverse)/2
wf3$understood <- (wf3$understood_mirror + wf3$understood_inverse)/2

vars <- c("chat.again","different","distant","enjoy","similar","understood")

library(corrplot)
par(mfrow = c(1, 3)) 
corrplot(cor(wf1[,vars]), order = "hclust", method = "ellipse", tl.col = "black") 
  mtext("Exp. 1", at=-1, line=1, cex=1.2)
corrplot(cor(wf2[,vars]), order = "hclust", method = "ellipse", tl.col = "black") 
  mtext("Exp. 2", at=-1, line=1, cex=1.2)
corrplot(cor(wf3[,vars]), order = "hclust", method = "ellipse", tl.col = "black") 
  mtext("Exp. 3", at=-1, line=1, cex=1.2)
par(mfrow = c(1, 1))


tmp <- rbind(data.frame(melt(round(cor(wf1[,vars]),2)),exp="Expt. 1"),
             data.frame(melt(round(cor(wf2[,vars]),2)),exp="Expt. 2"),
             data.frame(melt(round(cor(wf3[,vars]),2)),exp="Expt. 3"))

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




# # # # # Cronbach Alpha # # # # #
if (!require(psych)) {install.packages("psych")}; library(psych)
alpha1 <- alpha(wf1[,vars], check.keys=TRUE) # alpha
alpha2 <- alpha(wf2[,vars], check.keys=TRUE)
alpha3 <- alpha(wf3[,vars], check.keys=TRUE)
alpha1$total; alpha2$total; alpha3$total


# # # # # Factor Analysis # # # # # 
# KMO should ideally be > 0.6
KMO(wf1[,vars])
KMO(wf2[,vars])
KMO(wf3[,vars])

# Bartlett's test checks if variables are correlated at all (p-value should be < 0.05)
cortest.bartlett(wf1[,vars])
cortest.bartlett(wf2[,vars])
cortest.bartlett(wf3[,vars])

# This will output a plot and suggest a number of factors in the console
fa.parallel(wf1[,vars], fm = "minres", fa = "fa")
fa.parallel(wf2[,vars], fm = "minres", fa = "fa")
fa.parallel(wf3[,vars], fm = "minres", fa = "fa")

# Example: Running 2 factors based on the parallel analysis
fa_model <- fa(wf1[,vars], nfactors = 1, rotate = "oblimin", fm = "minres")
print(fa_model, cut = 0.3, digits = 2) # Print the detailed results
fa.diagram(fa_model)

fa_model <- fa(wf2[,vars], nfactors = 1, rotate = "oblimin", fm = "minres")
print(fa_model, cut = 0.3, digits = 2) # Print the detailed results
fa.diagram(fa_model)

fa_model <- fa(wf3[,vars], nfactors = 1, rotate = "oblimin", fm = "minres")
print(fa_model, cut = 0.3, digits = 2) # Print the detailed results
fa.diagram(fa_model)



