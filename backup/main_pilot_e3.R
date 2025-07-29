bfi <- read.csv("data/2025_gpt_bfi_scores.csv")

# opposite scores
bfi$extraversion_score <- bfi$extraversion_score - 3
bfi$agreeableness_score <- bfi$agreeableness_score - 3
bfi$conscientiousness_score <- bfi$conscientiousness_score - 3
bfi$openness_score <- bfi$openness_score - 3
bfi$neuroticism_score <- bfi$neuroticism_score - 3

bfi$extraversion_score_inv <- -1 * bfi$extraversion_score
bfi$agreeableness_score_inv <- -1 * bfi$agreeableness_score
bfi$conscientiousness_score_inv <- -1 * bfi$conscientiousness_score
bfi$openness_score_inv <- -1 * bfi$openness_score
bfi$neuroticism_score_inv <- -1 * bfi$neuroticism_score

scores <- c("extraversion_score","agreeableness_score","conscientiousness_score",
            "openness_score","neuroticism_score")
inv_scores <- c("extraversion_score_inv","agreeableness_score_inv","conscientiousness_score_inv",
                "openness_score_inv","neuroticism_score_inv")

bfi$pers_distance <- NA
for (i in 1:nrow(bfi)) {
  bfi$pers_distance[i] <- -1 * (unlist(bfi[i,scores]) %*% unlist(bfi[i,inv_scores]))
}
hist(bfi$pers_distance)


gpt <- read.csv("data/2025_gpt_pcq_bfi.csv")

library(reshape2)
ratings <- c("chat_again_mirror","chat_again_inverse","different_mirror","different_inverse",
             "enjoy_mirror","enjoy_inverse","similar_mirror","similar_inverse","understood_mirror",
             "understood_inverse","distant_mirror","distant_inverse")

gpt <- melt(gpt, measure.vars = ratings)

gpt$value <- gpt$value - 3

gpt$condition <- ifelse(grepl("mirror",gpt$variable),"mirror","inverse")
gpt$rating <- ifelse(grepl("chat_again",gpt$variable),"chat_again",
                     ifelse(grepl("different",gpt$variable),"different",
                            ifelse(grepl("enjoy",gpt$variable),"enjoy",
                                   ifelse(grepl("similar",gpt$variable),"similar",
                                          ifelse(grepl("understood",gpt$variable),"understood","distant")))))

table(gpt$condition,gpt$rating)

library(ggplot2)
ggplot(gpt, aes(x=condition,y=value)) + stat_summary() +
  facet_wrap(rating~.)

gpt$value <- ifelse(grepl("different",gpt$rating) | grepl("distant",gpt$rating),-1*gpt$value,gpt$value)

library(dplyr)
gptaff <- gpt %>% group_by(participant_ID,condition) %>%
  summarise(value=mean(value))

ggplot(gptaff, aes(x=condition,y=value)) + stat_summary() +
  labs(y="affiliation score")

gptaff$pers_distance <- NA
for (i in 1:nrow(bfi)) {
  gptaff$pers_distance[gptaff$participant_ID == bfi$participant_ID[i]] <- bfi$pers_distance[i]
} 

ggplot(gptaff, aes(x=pers_distance,y=value,col=condition)) + 
  geom_smooth(method = "lm")
summary(lm(value~pers_distance*condition,gptaff))

rep(2,5) %*% -rep(2,5)
c(2,-2,2,-2,2) %*% c(-2,2,-2,2,-2)
rep(1,5) %*% -rep(1,5)
rep(0,5) %*% -rep(0,5)

c(5,5,5,5,5) %*% c(1,1,1,1,1)
c(4,4,4,4,4) %*% c(2,2,2,2,2)
c(3,3,3,3,3) %*% c(3,3,3,3,3)

# # # # Sample Size Calculation # # # #
vec_mirror <- gptaff$value[gptaff$condition=="mirror"]
vec_inverse <- gptaff$value[gptaff$condition!="mirror"]
var.test(vec_mirror, vec_inverse, alternative = "two.sided")
# If p>.05. We can conclude that there is no differences between variances.
t.test(vec_mirror, vec_inverse, var.equal = T)
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
