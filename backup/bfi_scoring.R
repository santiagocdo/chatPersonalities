# read raw items
db <- read.csv("data/2025_gpt_bfi_44_data.csv")

# remove irrelevant column
db$Question <- NULL

# add prober column names
colnames(db)[1:44] <- paste0('bfi',1:44) 

# reorder columns
db <- db[,c("participant_ID", paste0('bfi',1:44) )]

# read scoring guide
scoring <- read.csv("data/BigFive44_scoring.csv")

# which subscales
subscales <- unique(scoring$subscale)

# which items to invert
inverted <- paste0('bfi',scoring$item[scoring$reversed==1])

# normalize so 0 is indifference, easier to reverse items by -1 multiplication
db[,-1] <- db[,-1] - 3

# reverse items
db[,inverted] <- -1 * db[,inverted]

# create empty matrix to fill with subscales
subscales_scores <- matrix(NA,nrow=nrow(db),ncol=length(subscales))

# name the columns (i.e., subscales)
colnames(subscales_scores) <- subscales

# score averages per subscale
for (i in 1:length(subscales)) {
  # columns for ith subscale
  columns <- paste0('bfi',scoring$item[scoring$subscale==subscales[i]]) 
  # average 
  subscales_scores[,i] <- rowMeans(db[,columns])
}

# transform back to the original range {1, 5} and add participant_ID
subscales_scores <- data.frame(participant_ID = db$participant_ID,subscales_scores + 3)

# now these scores should correspond to Dan's calculations

# read dan scoring
bfi <- read.csv("data/2025_gpt_bfi_scores.csv")

# if participant_id are the same then continue
sum(subscales_scores$participant_ID == bfi$participant_ID) == nrow(subscales_scores)

plot(subscales_scores$extraversion, bfi$extraversion_score)
plot(subscales_scores$agreeableness, bfi$agreeableness_score)
plot(subscales_scores$conscientiousness, bfi$conscientiousness_score)
plot(subscales_scores$neuroticism, bfi$neuroticism_score)
plot(subscales_scores$openness, bfi$openness_score)

