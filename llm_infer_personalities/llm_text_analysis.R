# # # # # # # # # # # Prolific# # # # # # # # # # # # # # # # # # # # # # # # ####
# inter <- read.csv("experiment3/2025_gptstudyusers_bfi.csv")
# participant_ID <- inter$participant_ID[!is.na(inter$participant_end_time)]

# # # # # # # # # # Sentiment Analysis # # # # # # # # # #
# read
interactions <- read.csv("experiment3/gpt_data_all_bfi.csv")

# participant id
participant_ID <- unique(interactions$userid)#intersect(participant_ID, interactions$userid)

for (i in 1:length(participant_ID)) {
  tmp <- interactions[interactions$userid == participant_ID[i],]
  if (i == 1) {
    
  } else {
    
  }
}