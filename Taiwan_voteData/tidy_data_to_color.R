require(dplyr)

ctkparty <- vote_tidy_data(dir)

FilterMaxVote <- function(x) filter(x, rank(-GetVote)==1)
FilterParty <- function(x, party) filter(x, PartyName==party)

# get LV1 data
LV1 <- ctkparty %>% filter(LV2==0) %>% group_by(County) %>% FilterMaxVote

# get LV2 data
LV2 <- ctkparty %>% filter(LV3==0) %>% group_by(Town) %>% FilterMaxVote

# get LV3 data
LV3 <- ctkparty %>% filter(LV3!=0) %>% group_by(Village) %>% FilterMaxVote
