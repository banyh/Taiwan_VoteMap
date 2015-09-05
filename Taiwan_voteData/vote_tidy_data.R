require(csvread)
require(dplyr)
require(XML)
require(stringr)

#dir <- c("Taiwan_voteData/2010年都市長議員及里長/市長", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/縣市長")
#dir <- c("Taiwan_voteData/2010年都市長議員及里長/區域議員", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/區域議員")
#dir <- c("Taiwan_voteData/2012年總統及立委/不分區政黨")
#dir <- c("Taiwan_voteData/2012年總統及立委/區域立委")
#dir <- c("Taiwan_voteData/2012年總統及立委/總統")
#dir <- c("Taiwan_voteData/2014年地方公職人員選舉/直轄市市長", "Taiwan_voteData/2014年地方公職人員選舉/縣市市長")
dir <- c("Taiwan_voteData/2014年地方公職人員選舉/直轄市區域議員", "Taiwan_voteData/2014年地方公職人員選舉/縣市區域議員")

cat(paste0(dir, "\n"))

#------------------ Determine Attributes --------------------------

# How to use:
#    if (attr(dir, "市長")) do something
#    if (!attr(dir, "總統")) do something
attributes(dir) <- list(
		市長 = grepl("市長$", dir[1]),
		議員 = grepl("議員$", dir[1]),
		里長 = grepl("里長$", dir[1]),
		立委 = grepl("立委$", dir[1]),
		總統 = grepl("總統$", dir[1]),
		政黨 = grepl("政黨$", dir[1])
	)
year <- sub(".*(\\d{4})年.*", "\\1", dir[1])

#------------------ Load Region ID ----------------------------------

r <- xmlRoot(xmlTreeParse("Taiwan_Region/region_name.xml", useInternalNodes = TRUE))
region_id <- str_replace_all(xpathSApply(r, "//Code", xmlValue), "\\t", "")
names(region_id) <- str_replace_all(xpathSApply(r, "//Content", xmlValue), "\\t", "")
region_name <- names(region_id)
names(region_name) <- region_id
rm(r)

#------------------ Load Region Data -------------------------------

i.base <- bind_rows(lapply(paste0(dir, "/elbase.csv"), function(fn) csvread(fn, coltypes = rep("string",6), header=F)))
for (col in 1:5) {
	i.base[[col]] <- as.numeric(gsub("'|\"", "",i.base[[col]]))
}
for (col in 6) {
	i.base[[col]] <- gsub("\"", "",iconv(i.base[[col]], "utf8", "utf8"))
}
colnames(i.base) <- c("LV1a", "LV1b", "LV1c", "LV2", "LV3", "Village")

#------------------ Load Candidate Data ------------------------------

i.cand <- bind_rows(lapply(paste0(dir, "/elcand.csv"), function(fn) csvread(fn, coltypes = rep("string",15), header=F)))
for (col in c(1:6,8:11)) {
	i.cand[[col]] <- as.numeric(gsub("'|\"", "",i.cand[[col]]))
}
for (col in c(7,12:15)) {
	i.cand[[col]] <- gsub("\"", "",iconv(i.cand[[col]], "utf8", "utf8"))
}
i.cand[[10]] <- as.Date(as.character(i.cand[[10]] + 19110000), "%Y%m%d")
colnames(i.cand) <- c("LV1a", "LV1b", "LV1c", "LV2", "LV3",
	"Number", "Name", "Party", "Sex", "Birthday",
	"Old", "School", "Current", "Winner", "Vice")

#------------------ Load Party Data ----------------------------------

i.party <-bind_rows(lapply( paste0(dir, "/elpaty.csv"), function(fn) csvread(fn, coltypes = rep("string",2), header=F)))
for (col in 1) {
	i.party[[col]] <- as.numeric(gsub("'|\"", "",i.party[[col]]))
}
for (col in 2) {
	i.party[[col]] <- gsub("\"", "",iconv(i.party[[col]], "utf8", "utf8"))
}
colnames(i.party) <- c("Party", "PartyName")
i.party <- unique(i.party)

#------------------ Load Election Profile Data -----------------------

i.prof <- bind_rows(lapply(paste0(dir, "/elprof.csv"), function(fn) csvread(fn, coltypes = rep("string",20), header=F)))
for (col in 1:20) {
	i.prof[[col]] <- as.numeric(gsub("'|\"", "",i.prof[[col]]))
}
colnames(i.prof) <- c("LV1a", "LV1b", "LV1c", "LV2", "LV3",
	"LV4", "ValidCnt", "InvalidCnt", "VoteCnt", "ElectorCnt",
	"PeopleCnt", "CandCnt", "WinnerCnt", "CandMale", "CandFemale",
	"WinnerMale", "WinnerFemale", "ElectorProp", "VoteProp", "WinnerProp")
i.prof <- i.prof %>% filter(LV4==0 & LV3==0) %>% select(-LV3, -LV4, -(ElectorCnt:PeopleCnt), -(CandCnt:WinnerProp))

#------------------ Load Vote Tickets Data ---------------------------

i.ctks <- bind_rows(lapply(paste0(dir, "/elctks.csv"), function(fn) csvread(fn, coltypes = rep("string",10), header=F)))
for (col in 1:9) {
	i.ctks[[col]] <- as.numeric(gsub("'|\"", "",i.ctks[[col]]))
}
for (col in 10) {
	i.ctks[[col]] <- gsub("\"", "",iconv(i.ctks[[col]], "utf8", "utf8"))
}
colnames(i.ctks) <- c("LV1a", "LV1b", "LV1c", "LV2", "LV3",
	"LV4", "Number", "GetVote", "GetVoteProp", "Winner")
i.ctks <- i.ctks %>% filter(LV4==0 & LV3==0) %>% select(-LV3, -LV4, -GetVoteProp, -Winner)

#------------------ Remove Unused Column -----------------------------

if (attr(dir, "市長") || attr(dir, "里長") || attr(dir, "總統") || attr(dir, "政黨")) {
	i.base <- select(i.base, -LV1c)
	i.cand <- select(i.cand, -LV1c)
	i.prof <- select(i.prof, -LV1c)
	i.ctks <- select(i.ctks, -LV1c)
}

#------------------ Mapping to Region ID ----------------------------

base_lv1 <- filter(i.base, LV2==0 & LV1a!=0) %>% select(LV1a, LV1b, LV2, County=Village) %>% mutate(Town=County)
base_lv2 <- filter(i.base, LV3==0 & LV2!=0) %>% select(LV1a, LV1b, LV2, Town=Village)
base_lv2 <- inner_join(base_lv2, base_lv1, by=c("LV1a","LV1b")) %>% select(-LV2.y, -Town.y) %>% rename(LV2=LV2.x, Town=Town.x)

i2.base <- bind_rows(base_lv1, base_lv2)
i2.base <- i2.base %>% mutate(ID1 = region_id[County], ID2 = region_id[Town])
err.id2 <- bind_rows(filter(i2.base, ID1 != substr(ID2,1,2) & str_length(ID1) == 2),filter(i2.base, ID1 != substr(ID2,1,5) & str_length(ID1) == 5))
for (i in seq_len(nrow(err.id2)))
{
	i2.base$ID2[i2.base$County == err.id2$County[i] & i2.base$Town == err.id2$Town[i]] <-
		region_id[grepl(err.id2$ID1[i], region_id) & err.id2$Town[i] == region_name]
}

#------------------ Combine All Data Together ------------------------

canparty <- select(inner_join(i.cand, i.party), LV1a:Name, PartyName)
if (attr(dir, "市長")) canparty <- select(canparty, -(LV2:LV3))
if (attr(dir, "總統")) canparty <- select(canparty, -(LV1a:LV3))
if (attr(dir, "政黨")) canparty <- select(canparty, -(LV1a:LV3))
if (attr(dir, "立委")) canparty <- select(canparty, -(LV2:LV3))
if (attr(dir, "議員")) canparty <- select(canparty, -(LV2:LV3))

ctkparty <- inner_join(i.ctks, canparty)

profname <- full_join(i.prof, i2.base)

ctkparty <- full_join(ctkparty, profname) %>%
	select(Number, Name:VoteCnt, GetVote, County:ID2) %>%
	mutate(VoteProp = GetVote / VoteCnt)


write.csv(ctkparty, "hello.csv", fileEncoding = "utf8", row.names = FALSE)

