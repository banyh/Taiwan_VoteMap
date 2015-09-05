require(csvread)
require(dplyr)
require(XML)
require(stringr)
require(jsonlite)

#dir <- c("Taiwan_voteData/1996年9任總統")
#dir <- c("Taiwan_voteData/1998年4屆立委/區域")
#dir <- c("Taiwan_voteData/1998年直轄市長", "Taiwan_voteData/1997年縣市長")
#dir <- c("Taiwan_voteData/1998年縣市議員/區域", "Taiwan_voteData/1998年直轄市議員/區域")
#dir <- c("Taiwan_voteData/2000年10任總統")
#dir <- c("Taiwan_voteData/2001年5屆立委/區域")
#dir <- c("Taiwan_voteData/2002年直轄市長", "Taiwan_voteData/2001年縣市長")
#dir <- c("Taiwan_voteData/2002年直轄市議員(區域)", "Taiwan_voteData/2002年縣市議員/區域")
#dir <- c("Taiwan_voteData/2004年11任總統")
#dir <- c("Taiwan_voteData/2004年第6屆立法委員/區域")
#dir <- c("Taiwan_voteData/2006年直轄市長", "Taiwan_voteData/2005年縣市長")
#dir <- c("Taiwan_voteData/2006年直轄市議員(區域)", "Taiwan_voteData/2005年縣市議員/區域")
#dir <- c("Taiwan_voteData/2008年立委/區域")
#dir <- c("Taiwan_voteData/2008年立委/不分區政黨")
#dir <- c("Taiwan_voteData/2008年總統")
#dir <- c("Taiwan_voteData/2010年都市長議員及里長/市長", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/縣市長")
#dir <- c("Taiwan_voteData/2010年都市長議員及里長/區域議員", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/區域議員")
#dir <- c("Taiwan_voteData/2012年總統及立委/不分區政黨")
#dir <- c("Taiwan_voteData/2012年總統及立委/區域立委")
#dir <- c("Taiwan_voteData/2012年總統及立委/總統")
#dir <- c("Taiwan_voteData/2014年地方公職人員選舉/直轄市市長", "Taiwan_voteData/2014年地方公職人員選舉/縣市市長")
#dir <- c("Taiwan_voteData/2014年地方公職人員選舉/直轄市區域議員", "Taiwan_voteData/2014年地方公職人員選舉/縣市區域議員")
#dir <- c("Taiwan_voteData/2014年地方公職人員選舉/直轄市山原議員", "Taiwan_voteData/2014年地方公職人員選舉/縣市山原議員")

#
# Description: 將選舉資料整理出鄉鎮市等級的部分
# dir:      存放選舉資料的位置，例如 "Taiwan_voteData/2012年總統及立委/總統"
# saveType: 是否將整理後的資料輸出到檔案中
#           "csv" - 存成csv檔
#           "json" - 存成json檔
# return:   tbl_df格式，儲存整理後的選舉資料
#
# 註: 如果資料有輸出到檔案中，可用以下方式讀取
#     vote <- fromJSON("Taiwan_voteData/2008年總統/tidydata.json")
#     csv <- read.csv("Taiwan_voteData/2008年總統/tidydata.csv", fileEncoding = "utf8")
#
vote_tidy_data <- function(dir, saveType = "")
{
	#cat(paste0(dir, "\n"))

	#------------------ Determine Attributes --------------------------

	# How to use:
	#    if (attr(dir, "市長")) do something
	#    if (!attr(dir, "總統")) do something
	attributes(dir) <- list(
			市長 = grepl("市長$", dir[1]),
			議員 = grepl("議員$", dir[1]) | grepl("議員\\(區域\\)$", dir[1]) | grepl("議員/區域$", dir[1]),
			里長 = grepl("里長$", dir[1]),
			立委 = grepl("立委$", dir[1]) | grepl("立委/區域$", dir[1]) | grepl("立法委員/區域$", dir[1]),
			總統 = grepl("總統$", dir[1]),
			政黨 = grepl("政黨$", dir[1])
		)
	year <- as.numeric(sub(".*(\\d{4})年.*", "\\1", dir[1]))

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

	i.cand <- bind_rows(lapply(paste0(dir, "/elcand.csv"), function(fn) csvread(fn, coltypes = rep("string",16), header=F)))
	for (col in c(1:6,8:11)) {
		i.cand[[col]] <- as.numeric(gsub("'|\"", "",i.cand[[col]]))
	}
	for (col in c(7,12:16)) {
		i.cand[[col]] <- gsub("\"", "",iconv(i.cand[[col]], "utf8", "utf8"))
	}
	i.cand[[10]] <- as.Date(as.character(i.cand[[10]] + 19110000), "%Y%m%d")
	colnames(i.cand) <- c("LV1a", "LV1b", "LV1c", "LV2", "LV3",
		"Number", "Name", "Party", "Sex", "Birthday",
		"Old", "BirthPlace", "School", "Current", "Winner", "Vice")
	i.cand <- i.cand %>% select(-Birthday)
	if (attr(dir, "總統")) i.cand <- filter(i.cand, Vice!="Y")

	#------------------ Load Party Data ----------------------------------

	i.party <- bind_rows(lapply(paste0(dir, "/elpaty.csv"), function(fn) csvread(fn, coltypes = rep("string",2), header=F)))
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
	i.ctks <- i.ctks %>% filter(LV4==0 & LV3==0) %>% select(-LV3, -LV4, -GetVoteProp) %>%
		mutate(Winner = ifelse(Winner=="*", TRUE, FALSE))

	#------------------ Remove Unused Column -----------------------------

	if (attr(dir, "市長") || attr(dir, "里長") || attr(dir, "總統") || attr(dir, "政黨")) {
		i.base <- select(i.base, -LV1c)
		i.cand <- select(i.cand, -LV1c)
		i.prof <- select(i.prof, -LV1c)
		i.ctks <- select(i.ctks, -LV1c)
	}

	#------------------ Old Name to New Name ----------------------------

	if (year <= 2012) {
		ty <- i.base %>% filter(Village == "桃園縣")
		all_ty <- i.base$LV1a == ty$LV1a & i.base$LV1b == ty$LV1b & i.base$LV3 == 0
		i.base$Village[all_ty] <- sub("(..)縣", "\\1市", sub("(..)(鄉|鎮|市)", "\\1區", i.base$Village[all_ty]))
	}
	if (year <= 2010) {
		i.base <- i.base %>% mutate(Village = ifelse(Village=="褔興鄉", "福興鄉", Village))
	}
	if (year <= 2008) {
		ty <- i.base %>% filter(Village == "臺北縣")
		all_ty <- i.base$LV1a == ty$LV1a & i.base$LV1b == ty$LV1b & i.base$LV3 == 0
		i.base$Village[all_ty] <- sub("臺北縣", "新北市", sub("(..)(鄉|鎮|市)", "\\1區", i.base$Village[all_ty]))

		ty <- i.base %>% filter(Village == "臺南縣")
		old.tn <- i.base %>% filter(Village == "臺南市")
		all_ty <- i.base$LV1a == ty$LV1a & i.base$LV1b == ty$LV1b & i.base$LV3 == 0
		i.base$Village[all_ty] <- sub("(..)縣", "\\1市", sub("(..)(鄉|鎮|市)", "\\1區", i.base$Village[all_ty]))

		ty <- i.base %>% filter(Village == "臺中縣")
		all_ty <- i.base$LV1a == ty$LV1a & i.base$LV1b == ty$LV1b & i.base$LV3 == 0
		i.base$Village[all_ty] <- sub("(..)縣", "\\1市", sub("(..)(鄉|鎮|市)", "\\1區", i.base$Village[all_ty]))

		ty <- i.base %>% filter(Village == "高雄縣")
		all_ty <- i.base$LV1a == ty$LV1a & i.base$LV1b == ty$LV1b & i.base$LV3 == 0
		i.base$Village[all_ty] <- sub("(..)縣", "\\1市", sub("(..)(鄉|鎮|市)", "\\1區", i.base$Village[all_ty]))

		i.base <- i.base %>% mutate(Village = ifelse(Village=="台西鄉", "臺西鄉", Village))
		i.base <- i.base %>% mutate(Village = ifelse(Village=="台東市", "臺東市", Village))
	}
	if (year <= 2003) {
		i.base <- i.base %>% mutate(Village = ifelse(Village=="大碑鄉", "大埤鄉", Village))

		mid.base <- i.base %>% filter(LV1b==old.tn$LV1b & Village=="中區")
		west.base <- i.base %>% filter(LV1b==old.tn$LV1b & Village=="西區")
		i.base <- i.base %>% filter(!(LV1b==old.tn$LV1b & Village=="中區"))
		i.base$Village[i.base$LV1b==old.tn$LV1b & i.base$Village=="西區"] <- "中西區"

		mid.ctks <- suppressMessages(inner_join(mid.base, i.ctks))
		west.ctks <- suppressMessages(inner_join(west.base, i.ctks))
		i.ctks <- i.ctks %>% filter(!(LV1b==old.tn$LV1b & LV2==mid.base$LV2))
		west.ctks$GetVote <- bind_rows(mid.ctks, west.ctks) %>% group_by(Number) %>% summarise_each(funs(sum), GetVote) %>% select(GetVote) %>% .[[1]]
		i.ctks$GetVote[i.ctks$LV1b==old.tn$LV1b & i.ctks$LV2==west.base$LV2] <- west.ctks$GetVote

		mid.prof <- suppressMessages(inner_join(mid.base, i.prof))
		west.prof <- suppressMessages(inner_join(west.base, i.prof))
		i.prof <- i.prof %>% filter(!(LV1b==old.tn$LV1b & LV2==mid.base$LV2))
		west.prof[,c("ValidCnt","InvalidCnt","VoteCnt")] <- as.numeric(bind_rows(mid.prof, west.prof) %>% summarise_each(funs(sum), ValidCnt:VoteCnt) %>% select(ValidCnt:VoteCnt))
		i.prof[i.prof$LV1b==old.tn$LV1b & i.prof$LV2==west.base$LV2, c("ValidCnt","InvalidCnt","VoteCnt")] <- west.prof %>% select(ValidCnt:VoteCnt)
	}
	if (year <= 2001) {
		i.base <- i.base %>% mutate(Village = ifelse(Village=="霧台鄉", "霧臺鄉", Village))
	}

	#------------------ Mapping to Region ID ----------------------------

	if (attr(dir, "市長") || attr(dir, "里長") || attr(dir, "總統") || attr(dir, "政黨")) {
		base_lv1 <- filter(i.base, LV2==0 & LV1a!=0)
	} else {
		base_lv1 <- filter(i.base, LV2==0 & LV1c == 0 & LV1a!=0)
	}
	base_lv1 <- base_lv1 %>% select(LV1a, LV1b, LV2, County=Village) %>% mutate(Town=County)
	base_lv2 <- filter(i.base, LV3==0 & LV2!=0) %>% select(LV1a, LV1b, LV2, Town=Village)
	base_lv2 <- suppressMessages(inner_join(base_lv2, base_lv1, by=c("LV1a","LV1b"))) %>% select(-LV2.y, -Town.y) %>% rename(LV2=LV2.x, Town=Town.x)

	i2.base <- bind_rows(base_lv1, base_lv2)
	i2.base <- i2.base %>% mutate(ID1 = region_id[County], ID2 = region_id[Town])
	err.id2 <- bind_rows(filter(i2.base, ID1 != substr(ID2,1,2) & str_length(ID1) == 2),filter(i2.base, ID1 != substr(ID2,1,5) & str_length(ID1) == 5))
	for (i in seq_len(nrow(err.id2)))
	{
		i2.base$ID2[i2.base$County == err.id2$County[i] & i2.base$Town == err.id2$Town[i]] <-
			region_id[grepl(err.id2$ID1[i], region_id) & err.id2$Town[i] == region_name]
	}

	#------------------ Combine All Data Together ------------------------

	canparty <- select(suppressMessages(inner_join(i.cand, i.party)), LV1a:Name, PartyName)
	if (attr(dir, "市長")) canparty <- select(canparty, -(LV2:LV3))
	if (attr(dir, "總統")) canparty <- select(canparty, -(LV1a:LV3))
	if (attr(dir, "政黨")) canparty <- select(canparty, -(LV1a:LV3))
	if (attr(dir, "立委")) canparty <- select(canparty, -(LV2:LV3))
	if (attr(dir, "議員")) canparty <- select(canparty, -(LV2:LV3))

	ctkparty <- suppressMessages(inner_join(filter(i.ctks, LV2 != 0), canparty))

	profname <- suppressMessages(full_join(i.prof, i2.base))

	ctkparty <- suppressMessages(full_join(ctkparty, profname)) %>%
		select(Number, Winner:VoteCnt, GetVote, County:ID2) %>%
		mutate(VoteProp = GetVote / VoteCnt) %>%
		filter(!is.na(Number), !is.na(ID1), !is.na(ID2)) %>% arrange(ID1, ID2)

	if (as.numeric(ctkparty %>% filter(is.na(ID2)) %>% summarise(n())) > 0) {
		message("區域名稱錯誤")
		print(ctkparty %>% filter(is.na(ID2)) %>% select(County:ID2))
	}

	if (tolower(saveType) == "csv") {
		fn <- paste0(dir[1], "/tidydata.csv")
		write.csv(ctkparty, fn, fileEncoding = "utf8", row.names = FALSE)
		message(sprintf("Export file %s", fn))
	}
	else if (tolower(saveType) == "json") {
		fn <- paste0(dir[1], "/tidydata.json")
		enc <- options()[["encoding"]]
		options(encoding = "utf8")
		write(toJSON(ctkparty, pretty = TRUE), fn)
		options(encoding = enc)
		message(sprintf("Export file %s", fn))
	}

	ctkparty
}
