require(dplyr)
require(tidyr)

genRankFilter <- function(rank) {
	return(function(x) filter(x, rank(-GetVote)==rank))
}

genPartyFilter <- function(party) {
	return(function(x) filter(x, PartyName==party))
}

party_to_hsv <- function (support, party)
{
	PFP <-  "親民黨"
	DPP <-  "民主進步黨"
	KMT <-  "中國國民黨"
	TSU <-  "台灣團結聯盟"
	NONE <- c("無", "無黨籍及未經政黨推薦")

	p <- hsv(1, 0, 1.0 - support * 0.8)	# 其他黨派是灰色
	p[party %in% KMT]  <- hsv(0.6,  1, 1.0 - 0.8*support[party %in% KMT])			# 國民黨: 藍色
	p[party %in% DPP]  <- hsv(0.25, 1, 1.0 - 0.8*support[party %in% DPP])			# 民進黨: 是綠色
	p[party %in% PFP]  <- hsv(1/12, 1, 1.0 - 0.8*support[party %in% PFP])			# 親民黨: 橘色
	p[party %in% TSU]  <- hsv(0.45, 1, 1.0 - 0.8*support[party %in% TSU])			# 台聯: 青色
	p[party %in% NONE] <- hsv(1/6,  1, 1.0 - 0.8*support[party %in% NONE])		# 無黨: 土黃色
	p
}

dir_to_title <- function(dir)
{
	if (grepl("市長$", dir[1])) {
		title <- "市長"
	}
	else if (grepl("議員$", dir[1]) | grepl("議員\\(區域\\)$", dir[1]) | grepl("議員/區域$", dir[1])) {
		title <- "議員"
	}
	else if (grepl("里長$", dir[1])) {
		title <- "里長"
	}
	else if (grepl("立委$", dir[1]) | grepl("立委/區域$", dir[1]) | grepl("立法委員/區域$", dir[1])) {
		title <- "立委"
	}
	else if (grepl("總統$", dir[1])) {
		title <- "總統"
	}
	else if (grepl("政黨$", dir[1])) {
		title <- "政黨"
	}
	year <- as.numeric(sub(".*(\\d{4})年.*", "\\1", dir[1]))
	return(paste0(year, "_", title))
}

votedir <- list(
#	c("Taiwan_voteData/1996年9任總統"),
#	c("Taiwan_voteData/1998年4屆立委/區域"),
#	c("Taiwan_voteData/1998年直轄市長", "Taiwan_voteData/1997年縣市長"),
#	c("Taiwan_voteData/1998年縣市議員/區域", "Taiwan_voteData/1998年直轄市議員/區域"),
	c("Taiwan_voteData/2000年10任總統"),
	c("Taiwan_voteData/2001年5屆立委/區域"),
	c("Taiwan_voteData/2002年直轄市長", "Taiwan_voteData/2001年縣市長"),
	c("Taiwan_voteData/2002年直轄市議員(區域)", "Taiwan_voteData/2002年縣市議員/區域"),
	c("Taiwan_voteData/2004年11任總統"),
	c("Taiwan_voteData/2004年第6屆立法委員/區域"),
	c("Taiwan_voteData/2006年直轄市長", "Taiwan_voteData/2005年縣市長"),
	c("Taiwan_voteData/2006年直轄市議員(區域)", "Taiwan_voteData/2005年縣市議員/區域"),
	c("Taiwan_voteData/2008年立委/區域"),
	c("Taiwan_voteData/2008年立委/不分區政黨"),
	c("Taiwan_voteData/2008年總統"),
	c("Taiwan_voteData/2010年都市長議員及里長/市長", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/縣市長"),
	c("Taiwan_voteData/2010年都市長議員及里長/區域議員", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/區域議員"),
	c("Taiwan_voteData/2012年總統及立委/不分區政黨"),
	c("Taiwan_voteData/2012年總統及立委/區域立委"),
	c("Taiwan_voteData/2012年總統及立委/總統"),
	c("Taiwan_voteData/2014年地方公職人員選舉/直轄市市長", "Taiwan_voteData/2014年地方公職人員選舉/縣市市長"),
	c("Taiwan_voteData/2014年地方公職人員選舉/直轄市區域議員", "Taiwan_voteData/2014年地方公職人員選舉/縣市區域議員")
)

source('vote_tidy_data.R', encoding = 'UTF-8')
source('parse_mapdata.R', encoding = 'UTF-8')

r <- xmlRoot(xmlTreeParse("Taiwan_Region/region_name.xml", useInternalNodes = TRUE))
region_id <- str_replace_all(xpathSApply(r, "//Code", xmlValue), "\\t", "")
names(region_id) <- str_replace_all(xpathSApply(r, "//Content", xmlValue), "\\t", "")
region_name <- names(region_id)
names(region_name) <- region_id
rm(r)

COLOR <- NULL
for (dir in votedir)
{
	# To load vote data, you can choose one from following methods:
	# 1. call vote_tidy_data() with specific directory
	#    ctkparty <- vote_tidy_data(dir)
	# 2. generate json file once, then you can load from json file anytime
	#    vote_tidy_data(dir, "json")
	#    ctkparty <- tbl_df(fromJSON(paste0(dir[1], "/tidydata.json")))
	# 3. generate csv file once, then you can load from csv file anytime
	#    ctkparty <- tbl_df(read.csv(paste0(dir[1], "/tidydata.csv"), fileEncoding = "utf8"))
	ctkparty <- vote_tidy_data(dir)

	lv1 <- ctkparty %>% group_by(ID1, PartyName) %>%
		summarise_each(funs(sum), GetVote, ValidCnt) %>%
		mutate(VoteProp = GetVote / ValidCnt)
	lv2 <- ctkparty %>% group_by(ID2, PartyName) %>%
		summarise_each(funs(sum), GetVote, ValidCnt) %>%
		mutate(VoteProp = GetVote / ValidCnt)

	if (nrow(lv1 %>% filter(VoteProp > 1)) > 0) {
		message("得票率錯誤")
		print(lv1 %>% filter(VoteProp > 1))
	}
	if (nrow(lv2 %>% filter(VoteProp > 1)) > 0) {
		message("得票率錯誤")
		print(lv2 %>% filter(VoteProp > 1))
	}

	winner <- genRankFilter(1)
	dpp <- genPartyFilter("民主進步黨")
	kmt <- genPartyFilter("中國國民黨")

	lv1 <-  winner(lv1)
	lv2 <-  winner(lv2)
	lv1$Color <- party_to_hsv(lv1$VoteProp, lv1$PartyName)
	lv2$Color <- party_to_hsv(lv2$VoteProp, lv2$PartyName)

	c <- c(lv1$Color, lv2$Color)
	names(c) <- c(lv1$ID1, lv2$ID2)
	print(dir)
	if (is.null(COLOR))
		COLOR <- t(c)
	else
		COLOR <- plyr::rbind.fill.matrix(COLOR, t(c))
}

# map$gen_geojson(2, COLOR, sapply(votedir, dir_to_title))


