#
# 在執行之前:
# 1.下載台灣區域界線，在 Taiwan_Map 目錄解壓縮
#	a. 縣(市)行政區域界線 http://data.gov.tw/node/7442
#	b. 鄉(鎮、市、區)行政區域界線 http://data.gov.tw/node/7441
#	c. 村里界圖(WGS84經緯度) http://data.gov.tw/node/7438
#	d. 將中文的檔名改掉
#		縣市界(經緯度)1031225_big5 改成 Level1_1031225_big5
#		鄉鎮界(經緯度)1031225_big5 改成 Level2_1031225_big5
#
# 2.下載選舉資料庫 http://data.gov.tw/node/13119
#	解壓縮後，將目錄改名為 Taiwan_voteData
#
# 3.執行此 script
#
# 4.執行 map$ggplot(1, COLOR) 來產生圖形
#   詳細的參數說明請參考 parse_mapdata.R
#

list.of.packages <- c("XML", "stringr", "sp", "rgdal", "ggplot2", "plyr", "Cairo",
	"csvread", "PBSmapping", "leafletR", "jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages)
rm(new.packages)

require(XML)
require(stringr)
setwd("d:/Project/Taiwan_VoteMap")

menu <- list(
	list(name="2009_2010_mayor", lv=c(T,T,T), dir=c("Taiwan_voteData/2010年都市長議員及里長/市長", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/縣市長")),
	list(name="2009_2010_councilors", lv=c(T,T,T), dir=c("Taiwan_voteData/2010年都市長議員及里長/區域議員", "Taiwan_voteData/2009年縣市長縣市議員及鄉鎮長/區域議員")),
	list(name="2010_village", lv=c(F,F,T), dir=c("Taiwan_voteData/2010年村里長", "Taiwan_voteData/2010年都市長議員及里長/里長")),
	list(name="2012_party", lv=c(T,T,T), dir=c("Taiwan_voteData/2012年總統及立委/不分區政黨")),
	list(name="2012_lagislator", lv=c(T,T,T), dir=c("Taiwan_voteData/2012年總統及立委/區域立委")),
	list(name="2012_president", lv=c(T,T,T), dir=c("Taiwan_voteData/2012年總統及立委/總統")),
	list(name="2014_mayor", lv=c(T,T,T), dir=c("Taiwan_voteData/2014年地方公職人員選舉/直轄市市長", "Taiwan_voteData/2014年地方公職人員選舉/縣市市長")),
	list(name="2014_councilors", lv=c(T,T,T), dir=c("Taiwan_voteData/2014年地方公職人員選舉/直轄市區域議員", "Taiwan_voteData/2014年地方公職人員選舉/縣市區域議員")),
	list(name="2014_village", lv=c(F,F,T), dir=c("Taiwan_voteData/2014年地方公職人員選舉/直轄市村里長", "Taiwan_voteData/2014年地方公職人員選舉/縣市村里長"))
)

r <- xmlRoot(xmlTreeParse("Taiwan_Region/region_name.xml", useInternalNodes = TRUE))
region_id <- str_replace_all(xpathSApply(r, "//Code", xmlValue), "\\t", "")
names(region_id) <- str_replace_all(xpathSApply(r, "//Content", xmlValue), "\\t", "")
region_name <- names(region_id)
names(region_name) <- region_id
rm(r)

source('Taiwan_Map/parse_mapdata.R', encoding = 'UTF-8')
source('Taiwan_voteData/parse_votedata.R', encoding = 'UTF-8')

for (i in seq_len(length(menu))) {
	message(sprintf("%3d: %s", i, menu[[i]]$name))
}
select <- as.numeric(readline(prompt = "請輸入選擇: "))
if (!is.na(select) && select <= length(menu) && select >= 1) {
	message(sprintf("你選擇了 %s", menu[[select]]$name))
	message(sprintf("輸入 map$plotGG(1, col_vote$COLOR, col_vote$VOTE) 來看結果"))
	col_vote <- voteData(menu[[select]])
}

# for (i in 1:length(menu)) {
# 	message(sprintf("%3d: %s", i, menu[[i]]$name))
# 	col_vote <- voteData(menu[[i]])
# 	if (menu[[i]]$lv[1]) g <- map$plotGG(1, col_vote$COLOR, col_vote$VOTE, type="pdf", filename=menu[[i]]$name)
# 	if (menu[[i]]$lv[2]) g <- map$plotGG(2, col_vote$COLOR, col_vote$VOTE, type="pdf", filename=menu[[i]]$name)
# 	if (menu[[i]]$lv[3]) g <- map$plotGG(3, col_vote$COLOR, col_vote$VOTE, type="pdf", filename=menu[[i]]$name)
# 	if (menu[[i]]$lv[2]) g <- map$plotGG(2, col_vote$COLOR, col_vote$VOTE, type="pdf", filename=menu[[i]]$name, county_select = "臺南市")
# 	if (menu[[i]]$lv[3]) g <- map$plotGG(3, col_vote$COLOR, col_vote$VOTE, type="pdf", filename=menu[[i]]$name, county_select = "新北市|臺北市")
# 	if (menu[[i]]$lv[3]) g <- map$plotGG(3, col_vote$COLOR, col_vote$VOTE, type="pdf", filename=menu[[i]]$name, town_select = "永康區")
# }

rm(i)
