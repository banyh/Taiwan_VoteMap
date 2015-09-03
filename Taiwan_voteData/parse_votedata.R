#
# 本程式的目的，是從每個選區中挑出一個政黨及該政黨的得票率
# 用政黨決定顏色，用得票率決定顏色的深淺
#
# 1.如何挑選政黨
#   a.選擇得票率最高的侯選人所在的政黨
#   b.選擇特定的政黨
#
# 2.如何決定顏色
#   目前是根據傳統的顏色印象，即國民黨=藍色、民進黨=綠色
#   但其他政黨並沒有特別標註，無黨籍暫定黃色、其他政黨都給灰色
#
# 3.傳回值
#   傳回值是一個name vector，可直接用區域代碼查詢顏色
#   例如COLOR["63"] 代表台北市的顏色、COLOR["6702900"] 代表關廟區的顏色
#   區域代碼請參考 region_name.xml
#
require(csvread)
require(plyr)
require(PBSmapping)
require(stringr)

# 參數 select_func 用來挑選政黨
#   例如有得票率資料如下:
#         1    16    999 <-- 這裡代表政黨代碼
#   63  0.5   0.4    0.1 <-- 這裡代表某個區域各政黨得票率
#   64  0.3   0.7    0.0
#
#   預設的 select_func 是 max.col 會找出最大值所在的欄
#   上面的例子中，會傳回 c(1, 2)，因為第一列最大值在第一欄，第二列在第二欄
#   所以會挑出最高得票率的侯選人所在的政黨
#	如果要挑出特定的政黨，可以用下列函式
DPP <- function(x) rep(which(colnames(x)==16), nrow(x))
KMT <- function(x) rep(which(colnames(x)==1), nrow(x))
NOPARTY <- function(x) rep(which(colnames(x)==999), nrow(x))

# 參數 color_func 是將政黨及得票率轉換為顏色
# 為了方便選擇同一顏色的深淺，使用 HSV color space
# 可先到 http://colorizer.org/ 先測試想要的顏色
party_to_hsv <- function (p, party, support)
{
	pid <- function(name) p$編號[p$政黨 %in% name]
	# 中國共產黨外圍組織: 勞動黨,新黨,中華統一促進黨
	CPC <- pid(c("勞動黨", "新黨", "中華統一促進黨"))
	# 第三勢力: 綠黨,人民民主陣線,人民最大黨,中華民主向日葵憲政改革聯盟,樹黨
	THIRD <- pid(c("綠黨","人民民主陣線","人民最大黨","中華民主向日葵憲政改革聯盟","樹黨"))

	PFP <- pid("親民黨")
	DPP <- pid("民主進步黨")
	KMT <- pid("中國國民黨")
	NPSU <- pid("無黨團結聯盟")
	TSU <- pid("台灣團結聯盟")
	NONE <- pid(c("無", "無黨籍及未經政黨推薦"))

	p <- hsv(1, 0, 1.2-support)	# 其他黨派是灰色
	p[party == KMT] <- hsv(0.6, 1, 1.2-support[party == KMT])			# 國民黨: 藍色
	p[party == DPP] <- hsv(0.25, 1, 1.2-support[party == DPP])			# 民進黨: 是綠色
	p[party == PFP] <- hsv(1/12, 1, 1.2-support[party == PFP])			# 親民黨: 橘色
	p[party == TSU] <- hsv(0.45, 1, 1.2-support[party == TSU])			# 台聯: 青色
	p[party %in% NONE] <- hsv(1/6, 1, 1.2-support[party %in% NONE])		# 無黨: 土黃色
	p[party %in% CPC] <- hsv(0, 1, 1.2-support[party %in% CPC])			# 共產黨: 紅色
	p[party %in% THIRD] <- hsv(5/6, 1, 1.2-support[party %in% THIRD])	# 第三勢力: 紫色
	p
}

factor_to_int <- function(fac)
{
	fac <- factor(fac)
	levels(fac) <- sub("'", "", levels(fac))
	as.integer(as.character(fac))
}

voteData <- function(dataset_name, color_func = party_to_hsv, select_func = max.col)
{
	dataset_year <- as.numeric(sub(".*(\\d{4}).*", "\\1", dataset_name$dir[1]))
	merge_vote <- data.frame()

	elparty <- function()
	{
		p <- NULL
		for (i in seq_len(length(dataset_name$dir))) {
			p <- rbind(p, csvread(paste0(dataset_name$dir[i],"/elpaty.csv"), coltypes = rep("string",2), header=F))
		}
		p$COL2 <- iconv(p$COL2, "UTF-8", "UTF-8")
		p$COL1 <- as.integer(gsub("'|\"", "", p$COL1))
		p$COL2 <- gsub("'|\"", "", p$COL2)
		names(p) <- c("編號", "政黨")
		p <- p[!duplicated(p$編號),]
		p
	}

	elbase <- function()
	{
		b <- NULL
		for (i in seq_len(length(dataset_name$dir))) {
			b <- rbind(b, csvread(paste0(dataset_name$dir[i],"/elbase.csv"), coltypes = rep("string",6), header=F))
		}
		b$COL6 <- iconv(b$COL6, "UTF-8", "UTF-8")
		b <- do.call(cbind, lapply(b, function(s) gsub(" ", "", gsub("'", "", gsub("\"", "", s)))))
		b <- as.data.frame(b)
		for (i in 1:5) b[,i] <- factor_to_int(b[,i])
		b[,6] <- as.character(b[,6])
		names(b) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "行政區")
		b <- arrange(b, 省市別, 縣市別, 選區別, 鄉鎮市區, 村里別)

		b$LV1 <- ifelse(b$省市別 > 10, b$省市別, b$省市別 * 1000 + b$縣市別)
		b$LV2 <- ifelse(b$LV1 > 100, b$LV1 * 100 + b$鄉鎮市區, b$LV1 * 100000 + b$鄉鎮市區 * 100)
		b$LV3 <- b$LV2 * 1000 + b$村里別

		# 因為多個區域可能合併投票，所以會出現「復興村、福沃村」，直接將後面的部分刪除
		merge_vote <<- data.frame(name=grep("、", b$行政區, value=TRUE), pos=grep("、", b$行政區))
		b$行政區 <- sub("(.*?)、.*", "\\1", b$行政區)
		if (dataset_year <= 2010) {
			# 霧臺鄉 霧台村
			tmp <- grep("霧臺鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="霧台村"] <- "霧臺村"
			# 斗南鎮 西歧里
			tmp <- grep("斗南鎮", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="西歧里"] <- "西岐里"
			# 斗六市 溝x里
			tmp <- grep("斗六市", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="溝\u57BB里"] <- "溝\u57E7里"
			# 二水鄉 上豐村
			tmp <- grep("二水鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="上豐村"] <- "上\u8C4A村"
			# 二水鄉 上豐村
			tmp <- grep("二水鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="上豐村"] <- "上\u8C4A村"
			# 埤頭鄉 豐崙村,永豐村,和豐村
			tmp <- grep("埤頭鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="豐崙村"] <- "\u8C4A崙村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="永豐村"] <- "永\u8C4A村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="和豐村"] <- "和\u8C4A村"
			# 秀水鄉 陜西村
			tmp <- grep("秀水鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="陜西村"] <- "陝西村"
			# 苑裡鎮 山腳里,上館里
			tmp <- grep("苑裡鎮", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="山腳里"] <- "山\u811A里"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="上館里"] <- "上\u8218里"
			# 三義鄉 雙湖村,雙潭村
			tmp <- grep("三義鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雙湖村"] <- "\u53cc湖村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雙潭村"] <- "\u53cc潭村"
			# 後龍鎮 溪州里
			tmp <- grep("後龍鎮", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="溪州里"] <- "溪洲里"
			# 竹南鎮 公館里
			tmp <- grep("竹南鎮", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="公館里"] <- "公\u8218里"
			# 三灣鄉 銅境村
			tmp <- grep("三灣鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="銅境村"] <- "銅鏡村"
			# 竹東鎮 雞林里
			tmp <- grep("竹東鎮", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雞林里"] <- "\u9DC4林里"
			# 蘆竹鄉 瓦x村
			tmp <- grep("蘆竹鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="瓦\u78D8村"] <- "瓦窯村"
			# 公館鄉 五穀村,玉榖村
			tmp <- grep("公館鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="五穀村"] <- "五谷村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="玉榖村"] <- "玉谷村"
			# 埔心鄉 埤腳村,舊館村,南館村,新館村
			tmp <- grep("埔心鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="埤腳村"] <- "埤\u811A村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="舊館村"] <- "舊\u8218村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="南館村"] <- "南\u8218村"
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="新館村"] <- "新\u8218村"
			# 彰化縣 褔興鄉
			tmp <- grep("彰化縣", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$縣市別==b$縣市別[tmp] & b$行政區=="褔興鄉"] <- "福興鄉"
		}
		if (dataset_year <= 2012) {
			# 2014年，桃園縣改制為桃園市，村改為里，鄉鎮市改為區
			taoyuan0 <- grep("桃園縣", b$行政區)[1]
			if (!is.na(taoyuan0)) {
				taoyuan1 <- b$省市別[taoyuan0]
				taoyuan2 <- b$縣市別[taoyuan0]
				tao <- b$省市別==taoyuan1 & b$縣市別==taoyuan2
				b$行政區[tao] <- sub("(..)村", "\\1里", b$行政區[tao])
				b$行政區[tao] <- sub("(..)[鄉鎮市]", "\\1區", b$行政區[tao])
				b$行政區[taoyuan0] <- "桃園市"
			}

			# 萬里區 崁腳里
			tmp <- grep("萬里區", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="崁腳里"] <- "崁\u811A里"
			# 恆春鎮 山腳里
			tmp <- grep("恆春鎮", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="山\u811A里"] <- "山腳里"
			# 新化區 那拔里
			b$行政區[grep(".拔里", b$行政區)] <- "\ue000拔里"
			# 宜蘭市 大東里和新興里合併，變成大新里
			tmp <- grep("宜蘭市", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="大東里"] <- "大新里"
			# 板橋區 公館里
			tmp <- grep("板橋區", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="公館里"] <- "公\u8218里"
			# 朴子市 雙溪里
			tmp <- grep("朴子市", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雙溪里"] <- "\u53cc溪里"
			# 梅山鄉 雙溪村
			tmp <- grep("梅山鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雙溪村"] <- "\u53cc溪村"
			# 梅山鄉 雙福村
			tmp <- grep("梅山鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雙福村"] <- "\u53cc福村"
			# 梅山鄉 瑞峰村
			tmp <- grep("梅山鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="瑞峰村"] <- "瑞\u5cef村"
			# 民雄鄉 雙福村
			tmp <- grep("民雄鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="雙福村"] <- "\u53cc福村"
			# 坪林區,龍崎區 石x里
			b$行政區[grep("石.{12}里", b$行政區)] <- "石\ue001里"
			# 口湖鄉 台子村
			tmp <- grep("口湖鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="台子村"] <- "臺子村"
			# 信義區 富台里
			tmp <- grep("信義區", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="富台里"] <- "富臺里"
			# 大城鄉 台西村
			tmp <- grep("大城鄉", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="台西村"] <- "臺西村"
			# 麻豆區 晉江里
			tmp <- grep("麻豆區", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="晉江里"] <- "\u664b江里"
			# 彰化市 南瑤里
			tmp <- grep("彰化市", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="南\u7476里"] <- "南瑤里"
			# 新店區 五峰里
			tmp <- grep("新店區", b$行政區)[1]
			if (!is.na(tmp)) b$行政區[b$省市別==b$省市別[tmp] & b$鄉鎮市區==b$鄉鎮市區[tmp] & b$行政區=="五峰里"] <- "五\u5CEF里"
		}
		b$ID <- region_id[b$行政區]

		# 麻煩1: 因為從region_id中查詢時，同名的地區只會抓第一個id，導致下列地區錯誤
		#        北區、東區、西區、大安區、信義區
		b$AREA <- NA
		bc <- b[b$村里別==0 & b$鄉鎮市區==0 & b$選區別==0,]			# 包含所有縣市等級的代碼
		bcrow <- c(as.numeric(row.names(bc)), nrow(b)+1)
		for (i in seq_len(length(bcrow)-1)) {
			b$AREA[seq(from=bcrow[i], to=bcrow[i+1]-1)] <- bc$ID[i]
		}
		# rescan包含所有代碼與上層代碼對不上的列數
		# 例如基隆市信義區查詢到6300200，但基隆市是10017，就需要更正
		rescan <- which(b$AREA != substr(b$ID, 1, 2) & b$鄉鎮市區 != 0)
		for (i in rescan) {
			s <- grep(b$AREA[i], region_id[region_name == b$行政區[i]], value=TRUE)
			if (length(s) == 1) b$ID[i] <- s
		}
		# 麻煩2: 解決同名的村里，例如全台有五個「新安里」
		bc <- b[b$村里別==0 & b$鄉鎮市區!=0,]
		bcrow <- c(as.numeric(row.names(bc)), nrow(b)+1)
		for (i in seq_len(length(bcrow)-1)) {
			b$AREA[seq(from=bcrow[i], to=bcrow[i+1]-1)] <- bc$ID[i]
		}
		# rescan包含所有代碼與上層代碼對不上的列數
		rescan <- which(b$AREA != substr(b$ID, 1, 7) & b$村里別 != 0)
		for (i in rescan) {
			s <- grep(b$AREA[i], region_id[region_name == b$行政區[i]], value=TRUE)
			if (length(s) == 1) b$ID[i] <- s else b$ID[i] <- NA
		}
		b$COL1 <- as.numeric(substr(b$ID, 1, 2))
		b$COL2 <- as.numeric(substr(b$ID, 3, 5))
		b$COL3 <- as.numeric(substr(b$ID, 6, 7))
		b$COL4 <- as.numeric(substr(b$ID, 9, 11))
		b$COL1[is.na(b$COL1)] <- 0
		b$COL2[is.na(b$COL2)] <- 0
		b$COL3[is.na(b$COL3)] <- 0
		b$COL4[is.na(b$COL4)] <- 0
		b$COL3[b$COL1>10] <- b$COL2[b$COL1>10]
		b$COL2[b$COL1>10] <- 0

		b$NLV1 <- ifelse(b$COL1 > 10, b$COL1, b$COL1 * 1000 + b$COL2)
		b$NLV2 <- ifelse(b$NLV1 > 100, b$NLV1 * 100 + b$COL3, b$NLV1 * 100000 + b$COL3 * 100)
		b$NLV3 <- b$NLV2 * 1000 + b$COL4
		b
	}

	elcand <- function()
	{
		f <- NULL
		for (i in seq_len(length(dataset_name$dir))) {
			f <- rbind(f, csvread(paste0(dataset_name$dir[i],"/elcand.csv"), coltypes = rep("string",16), header=F))
		}
		f$COL7 <- iconv(f$COL7, "UTF-8", "UTF-8")
		f$COL12 <- iconv(f$COL12, "UTF-8", "UTF-8")
		f$COL13 <- iconv(f$COL13, "UTF-8", "UTF-8")
		f <- do.call(cbind, lapply(f, function(s) gsub("'", "", gsub("\"", "", s))))
		f <- as.data.frame(f)
		for (i in 1:5) f[,i] <- factor_to_int(f[,i])
		names(f) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "號次", "名字", "政黨代號", "性別", "出生日期", "年齡", "出生地", "學歷", "現任", "當選註記", "副手")
		f$政黨代號 <- as.integer(as.character(f$政黨代號))
		f$號次 <- as.integer(as.character(f$號次))
		if (dataset_year >= 2014) f$鄉鎮市區 <- f$鄉鎮市區 / 10
		f
	}

	elctks <- function(elbase)
	{
		g <- NULL
		for (i in seq_len(length(dataset_name$dir))) {
			g <- rbind(g, read.csv(paste0(dataset_name$dir[i],"/elctks.csv"), header=F))
		}
		for (i in 1:5) g[,i] <- factor_to_int(g[,i])
		names(g) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "投開票所", "候選人號次", "得票數", "得票率", "當選註記")

		if (dataset_year >= 2014) {
			g$鄉鎮市區 <- g$鄉鎮市區 / 10
			g$LV1 <- ifelse(g$省市別 > 10, g$省市別, g$省市別 * 1000 + g$縣市別)
			g$LV2 <- ifelse(g$LV1 > 100, g$LV1 * 100 + g$鄉鎮市區, g$LV1 * 100000 + g$鄉鎮市區 * 100)
			g$LV3 <- g$LV2 * 1000 + g$村里別
		} else {
			lv1_map <- elbase$NLV1
			lv2_map <- elbase$NLV2
			lv3_map <- elbase$NLV3
			names(lv1_map) <- elbase$LV1
			names(lv2_map) <- elbase$LV2
			names(lv3_map) <- elbase$LV3
			g$OLV1 <- ifelse(g$省市別 > 10, g$省市別, g$省市別 * 1000 + g$縣市別)
			g$OLV2 <- ifelse(g$OLV1 > 100, g$OLV1 * 100 + g$鄉鎮市區, g$OLV1 * 100000 + g$鄉鎮市區 * 100)
			g$OLV3 <- g$OLV2 * 1000 + g$村里別
			g$LV1 <- as.numeric(lv1_map[as.character(g$OLV1)])
			g$LV2 <- as.numeric(lv2_map[as.character(g$OLV2)])
			g$LV3 <- as.numeric(lv3_map[as.character(g$OLV3)])
		}
		g
	}

	elprof <- function()
	{
		h <- NULL
		for (i in seq_len(length(dataset_name$dir))) {
			h <- rbind(h, read.csv(paste0(dataset_name$dir[i],"/elprof.csv"), header=F, colClasses=rep("character",10)))
		}
		h <- h[,1:10]
		for (i in 1:5) h[,i] <- factor_to_int(h[,i])
		names(h) <- c("省市別", "縣市別", "選區別", "鄉鎮市區", "村里別", "投開票所", "有效票", "無效票", "投票數", "選舉人數")
		if (dataset_year >= 2014) h$鄉鎮市區 <- h$鄉鎮市區 / 10
		h
	}

	find_party <- function(ctks, cand)
	{
		party_id <- cand$政黨代號
		if (grepl("市長$", dataset_name$dir[1])) {
			regionid <- sprintf("%02d%02d%02d", ctks$省市別, ctks$縣市別, ctks$候選人號次)
			names(party_id) <- sprintf("%02d%02d%02d", cand$省市別, cand$縣市別, cand$號次)
		}
		else if (grepl("議員$", dataset_name$dir[1])) {
			regionid <- sprintf("%02d%02d%02d%02d", ctks$省市別, ctks$縣市別, ctks$選區別, ctks$候選人號次)
			names(party_id) <- sprintf("%02d%02d%02d%02d", cand$省市別, cand$縣市別, cand$選區別, cand$號次)
		}
		else if (grepl("里長$", dataset_name$dir[1])) {
			regionid <- sprintf("%02d%02d%02d%03d%02d", ctks$省市別, ctks$縣市別, ctks$鄉鎮市區, ctks$村里別, ctks$候選人號次)
			names(party_id) <- sprintf("%02d%02d%02d%03d%02d", cand$省市別, cand$縣市別, cand$鄉鎮市區, cand$村里別, cand$號次)
		}
		else if (grepl("立委$", dataset_name$dir[1])) {
			regionid <- sprintf("%02d%02d%02d%02d", ctks$省市別, ctks$縣市別, ctks$選區別, ctks$候選人號次)
			names(party_id) <- sprintf("%02d%02d%02d%02d", cand$省市別, cand$縣市別, cand$選區別, cand$號次)
		}
		else if (grepl("總統$", dataset_name$dir[1]) || grepl("政黨$", dataset_name$dir[1])) {
			regionid <- sprintf("%02d", ctks$候選人號次)
			names(party_id) <- sprintf("%02d", cand$號次)
		}
		ctks$政黨代號 <- party_id[regionid]
		ctks
	}

	division_planning <- function(region, array)
	{
		if (dataset_year <= 2010) {
			# 由 桃園縣八德市茄苳里 分割出 桃園縣八德市永豐里
			array["6800800-047"] <- array["6800800-011"]
			# 從 桃園縣八德市大華里 分割出 桃園縣八德市大順里
			array["6800800-048"] <- array["6800800-025"]
			# 由 桃園縣中壢市至善里 分割出 桃園縣中壢市龍慈里
			array["6800200-082"] <- array["6800200-063"]
			# 由 桃園縣中壢市自立里 分割出 桃園縣中壢市自信里
			array["6800200-083"] <- array["6800200-008"]
			# 由 桃園縣中壢市新興里 分割出 桃園縣中壢市林森里
			array["6800200-084"] <- array["6800200-025"]
			# 由 桃園縣中壢市水尾里 分割出 桃園縣中壢市金華里
			array["6800200-085"] <- array["6800200-031"]
			# 由 桃園縣楊梅鎮梅溪里 分割出 桃園縣楊梅鎮瑞溪里
			array["6800400-036"] <- array["6800400-015"]
			# 由 桃園縣楊梅鎮高山里 分割出 桃園縣楊梅鎮高上里
			array["6800400-037"] <- array["6800400-016"]
			# 由 桃園縣楊梅鎮富岡里 分割出 桃園縣楊梅鎮富豐里
			array["6800400-038"] <- array["6800400-020"]
			# 由 桃園縣楊梅鎮裕成里 分割出 桃園縣楊梅鎮裕新里
			array["6800400-039"] <- array["6800400-029"]
			# 由 桃園縣楊梅鎮金溪里 分割出 桃園縣楊梅鎮三民里
			array["6800400-040"] <- array["6800400-028"]
			# 由 桃園縣楊梅鎮三湖里 分割出 桃園縣楊梅鎮頭湖里
			array["6800400-041"] <- array["6800400-024"]
			# 由 嘉義市東區東川里,嘉義市東區短竹里 分割出 嘉義市東區蘭潭里
			array["1002001-065"] <- array["1002001-038"]
			# 由 嘉義市東區盧厝里,嘉義市東區長竹里 分割出 嘉義市東區文雅里
			array["1002001-066"] <- array["1002001-041"]
			# 由 嘉義市東區安寮里,嘉義市東區興安里,嘉義市西區美源里 分割出 嘉義市東區安業里
			array["1002001-067"] <- array["1002001-052"]
			# 由 嘉義市東區仁義里,嘉義市東區頂庄里 分割出 嘉義市東區義教里
			array["1002001-068"] <- array["1002001-003"]
		}
		if (dataset_year <= 2012) {
			# 宜蘭市重劃調整 (http://www.ilancity.gov.tw/news_view.asp?id=1682)
			array["1000201-041"] <- array["1000201-015"]
			array["1000201-042"] <- array["1000201-014"]
			array["1000201-043"] <- array["1000201-038"]
			array["1000201-044"] <- array["1000201-037"]
			array["1000201-046"] <- array["1000201-017"]
			array["1000201-047"] <- array["1000201-008"]
			# 蘇澳市調整 (http://sahhr.e-land.gov.tw/releaseRedirect.do?unitID=314&pageID=10722)
			array["1000203-027"] <- array["1000203-009"]
			# 南投都達村 http://m.ltn.com.tw/news/local/paper/806651
			array["1000813-016"] <- array["1000813-012"]
			# 麥寮鄉中興村 http://www.epochtimes.com/b5/14/7/1/n4190419.htm
			array["1000913-013"] <- array["1000913-008"]
			# 杉林區 大愛里 http://khdoc.ksu.edu.tw/node/82112
			array["6403400-008"] <- array["6403400-006"]
			# 新竹市 中雅里 http://news.ltn.com.tw/news/local/paper/691084
			array["1001802-045"] <- array["1001802-030"]
			# 新竹市 關新里
			array["1001801-054"] <- array["1001801-043"]
			# 竹北市 十興里 http://news.ltn.com.tw/news/local/paper/659633
			array["1000401-027"] <- array["1000401-021"]
			array["1000401-028"] <- array["1000401-021"]
			array["1000401-029"] <- array["1000401-021"]
			array["1000401-030"] <- array["1000401-021"]
			# 觀音區 https://zh.wikipedia.org/zh-tw/%E8%A7%80%E9%9F%B3%E5%8D%80
			array["6801200-024"] <- array["6801200-021"]
			# 蘆竹區 http://www.dgbas.gov.tw/public/data/dgbas03/bs1/%E8%A1%8C%E6%94%BF%E5%8D%80%E5%9F%9F%E5%8F%8A%E6%9D%91%E9%87%8C%E4%BB%A3%E7%A2%BC/4815155311RP37PKRX.pdf
			array["6800500-027"] <- array["6800500-018"]
			array["6800500-028"] <- array["6800500-018"]
			array["6800500-029"] <- array["6800500-018"]
			array["6800500-030"] <- array["6800500-018"]
			array["6800500-031"] <- array["6800500-026"]
			array["6800500-032"] <- array["6800500-026"]
			array["6800500-033"] <- array["6800500-009"]
			array["6800500-034"] <- array["6800500-009"]
			array["6800500-035"] <- array["6800500-008"]
			array["6800500-036"] <- array["6800500-001"]
			array["6800500-037"] <- array["6800500-001"]
			array["6800500-038"] <- array["6800500-025"]
		}
		for (i in seq_len(nrow(merge_vote))) {
			town <- region[region$省市別==region$省市別[merge_vote$pos[i]] &
							region$縣市別==region$縣市別[merge_vote$pos[i]] &
							region$選區別==region$選區別[merge_vote$pos[i]] &
							region$鄉鎮市區==region$鄉鎮市區[merge_vote$pos[i]] &
							region$村里別==0,]
			village <- grep(town$NLV2, region_id, value=TRUE)
			tmp <- str_split(merge_vote$name[i], "、")[[1]]
			array[village[tmp[2:length(tmp)]]] <- array[village[tmp[1]]]
		}

		array
	}

	generate_color <- function(region, ctks, party, func)
	{
		level_column <- c("LV1","LV2","LV3")
		COLOR <- character()
		WINNER <- character()
		gg <- list()
		gg[[3]] <- ctks[ctks$投開票所 == 0 & ctks$村里別 != 0 & ctks$鄉鎮市區 != 0 & ctks$省市別 != 0,]
		gg[[2]] <- ctks[ctks$村里別 == 0 & ctks$鄉鎮市區 != 0 & ctks$省市別 != 0,]
		gg[[1]] <- ctks[ctks$鄉鎮市區 == 0 & ctks$省市別 != 0,]

		for (lv in 1:3)
		{
			if (grepl("議員$", dataset_name$dir[1])) {
				partyVote <- tapply(gg[[lv]]$得票數, list(gg[[lv]][[level_column[lv]]],gg[[lv]]$政黨代號), sum)
				partyVote[is.na(partyVote)] <- 0
				partyVote <- partyVote[rowSums(partyVote)!=0,]
				partyVote <- apply(partyVote, 2, function(x) x/rowSums(partyVote)*100)
			} else {
				partyVote <- tapply(gg[[lv]]$得票率, list(gg[[lv]][[level_column[lv]]],gg[[lv]]$政黨代號), max)
				partyVote[is.na(partyVote)] <- 0
				partyVote <- partyVote[rowSums(partyVote)!=0,]
			}
			partyVote <- partyVote
			regWinner <- data.frame(
				id=as.numeric(row.names(partyVote)),
				vote=partyVote[(func(partyVote)-1)*nrow(partyVote)+seq_len(nrow(partyVote))],
				party=colnames(partyVote)[func(partyVote)])
			color <- suppressWarnings(color_func(party, regWinner$party, regWinner$vote / 100))
			if (lv == 1) {
				names(color) <- ifelse(regWinner$id > 100, sprintf("%05d", regWinner$id), regWinner$id)
			} else if (lv == 2) {
				names(color) <- sprintf("%07d", regWinner$id)
			} else {
				names(color) <- sprintf("%07d-%03d", as.integer(regWinner$id / 1000), regWinner$id %% 1000)
			}
			tmp <- as.character(regWinner$vote)
			names(tmp) <- names(color)
			WINNER <- c(WINNER, tmp)
			COLOR <- c(COLOR, color)
		}
		COLOR <- division_planning(region, COLOR)
		WINNER <- division_planning(region, WINNER)
		list(COLOR=COLOR, VOTE=WINNER)
	}

	party  <- elparty()
	region <- elbase()
	cand   <- elcand()
	ctks   <- elctks(region)
	prof   <- elprof()
	ctks2  <- find_party(ctks, cand)
	col_vote <- generate_color(region, ctks2, party, select_func)
	col_vote
}
