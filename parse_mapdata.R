require(sp)
require(rgdal)
require(ggplot2)
require(Cairo)
require(leafletR)

#
# 地圖和資料是獨立的
# 資料代表區域和顏色的對應，例如: 高雄市->綠色、新北市->藍色等
# 地圖則是區域和多邊形的對應
#
# generate_map() 會傳回 plot_map() 函式
# plot_map(
#	level,					1 or 2 or 3，代表不同層級的地圖
#	COLOR,					COLOR 必須有區域代碼與顏色的對應
#	type = "none",			輸出格式，"pdf"或"png"會輸出為檔案，其他則會輸出在 R plot window
#	filename = "Votemap",	輸出檔名
#	county_select = "",		選擇要畫的區域，必須是縣市層級
#	town_select = "",		選擇要畫的區域，必須是鄉鎮市層級
#	village_select = ""		選擇要畫的區域，必須是村里層級
# )
#
# 範例:
#	a. 產生 2014_vote_major_lv2.png
#		plot_map(2, COLOR, "2014_vote_major", "png")
#   b. 產生 2014_vote_major_lv3.pdf
#		plot_map(3, COLOR, "2014_vote_major", "pdf")
#   c. 指定要產生的地區(可用 | 分隔)
#		plot_map(3, COLOR, town_select = "神岡區|大雅區|沙鹿區|清水區")
#		plot_map(3, COLOR, county_select = "臺北市")
#		plot_map(3, COLOR, village_select = "山崙里")
#   d. 需要產生同樣大小的圖檔，可用 png_width, png_height 指定大小 (單位=inch, 600dpi)

#
# 為了節省時間，預先處理地圖檔，通常只有第一次需要執行
# 讀取SHR檔，並將SpatialPolygonsDataFrame物件存入檔案中
#
prepare_map <- function()
{
	sh <- list()
	sh[[1]] <- readOGR("Taiwan_Map", "Level1_1031225_big5", verbose = FALSE)
	sh[[2]] <- readOGR("Taiwan_Map", "Level2_1031225_big5", verbose = FALSE)

	#
	# 為了相容不同來源的地圖，需要將column name一致化
	# Level 1 縣級地圖，必須有 COUNTY_ID
	# Level 2 鎮級地圖，必須有 TOWN_ID
	# Level 3 村級地圖，必須有 VILLAGE_ID
	#
	names(sh[[1]]@data) <- str_replace(names(sh[[1]]@data), "County_ID", "COUNTY_ID")
	names(sh[[2]]@data) <- str_replace(names(sh[[2]]@data), "Town_ID", "TOWN_ID")
	div_id <- c("COUNTY_ID", "TOWN_ID")

	levels(sh[[1]]@data$C_Name) <- iconv(levels(sh[[1]]@data$C_Name), "utf8", "utf8")
	levels(sh[[1]]@data$Remark) <- iconv(levels(sh[[1]]@data$Remark), "utf8", "utf8")
	levels(sh[[1]]@data$Add_Accept) <- iconv(levels(sh[[1]]@data$Add_Accept), "utf8", "utf8")

	levels(sh[[2]]@data$C_Name) <- iconv(levels(sh[[2]]@data$C_Name), "utf8", "utf8")
	levels(sh[[2]]@data$T_Name) <- iconv(levels(sh[[2]]@data$T_Name), "utf8", "utf8")
	levels(sh[[2]]@data$Remark) <- iconv(levels(sh[[2]]@data$Remark), "utf8", "utf8")
	levels(sh[[2]]@data$Add_Accept) <- iconv(levels(sh[[2]]@data$Add_Accept), "utf8", "utf8")

	# 刪除不必要的資料
	sh[[1]]@data$Area <- NULL
	sh[[1]]@data$Add_Date <- NULL
	sh[[1]]@data$Add_Accept <- NULL
	sh[[1]]@data$Remark <- NULL

	sh[[2]]@data$T_UID <- NULL
	sh[[2]]@data$Area <- NULL
	sh[[2]]@data$Add_Date <- NULL
	sh[[2]]@data$Add_Accept <- NULL
	sh[[2]]@data$Remark <- NULL

	saveRDS(sh, "Taiwan_Map/Shape.RDS")
}

#
# 傳回list，包含四個函式
#	plotBase: 用base graphic畫地圖
#	plotGG: 用ggplot2畫地圖
#	plotLeaflet: 輸出html及json檔
#	exportKML: 輸出KML檔
#
generate_map <- function()
{
	options(encoding = "utf8")	# 必須設為utf8，toGeoJSON才能正確運作
	sh <- readRDS("Taiwan_Map/Shape.RDS")
	div_id <- c("COUNTY_ID", "TOWN_ID")
	div_name <- c("C_Name", "T_Name")

	gen_geojson <- function(level, COLOR, COLNAME, filename = "Votemap")
	{
		shp <- sh[[level]]

		for (i in 1:18) {
			color <- as.character(COLOR[i, as.character(shp@data[[div_id[level]]])])
			color[is.na(color)] <- "#ffffff"
			shp@data[[COLNAME[i]]] <- color
		}

		json_fn <- toGeoJSON(shp, filename, "Output")
	}

	plot_leaflet <- function(level, COLOR, filename = "Votemap",
							 county_select = "", town_select = "")
	{
		#filename <- paste0(filename, "_", level, "_", county_select, town_select)
		#filename <- str_replace_all(filename,"\\|","_")

		select <- rep(TRUE, nrow(sh[[level]]))
		if (county_select != "")
			select <- grep(county_select, sh[[level]]@data$C_Name)
		else if (town_select != "")
			select <- grep(town_select, sh[[level]]@data$T_Name)

		shp <- sh[[level]][select,]

		color <- as.character(COLOR[as.character(sh[[level]]@data[[div_id[level]]][select])])
		color[is.na(color)] <- "black"
		shp@data[["COLOR"]] <- color

		names(shp@data) <- str_replace(names(shp@data), div_name[level], "行政區")

		json_fn <- toGeoJSON(shp, filename, "Output")
		sty <- styleSingle(col="white", lwd=0.5, alpha=0.8, fill.alpha=0.6, fill="black")
		ll <- leaflet(json_fn, "Output", filename, style = sty, popup = c("行政區"),
			controls = c("zoom","scale","layer"), incl.data = FALSE)
		ll
	}

	plot_base <- function(level, COLOR, type = "none", filename = "Votemap",
							png_width = 0, png_height = 0,
							county_select = "", town_select = "")
	{
		type <- tolower(type)
		#filename <- paste0(filename, "_", level, "_", county_select, town_select, ".", type)
		#filename <- str_replace_all(filename,"\\|","_")
		if (county_select == "" && town_select == "") {
			lty <- 0
		} else {
			lty <- 1
		}
		lwd <- ifelse(type=="png", 1, 0.01)

		select <- rep(TRUE, nrow(sh[[level]]))
		if (county_select != "")
			select <- grep(county_select, sh[[level]]@data$C_Name)
		else if (town_select != "")
			select <- grep(town_select, sh[[level]]@data$T_Name)

		shp <- sh[[level]][select,]
		width <- bbox(shp)["x","max"] - bbox(shp)["x","min"]
		height <- bbox(shp)["y","max"] - bbox(shp)["y","min"]
		if (type == "pdf") {
			width <- ifelse(png_width > 0, png_width, width*level)
			height <- ifelse(png_height > 0, png_height, height*level)
			pdf(paste0(filename, ".", type), width = width, height = height)
		}
		else if (type == "none") {
			# display in R
		}
		else {
			png(paste0(filename, ".", type), width = width*level, height = height*level, res = 600, units = "in", bg = "transparent")
		}

#		xlim <- bbox(shp)["x",]
#		ylim <- bbox(shp)["y",]
#		xlim <- c(118.192967, 122.034616)
#		ylim <- c(21.814761, 26.38528)
		xlim <- c(118,123)
		ylim <- c(21.5,25.5)
		color <- COLOR[as.character(sh[[level]]@data[[div_id[level]]][select])]
		g <- plot(shp, col=color, lty=lty, lwd=lwd, xlim = xlim, ylim = ylim)

		if (county_select == "" && town_select == "") {
			par(new=TRUE)
			plot(sh[[1]], bg="#ff000000", lty = 1, lwd = lwd, xlim = xlim, ylim = ylim, main = filename)
		}

		if (type == "pdf" || type == "png") dev.off()
	}

	plot_gg <- function(level, COLOR, type = "none", filename = "Votemap",
						png_width = 0, png_height = 0, debug_id = FALSE,
						county_select = "", town_select = "")
	{
		windowsFonts(chFont=windowsFont("細明體"))
		par(family="chFont")

		shp <- sh[[level]][sh[[level]]@plotOrder,]
		select_c <- rep(TRUE, nrow(shp))
		select_t <- select_c
		if (county_select != "")
			select_c <- grepl(county_select, shp@data$C_Name)
		if (town_select != "")
			select_t <- grepl(town_select, shp@data$T_Name)
		select <- select_c & select_t
		shp <- shp[select,]
		width <- bbox(shp)["x","max"] - bbox(shp)["x","min"]
		height <- bbox(shp)["y","max"] - bbox(shp)["y","min"]
		xlim <- bbox(shp)["x",]
		ylim <- bbox(shp)["y",]
		if (county_select == "" && town_select == "") {
			xlim <- c(118,123)
			ylim <- c(21.5,25.5)
		}

		color <- COLOR[as.character(shp@data[[div_id[level]]])]
		names(color) <- NULL
		shp <- spChFIDs(shp, sprintf("%06d",seq_len(nrow(shp))))
		shp.df <- fortify(shp)

		ggobj <- ggplot() +
			geom_polygon(aes(long, lat, group=group, fill=id), data = shp.df) +
			coord_equal(xlim = xlim, ylim = ylim) +
			scale_fill_manual(values = color) +
			theme(legend.position="none")

		gnames <- data.frame()
		for(i in seq_len(nrow(shp))) gnames <- rbind(gnames,rowMeans(bbox(shp[i,])))
		colnames(gnames) <- c("long", "lat")
		gnames$color <- factor(as.integer(is.na(color)))
		gnames$name <- ifelse(debug_id & is.na(color), as.character(shp@data[[div_id[level]]][is.na(color)]), as.character(shp@data[[div_name[level]]]))

		if (county_select == "" && town_select == "") {
			if (level == 2) {
				width <- width * 2
				height <- height * 2
				text_size <- 0.2
				ggobj <- ggobj + geom_polygon(data = fortify(sh[[1]]), aes(long, lat, group=group), size=0.04, fill="#FF000000", color = "black")
				ggobj <- ggobj + geom_text(aes(long, lat, label=name, color=color), gnames, size=text_size, family="chFont") + scale_color_manual(values = c("white","red"))
			}
		} else {
			if (level == 2 && county_select != "") {
				tselect <- grepl(county_select, sh[[2]]@data$C_Name)
				ggobj <- ggobj + geom_polygon(data = fortify(sh[[2]][tselect,]), aes(long, lat, group=group), size=0.02, fill="#FF000000", color = "black")
				ggobj <- ggobj + geom_text(aes(long, lat, label=name, color=color), gnames, size=1, family="chFont") + scale_color_manual(values = c("white","red"))
			}
			width <- 3 * level
			height <- 3 * level
		}

		type <- tolower(type)
		if (type == "pdf" || type == "png") {
			#filename <- paste0(filename, "_", level, "_", county_select, town_select, village_select, ".", type)
			#filename <- str_replace_all(filename,"\\|","_")
			if (type == "pdf") {
				ggsave(paste0(filename, ".", type), ggobj, device=cairo_pdf, width = width, height = height, dpi = 600)
			} else {
				width <- ifelse(png_width > 0, png_width, width)
				height <- ifelse(png_height > 0, png_height, height)
				ggsave(paste0(filename, ".", type), ggobj, width = width, height = height, dpi = 600)
			}
		}
		ggobj
	}

	exportKML <- function(level, COLOR, filename = "Votemap")
	{
		filename <- paste0("Output/", filename, "_", level, ".kml")
		shp <- spTransform(sh[[level]], CRS("+proj=longlat +datum=WGS84 +no_defs"))
		shp@data$COLOR <- COLOR[as.character(shp@data[[div_id[level]]])]
		writeOGR(shp, filename, "", "KML")
		filename
	}

	list(plotBase=plot_base, plotGG=plot_gg, plotLeaflet=plot_leaflet, exportKML=exportKML, gen_geojson=gen_geojson)
}

map <- generate_map()

