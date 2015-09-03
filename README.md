目錄結構
======================================================

* `Output`: 用來放輸出的pdf、png、或html檔
* `Taiwan_Map`: 各行政區的界線，如果要自行下載，底下有網址
	* `Level1_1031225_big5.*`: [縣(市)行政區域界線](http://data.gov.tw/node/7442)
	* `Level2_1031225_big5.*`: [鄉(鎮、市、區)行政區域界線](http://data.gov.tw/node/7441)
	* `Village_NLSC_TWD97_1040624.*`: [村里界圖(WGS84經緯度)](http://data.gov.tw/node/7438)
	* `Shape.RDS`: R讀入shape file後，再重新用自己的格式儲存，讀取速度較快
	* `parse_mapdata.R`: 負責處理地圖及畫地圖的function
* `Taiwan_Region`: 與行政區代碼有關的資料
	* `region_name.xml`: 各行政區的代碼對照
* `Taiwan_voteData`: [中選會選舉資料庫的原始資料](http://data.gov.tw/node/13119)
	* `格式說明.doc`: 中選會提供的說明文件
	* `parse_votedata.R`: 負責將投票率讀出並轉換成顏色
* `Taiwan_VoteMap.R`: 主程式，利用`parse_votedata.R`產生顏色，利用`parse_mapdata.R`畫出地圖


執行步驟
======================================================

1. 按右上角的Fork，複製一份自己的repository，並抓回電腦準備編輯

	例如: `git clone https://github.com/banyh/Taiwan_VoteMap`

2. 安裝R (或加上R Studio) [參考用的安裝教學]
(http://www.dotblogs.com.tw/michael80321/archive/2014/12/15/147656.aspx)

3. 修改`Taiwan_VoteMap.R`

	在第一行加上`setwd("你的原始碼目錄")`，例如`setwd("d:/Project/Map")`

4. 在R Studio中執行`source('D:/Project/Taiwan_VoteMap/Taiwan_VoteMap.R', encoding = 'UTF-8')`
	或是打開`Taiwan_VoteMap.R`後，按上方的Source按鈕

	* 有缺少的library應該會自動安裝
	* 會看到一連串載入library的訊息

5. library載入完後會出現以下訊息

	```
	  1: 2009_2010_mayor
	  2: 2009_2010_councilors
	  3: 2010_village
	  4: 2012_party
	  5: 2012_lagislator
	  6: 2012_president
	  7: 2014_mayor
	  8: 2014_councilors
	  9: 2014_village
	請輸入選擇:
	```
	* village代表村里長選舉
	* mayor代表市長選舉
	* president代表總統選舉
	* party代表立委選舉中的政黨票
	* councilors代表議員選舉
	* lagislator代表區域立委選舉

6. 選擇後會出現以下訊息

	```
	你選擇了 2014_village
	輸入 map$plotGG(1, col_vote$COLOR, col_vote$VOTE) 來看結果
	```
	* 這時已經產生`col_vote`物件，裡面包含顏色資料`COLOR`及得票率資料`VOTE`

7. 執行`map$plotGG`來產生圖形

	* `g <- map$plotGG(1, col_vote$COLOR)`
		產生縣市等級的地圖，著色資料來自col_vote$COLOR
	* `g <- map$plotGG(2, col_vote$COLOR, "pdf")`
		產生鄉鎮等級的地圖，輸出到pdf檔
	* `g <- map$plotGG(3, col_vote$COLOR, "png", "Taiwan_Map")`
		產生村里等級的地圖，輸出到png檔，檔名為Taiwan_Map(可以忽略)
	* `g <- map$plotGG(3, col_vote$COLOR, "pdf", county_select = "新北市|臺北市")`
		產生村里等級的地圖，範圍限定為新北市和臺北市 (注意: 是「臺」不是「台」)
	* `g <- map$plotGG(3, col_vote$COLOR, "pdf", town_select = "板橋區")`
		產生村里等級的地圖，範圍限定為板橋區
	* 詳細的參數說明請參考`parse_mapdata.R`

8. 執行`map$plotLeaflet`來產生互動式地圖(html+json的組合)

	* `map$plotLeaflet(1, COLOR)` 產生縣市等級地圖
	* `map$plotLeaflet(2, COLOR)` 產生鄉鎮等級地圖
	* 在村里等級，因為資料量太大，建議只產生特定縣市，例如
		`map$plotLeaflet(3, COLOR, county_select = "新北市|臺北市")`


程式架構說明
======================================================

1. 資料分為raw data及processed data，直接下載而來的是raw data，因為raw dat使用不便，
	所以會先處理過，然後存在R方便讀取的格式中

	* 地圖的raw data，可執行`prepare_map()`後，產生`Taiwan_Map/Shape.RDS`
	* 選舉的raw data還沒有寫處理函式，所以都是直接讀raw data

2. 程式架構分為地圖端和顏色端

	* `parse_mapdata.R`是地圖端，負責讀取地圖，並處理地區的名稱，但本身不包含顏色資料
	* 地圖由大量多邊形組成，每個多邊形可以填一種顏色
	* 每個多邊形都有2014年主計處版的區域代碼，會由此代碼去查出多邊形應該填的顏色
	* `parse_votedata.R`是顏色端，負責產生顏色及得票率

3. 2014年的區域代碼在`Taiwan_Map/region_name.xml`中

	* 區域代碼是地圖著色的依據，內政部每一年的區域代碼都有些不同
	* 但因為地圖中的是2014年的代碼，所以需要將過去的舊代碼轉換，以2014年的為準
	* [台灣區域代碼的演變](http://www.dgbas.gov.tw/ct.asp?xItem=951&ctNode=5485)

4. 正確產生的顏色資料，必須包含所有區域代碼及顏色的對應，例如:

	* `COLOR["63"] == "#6D6D00"` 這是台北市的顏色
	* `COLOR["6300100"] == "#757500"` 這是松山區的顏色
	* `COLOR["6300100-002"] == "#787800"` 這是松山區莊敬里的顏色
	* 在`map$plotGG`畫圖時，會找每一塊多邊形對應的代碼，再到`COLOR`中查出顏色


我想用自己的顏色，取代得票率的顏色，該怎麼作?
======================================================

1. 顏色是named character vector，如果你已經知道區域代碼及顏色，就直接產生vector
	空缺的顏色可以用NA

	name        | value
	------------|------------------
	63          | "#808000"
	6300100     | "#804000"
	6300100-002 | "#0080F0"

2. 如果你知道行政區名稱，但沒有代碼，可以利用`region_id`來查詢

	* `region_id[c("臺北市","松山區")]`會得到`"63" "6300100"`
	* 要注意的是，村里等級有很多同名資料，這部分的處理方法請自己trace一下source code
		如果嫌麻煩，就只用縣市、鄉鎮市等級就好

3. 畫地圖(Lv1為縣市，Lv2為鄉鎮市，Lv3為村里)

	* `map$plotGG(level = 1, COLOR = myColor)`
	* `map$plotGG(level = 2, COLOR = myColor)`


如何節省GeoJSON的空間(轉換成TopoJSON)
======================================================

1. `map$plotLeaflet`會產生兩個檔案，一個是html，一個是geojson。其中geojson占用空間很大，LV1地圖為10MB，LV2地圖為35MB，LV3地圖為80MB，造成使用上不便

2. 為了節省空間，建議採用topojson的格式，它會將原本的線條變成類似格線的形式，行政區的邊界放大看，會看到鋸齒狀，但是可以將GeoJSON的空間節省非常多，例如LV2地圖只需要1.1MB。

3. 將GeoJSON轉成TopoJSON(使用node.js)

	1. 打開Node.js command prompt，輸入`npm install topojson`

	2. 安裝完後，輸入`topojson -p -o Votemap_2.topojson -- Votemap_2.geojson`，表示將`Votemap_2.geojson`轉換成`Votemap_2.topojson`

4. 修改html檔，以讀取TopoJSON

	1. 在`<head>`中增加`<script src="http://d3js.org/topojson.v1.min.js"></script>`

	2. 在`<head>`中找到`Votemap_2_.geojson`，改成`Votemap_2_.topojson`

	3. 在`<script>`中，找到`// data layers`，在前面加入下列程式碼

		```
		L.TopoJSON = L.GeoJSON.extend({
			addData: function(jsonData) {
				if (jsonData.type === "Topology") {
					for (key in jsonData.objects) {
						geojson = topojson.feature(jsonData, jsonData.objects[key]);
						L.GeoJSON.prototype.addData.call(this, geojson);
					}
				}
				else {
					L.GeoJSON.prototype.addData.call(this, jsonData);
				}
			}
		});
		// Copyright (c) 2013 Ryan Clark
		```

	4. 在`function addDataToMap`的第一行，將`L.geoJson`改成`new L.TopoJSON`
