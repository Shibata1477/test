#パッケージの読み込み
download.packages(dplyr)
download.packages(sf)
library(dplyr)
library(sf)

#ファイルの読み込み
tidai_shp=st_read("E:/user/Shibata/athome_date/shpファイル/大阪府/tidai_osaka.shp")
plateau_shp=st_read("E:/user/Shibata/GIS/shp/大阪府_plateau/大阪市/osaka_osaka_plateau.shp")

#データ形式の調整
tidai_sf=st_as_sf(tidai_shp,coords = c("longitude","lantitude"),crs=4326)
plateau_sf=st_as_sf(plateau_shp,coords = c("longitude","lantitude"),crs=4326)
if (st_crs(tidai_sf) != st_crs(plateau_sf)){
  tidai_sf <- st_transform(tidai_sf,st_crs(plateau_sf))
}
tidai_sf <- st_make_valid(tidai_sf)
plateau_sf <- st_make_valid(plateau_sf)

#データの結合
joined_data <- st_join(tidai_sf,plateau_sf)

#固有値の指定
c0 = 3.711　#ガンマ0
c1 = 0.829　#ガンマ1
c2 = 0.205　#ガンマ2
e = 0.838　#イータ

#データの確認
joined_data$rent <- as.numeric(joined_data$rent)
joined_data$bldg_store <- as.numeric(joined_data$bldg_store)
joined_data$area <- as.numeric(joined_data$area)
joined_data$room_ar <- as.numeric(joined_data$room_ar)

#計算を適用  
joined_data$kappa <- ((joined_data$rent*12/joined_data$room_ar)/1000000) * e * c0 * c1 * ((c0/joined_data$bldg_store)^((1-c1)/c1)) * joined_data$area^((c1+c2-1)/c1)
joined_data <- joined_data %>% filter(!is.na(kappa))　#NAの削除
joined_data <- joined_data %>% filter(!is.infinite(kappa))　#Infの削除
sf_joined_data <- st_as_sf(joined_data,coords = c("longitude","lantitude"),crs=4326)

#街区レベルへの結合
gaiku_shp=st_read("E:/user/Shibata/GIS/shp/境界データ/大阪/r2ka27.shp")
gaiku_sf=st_as_sf(gaiku_shp,coords = c("longitude","lantitude"),crs=4326)

average_kappa <- sf_joined_data %>%
  group_by(add2) %>%
  summarise(mean_kappa = mean(kappa, na.rm = TRUE),   # kappa の平均を計算
            geometry = st_union(geometry))            # geometry 情報を統合

sf_average <- st_as_sf(average_kappa,coords = c("longitude","lantitude"),crs=4326)
average_kappa_pol <- st_join(gaiku_sf,sf_average,left = TRUE)
average_kappa_pol <- average_kappa_pol %>% filter(!is.na(mean_kappa))　#NAの削除
average_kappa_pol <- average_kappa_pol %>% filter(!is.infinite(mean_kappa))　#Infの削除
sf_average_pol <- st_as_sf(average_kappa_pol,coords = c("longitude","lantitude"),crs=4326)

#shpファイルでの出力
st_write(sf_average_pol, "E:/user/Shibata/R/241008/output/大阪市/output.shp", layer_options = "ENCODING=UTF-8")


#地図にプロット
download.packages(ggplot2)
library(ggplot2)

map01 <- ggplot() + 
  geom_sf(data = plateau_sf,aes(geometry = geometry), fill = "lightgray", color = "azure1",alpha = 1)

kappa.map <- map01 + 
  geom_sf(data=aaverage_kappa,aes(color=mean_kappa),lwd = 0.1, alpha=0.2)+
  scale_color_viridis_c(option = "viridis",begin = 0.1,limits=c(50000,250000),oob = scales::squish) +
  theme_minimal()

#削除
rm(list=ls())
gc();gc();
          