library(haven)
library(ggplot2)
library(tidyr)
library(RColorBrewer) 
library(dplyr)
library(ggstatsplot)
library(conflicted)
library(dplyr)
library(readxl)
library(sf)
library(sp)
library(raster)
library(geodata)

china_map <- raster::getData(
  name = "GADM",
  country = "CHN", # 中国的 ISO3 代码
  level = 3, # 国家=0 省=1 市=2 县=3
  type = "sf", # 返回数据类型为 sf 类型
  path = "E:/Data/R_gis" # 保存到本地目录，以便复用
)
china_map


Shanghai_map <- china_map[china_map$NAME_1 == "Shanghai" & china_map$NAME_2 == "Shanghai", ]
plot(Shanghai_map["NAME_3"], main = "", key.pos = NULL)


# 创建数据框 df
df <- data.frame(
  所属区县 = c("宝山区", "崇明县", "奉贤区", "嘉定区", "金山区", "闵行区", "浦东新区", "青浦区", "上海市区", "松江区"),
  N = c(13, 18, 24, 54, 8, 33, 54, 9, 106, 13)
)

# 显示数据框 df
print(df)


# 合并地图数据和区县数据
Shanghai_map_data <- left_join(Shanghai_map, df, by = c("NL_NAME_3" = "所属区县"))
Shanghai_map_data <- Shanghai_map_data %>%
  mutate(N = ifelse(is.na(N), 0, N))

# 绘制热力图
ggplot() +
  geom_sf(data = Shanghai_map_data, aes(fill = N), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "企业数") +
  labs(title = "上海数字内容企业分布（按数量）") +
  theme_minimal()

