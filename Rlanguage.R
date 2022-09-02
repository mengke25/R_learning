## project1 清洗数据



##################
###   路径设置 ###
##################

rm(list=ls())
getwd()
setwd("D:/R_learning/project1/sourcedata")


##################
###   导入数据 ###
##################
filename <- dir()
data1 <- read.csv(filename[1])
data2 <- read.csv(filename[2])
data3 <- read.csv(filename[3])
data4 <- read.csv(filename[4])
data5 <- read.csv(filename[5])
data6 <- read.csv(filename[6])
data7 <- read.csv(filename[7])



##################
###   查看数据 ###
##################
head(data1)
dim(data1)
summary(data1$TradeValue.in.1000.USD)
mean(data1$TradeValue.in.1000.USD, na.rm = TRUE)



##################
### gen replace###
##################

# 生成特定条件的变量
data1$tradeif <- ifelse(data1$TradeValue.in.1000.USD >= mean(data1$TradeValue.in.1000.USD) ,1,0) 
#data1$post <- ifelse(data1$Year >=2010 , "TRUE","FALSE")



##################
###   bysort   ###
##################

# 相当于bysort
data1_tmp2_1 <- aggregate(data1$TradeValue.in.1000.USD, by=list(data1$PartnerISO3,data1$Year),sum)
data1_tmp2_2 <- aggregate(data1$TradeValue.in.1000.USD, by=list(data1$PartnerISO3,data1$Year),mean)
data1_tmp2_3 <- aggregate(data1$TradeValue.in.1000.USD, by=list(data1$PartnerISO3,data1$Year),length)


# grouping(data1$PartnerISO3,data1$Year)


##################
###    merge   ###
##################

# 相当于 merge
data1_tmp2_12 <- merge(data1_tmp2_1,data1_tmp2_2,by.x = c("Group.1","Group.2"), by.y = c("Group.1","Group.2") )
# all.x = TRUE 
# all.y = TRUE
data1_tmp2 <- merge(data1_tmp2_12,data1_tmp2_3,by.x = c("Group.1","Group.2"), by.y = c("Group.1","Group.2") )
#View(data1_tmp2)



##################
###生成数据子集###
##################

# 生成子数据集（横）
data1_tmp <- subset(data1, data1$tradeif == 1)
# View(data1_tmp)

# 提取左侧两列构建新的数据框（纵）
mydata <- data.frame(data1_tmp2$ISO,data1_tmp2$year)
rm(mydata)




##################
###   重命名   ###
##################

## 1.集体重命名
names(data1_tmp2) <-c("ISO","year","total","mean","count")
rm(data1_tmp2_1,data1_tmp2_2,data1_tmp2_3,data1_tmp2_12)

## 2.单个重命名
data1_tmp2 <- rename(data1_tmp2,c(year = "Year"))




##################
###   append   ###
##################

# 纵向合并 相当于append
data1_dup <- rbind(data1_tmp2,data1_tmp2)


##################
### duplicates ###
##################

#相当于duplicates drop
## 方法1: 
data1_dup$dup_id <- duplicated(data1_dup)
data1_dup <- subset(data1_dup, data1_dup$dup_id == TRUE)

## 方法2: 
data1_dup[!duplicated(data1_dup),]



##################
### 提取字符串 ###
##################

data1$flowcode <- substr(data1$TradeFlowName, 1, 2)


##################
###  类型转换  ###
##################

#as.character(data1$Year)
#as.numeric(data1$Year)
mat <- as.matrix(data.frame(data1_tmp2$Year,data1_tmp2$total,data1_tmp2$mean,data1_tmp2$count))



##################
###  tabulate  ###
##################

# 相当于tab
table(data1$ReporterISO3,data1$Year)
im.ex.tab <- table(im = data1$ReporterISO3, ex = data1$PartnerISO3 , value = data1$TradeValue.in.1000.USD ) 






#lm(formula = data1_tmp2$mean ~ data1_tmp2$count)
