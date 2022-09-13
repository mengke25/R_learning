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

# temp = list.files(pattern="*.csv")
# for (i in 1:length(temp)) {
#  filename <- substr(temp[i], 1, nchar(temp[i])-4);
#   assign(filename, read.csv(temp[i], header = T));
#   save(list = filename, file = paste(filename, ".Rdata", sep = ""))
# }



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
mode(data1$TradeValue.in.1000.USD)
quantile(data1$TradeValue.in.1000.USD, probs = seq(0,1,  by = 0.1))
sd(data1$TradeValue.in.1000.USD)
var(data1$TradeValue.in.1000.USD)
sum(is.na(data1$TradeValue.in.1000.USD))  #NA的个数


##################
### gen replace###
##################

# 生成特定条件的变量
#data1$tradeif <- ifelse(data1$TradeValue.in.1000.USD >= mean(data1$TradeValue.in.1000.USD) ,1,0) 
#data1$post <- ifelse(data1$Year >=2010 , "TRUE","FALSE")

#data1 <- sapply(data1$TradeValue.in.1000.USD,mean)



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
###   reshape  ###
##################
#melt(data, id.vars, measure.vars,
#     variable.name = "variable", ..., na.rm = FALSE, value.name = "value",
#     factorsAsStrings = TRUE)
# 将宽数据转换为长数据

#dcast(data, formula, fun.aggregate = NULL, ..., margins = NULL,
#      subset = NULL, fill = NULL, drop = TRUE,
#      value.var = guess_value(data))
# 将长数据转换为宽数据





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

#data1$flowcode <- substr(data1$TradeFlowName, 1, 2)


##################
###  类型转换  ###
##################

#as.character(data1$Year)
#as.numeric(data1$Year)



##################
###    矩阵    ###
##################
# mat <- as.matrix(data.frame(data1_tmp2$Year,data1_tmp2$total,data1_tmp2$mean,data1_tmp2$count))
# nrow(as.matrix(data.frame(data1_tmp2$Year,data1_tmp2$total,data1_tmp2$mean,data1_tmp2$count)))
# ncol(as.matrix(data.frame(data1_tmp2$Year,data1_tmp2$total,data1_tmp2$mean,data1_tmp2$count)))

#colSums(as.matrix(data.frame(data1_tmp2$Year,data1_tmp2$total,data1_tmp2$mean,data1_tmp2$count)))
#rowSums(as.matrix(data.frame(data1_tmp2$Year,data1_tmp2$total,data1_tmp2$mean,data1_tmp2$count)))

##################
###  tabulate  ###
##################

# 相当于tab
table(data1$ReporterISO3,data1$Year)
im.ex.tab <- table(im = data1$ReporterISO3, ex = data1$PartnerISO3 , value = data1$TradeValue.in.1000.USD ) 


#lm(formula = data1_tmp2$mean ~ data1_tmp2$count)




# Case Learning Figs

##1.分别提取进口数据和出口数据
Data <- rbind(data1,data2,data3,data4,data5,data6,data7)
dataimport <- subset(Data,Data$TradeFlowName=="Import")
dataexport <- subset(Data,Data$TradeFlowName=="Export")

##2.加总到中国-时间层面(1996~2019)
cn_year_import <- aggregate(dataimport$TradeValue.in.1000.USD,by=list(dataimport$Year),sum)
names(cn_year_import) <- c("year","v_import") 
cn_year_import$lnImp <- log(cn_year_import$v_import)

cn_year_export <- aggregate(dataexport$TradeValue.in.1000.USD,by=list(dataexport$Year),sum)
names(cn_year_export) <- c("year","v_export") 
cn_year_export$lnExp <- log(cn_year_export$v_export)

cn_year <- merge(cn_year_import,cn_year_export,by.x=c("year"),by.y="year")
View(cn_year)

cn_year$WTO <-  ifelse(cn_year$year >= 2001, 1,0) 


# 柱状图 
barplot(cn_year$lnImp, names.arg = cn_year$year, xlab = "year" , main = "进口额")

cn_year.ptable <- prop.table(table(cn_year$WTO ,exclude = NULL))
barplot(cn_year.ptable)

# 直方图
hist(cn_year$v_import,freq= FALSE, 
     main = "进口密度" , 
     xlab = "进口额" ,
     breaks = seq(0,7000000000,500000000))
abline(v = median(cn_year$v_import))
abline(h = 0.3)
abline(a = -0.1, b =0.0000000004)
tmp.dens <- density(cn_year$v_import)
lines(tmp.dens, lwd=2, col='blue')

# 箱状图
boxplot(cn_year$v_import,cn_year$v_export,names = c("进口金额","出口金额"))
boxplot(cn_year$lnImp,cn_year$lnExp,names = c("进口金额的对数","出口金额的对数"))

library(reshape2)
cn_year_long <- melt(cn_year, 
                     id.var = "year", 
                     measure.vars = c("v_import","lnImp","v_export","lnExp"),
                     na.rm = T,
                     variable.name = "Vars", value.name = "Vals")

cn_year_wide <- dcast(cn_year_long, year ~ cn_year_long$Vars)




cn_year_long1 <- cn_year_long[which(cn_year_long$Vars=="v_import"|cn_year_long$Vars=="v_import"),]
cn_year_long2 <- cn_year_long[which(cn_year_long$Vars=="lnImp"|cn_year_long$Vars=="lnExp"),]

boxplot(cn_year_long1$Vals ~ cn_year_long1$Vars)

cn_year_lnImp <- subset(cn_year_long,cn_year_long$Vars=="lnImp")
cn_year_lnExp <- subset(cn_year_long,cn_year_long$Vars=="lnExp")
cn_year_v_import <- subset(cn_year_long,cn_year_long$Vars=="v_import")
cn_year_v_export <- subset(cn_year_long,cn_year_long$Vars=="v_export")

cn_year_ln <- rbind(cn_year_lnImp,cn_year_lnExp)
cn_year_v <- rbind(cn_year_v_import,cn_year_v_export)
boxplot(cn_year_ln$Vals ~ cn_year_ln$Vars)
boxplot(cn_year_v$Vals ~ cn_year_v$Vars)




# tapply(cn_year$v_import, cn_year$WTO,mean)



# 散点图
plot (cn_year$v_export,cn_year$v_import)
abline(a=0,b=1)


cn_year2 <- cn_year
cn_year2$v_import <- cn_year2$v_import*0.7 
cn_year2$v_export <- cn_year2$v_export*0.7 
cn_year2$lnImp <- cn_year2$lnImp*0.7 
cn_year2$lnExp <- cn_year2$lnExp*0.7 

plot(cn_year$v_export,cn_year$v_import,col = "blue")
points(cn_year2$v_export,cn_year2$v_import,col = "red", pch = 17)


# 分位数-分位数(QQ图)
qqplot(cn_year$v_import,cn_year$v_export)
abline(0,1)


# 时间趋势图
# data_time <- as.data.frame(cn_year)

library(ggplot2)
ggplot(cn_year, aes(x=year, y=v_import)) +
  geom_line() + 
  xlab("")


