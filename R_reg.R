rm(list=ls())
library(haven)
library(ggplot2)
library(tidyr)
library(RColorBrewer) 
library(dplyr)
library(ggstatsplot)
library(panelView)
library(fect)
library(wooldridge)
library(plm)
library(stargazer)
library(AER)
setwd("D:/学习/UIBE_sr/10.SOE/soe_paper/soe")
dir()
data <- read_dta("Data_rr.dta")
View(data)


# check X 的情况
panelview(lnexp_e ~ sofdi_d , 
          data = data, index = c("iso","year"), 
          xlab = "Year", ylab = "iso", by.timing = TRUE, 
          pre.post = TRUE)


# check Y 的情况
panelview(lnexp_e ~ sofdi_d , 
          data = data, index = c("iso","year"), 
          type = "outcome", main = "EDR Reform and Turnout", 
          legend.labs = c("无工程承包国家","Treated (承包之前)", 
                          "Treated (承包之后)"))

# check画X Y 的关系
panelview(lnexp_e ~ sofdi_d , data = data, 
          index = c("iso","year"), 
          type = "bivariate",
          style = c("c","b"),
          main = "对外承包工程和机械设备出口", ylab = "机械设备出口额")


# 基准回归
# 在stata中，基准回归命令是

# reghdfe lnexp_e sofdi_d ,absorb(country year c.gdp_a#i.year c.gdp_s#i.year) vce(r)
# reghdfe lnexp_e sofdi_a ,absorb(country year c.gdp_a#i.year c.gdp_s#i.year) vce(r)
# reghdfe lnexp_e contract ,absorb(country year c.gdp_a#i.year c.gdp_s#i.year) vce(r)

# 相对应的
plm::is.pbalanced(data$iso,data$year)

modfe <- plm::plm(lnexp_e ~ sofdi_d + gdp_a:as.factor(year)+ gdp_s:as.factor(year) +as.factor(year),index = c("iso", "year"),model ="within", data = data)
summary(modfe)

modfe2 <- plm::plm(lnexp_e ~ sofdi_a1 + gdp_a:as.factor(year)+ gdp_s:as.factor(year) +as.factor(year) ,index = c("iso", "year"),model ="within", data = data)
summary(modfe2)

modfe3 <- plm::plm(lnexp_e ~ contract + gdp_a:as.factor(year)+ gdp_s:as.factor(year) + as.factor(year) ,index = c("iso", "year"),model ="within", data = data)
summary(modfe3)


# 回归结果拟合图
modfe_fit <- fitted(modfe2)

plot(data$lnexp_e , data$sofdi_a1)
lines(data$lnexp_e, modfe_fit, col = "red")
#arrows(data$lnexp_e, modfe_fit, data$lnexp_e, data$sofdi_a1,
#      length = 0.1, col = "blue")


# 回归结果的输出
plm::is.pbalanced(data$iso,data$year)

modfe <- plm::plm(lnexp_e ~ sofdi_d + gdp_a:as.factor(year)+ gdp_s:as.factor(year) +as.factor(year),index = c("iso", "year"),model ="within", data = data)
result1<- summary(modfe)
result1[["coefficients"]]

# 回归命令参考
# 基本回归命令
# reg lwage i.numdep
# lm(lwage ~ as.factor(numdep), data= wage1)    

# reg lwage c.educ#c.exper
# lm(lwage ~ educ:exper, data =wage1) 

# reg lwage c.educ##c.exper   // return full factorial specification
# lm(lwage ~ educ*exper, data =wage1) 

# reg lwage c.exper##i.numdep // return full, interact continuous and categorical
# lm(wage ~ exper*as.factor(numdep),data = wage1)     





# 面板回归命令
# xtset id year
# plm::is.pbalanced(murder$id,murder$year)

# xtreg mrdrte unem, fe  // fixed effects regressi
# modfe <- plm::plm(mrdrte ~ unem,index = c("id", "year"),model ="within", data = murder)






# 工具变量
# ivreg lwage (educ = fatheduc), first
# ivreg lwage (educ = fatheduc)

# modiv <- AER::ivreg(lwage ~ educ |fatheduc, data = mroz) 
# summary(modiv, diagnostics = TRUE)  






# 非线性
# logit inlf nwifeinc educ
# mod_log <- glm(inlf~nwifeinc + educ+ family=binomial(link="logit"),data=mroz)     

# probit inlf nwifeinc educ
# mod_pro <- glm(inlf~nwifeinc + educ+ family=binomial(link="probit"),data=mroz)   

# tobit hours nwifeinc educ, ll(0)
# mod_tob <- AER::tobit(hours ~ nwifeinc + educ, left = 0, data = mroz) 

# margin
# margins::prediction(mod_pro)
# margins::prediction(mod1, at = list(exper = seq(1,51,10)))  


