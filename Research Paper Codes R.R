####

getwd()
setwd("D:/University of Toronto/Courses/2021/Fall 2021/POL419 Quantitative Methods and Data Analysis/Assignments/Empirical Research Paper/Upsala Dataset/Afghanistan")
getwd()

####Read Data
my.data6 <- read.csv("10_11_21_0738pm_wep.csv")
View(my.data6)

####Install the necessar libraries
library(readxl) #Import Excel Files into R
library(curl) #Connection Interface
library(data.table) #Data Management
library(tidyr)# Data Management
library(dplyr)#Data Management
library(ggplot2)#plot
library(scales)#Graphical Tools
library(stringr)#String Manipulation
library(haven)
library(stacks)
library(plyr)
library(tidyverse)
library(dplyr)

###for future log of variables and making new variables,
###Let's do some transformations

####Let's create a new variable of aid dependency
my.data6 <- my.data6 %>%
  mutate(netoda_WDI = replace(netoda_WDI, netoda_WDI == 0, 0.01))
my.data6 <- my.data6 %>%
  mutate(gdp_WDI = replace(gdp_WDI, gdp_WDI == 0, 0.01))

my.data6 <- my.data6 %>% mutate(aid_gdp_WDI = netoda_WDI/gdp_WDI)

#Now make the values of 0 to 0.1 to be able to make log and not have NaN values
my.data6 <- my.data6 %>%
  mutate(aid_gdp_WDI = replace(aid_gdp_WDI, aid_gdp_WDI == 0, 0.000001))

my.data6 <- my.data6 %>%
  mutate(aid_gdp_WDI = replace(aid_gdp_WDI, aid_gdp_WDI < 0, 0.000001))
#Let's create the logged variables of GDP per capita and population
my.data6 <- my.data6 %>% mutate(log_pop = log(pop_WDI))


my.data6 <- my.data6 %>% mutate(log_gdppc = log(gdppc_WDI))

####Create the lagged dependent variable

library(dplyr)
my.data6 <- my.data6 %>%                            # Add lagged column
  group_by(country) %>%
  dplyr::mutate(lag1_gdppc = dplyr::lag(log_gdppc, n = 1, default = NA)) %>% 
  as.data.frame()


###let's create a new variable of population growth
summary(my.data6$pop_WDI)
hist(my.data6$pop_WDI)
hist(log(my.data6$pop_WDI))



hist(my.data6$log_pop)
summary(my.data6$life_exp_WDI)
hist(my.data6$life_exp_WDI)

summary(my.data6$lsc_2024_BL)
hist(my.data6$lsc_2024_BL)
hist(log(my.data6$lsc_2024_BL))

summary(my.data6$lit_WDI)
hist(my.data6$lit_WDI)
hist(log(my.data6$lit_WDI))
my.data6 <- my.data6 %>% mutate(log_lit = log(lit_WDI))

my.data6 <- my.data6 %>%
  mutate(polity_P4 = replace(polity_P4, polity_P4 == -89, NA))
my.data6 <- my.data6 %>%
  mutate(polity_P4 = replace(polity_P4, polity_P4 == -88, NA))
table(my.data6$polity_P4, exclude=NULL) %>% addmargins()
my.data6 <- my.data6 %>%
  mutate(polity_P4 = replace(polity_P4, polity_P4 == -77, NA))
my.data6 <- my.data6 %>%
  mutate(polity_P4 = replace(polity_P4, polity_P4 == -66, NA))
table(my.data6$polity_P4, exclude=NULL) %>% addmargins()
summary(my.data6$polity_P4)

table(my.data6$democ_P4, exclude=NULL) %>% addmargins()
my.data6 <- my.data6 %>%
  mutate(democ_P4 = replace(democ_P4, democ_P4 == -88, NA))
table(my.data6$democ_P4, exclude=NULL) %>% addmargins()
my.data6 <- my.data6 %>%
  mutate(democ_P4 = replace(democ_P4, democ_P4 == -77, NA))
my.data6 <- my.data6 %>%
  mutate(democ_P4 = replace(democ_P4, democ_P4 == -66, NA))
table(my.data6$democ_P4, exclude=NULL) %>% addmargins()


summary(my.data6$aid_gdp_WDI)
table(my.data6$aid_gdp_WDI, exclude=NULL) %>% addmargins()

                 ####Let's do an OLS regression###

### subset the data
my.data2018 <- subset(my.data6, year == "2018")
my.data2015 <- subset(my.data6, year == "2015")
view(my.data2015)
view(my.data2018)
summary(my.data2018$gdppc_WDI)
###Let's create our own dataset
summary(my.data2015$log_laggdppc1)
my.data2015 <- my.data2015 %>%
  select(country, year, gdppc_WDI, pop_WDI, log_gdppc, propright_EF,log_pop,polity_P4,v2x_cspart_VDEM,sfi_SFI,democ_P4,lag1_gdppc, aid_gdp_WDI)
view(my.data2015)
summary(my.data2015$aid_gdp_WDI)
# estimate simple regression models using 2015 and 2018 data
model_1 <- lm( log_gdppc ~ propright_EF, data = my.data2015)

summary(model_1)


###plot the Regression Line of log(lag1_gdppc) on propright_EF (Property Rights)
dev.new()
plot(x= my.data2015$propright_EF, y= my.data2015$log_gdppc, xlab = "Protection of Property Rights (0-10)",
     ylab = "GDP per Capita Growth",
     main = "Figure 5: Causal Relationship Between Economic Institutions
     and Income Per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_1)

summary(my.data2015$log_gdppc)
hist(my.data2015$log_pop)
# estimate simple regression models using 2015 and 2018 data
model_2 <- lm(log_gdppc ~ propright_EF + lag1_gdppc, data = my.data2015)

summary(model_2)

###plot the Regression Line of log(lag1_gdppc) on propright_EF (Property Rights)
dev.new()
plot(x= my.data2015$democ_P4, y= my.data2015$log_gdppc, xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Property Rights Protection on Income Per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_2)


###plot the Regression Line of log(lag1_gdppc) on log_pop (Population Growth)
dev.new()
plot(x= my.data2015$log_pop, y= log(my.data2015$lag1_gdppc), xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Property Rights Protection on Income Per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_2)







# estimate simple regression models using 2015 and 2018 data
model_3 <- lm(log_gdppc ~ propright_EF + lag1_gdppc + polity_P4 + I(polity_P4^2), data = my.data2015)

summary(model_3)

summary(my.data2015$polity_P4)
hist(my.data2015$polity_P4)
hist(log(my.data2015$polity_P4))
###plot the Regression Line of log(lag1_gdppc) on v2x_polyarchy_VDEM ( Electoral Democracy Index)
dev.new()
plot(x= my.data2015$polity_P4, y= my.data2015$log_laggdppc3, xlab = "Electoral Democracy Index",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Electoral Democracy on Income Per Capita Growth",
     col = "steelblue", pch= 20)




# estimate simple regression models using 2015 and 2018 data
model_4 <- lm(log_gdppc ~ propright_EF + lag1_gdppc + polity_P4 +I(polity_P4^2) + log_pop, data = my.data2015)

summary(model_4)

###plot the Regression Line of log(lag1_gdppc) on v2xcs_ccsi_VDEM ( Core civil society index [VDEM]) and 
dev.new()
plot(x= my.data2015$log_pop, y= log(my.data2015$log_gdppc), xlab = "Core civil society index [VDEM]",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Civil Society on Income Per Capita Growth",
     col = "steelblue", pch= 20)
hist(my.data2015$sfi_SFI)
hist(log(my.data2015$sfi_SFI))

summary(my.data2015$aid_gdp_WDI)
# estimate simple regression models using 2015 and 2018 data
model_5 <- lm(log_gdppc ~ propright_EF + lag1_gdppc + polity_P4 +I(polity_P4^2) + log_pop + sfi_SFI, data = my.data2015)

summary(model_5)

###plot the Regression Line of log(lag1_gdppc) on v2xcs_ccsi_VDEM ( Core civil society index [VDEM]) and 
dev.new()
plot(x= my.data2015$sfi_SFI, y= log(my.data2015$log_laggdppc3), xlab = "Core civil society index [VDEM]",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Civil Society on Income Per Capita Growth",
     col = "steelblue", pch= 20)
hist(my.data2015$sfi_SFI)
hist(log(my.data2015$sfi_SFI))


# estimate simple regression models using 2015 and 2018 data
model_6 <- lm(log_gdppc ~ propright_EF + lag1_gdppc + polity_P4 +I(polity_P4^2) + log_pop + sfi_SFI + log(aid_gdp_WDI), data = my.data2015)

summary(model_6)

###plot the Regression Line of log(lag1_gdppc) on v2xcs_ccsi_VDEM ( Core civil society index [VDEM]) and 
dev.new()
plot(x= my.data2015$sfi_SFI, y= log(my.data2015$log_laggdppc3), xlab = "Core civil society index [VDEM]",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Civil Society on Income Per Capita Growth",
     col = "steelblue", pch= 20)
hist(my.data2015$sfi_SFI)
hist(log(my.data2015$sfi_SFI))












                ####Let's do a plm regression###
library(plm)
model_7 <- plm (log_gdppc ~ propright_EF, 
                data = my.data6,
                index = c("country", "year"), 
                model = "within",
                effect = "twoways")
                


summary(model_7)

hist(my.data6$lag1_growth)
###plot the Regression Line of GDP per capita growth on property rights Panel Data (1960-2020)

dev.new()
plot(x= my.data6$propright_EF, y= log(my.data6$log_gdppc), xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Property Rights Protection on Income per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_5)
summary(my.data6$democ_P4)


#let's do the third plm model with the inclusion of democracy
model_8 <- plm (log_gdppc ~ propright_EF + polity_P4 + I(polity_P4^2), 
                data = my.data6,
                index = c("country", "year"), 
                model = "within",
                effect = "twoways")
                


summary(model_8)


###plot the Regression Line of GDP per capita growth on property rights
#and v2x_polyarchy_VDEM (democracy index) with Panel Data (1960-2020)

dev.new()
plot(x= my.data6$log_pop, y= my.data6$log_laggdppc3, xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Democracy on Income per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_7)







#let's do the third plm model with the inclusion of democracy
model_9 <- plm (log_gdppc ~ propright_EF + polity_P4 + I(polity_P4^2) + log_pop, 
                data = my.data6,
                index = c("country", "year"), 
                model = "within",
                effect = "twoways")
               


summary(model_9)


###plot the Regression Line of GDP per capita growth on property rights
#and v2x_polyarchy_VDEM (democracy index) with Panel Data (1960-2020)

dev.new()
plot(x= my.data6$v2x_polyarchy_VDEM, y= my.data6$log_laggdppc3, xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Democracy on Income per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_8)



#let's do the third plm model with the inclusion of democracy
model_10 <- plm (log_gdppc ~ propright_EF + polity_P4 + I(polity_P4^2) + log_pop + sfi_SFI, 
                data = my.data6,
                index = c("country", "year"), 
                model = "within",
                effect = "twoways")


summary(model_10)


###plot the Regression Line of GDP per capita growth on property rights
#and v2x_polyarchy_VDEM (democracy index) with Panel Data (1960-2020)

dev.new()
plot(x= my.data6$v2x_polyarchy_VDEM, y= my.data6$log_laggdppc3, xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Democracy on Income per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_8)



#let's do the third plm model with the inclusion of democracy
model_11 <- plm (log_gdppc ~ propright_EF + polity_P4 + I(polity_P4^2) + log_pop + sfi_SFI + log(aid_gdp_WDI), 
                 data = my.data6,
                 index = c("country", "year"), 
                 model = "within",
                 effect = "twoways")



summary(model_11)


###plot the Regression Line of GDP per capita growth on property rights
#and v2x_polyarchy_VDEM (democracy index) with Panel Data (1960-2020)

dev.new()
plot(x= my.data6$v2x_polyarchy_VDEM, y= my.data6$log_laggdppc3, xlab = "Property Rights Protection",
     ylab = "GDP per Capita Growth",
     main = "The Effect of Democracy on Income per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_8)




# load the stargazer package
library(stargazer)
# gather robust standard errors in a list
models <- list(sqrt(diag(vcovHC(model_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_2, type = "HC1"))),
               sqrt(diag(vcovHC(model_3, type = "HC1"))),
               sqrt(diag(vcovHC(model_4, type = "HC1"))),
               sqrt(diag(vcovHC(model_5, type = "HC1"))),
               sqrt(diag(vcovHC(model_6, type = "HC1"))),
               sqrt(diag(vcovHC(model_7, type = "HC1"))),
               sqrt(diag(vcovHC(model_8, type = "HC1"))))
# generate a Latex table using stargazer
stargazer(model_1, model_2, model_3, model_4,model_5,model_6, model_7, model_8,
          se = models,
          digits = 5,
          column.labels = c("(1)", "(2)", "(3)", "(4)","(5)","(6)","(7)", "(8)"))

#make the table
models <- list(model_1, model_2, model_3, model_4,model_5,model_6, model_7, model_8,model_9)
stargazer(models, type = "html", keep.stat = c("n", "rsq"), out = "Table.15.html")




library(stargazer)
# gather robust standard errors in a list
models <- list(sqrt(diag(vcovHC(model_1, type = "HC1"))),
               sqrt(diag(vcovHC(model_2, type = "HC1"))),
               sqrt(diag(vcovHC(model_3, type = "HC1"))),
               sqrt(diag(vcovHC(model_4, type = "HC1"))),
               sqrt(diag(vcovHC(model_5, type = "HC1"))))
               
# generate a Latex table using stargazer
stargazer(model_1, model_2, model_3, model_4,model_5,
          se = models,
          digits = 5,
          column.labels = c("(1)", "(2)", "(3)", "(4)","(5)"))

#make the table
models <- list(model_1, model_2, model_3, model_4,model_5,model_6,model_7,model_8)
dev.new()
stargazer(model_1, model_2, model_3, model_4,model_5, type = "text",
          dep.var.labels = c("GDP per Capita Growth"), title = "Table 2: Regression Results",
          covariate.labels = c("Lagged GDP per Capita Growth", "Protection of Property Rights", "Logged Population",
                               "Institutionalized Democracy Score (0-10)","Civil society participation index ","State Fragility"),
          out = "Table2.txt")



summary(my.data2015$propright_EF)
stargazer(my.data2015[c("log_gdppc","lag1_gdppc", "propright_EF","democ_P4","log_pop","sfi_SFI", "aid_gdp_WDI")],
          type = "html", title = "Table 1: Summary Statistics of Cross-Section Data (2015)", out = "Table1.html",digits = 2, covariate.labels = 
            c("GDP per Capita Growth","GDP per Capita Growth (T-3) Lagged", "Protection of Property Rights","Democracy Indicator (0-10)",
              "Logged Population","State Fragility", "Aid Dependency"))


stargazer(my.data6[c("log_gdppc", "propright_EF","democ_P4","log_pop","sfi_SFI", "aid_gdp_WDI")],
          type = "html", title = "Table 2: Summary Statistics of Panel Data (1970-2015)", out = "Table2.html",digits = 2, covariate.labels = 
            c("GDP per Capita Growth)", "Protection of Property Rights","Democracy Indicator (0-10)",
              "Logged Population","State Fragility", "Aid Dependency"))




my.data2015 <- my.data2015 %>%
  select(lag1_gdppc,propright_EF,log_pop,polity_P4,v2x_cspart_VDEM,sfi_SFI)

summary(my.data6$democ_P4)

dev.new()
par(mfrow=c(1,2))
plot(x= my.data2015$propright_EF, y= my.data2015$log_gdppc, xlab = "Protection of Property Rights",
     ylab = "GDP per Capita Growth",
     main = "Figure 1(a):Relationship Between Economic
     Institutions and Income Per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_1)
plot(x= my.data6$polity_P4, y= my.data6$log_gdppc, xlab = "Democracy Indicator (0 to 10)",
     ylab = "GDP per Capita Growth",
     main = "Figure 1(b): Relationship Between Political
     Institutions and Income per Capita Growth",
     col = "steelblue", pch= 20)
abline(model_11)
order_id <- order(my.data2015$polity_P4)
lines(x = my.data2015$democ_P4[order_id],
      y = fitted(model_12)[order_id])
# draw a scatterplot of the observations for income and test score
quadratic_model <- lm(log_gdppc ~ polity_P4 + I(polity_P4^2), data = my.data6)
dev.new()
plot(x= my.data6$polity_P4, y= my.data6$democ_P4, xlab = "Democracy Indicator (0-10)",
     ylab = "GDP per Capita Growth",
     main = "Figure 1(b): Relationship Between Political
     Institutions and Income per Capita Growth",
     col = "steelblue", pch= 20)
# add a linear function to the plot
abline(linear_model, col = "black", lwd = 2)
# add quatratic function to the plot
order_id <- order(my.data6$polity_P4)
lines(x = my.data6$polity_P4[order_id],
      y = fitted(quadratic_model)[order_id],
      col = "red",
      pch= 20,lwd = 2)      

lines(my.data2015$democ_P4,predict(model_12),lty=2,col="red",lwd=3)
#create sequence of hour values
democracy_values <- seq(0, 10)
data(my.data2015)
#create list of predicted happines levels using quadratic model
democracy_Predict <- predict(model_12,list(democ_P4=democracy_values, I(democ_P4^2)=democracy_values^2), data(my.data2015))

#create scatterplot of original data values
plot(data$hours, data$happiness, pch=16)
#add predicted lines based on quadratic regression model
lines(hourValues, happinessPredict, col='blue')





plot(my.data2015$democ_P4, my.data2015$lag1_gdppc)
summary(my.data2015$democ_P4)
model_11 <- lm(lag1_gdppc ~ democ_P4, data = my.data2015)
model_12 <- lm(lag1_gdppc ~ democ_P4 + I(democ_P4^2),data = my.data2015,
               )

summary(model_11)
g <- ggplot(my.data2015, aes(x=propright_EF, y=lag1_gdppc)) + geom_point() + geom_smooth(method="lm") 
plot(g)
g2 <- ggplot(my.data2015, aes(x=democ_P4, y=lag1_gdppc)) + geom_point() + geom_smooth(method="lm") 
plot(g2)


dev.new()
ggplot(data=my.data2015, xName='democ_P4',yName='log_gdppc',
                    mainTitle="Relationship Between Political Institutions and Economic Growth",
                    xtitle="Democracy Indicator", ytitle="GDP per Capita Growth")
ggplot(my.data2015, aes(x=democ_P4, y=log_gdppc)) +
  geom_point()+
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))
  


ggplot(my.data2015, aes(democ_P4, lag1_gdppc) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, se = TRUE))

stargazer(model_1, model_2, model_3, model_4,model_5,model_6,model_7,model_8,model_9,model_10, model_11,model_12, type = "html",
          dep.var.labels = c("GDP per Capita Growth"), title = "Table 2: Regression Results",
          covariate.labels = c("Protection of Property Rights", "GDP per Capita (T-1) logged",
                               "Democracy Indicator (0-10)",
                               "Squared Democracy Indicator (0-10)",
                               "Logged Population", 
                               "State Fragility", "Aid Dependency"),
          out = "Table4.html")
                               
          
stargazer(model_1, model_2, model_3, model_4,model_5,model_6,model_7,model_8,model_9,model_10,model_11, type = "html",
          dep.var.labels = c("GDP per Capita Growth"), title = "Table 3: Regression Results",
          covariate.labels = c("Protection of Property Rights", "GDP per Capita (T-1) logged",
                               "Polity Score (-10 to 10)",
                               "Squared Polity Score (-10 to +10)",
                               "Logged Population", 
                               "State Fragility",
                               "Aid Dependency"),
          out = "Table14.html")

















###Presentation/Revise

dev.new()

hist(my.data2015$gdppc_WDI, breaks = 30, xlim = c(0,120000), ylim = c(0,100),
     main = "Figure 2(a). GDP per Capita Across Countries",
     ylab = "Number of countries",
     xlab = "GDP per Capita in US Dollars",
     col="darkorange")
dev.copy(jpeg,'Scatterplots/GDP per Capita Across Countries.jpg')
dev.off()
dev.new()
hist(my.data2015$log_gdppc, breaks = 15,
     main = "Figure 2(b). GDP per capita Growth (Natural Log of GDP
     per capita Across Countries)",
     ylab = "Number of countries",
     xlab = "Natural Log of GDP per Capita",
     col="darkorange")
dev.copy(jpeg,'Scatterplots/GDP per capita Growth (Natural Log of GDP per capita Across Countries).jpg')
dev.off()

###Independent Variable
dev.new()
hist(my.data2015$propright_EF,
     main = "Figure 3. Protection of Property Rights Across Countries",
     ylab = "Number of countries",
     xlab = "Protection of Property Rights (0-10)",
     col="darkorange")
dev.copy(jpeg,'Scatterplots/Protection of Property Rights Across Countries.jpg')
dev.off()
summary(my.data2015$log_gdppc)

###Control Variable (Population)
dev.new()
hist(my.data2015$log_gdppc,
     main = "Population Growth Across Countries",
     ylab = "Number of countries",
     xlab = "Log Population",
     col="darkorange")
dev.copy(jpeg,'Scatterplots/Population Growth Across Countries.jpg')
dev.off()
summary(my.data2015$propright_EF)
###Control Variable (Population)
dev.new()
hist(my.data2015$log_gdppc,
     main = "Population Growth Across Countries",
     ylab = "Number of countries",
     xlab = "Log Population",
     col="darkorange")
dev.copy(jpeg,'Scatterplots/Population Growth Across Countries.jpg')
dev.off()

stargazer(model_1, type = "html",
          dep.var.labels = c("GDP per Capita Growth"), title = "Table 5: Regression Results of Bivariate Relationship",
          covariate.labels = c("Protection of Property Rights"),
          out = "Table5.html")

summary(my.data2015$gdppc_WDI)











