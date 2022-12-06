library(MASS)
library(ISLR)
library(corrplot)

cleandata <- na.omit(Variables[1:1762,])
#use only from Janaury 2021 - July 2022
cleandatareform <- cleandata[763:1166,]

library(dplyr)
df = data.frame(cleandatareform$Date, cleandatareform$`Carbon price`)
rate <- df %>% 
  mutate(rate = 100 * (cleandatareform$`Carbon price` - lag(cleandatareform$`Carbon price`))/lag(cleandatareform$`Carbon price`))
cleandatareform$perchange <- rate[,3]
pricechange = cleandatareform$perchange
plot(cleandatareform$Date, pricechange, type = "l", main = "Carbon price fluctuation",
     xlab = "Date", ylab = "% change")
abline(h = 0, col = "red")

#add classification variable
cleandatareform <- cleandatareform  %>%
  mutate(buysell = case_when(perchange < -2 ~ 'decrease',
                             (-2 < perchange & perchange < 2) ~ 'neutral',
                             perchange > 2 ~ 'increase'))
cleandatareform$buysell <- as.factor(cleandatareform$buysell)
#energyfund <- data.frame(cleandatareform[, c(2, 4:16,37,38)])

#try understand variation
cleandatareform <- cleandatareform  %>%
  mutate(variation = case_when(perchange < 0 ~ 'decrease',
                               perchange >= 0 ~'increase'))
summary(cleandatareform)
library(pastecs)
stat.desc(cleandatareform)
library(psych)
describe(cleandatareform[, 1:37])

names(cleandatareform)[1] <- "date"
names(cleandatareform)[2] <- "carbonprice"
names(cleandatareform)[3] <- "volets"
names(cleandatareform)[4] <- "coalp"
names(cleandatareform)[5] <- "oilp"
names(cleandatareform)[6] <- "ngasp"
names(cleandatareform)[7] <- "elecp"
names(cleandatareform)[8] <- "renewp"
names(cleandatareform)[9] <- "gasoilp"
names(cleandatareform)[10] <- "ffp"
names(cleandatareform)[11] <- "powgenp"
names(cleandatareform)[12] <- "elecgenp"
names(cleandatareform)[13] <- "allenp"
names(cleandatareform)[14] <- "subffelec"
names(cleandatareform)[15] <- "subcoalgas"
names(cleandatareform)[16] <- "subffren"
names(cleandatareform)[17] <- "tmean"
names(cleandatareform)[18] <- "tmax"
names(cleandatareform)[19] <- "tmin"
names(cleandatareform)[20] <- "wind"
names(cleandatareform)[21] <- "preci"
names(cleandatareform)[22] <- "stock"
names(cleandatareform)[23] <- "bond"
names(cleandatareform)[24] <- "com"
names(cleandatareform)[25] <- "indus"
names(cleandatareform)[26] <- "shell"
names(cleandatareform)[27] <- "tte"
names(cleandatareform)[28] <- "eni"
names(cleandatareform)[29] <- "bp"
names(cleandatareform)[30] <- "equinor"
names(cleandatareform)[31] <- "neste"
names(cleandatareform)[32] <- "repsol"
names(cleandatareform)[33] <- "aker"
names(cleandatareform)[34] <- "tenaris"
names(cleandatareform)[35] <- "omv"
names(cleandatareform)[36] <- "clean"
names(cleandatareform)[37] <- "perchange"
names(cleandatareform)[38] <- "buysell"
names(cleandatareform)[39] <- "variation"


#disribution
library(ggpubr)
#install.packages("nortest")
library(nortest)
ggdensity(pricechange, main = 'Carbon price change density')
ggqqplot(pricechange, main = 'Carbon price change density')
hist(pricechange)
shapiro.test(pricechange)
lillie.test(pricechange)
?lillie.test
?shapiro.test

#moving avg
library(TTR)
smapricechange <- SMA(pricechange,n = 30)
plot(date,smapricechange, main = "moving average carbon price change",
     type = 'l', xlab = "Date", ylab = "MA carbon price change")
library(forecast)
library(tseries)
library(ggplot2)
library(TTR)
library(dplyr)

comparison <- data.frame(pricechange, cleandatareform$carbonprice, smapricechange)
ggplot(data = comparison) +
  geom_line(mapping = aes(x = cleandatareform$date, y = smapricechange), color = "red", size = 1)+
  geom_line(mapping = aes(x = cleandatareform$date, y = pricechange), color = "blue") +
  labs(x = "Date", y = "EUA price change (%)", title = "Carbon price fluctuations") + 
  theme_minimal()

#transform carbon price into time series
cpchange <- ts(pricechange, frequency = 1)
plot(cpchange)
adf.test(cpchange)

#white noise
library(tseries)
print(acf(cpchange, pl = FALSE, na.action = na.pass))
print(acf(cpchange, pl = TRUE, na.action = na.pass))

timeseries %>%
  autoplot()


#change all to %change
#------------------------------------------------------------------------------------------------------------------------------------
#volets
#create cumulative function for the EUA in circulation

VolETS <- cleandatareform$volets

EUAcirc <- data.frame(cleandatareform$date, VolETS)
cleandatareform$EUAcirc <- EUAcirc %>%
  group_by(year = lubridate::floor_date(cleandatareform$date, 'year'))%>%
  summarize(EUA = cumsum(VolETS))
EUA = cleandatareform$EUAcirc$EUA

library(ggplot2)
plot(cleandatareform$date, EUA,
     xlab = 'Date', ylab = 'EUA in circulation')
ggplot(data = cleandatareform) +
  geom_line(mapping = aes(x = date, y = EUA), size = 1)+
  labs(x = "Date", y = "Cumulative sum of EUA traded", title = "EUA in circulation") + 
  theme_minimal()

scatter.smooth(diff(cleandatareform$volets), pricechange[-1], main = 'Carbon price change and volume EUA traded',
               xlab = 'Difference in volume ETS traded', ylab = "EUA price (%)")
diffvol <- data.frame(diff(cleandatareform$volets), cleandatareform$perchange[-1])

table <- as.matrix(cor(diffvol))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")

library(tseries)
#install.packages('ggalt')
library(ggalt)
adf.test(diff(cleandatareform$volets))

diffvolets <- diff(cleandatareform$volets)

ggplot(cleandatareform[-1,], aes(x = diffvolets, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Difference in volume of EUA traded", y = "EUA price (%)")

ggplot(cleandatareform[-1,], aes(x = date))+
  geom_line(aes(y = perchange, fill = 'perchange'))+
  geom_line(aes(y = diffvolets, fill = 'diffvolets'))



ggplot(cleandatareform[-1,], aes(x = lag(diffvolets, n= 1), y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Difference in volume (lag 1)", y = "EUA price (%)")

ggplot(cleandatareform[-1,], aes(x = lag(diffvolets, n= 7), y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Difference in volume (lag 7)", y = "EUA price (%)")

ggplot(cleandatareform[-1,], aes(x = lag(diffvolets, n= 30), y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Difference in volume (lag 30)", y = "EUA price (%)")

ccfelec = ccf(ts(diffvol)[-1], ts(pricechange)[-1], 30)
lag2.plot(ts(cleandatareform$volets)[-1], ts(pricechange)[-1], max.lag = 30, smooth = TRUE)



#------------------------------------------------------------------------------------------------------------------------------------
#energy determinants
cleandatareform$coalp <- (100 * (cleandatareform$coalp - lag(cleandatareform$coalp))/lag(cleandatareform$coalp))
cleandatareform$oilp <- (100 * (cleandatareform$oilp - lag(cleandatareform$oilp))/lag(cleandatareform$oilp))
cleandatareform$ngasp <- (100 * (cleandatareform$ngasp - lag(cleandatareform$ngasp))/lag(cleandatareform$ngasp))
cleandatareform$elecp <- (100 * (cleandatareform$elecp - lag(cleandatareform$elecp))/lag(cleandatareform$elecp))
cleandatareform$renewp <- (100 * (cleandatareform$renewp - lag(cleandatareform$renewp))/lag(cleandatareform$renewp))
cleandatareform$gasoilp <- (100 * (cleandatareform$gasoilp - lag(cleandatareform$gasoilp))/lag(cleandatareform$gasoilp))
cleandatareform$ffp <- (100 * (cleandatareform$ffp - lag(cleandatareform$ffp))/lag(cleandatareform$ffp))
cleandatareform$elecgenp <- (100 * (cleandatareform$elecgenp - lag(cleandatareform$elecgenp))/lag(cleandatareform$elecgenp))
cleandatareform$powgenp <- (100 * (cleandatareform$powgenp - lag(cleandatareform$powgenp))/lag(cleandatareform$powgenp))
cleandatareform$allenp <- (100 * (cleandatareform$allenp - lag(cleandatareform$allenp))/lag(cleandatareform$allenp))
cleandatareform$subffelec <- (100 * (cleandatareform$subffelec - lag(cleandatareform$subffelec))/lag(cleandatareform$subffelec))
cleandatareform$subcoalgas <- (100 * (cleandatareform$subcoalgas - lag(cleandatareform$subcoalgas))/lag(cleandatareform$subcoalgas))
cleandatareform$subffren <- (100 * (cleandatareform$subffren - lag(cleandatareform$subffren))/lag(cleandatareform$subffren))





ggplot(cleandatareform[-1,], aes(x = date))+
  geom_line(aes(y = coalp, col = 'coalprice'))+
  geom_line(aes(y = oilp, col = 'oil price'))+
  geom_line(aes(y = ngasp, col = 'ngasprice'))+
  geom_line(aes(y = elecp, col = 'elec price'))+
  geom_line(aes(y = renewp, col = 'renewp'))

par(mfrow = c(1,1))

table <- as.matrix(cor(cleandatareform[-1,c(4:16, 37)]))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")


par(mfrow = c(4,4))
scatter.smooth(cleandatareform$coalp, pricechange, type = "p", main = "Carbon price vs Coal price",
               ylab = "EUA price change (%)", xlab = "Coal price  (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$oilp, pricechange, main = "Carbon price vs Oil price",
               ylab = "EUA price change (%)", xlab = "Oil price  (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$ngasp,pricechange,  main = "Carbon price vs Natural gas price",
               ylab = "EUA price change (%)", xlab = "Natural gas price  (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$elecp,pricechange,  main = "Carbon price vs Electricity price",
               ylab = "EUA price change (%)", xlab = "Electricity price (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$renewp,pricechange,  main = "Carbon price vs Renewable energy price",
               ylab = "EUA price change (%)", xlab = "Renewable energy price (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$powgenp, pricechange,  main = "Carbon price vs Power generation price",
               ylab = "EUA price change (%)", xlab = "Power generation price (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$ffp,pricechange,  main = "Carbon price vs Fossil fuels price",
               ylab = "EUA price variation (%)", xlab = "Fossil fuels price (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$gasoilp, pricechange,  main = "Carbon price vs Fossil fuels price",
               ylab = "EUA price change (%)", xlab = "Fossil fuels price (%)", 
               pch = 20, frame = FALSE)


scatter.smooth(cleandatareform$elecgenp,pricechange,  main = "Carbon price vs Electricity generation price",
               ylab = "EUA price change (%)", xlab = "Electricity generation price (%)", 
               pch = 20, frame = FALSE)

scatter.smooth( cleandatareform$allenp, pricechange,main = "Carbon price vs Average energy price",
                ylab = "EUA price change (%)", xlab = "Average energy price (%)", 
                pch = 20, frame = FALSE)

#remove outliers subffelec

data <- cleandatareform[-1, c(14,37)]
warning()
quartiles <- quantile(data$subffelec, probs = c(0.05, 0.95), na.rm = FALSE)
IQR <- IQR(data$subffelec)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data$subffelec > Lower & data$subffelec < Upper)
dim(data_no_outlier)
scatter.smooth(data_no_outlier$subffelec, data_no_outlier$perchange,  main = "Carbon price vs Subs power generator",
               ylab = "EUA price change (%)", xlab = "Fossil fuels - Electricity (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$subffren,pricechange,  main = "Carbon price vs Subs electricity generator",
               ylab = "EUA price change (%)", xlab = "Fossil fuels - Renewable energy (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$subcoalgas,pricechange,  main = "Carbon price vs Subs oil/gas and coal",
               ylab = "EUA price change (%)", xlab = "Oil/gas - Coal (%)", 
               pch = 20, frame = FALSE)


scatter.smooth(cleandatareform$subffelec,pricechange,  main = "Carbon price vs Subs power generator",
               ylab = "EUA price change (%)", xlab = "Fossil fuels - Electricity (%)", 
               pch = 20, frame = FALSE)



#check lag of the variables
par(mfrow = c(4,1))
library(tseries)
#install.packages('astsa')
library(astsa)
#install.packages('dynlm')
library(dynlm)
?ccf
ccfcoal = ccf(ts(cleandatareform$coalp)[-1], ts(pricechange)[-1], 20)
ccfoil = ccf(ts(cleandatareform$oilp)[-1], ts(pricechange)[-1], 20)
ccfgas = ccf(ts(cleandatareform$ngasp)[-1], ts(pricechange)[-1], 20)
ccfelec = ccf(ts(cleandatareform$elecp)[-1], ts(pricechange)[-1], 20)
ccfren = ccf(ts(cleandatareform$renewp)[-1], ts(pricechange)[-1], 20)
ccfff = ccf(ts(cleandatareform$ffp)[-1], ts(pricechange)[-1], 20)
ccfelecgen = ccf(ts(cleandatareform$elecgenp)[-1], ts(pricechange)[-1], 20)
ccfpowegen = ccf(ts(cleandatareform$powgenp)[-1], ts(pricechange)[-1], 20)


#lag2.plot(ts(cleandatareform$elecp)[-1], ts(pricechange)[-1], max.lag = 30, smooth = TRUE)
tspc <- ts(pricechange)
tsffp <- ts(cleandatareform$ffp)
tselecp <- ts(cleandatareform$elecp)
tsren <- ts(cleandatareform$renewp)
modelag <- dynlm(tspc ~ L(tsffp,0:7) + L(tselecp, 0:7) + L(tsren, 0:7), data = cleandatareform)
round(summary(modelag)$coef, 7)


par(mfrow = c(4,1))
ccfsubff = ccf(ts(cleandatareform$subcoalgas)[-1], ts(pricechange)[-1], 60)
ccfsubffelec = ccf(ts(data_no_outlier$subffelec)[-1], ts(data_no_outlier$perchange)[-1], 60)
ccfsubffren = ccf(ts(cleandatareform$subffren)[-1], ts(pricechange)[-1], 60)


library(car)
#only individuals
model1 <- lm(lag(cleandatareform$perchange) ~ cleandatareform$coalp+cleandatareform$oilp+cleandatareform$ngasp + cleandatareform$elecp+cleandatareform$renewp +cleandatareform$gasoilp +cleandat, cleandatareform)
summary(model1)
vif(model1)
#aggregation ff
model2 <- lm(cleandatareform$perchange ~ cleandatareform$ffp+ cleandatareform$elecp+cleandatareform$renewp, cleandatareform)
summary(model2)
vif(model2)
#aggreagtion oil/gas
library(car)
model3 <- lm(cleandatareform$perchange ~ cleandatareform$coalp+cleandatareform$gasoilp + cleandatareform$elecp+cleandatareform$renewp, cleandatareform)
summary(model3)
vif(model3)
1/(1-0.8257)
#aggregation elecgen
model4 <- lm(cleandatareform$perchange ~ cleandatareform$elecp+cleandatareform$elecgenp, cleandatareform)
summary(model4)
vif(model4)
#aggregation power gen
model5 <- lm(cleandatareform$perchange ~ cleandatareform$powgenp+cleandatareform$renewp, cleandatareform)
summary(model5)
vif(model5)
#aggregation all
model6 <- lm(cleandatareform$perchange ~ cleandatareform$allenp, cleandatareform)
summary(model6)
vif(model6)


#ff+elec+ren
ggplot(cleandatareform, aes(x = ffp, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Fossi fuel price change (%)", y = "EUA price (%)")

ggplot(cleandatareform, aes(x = elecp, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Electricity price change (%)", y = "EUA price (%)")

ggplot(cleandatareform, aes(x = renewp, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Renewable energy price change (%)", y = "EUA price (%)")

ggplot(cleandatareform, aes(x = subffren, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Fossil fuels - renewable energy (%)", y = "EUA price (%)")

ggplot(cleandatareform, aes(x = subcoalgas, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Coal - oil/natural gas (%)", y = "EUA price (%)")

ggplot(data_no_outlier, aes(x = subffelec, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Fossil fuels and electricity (%)", y = "EUA price (%)")


#check utilities of adding the subs
model6 <- lm(cleandatareform$perchange ~ cleandatareform$ffp + cleandatareform$elecp + cleandatareform$renewp, cleandatareform)
summary(model6)
extractAIC(model6)
#AIC = 2056.5

model7 <- lm(cleandatareform$perchange ~ cleandatareform$ffp + cleandatareform$elecp + cleandatareform$renewp + cleandatareform$subffelec, cleandatareform)
summary(model7)
par(mfrow = c(4,4))
plot(model7)
extractAIC(model7)
#AIC= 2056.5

model8 <- lm(cleandatareform$perchange ~ cleandatareform$ffp + cleandatareform$elecp + cleandatareform$renewp + cleandatareform$subffelec + cleandatareform$subcoalgas, cleandatareform)
summary(model8)
extractAIC(model8)
#AIC = 2058.1

model9 <- lm(cleandatareform$perchange ~ cleandatareform$ffp + cleandatareform$elecp + cleandatareform$renewp + cleandatareform$subffelec + cleandatareform$subcoalgas + cleandatareform$subffren, cleandatareform)
summary(model9)
extractAIC(model9)
#AIC = 2058.1

#install.packages('AICcmodavg')
library(AICcmodavg)
models <- list(model6, model7, model8, model9)
mod.names <- c('nothing', 'subffelec', 'subffelec,coalgas', 'subffelec,coal,ren')
aictab(cand.set = models, modnames = mod.names)

ggplot(cleandatareform, aes(x = SubFFElec, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Substitution between fossil fuels and electricity price", y = "EUA price (%)")

ggplot(cleandatareform, aes(x = SubCoaloil, y = perchange))+
  geom_point()+
  geom_smooth(method = 'loess', se = F, formula = y~x)+
  labs(x = "Substitution between coal and oil", y = "EUA price (%)")





#----------------------------------------------------------------------------------------------------------------------------------------------
#weather determinants
par(mfrow = c(3,2))
plot(CP, Tmean, main = "carbon price vs dev average temperature",
     xlab = "carbon price", ylab = "dev average temperature", 
     pch = 20, frame = FALSE)
abline(lm(Tmean ~ CP), col = "blue")

plot(CP, Tmax, main = "carbon price vs deviation max temperature",
     xlab = "carbon price", ylab = "deviation max temperature", 
     pch = 20, frame = FALSE)
abline(lm(Tmax ~ CP), col = "blue")

plot(CP, Tmin, main = "carbon price vs deviation min temperature",
     xlab = "carbon price", ylab = "deviation min temperature", 
     pch = 20, frame = FALSE)
abline(lm(Tmin ~ CP), col = "blue")

plot(CP, Wind, main = "carbon price vs deviation wind",
     xlab = "carbon price", ylab = "deviation wind", 
     pch = 20, frame = FALSE)
abline(lm(Wind~CP), col = "blue")

plot(CP, Prec, main = "carbon price vs deviation precipitation",
     xlab = "carbon price", ylab = "deviation precipitation", 
     pch = 20, frame = FALSE)
abline(lm(Prec ~ CP), col = "blue")

table <- as.matrix(cor(cleandatareform[-1,c(17:21, 37)]))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")

#-------------------------------------------------------------------------------------------------------------------------------------------
#weather and energy
par(mfrow = c(1,1))
table <- as.matrix(cor(cleandatareform[-1,c(8,12, 17:21)]))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")


Temponelecgen = lm (AggElecGen ~ Tmean + Tmax + Tmin)
Windonren = lm(renewP ~ Wind + Prec)
summary(Temponelecgen)
summary(Windonren)

par(mfrow = c(3,3))

scatter.smooth(cleandatareform$renewp, cleandatareform$preci, main = " Renewable energy price vs deviation precipitation",
               xlab = "Renewable energy price (%)", ylab = "deviation precipitation", 
               pch = 20, frame = FALSE)
scatter.smooth(cleandatareform$renewp, cleandatareform$wind, main = "Renewable energy price vs deviation wind",
               xlab = "Renewable energy price (%)", ylab = "deviation wind", 
               pch = 20, frame = FALSE)
scatter.smooth(cleandatareform$elecgenp, cleandatareform$tmean, main = "Electricity generation price vs deviation average temp",
               xlab = "Electricity generation price (%)", ylab = "deviation average temp", 
               pch = 20, frame = FALSE)
scatter.smooth(cleandatareform$elecgenp, cleandatareform$tmin,main = "Electricity generation price vs deviation min temp",
               xlab = "Electricity generation price (%)", ylab = "deviation min temp", 
               pch = 20, frame = FALSE)
scatter.smooth(cleandatareform$elecgenp, cleandatareform$tmax,main = "Electricity generation price vs deviation max temp",
               xlab = "Electricity generation price (%)", ylab = "deviation max temp", 
               pch = 20, frame = FALSE)


#-----------------------------------------------------------------------------------------------------------------------------------
#macroeconomic
cleandatareform$stock <- (100 * (cleandatareform$stock - lag(cleandatareform$stock))/lag(cleandatareform$stock))
cleandatareform$bond <- (100 * (cleandatareform$bond - lag(cleandatareform$bond))/lag(cleandatareform$bond))
cleandatareform$com <- (100 * (cleandatareform$com - lag(cleandatareform$com))/lag(cleandatareform$com))
cleandatareform$indus <- (100 * (cleandatareform$indus - lag(cleandatareform$indus))/lag(cleandatareform$indus))

par(mfrow = c(1,1))
table <- as.matrix(cor(cleandatareform[-1,c(22:25, 37)]))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")

par(mfrow = c(1,1))
scatter.smooth(cleandatareform$perchange, cleandatareform$stock, main = "Carbon price vs Stoxx 600",
               xlab = "EUA price (%)", ylab = "Stoxx 600 (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$perchange, cleandatareform$bond, main = "Carbon price vs S&P Eurozone Bond ",
               xlab = "EUA price (%)", ylab = "S&P Eurozone Sovereign Bond Index (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$perchange, cleandatareform$com, main = "Carbon price vs S&P GSCI non energy",
               xlab = "EUA price (%)", ylab = "S&P GSCI Non-Energy (%)", 
               pch = 20, frame = FALSE)


plot(cleandatareform$perchange, cleandatareform$indus, main = "Carbon price vs Industrial production",
     xlab = "EUA price (%)", ylab = "Industrial production index (%)", 
     pch = 20, frame = FALSE)

modelx <- lm(cleandatareform$perchange~., data = cleandatareform[, c(22:25, 37)])
summary(modelx)
library(car)
vif(modelx)

par(mfrow = c(4,1))

ccfstock = ccf(ts(cleandatareform$stock)[-1], ts(pricechange)[-1], 60)
ccfbond = ccf(ts(cleandatareform$bond)[-1], ts(pricechange)[-1], 60)
ccfcom = ccf(ts(cleandatareform$com)[-1], ts(pricechange)[-1], 60)
ccfind = ccf(ts(cleandatareform$indus)[-1], ts(pricechange)[-1], 60)

#-----------------------------------------------------------------------------------------------------------------------------
#energy firms return
cleandatareform$shell <- (100 * (cleandatareform$shell - lag(cleandatareform$shell))/lag(cleandatareform$shell))
cleandatareform$tte <- (100 * (cleandatareform$tte - lag(cleandatareform$tte))/lag(cleandatareform$tte))
cleandatareform$bp <- (100 * (cleandatareform$bp - lag(cleandatareform$bp))/lag(cleandatareform$bp))
cleandatareform$eni <- (100 * (cleandatareform$eni - lag(cleandatareform$eni))/lag(cleandatareform$eni))
cleandatareform$equinor <- (100 * (cleandatareform$equinor - lag(cleandatareform$equinor))/lag(cleandatareform$equinor))
cleandatareform$neste <- (100 * (cleandatareform$neste - lag(cleandatareform$neste))/lag(cleandatareform$neste))
cleandatareform$repsol <- (100 * (cleandatareform$repsol - lag(cleandatareform$repsol))/lag(cleandatareform$repsol))
cleandatareform$aker <- (100 * (cleandatareform$aker - lag(cleandatareform$aker))/lag(cleandatareform$aker))
cleandatareform$tenaris <- (100 * (cleandatareform$tenaris - lag(cleandatareform$tenaris))/lag(cleandatareform$tenaris))
cleandatareform$omv <- (100 * (cleandatareform$omv - lag(cleandatareform$omv))/lag(cleandatareform$omv))
cleandatareform$clean <- (100 * (cleandatareform$clean - lag(cleandatareform$clean))/lag(cleandatareform$clean))

par(mfrow =c(1,1))

table <- as.matrix(cor(cleandatareform[-1,c(26:37)]))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")

#PCA for energy return market because can't use avg because maybe one of the company is clearly dominant
cleandatareform <- cleandatareform[-1,]
energyreturn <- cleandatareform[,26:36]
pr.out = prcomp(energyreturn, scale = TRUE)
#scale = true means that we standardised all the variables
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
?biplot

biplot(pr.out, scale = 0)
#pr.out$rotation = -pr.out$rotation
#pr.out$x = -pr.out$x
#biplot(pr.out, scale = 0)
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve
plot(pve , xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type = 'b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), type = 'b') 
a = c(1,2,8, -3)
cumsum(a)
which(cumsum(pve) >= 0.8)[1]
pr.out$rotation[,1:5]
pr.out$x[, 1:5]

cleandatareform$energyreturnpca <- pr.out$x[,1:5]
pcaenergyret = cleandatareform$energyreturnpca

energyreturnpca1 = cleandatareform$energyreturnpca[,"PC1"]
energyreturnpca2 = cleandatareform$energyreturnpca[,"PC2"]
energyreturnpca3 = cleandatareform$energyreturnpca[,"PC3"]
energyreturnpca4 = cleandatareform$energyreturnpca[,"PC4"]
energyreturnpca5 = cleandatareform$energyreturnpca[,"PC5"]


par(mfrow = c(3,3))
scatter.smooth(cleandatareform$perchange, cleandatareform$energyreturnpca[,"PC1"] , main = "Carbon price vs Energy firms' return PC1",
               xlab = "EUA price (%)", ylab = "Energy return PC1 (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$perchange, cleandatareform$energyreturnpca[,"PC2"] , main = "Carbon price vs Energy firms' return PC2",
               xlab = "EUA price (%)", ylab = "Energy return PC2 (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$perchange, cleandatareform$energyreturnpca[,"PC3"] , main = "Carbon price vs Energy firms' return PC3",
               xlab = "EUA price (%)", ylab = "Energy return PC3 (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$perchange, cleandatareform$energyreturnpca[,"PC4"] , main = "Carbon price vs Energy firms' return PC4",
               xlab = "EUA price (%)", ylab = "Energy return PC4 (%)", 
               pch = 20, frame = FALSE)

scatter.smooth(cleandatareform$perchange, cleandatareform$energyreturnpca[,"PC5"] , main = "Carbon price vs Energy firms' return PC5",
               xlab = "EUA price (%)", ylab = "Energy return PC5 (%)", 
               pch = 20, frame = FALSE)

par(mfrow = c(5,1))
ccfind = ccf(ts(cleandatareform$energyreturnpca[,"PC1"])[-1], ts(pricechange)[-1], 20)
ccfind = ccf(ts(cleandatareform$energyreturnpca[,"PC2"])[-1], ts(pricechange)[-1], 20)
ccfind = ccf(ts(cleandatareform$energyreturnpca[,"PC3"])[-1], ts(pricechange)[-1], 20)
ccfind = ccf(ts(cleandatareform$energyreturnpca[,"PC4"])[-1], ts(pricechange)[-1], 20)
ccfind = ccf(ts(cleandatareform$energyreturnpca[,"PC5"])[-1], ts(pricechange)[-1], 20)

#---------------------------------------------------------------------------------------------------------------------------------------
#energy firms return and energy price
#change

par(mfrow=c(1,1))
table <- as.matrix(cor(cleandatareform[,c(7,8,10,41)]))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(table, method = "color", type = "lower", col = col(200),
         number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black")



library(dplyr)
scatter.smooth(testset$perchange, lag(testset$elecp, n=30), main = "Carbon price vs elec price")

library(ggplot2)
ggplot(testset, aes(x = testset$variation, y = lag(testset$elecp), n= 30)) +
  geom_violin()

