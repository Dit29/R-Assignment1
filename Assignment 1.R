# install.packages("psych", dependencies = TRUE)
# install.packages("car", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("DAAG", dependencies = TRUE)
library("psych") # pairs.panels, cor.ci
library(ggplot2) # ggplot with a lot of dependencies
library(car)
library(leaps) # regsubsets
library(DAAG) # cv.lm
library(bootstrap) # crossval


# set working directory
setwd("C:/Users/sacri/Downloads/R Assignment 1")


# load relevant targets and predictors
wb.agriland <- read.csv(file="API_AG.LND.AGRI.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.cropprod <- read.csv(file="API_AG.PRD.CROP.XD_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.foodprod <- read.csv(file="API_AG.PRD.FOOD.XD_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.fdi <- read.csv(file="API_BX.KLT.DINV.CD.WD_DS2_en_csv_v2.csv", skip=4, header=TRUE)   # foreign direct investment
wb.airpoll <- read.csv(file="API_EN.ATM.PM25.MC.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.airtrans <- read.csv(file="API_IS.AIR.DPRT_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.internet <- read.csv(file="API_IT.NET.USER.P2_DS2_en_csv_v2.csv", skip=4, header=TRUE) # target variable
wb.adultlitr <- read.csv(file="API_SE.ADT.LITR.FE.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE) # target variable
wb.healthexp <- read.csv(file="API_SH.XPD.TOTL.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.empfserv <- read.csv(file="API_SL.SRV.EMPL.FE.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE)  # employement of female in services
wb.lfexpbirth <- read.csv(file="API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip=4, header=TRUE)  # life expectancy at birth
wb.mobcelsubs <- read.csv(file="API_IT.CEL.SETS.P2_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.Greenhousegas <- read.csv(file="API_EN.ATM.GHGT.KT.CE_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.watersrvcs <- read.csv(file="API_SH.H2O.SAFE.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.sanitationfclts <- read.csv(file="API_SH.STA.ACSN.UR_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.militaryexp <- read.csv(file="API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.secureintrnt <- read.csv(file="API_IT.NET.SECR.P6_DS2_en_csv_v2.csv", skip=4, header=TRUE)
wb.gdpcap <- read.csv(file="API_NY.GDP.PCAP.CD_DS2_en_csv_v2.csv", skip=4, header=TRUE)  # gdp per capita
wb.researchersrnd <- read.csv(file="API_SP.POP.SCIE.RD.P6_DS2_en_csv_v2.csv", skip=4, header=TRUE)


# individual plotting of chosen variables
plot(density(wb.agriland$X2012, na.rm = TRUE))
plot(density(wb.cropprod$X2012, na.rm = TRUE))
plot(density(wb.foodprod$X2012, na.rm = TRUE))
plot(density(wb.fdi$X2012, na.rm = TRUE))
plot(density(wb.airpoll$X2012, na.rm = TRUE))
plot(density(wb.airtrans$X2012, na.rm = TRUE))
plot(density(wb.internet$X2012, na.rm = TRUE))
plot(density(wb.gdpcap$X2012, na.rm = TRUE))
plot(density(wb.adultlitr$X2012, na.rm = TRUE))
plot(density(wb.healthexp$X2012, na.rm = TRUE))
plot(density(wb.empfserv$X2012, na.rm = TRUE))
plot(density(wb.lfexpbirth$X2012, na.rm = TRUE))
plot(density(wb.mobcelsubs$X2012, na.rm = TRUE))
plot(density(wb.Greenhousegas$X2012, na.rm = TRUE))
plot(density(wb.watersrvcs$X2012, na.rm = TRUE))
plot(density(wb.sanitationfclts$X2012, na.rm = TRUE))
plot(density(wb.militaryexp$X2012, na.rm = TRUE))
plot(density(wb.secureintrnt$X2012, na.rm = TRUE))
plot(density(wb.researchersrnd$X2012, na.rm = TRUE))


# choose year to identify
agriland <- wb.agriland$X2012
cropprod <- wb.cropprod$X2012
foodprod <- wb.foodprod$X2012
fdi <- wb.fdi$X2012
airpoll <- wb.airpoll$X2012
airtrans <- wb.airtrans$X2012
internet <- wb.internet$X2012
adultlitr <- wb.adultlitr$X2012
healthexp <- wb.healthexp$X2012
empfserv <- wb.empfserv$X2012
lfexpbirth <- wb.lfexpbirth$X2012
mobcelsubs <- wb.mobcelsubs$X2012
Greenhousegas <- wb.Greenhousegas$X2012
watersrvcs <- wb.watersrvcs$X2012
sanitationfclts <- wb.sanitationfclts$X2012
militaryexp <- wb.militaryexp$X2012
secureintrnt <- wb.secureintrnt$X2012
researchersrnd <- wb.researchersrnd$X2012
gdpcap <- wb.gdpcap$X2012
researchersrnd <- wb.researchersrnd$X2012

# load all 2012 variables to single data frame
all.2012 <- data.frame(
  agriland,
  cropprod,
  foodprod,
  fdi,
  airpoll,
  airtrans,
  adultlitr,
  healthexp,
  empfserv,
  mobcelsubs,
  watersrvcs,
  sanitationfclts,
  militaryexp,
  secureintrnt,
  lfexpbirth,
  Greenhousegas,
  internet,
  researchersrnd,
  gdpcap
  )

summary(all.2012)
View(all.2012)
pairs.panels(all.2012, col="red")
cor.ci(all.2012)

# replacing all NAs with 0.
all.2012.na.zeros <- all.2012
all.2012.na.zeros[is.na(all.2012.na.zeros)] <- 0
summary(all.2012.na.zeros)
pairs.panels(all.2012.na.zeros, col="red")


# replacing all NAs with the mean of each row
rm.agriland <- rowMeans(wb.agriland[,-c(1:34)], na.rm = TRUE)
rm.cropprod <- rowMeans(wb.cropprod[,-c(1:34)], na.rm = TRUE)
rm.foodprod <- rowMeans(wb.foodprod[,-c(1:34)], na.rm = TRUE)
rm.fdi <- rowMeans(wb.fdi[,-c(1:34)], na.rm = TRUE)
rm.airpoll <- rowMeans(wb.airpoll[,-c(1:34)], na.rm = TRUE)
rm.airtrans <- rowMeans(wb.airtrans[,-c(1:34)], na.rm = TRUE)
rm.adultlitr <- rowMeans(wb.adultlitr[,-c(1:34)], na.rm = TRUE)
rm.secureintrnt <- rowMeans(wb.secureintrnt[,-c(1:34)], na.rm = TRUE)
rm.empfserv <- rowMeans(wb.empfserv[,-c(1:34)], na.rm = TRUE)
rm.healthexp <- rowMeans(wb.healthexp[,-c(1:34)], na.rm = TRUE)
rm.mobcelsubs <- rowMeans(wb.mobcelsubs[,-c(1:34)], na.rm = TRUE)
rm.watersrvcs <- rowMeans(wb.watersrvcs[,-c(1:34)], na.rm = TRUE)
rm.sanitationfclts <- rowMeans(wb.sanitationfclts[,-c(1:34)], na.rm = TRUE)
rm.militaryexp <- rowMeans(wb.militaryexp[,-c(1:34)], na.rm = TRUE)
rm.internet <- rowMeans(wb.internet[,-c(1:34)], na.rm = TRUE)
rm.lfexpbirth <- rowMeans(wb.lfexpbirth[,-c(1:34)], na.rm = TRUE)
rm.Greenhousegas <- rowMeans(wb.Greenhousegas[,-c(1:34)], na.rm = TRUE)
rm.researchersrnd <- rowMeans(wb.researchersrnd[,-c(1:34)], na.rm = TRUE)
rm.gdpcap <- rowMeans(wb.gdpcap[,-c(1:34)], na.rm = TRUE)


# create a data frame of all indicator row means
all.na.rowmeans <- data.frame(
  rm.agriland,
  rm.cropprod,
  rm.foodprod,
  rm.fdi,
  rm.airpoll,
  rm.airtrans,
  rm.adultlitr,
  rm.healthexp,
  rm.empfserv,
  rm.mobcelsubs,
  rm.watersrvcs,
  rm.sanitationfclts,
  rm.militaryexp,
  rm.secureintrnt,
  rm.lfexpbirth,
  rm.Greenhousegas,
  rm.internet,
  rm.researchersrnd,
  rm.gdpcap
)

# See the statistics and correlations
View(all.na.rowmeans)
summary(all.na.rowmeans)
pairs.panels(all.na.rowmeans, col="red")

# Now we can eliminate NaNs with column means as before
for(i in 1:ncol(all.na.rowmeans)){
  all.na.rowmeans[is.nan(all.na.rowmeans[,i]), i] <- 
    mean(all.na.rowmeans[,i], na.rm = TRUE)
}
summary(all.na.rowmeans)
pairs.panels(all.na.rowmeans, col="red")


# Note that all indicators are %, except for co2em which are metric tonnes per capita
# We could try to trasform it to better match the units of the remaning variables
all.transformed <- all.na.rowmeans
all.transformed$rm.militaryexp <- all.transformed$rm.militaryexp^(1/6)
all.transformed$rm.fdi <- all.transformed$rm.fdi^(1/6)
all.transformed$rm.Greenhousegas <- all.transformed$rm.Greenhousegas^(1/6)
pairs.panels(all.transformed, col="red")

# If you have a lot of variables a more compact view is preferred
# You can also use this method to find out how much you can trust correlations (p-value)
cor.ci(all.transformed)

# Remove variables which have little impact on the dependent variable
# Also remove all correlated independent variables (between themselves)
# In this case, I dropped military, greenhouse gas emission, health expectancy, air transportation, air pollution, fdi, food production, crop production, and agriculture land
all.result <- subset(all.transformed, select = -c(rm.agriland, rm.cropprod, rm.foodprod, rm.airpoll, rm.airtrans, rm.adultlitr, rm.healthexp, rm.militaryexp, rm.Greenhousegas))
pairs.panels(all.result, col="red")
cor.ci(all.result)

# Remove multi-colinearity (life expectancy birth)
all.result <- subset(all.transformed, select = -c(rm.agriland, rm.cropprod, rm.foodprod, rm.airpoll, rm.airtrans, rm.adultlitr, rm.healthexp, rm.militaryexp, rm.Greenhousegas, rm.lfexpbirth))
pairs.panels(all.result, col="red")
cor.ci(all.result)

# Density plots after several processes
plot(density(all.result$rm.internet))
plot(density(all.result$rm.mobcelsubs))
plot(density(all.result$rm.sanitationfclts))
plot(density(all.result$rm.gdpcap))

# Scatterplots
plot(x=all.result$rm.internet, y=all.result$rm.gdpcap,
     xlab="Internet", ylab = "GDP per Capita",
     col="red", main="Internet vs GDP per Capita (1960-2015)")
abline(h = mean(all.result$rm.internet), col="blue")

plot(x=all.result$rm.sanitationfclts, y=all.result$rm.gdpcap,
     xlab="Sanitation Facilities", ylab = "GDP per Capita",
     col="red", main="Sanitation Facilities  vs GDP per Capita (1960-2015)")
abline(h = mean(all.result$rm.sanitationfclts), col="blue")

# ggplot
ggplot(all.result, aes(x=rm.gdpcap, y=rm.internet)) +
  ggtitle("GDP per Capita vs. Internet") +
  labs(x="GDP per Capita", y="Internet Accessibility") +
  geom_point(color="gray", size=1) + 
  stat_smooth(method="loess", color="red", se=TRUE, fullrange=TRUE, size=0.5) +
  stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=0.5)

ggplot(all.result, aes(x=rm.gdpcap, y=rm.sanitationfclts)) +
  ggtitle("GDP per Capita vs. Sanitation Facilities") +
  labs(x="GDP per Capita", y="Sanitation Facilities") +
  geom_point(color="gray", size=1) + 
  stat_smooth(method="loess", color="red", se=TRUE, fullrange=TRUE, size=0.5) +
  stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=0.5)

ggplot(all.result, aes(x=rm.gdpcap, y=rm.mobcelsubs)) +
  ggtitle("GDP per Capita vs. Mobile Cellphone Subscription") +
  labs(x="GDP per Capita", y="Mobile Cellphone Subscription") +
  geom_point(color="gray", size=1) + 
  stat_smooth(method="loess", color="red", se=TRUE, fullrange=TRUE, size=0.5) +
  stat_smooth(method="lm", color="blue", se=FALSE, fullrange=TRUE, size=0.5)

# Develop a linear model
# The model will be built using the training sample of the data
# The model will be validated using the validation sample of the data

# Split data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(all.result$rm.gdpcap), round(length(all.result$rm.gdpcap) * train.size))
train.sample <- all.result[train.index,]
valid.sample <- all.result[-train.index,]


# using stepwise selection of variables by backwards elimination
# F-statistic means this statistic value is x times better than taking the mean.
# r-square means how how much is variation this model has

fit <- lm(rm.gdpcap ~ ., data=train.sample)
summary(fit)
# Residual standard error: 9241 on 199 degrees of freedom
# Multiple R-squared:  0.6874,	Adjusted R-squared:  0.6748 
# F-statistic:  54.7 on 8 and 199 DF,  p-value: < 2.2e-16

# remove empfserv
fit <- lm(rm.gdpcap ~ rm.fdi + rm.mobcelsubs + rm.watersrvcs + rm.sanitationfclts + rm.secureintrnt + rm.internet + rm.researchersrnd, data= train.sample)
summary (fit)
# Residual standard error: 9220 on 200 degrees of freedom
# Multiple R-squared:  0.6872,	Adjusted R-squared:  0.6763 
# F-statistic: 62.78 on 7 and 200 DF,  p-value: < 2.2e-16

# remove research r n d
fit <- lm(rm.gdpcap ~ rm.fdi + rm.mobcelsubs + rm.watersrvcs + rm.sanitationfclts + rm.secureintrnt + rm.internet, data= train.sample)
summary (fit)
# Residual standard error: 9205 on 201 degrees of freedom
# Multiple R-squared:  0.6867,	Adjusted R-squared:  0.6774 
# F-statistic: 73.43 on 6 and 201 DF,  p-value: < 2.2e-16

# remove water service
fit <- lm(rm.gdpcap ~ rm.fdi + rm.mobcelsubs + rm.sanitationfclts + rm.secureintrnt + rm.internet, data= train.sample)
summary (fit)
# Residual standard error: 9196 on 202 degrees of freedom
# Multiple R-squared:  0.6857,	Adjusted R-squared:  0.678 
# F-statistic: 88.15 on 5 and 202 DF,  p-value: < 2.2e-16

# remove secure internet
fit <- lm(rm.gdpcap ~ rm.fdi + rm.mobcelsubs + rm.sanitationfclts + rm.internet, data= train.sample)
summary (fit)
# Residual standard error: 9207 on 203 degrees of freedom
# Multiple R-squared:  0.6834,	Adjusted R-squared:  0.6772 
# F-statistic: 109.5 on 4 and 203 DF,  p-value: < 2.2e-16

# remove foreign direct investment
fit <- lm(rm.gdpcap ~ rm.mobcelsubs + rm.sanitationfclts + rm.internet, data= train.sample)
summary (fit)
# Residual standard error: 9206 on 207 degrees of freedom
# Multiple R-squared:  0.6784,	Adjusted R-squared:  0.6737 
# F-statistic: 145.5 on 3 and 207 DF,  p-value: < 2.2e-16

# show the plot of the lastest fit (the best model)
plot(fit)
vif(fit)
#      rm.mobcelsubs rm.sanitationfclts        rm.internet 
#     2.0559             1.7548             2.3973 

# Note however that we found some extreme values, which should be removed, here they are
train.sample[-which(rownames(train.sample) %in% c("161", "148", "136")),]

# evaluate the final linear model
# Find all predicted values for both a training set and a validation set
train.sample$pred.gdpcap <- predict(fit, newdata = subset(train.sample, select=c(rm.mobcelsubs, rm.sanitationfclts, rm.internet)))
valid.sample$pred.gdpcap <- predict(fit, newdata = subset(valid.sample, select=c(rm.mobcelsubs, rm.sanitationfclts, rm.internet)))

# The theoretical model performance is defined here as R-Squared
summary(fit)

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!
train.sample <- train.sample[-which(rownames(train.sample) %in% c("161", "148", "136")),]

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!
train.sample <- train.sample[-which(rownames(train.sample) %in% c("161", "148", "136")),]

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!
crPlots(fit)            


# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$pred.gdpcap, train.sample$rm.gdpcap), 2)
train.RMSE <- round(sqrt(mean((train.sample$pred.gdpcap - train.sample$rm.gdpcap)^2)))
train.MAE <- round(mean(abs(train.sample$pred.gdpcap - train.sample$rm.gdpcap)))
c(train.corr^2, train.RMSE, train.MAE)
# 0.6724 9119.0000 5661.0000

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$pred.gdpcap, valid.sample$rm.gdpcap), 2)
valid.RMSE <- round(sqrt(mean((valid.sample$pred.gdpcap - valid.sample$rm.gdpcap)^2)))
valid.MAE <- round(mean(abs(valid.sample$pred.gdpcap - valid.sample$rm.gdpcap)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# 0.5776 9454.0000 5601.0000


fit1 <- lm(rm.gdpcap ~ ., data=all.result)
summary(fit1) # Finds the most influential variable based on t-test
# Residual standard error: 9144 on 252 degrees of freedom
# Multiple R-squared:  0.6766,	Adjusted R-squared:  0.6664 
# F-statistic: 65.91 on 8 and 252 DF,  p-value: < 2.2e-16


# Check the regression properties
scr <- par(mfrow=c(2,2))
plot(fit1, ask=FALSE)
par(scr)
vif(fit1)
#             rm.fdi        rm.empfserv      rm.mobcelsubs      rm.watersrvcs rm.sanitationfclts    rm.secureintrnt        rm.internet    rm.researchersrnd 
#          1.172750           1.808550           2.229369           3.105833      3.256607            1.266293             4.203537       1.634079 


# modified validation test (dropped 2 variables)
fit1 <- lm(rm.gdpcap ~ rm.mobcelsubs + rm.sanitationfclts + rm.internet, data=all.result)
summary(fit1) # Finds the most influential variable based on t-test
# Residual standard error: 9252 on 260 degrees of freedom
# Multiple R-squared:  0.6595,	Adjusted R-squared:  0.6556 
# F-statistic: 167.9 on 3 and 260 DF,  p-value: < 2.2e-16

# Check the regression properties
scr <- par(mfrow=c(2,2))
plot(fit1, ask=FALSE)
par(scr)
vif(fit1)
# rm.mobcelsubs rm.sanitationfclts        rm.internet 
#   2.144475           1.844271           2.497945 


# Step AIC
rm.stepAIC <- step(lm(rm.gdpcap ~ 1, data=all.result), direction="forward", 
                   rm.gdpcap ~ rm.fdi + rm.mobcelsubs + rm.watersrvcs + 
                     rm.researchersrnd)
rm.stepAIC$coefficients

# Produce the fit recommended by the AIC "forward" addition
fit2 <- lm(rm.gdpcap ~ rm.fdi + rm.empfserv + rm.mobcelsubs + rm.watersrvcs + 
             rm.sanitationfclts + rm.secureintrnt + rm.internet + rm.researchersrnd, data=all.result)
summary(fit2)
# Residual standard error: 9144 on 252 degrees of freedom
# Multiple R-squared:  0.6766,	Adjusted R-squared:  0.6664 
# F-statistic: 65.91 on 8 and 252 DF,  p-value: < 2.2e-16



# using leaps to select variables
# Create a set of models
gdpcap.leaps <- regsubsets(rm.gdpcap ~ ., data=all.result, nbest=1, method="exhaustive")


# Plot the models in different ways using different statistics
# Select variables using the Leaps package
colfunc <- colorRampPalette(c("navy", "red", "orange"))
scr <- par(mfrow=c(2,2))
plot(gdpcap.leaps, scale="bic", main = "GDP per capita Reg Model Ranking by BIC", col=colfunc(10))
plot(gdpcap.leaps, scale="adjr2", main = "GDP per capita Reg Model Ranking by Adjusted R^2", col=colfunc(50))
plot(gdpcap.leaps, scale="Cp", main = "GDP per capita Reg Model Ranking by Mallows' Cp", col=colfunc(50))
par(mfrow=c(1,1))

# Plot the models with R^2 but contrast against Mallow CP
# The highest R^2 is best, but the smallest CP closest to the number of vars
op <- par(mfrow=c(2,2))
gdpcap.legend <- subsets(gdpcap.leaps, statistic="adjr2", legend = FALSE, min.size = 3, main = "Adjusted R^2 (Max)")
gdpcap.legend <- subsets(gdpcap.leaps, statistic="cp", legend = FALSE, min.size = 3, main = "Mallow Cp (Min / Diag)")
abline(a = 1, b = 1, lty = 2)
gdpcap.legend <- subsets(gdpcap.leaps, statistic="rss", legend = FALSE, min.size = 3, main = "Residual Sum Sq (Min)")
gdpcap.legend <- subsets(gdpcap.leaps, statistic="bic", legend = FALSE, min.size = 3, main = "BIC (Min)")
par(mfrow=c(1,1))
gdpcap.legend

# Let us use Adj R^2 as our preferred selection statistic
best.fit <- lm(rm.gdpcap ~ rm.mobcelsubs + rm.sanitationfclts + rm.internet, data=all.result)
summary(best.fit)
# Residual standard error: 9252 on 260 degrees of freedom
# Multiple R-squared:  0.6595,	Adjusted R-squared:  0.6556 
# F-statistic: 167.9 on 3 and 260 DF,  p-value: < 2.2e-16


### Plot how would this model fair in DAAG 3-fold cross-validation
cv.lm(data=all.result, best.fit, m=3, plotit="Residual") # Try plotit=Observed, Residual


# cross validation procedure
# Bootstrap crossval for the selected optimum regression model
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x) %*% fit$coef}

# matrix of predictors, selected by Adj R^2 and BIC (same as hand crafted, prev. lesson)
X.adjr2 <- as.matrix(all.result[c("rm.gdpcap", "rm.fdi", "rm.empfserv", "rm.mobcelsubs", 
                                    "rm.watersrvcs", "rm.sanitationfclts", "rm.secureintrnt", "rm.internet",
                                  "rm.researchersrnd")])
X.bic <- as.matrix(all.result[c("rm.gdpcap", "rm.mobcelsubs", "rm.sanitationfclts", "rm.internet")])


# vector of predicted values
y <- as.matrix(all.result[c("rm.gdpcap")])

results <- crossval(X.adjr2, y, theta.fit, theta.predict, ngroup=10)
cor(y, fit2$fitted.values)**2 # raw R2 from AIC recommendation
cor(y,results$cv.fit)**2 # cross-validated R2, realistic

# end of program