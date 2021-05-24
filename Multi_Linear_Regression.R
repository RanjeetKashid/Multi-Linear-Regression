########## Corolla Price Prediction ##########

library(readr)
corolla <- read.csv(file.choose())
corolla<-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
attach(corolla)

### EDA ###

library(lattice)
library(moments)

summary(corolla)
str(corolla)
sum(is.na(corolla))
hist(Price)
boxplot(Age_08_04)$out # outlier -  4 4 4 2 2 1 1
boxplot(Price)$out
hist(Price)
boxplot(KM)$out
boxplot(HP)$out
boxplot(cc)$out # there is a value at the 81st observation - 16000, looks like thats a typo
corolla["cc"][corolla["cc"] == "16000"] <- "1600" # corrected the typo from 16000 to 1600
boxplot(Doors)$out
boxplot(Gears)$out # we will need to consult the SME about 3 & 4 gears
boxplot(Quarterly_Tax)$out # will need to consult the SME
boxplot(Weight)$out

### Correlation analysis ###

# Converting to numeric
corolla$Price <- as.numeric(corolla$Price)
corolla$Age_08_04 <- as.numeric(corolla$Age_08_04)
corolla$KM <- as.numeric(corolla$KM)
corolla$HP <- as.numeric(corolla$HP)
corolla$cc <- as.numeric(corolla$cc)
corolla$Doors <- as.numeric(corolla$Doors)
corolla$Gears <- as.numeric(corolla$Gears)
corolla$Quarterly_Tax <- as.numeric(corolla$Quarterly_Tax)
corolla$Weight <- as.numeric(corolla$Weight)

pairs(corolla)

cor(corolla)

library(corpcor)
cor2pcor(cor(corolla))

library(GGally)
library(stringi)

cor(cc,Price)
cor(Doors,Price)
cor(Gears,Price)
cor(Quarterly_Tax,Price)
cor(HP,Price)

# Remove column cc,Doors,Gears,Quarterly_Tax, HP which don't have a correlation with Price


cn <- corolla[c("Price","Age_08_04","KM","Weight")]
attach(cn)
plot(cn)
cor(cn)
cor2pcor(cor(cn))
ggpairs(cn)

### Model Building ###

cn_linear <- lm(Price ~ Age_08_04+KM+Weight, data = cn)
summary(cn_linear)

library(car)

vif(cn_linear)
sqrt(mean(cn_linear$residuals^2))

### Splitting ###

library(caTools)

split <- sample.split(cn$Price, SplitRatio = 0.70)
split
table(split) #table function gives proportion of data
cn_train <- subset(cn,split == TRUE)
cn_test <- subset(cn,split == FALSE)

#### Linear Model ####

cn_linear <- lm(Price ~ Age_08_04+KM+Weight, data = cn_train)
summary(cn_linear)
mean(cn_linear$residuals)
cn_linear_train_RMSE <- sqrt(mean(cn_linear$residuals^2))
cn_linear_train_RMSE # 1482.155
plot(cn_linear) # Identically, normal and homoscedatic

# Prediction
cn_linear_pred <- predict(cn_linear,cn_test[,-cn_test$Price])
linear_test_errors <- cn_test$Price - cn_linear_pred
cn_linear_test_RMSE <- sqrt(mean(linear_test_errors^2))
cn_linear_test_RMSE # 1224.787

#### Log Transformation ####

cor(log(Age_08_04),Price) # -0.8763467
cor(log(KM),Price) # -0.5195515
cor(log(Weight),Price) # 0.5853314

cor(Age_08_04,log(Price)) # -0.8767022
cor(KM,log(Price)) # -0.6089552
cor(Weight,log(Price)) # 0.5077335

cor(log(Age_08_04),log(Price)) # -0.8349018
cor(log(KM),log(Price)) # -0.4945677
cor(log(Weight),log(Price)) # 0.5152091

cn_log <- lm(log(Price) ~ Age_08_04+KM+Weight,data = cn_train)
summary(cn_log) #
cn_log_train_pred <- exp(cn_log$fitted.values)
cn_log_train_error <- cn_train$Price - cn_log_train_pred
mean(cn_log_train_error)
cn_log_train_RMSE <-  sqrt(mean(cn_log_train_error^2))
cn_log_train_RMSE # 1351.664

# Prediction

cn_log_test_pred <- exp(predict(cn_log,newdata = cn_test[,-cn_test$Price]))
cn_log_test_error <- cn_test$Price - cn_log_test_pred
mean(cn_log_test_error)
cn_log_test_RMSE <- sqrt(mean(cn_log_test_error^2))
cn_log_test_RMSE # 1135.387

plot(cn_log)  # Identically, normal and homoscedatic

#### Sqrt Transformation ####

cor(Age_08_04,sqrt(Price)) # -0.8834493
cor(KM,sqrt(Price)) # -0.5923278
cor(Weight,sqrt(Price)) # 0.5468473

cn_sqrt <- lm(sqrt(Price) ~ Age_08_04+KM+Weight,data = cn_train)
summary(cn_sqrt)
cn_sqrt_train_pred <- ((cn_sqrt$fitted.values)^2)
cn_sqrt_train_error <- cn_train$Price - cn_sqrt_train_pred
mean(cn_sqrt_train_error)
cn_sqrt_train_RMSE <- sqrt(mean(cn_sqrt_train_error^2))
cn_sqrt_train_RMSE # 1393.482

# Prediction

cn_sqrt_test_pred <- (predict(cn_sqrt,newdata = cn_test[,-cn_test$Price]))^2
cn_sqrt_test_error <- cn_test$Price - cn_sqrt_test_pred
cn_sqrt_test_RMSE <- sqrt(mean(cn_sqrt_test_error^2))
cn_sqrt_test_RMSE # 1151.036

plot(cn_sqrt) # Identically, normal and homoscedatic

#### Polynomial Transformation ####

cor(Age_08_04,sqrt(Price))
cor(Age_08_04*Age_08_04,sqrt(Price))
cor(KM,sqrt(Price))
# cor(KM*KM,sqrt(Price)) # If used gives high p-value
cor(Weight,sqrt(Price))
cor(Weight*Weight,sqrt(Price))

cn_poly <- lm(sqrt(Price) ~ Age_08_04 +I(Age_08_04*Age_08_04)+ KM + Weight +I(Weight*Weight), data = cn_train)
summary(cn_poly)
cn_poly_train_pred <- ((cn_poly$fitted.values)^2)
cn_poly_train_error <- cn_train$Price - cn_poly_train_pred
mean(cn_poly_train_error)
cn_poly_train_RMSE <- sqrt(mean(cn_poly_train_error^2))
cn_poly_train_RMSE # 1348.15

# Prediction

cn_poly_test_pred <- (predict(cn_poly,newdata = cn_test[,-cn_test$Price]))^2
cn_poly_test_error <- cn_test$Price - cn_poly_test_pred
cn_poly_test_RMSE <- sqrt(mean(cn_poly_test_error^2))
cn_poly_test_RMSE # 1137.51

plot(cn_poly) # Identically, normal and homoscedatic



########## Startup Profit Prediction ##########

startup <- read.csv(file.choose())
names(startup)[1] <- "RD"
names(startup)[2] <- "Admin"
names(startup)[3] <- "MS"
str(startup)
summary(startup)
attach(startup)

### EDA ###

hist(Profit)
boxplot(RD)
boxplot(Admin)
boxplot(MS)
boxplot(Profit)

### Creating dummy variables ###

library(fastDummies)
X<- dummy_cols(startup,select_columns = "State")
sp <- X[,-4]

names(sp)[5] <- "Cali"
names(sp)[6] <- "Florida"
names(sp)[7] <- "NY"

attach(sp)

### Correlation Analysis ###

pairs(sp)

cor(sp)
plot(Admin,Profit)

cor2pcor(cor(sp))

cor(Admin,Profit) # 0.20

#### Trial Model ####

sp_model <- lm(Profit~RD + Admin + MS,data = sp)
summary(sp_model)
vif(sp_model)
avPlots(sp_model)

# Remove column Admin which has a very weak correlation with Price

sp <- sp[,-2]
str(sp)
sp$Cali <- factor(sp$Cali)
sp$Florida <- factor(sp$Florida)
sp$NY <- factor(sp$NY)
str(sp)

### Splitting ###

### Startified Sampling ###

sp_Cali <- sp[sp$Cali=="1",] # 17
sp_Florida <- sp[sp$Florida=="1",] # 16
sp_NY <- sp[sp$NY=="1",] # 17
sp_train <- rbind(sp_Cali[1:12,],sp_Florida[1:11,],sp_NY[1:12,])
sp_test <- rbind(sp_Cali[13:17,],sp_Florida[12:16,],sp_NY[13:17,])
attach(sp_train)
attach(sp_test)

#### Linear Model ####

sp_linear <- lm(Profit~.,data = sp_train) # MS p value high
summary(sp_linear)
mean(sp_linear$residuals)
sp_linear_train_RMSE <- sqrt(mean(sp_linear$residuals^2))
sp_linear_train_RMSE #6669.095

# Prediction

sp_linear_pred <- predict(sp_linear,newdata = sp_test[,-sp_test$Profit])
sp_linear_test_error <- sp_test$Profit - sp_linear_pred
sp_linear_test_RMSE <- sqrt(mean(sp_linear_test_error^2))
sp_linear_test_RMSE # 13435.03

plot(sp_linear)

#### Log Transformation ####

sp_log <- lm(log(Profit)~.,data = sp_train) # MS p value high 
summary(sp_log)
sp_log_train_pred <- exp(sp_log$fitted.values)
sp_log_train_error <- sp_train$Profit - sp_log_train_pred
mean(sp_log_train_error)
sp_log_train_RMSE <- sqrt(mean(sp_log_train_error^2))
sp_log_train_RMSE # 7113.043

# Prediction

sp_log_test_pred <- exp(predict(sp_log,newdata = sp_test[,-sp_test$Profit]))
sp_log_test_error <- sp_test$Profit - sp_log_test_pred
sp_log_test_RMSE <- sqrt(mean(sp_log_test_error^2))
sp_log_test_RMSE # 19410.5

plot(sp_log)

#### Sqrt Transformation ####

cor(sqrt(RD),sqrt(Profit))
cor(sqrt(MS),sqrt(Profit))
cor(MS,Profit)
cor(sqrt(MS), Profit)

sp_sqrt <- lm(Profit~RD+sqrt(MS)+Cali+Florida+NY,data = sp_train)
summary(sp_sqrt)
sp_sqrt_train_error <- sp_test$Profit - sp_sqrt$fitted.values
sp_sqrt_train_RMSE <- sqrt(mean(sp_sqrt_train_error^2))
sp_sqrt_train_RMSE # 74045.3

# Prediction

sp_sqrt_test_pred <- predict(sp_sqrt,newdata = sp_test[,-sp_test$Profit])
sp_sqrt_test_error <- sp_test$Profit - sp_sqrt_test_pred
sp_sqrt_test_RMSE <- sqrt(mean(sp_sqrt_test_error^2))
sp_sqrt_test_RMSE # 13461.95

plot(sp_sqrt)
