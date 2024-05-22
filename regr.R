pairs(Clothing_1_[c(2,3,4,5,6,7,8)], lower.panel=NULL)
 summary(Clothing_1_)



variables<- c(3,4,5,7,8)
cor(Clothing_1_)
#it seems that we have a potential multicollinearity between hoursw and hourpw
cor.test(Clothing_1_$hoursw,Clothing_1_$hourspw)
r=0.8077
cor.test(Clothing_1_$tsales,Clothing_1_$hoursw)
r=0.709
cor.test(Clothing_1_$tsales,Clothing_1_$hourspw)
r=0.552

cor.test(Clothing_1_$tsales,Clothing_1_$sales)
r=0.47
cor.test(Clothing_1_$tsales,Clothing_1_$margin)
r=0.24
cor.test(Clothing_1_$tsales,Clothing_1_$hoursw)
r=0.709
cor.test(Clothing_1_$tsales,Clothing_1_$hourspw)
r=0.552
cor.test(Clothing_1_$tsales,Clothing_1_$inv1)
r=0.19
cor.test(Clothing_1_$tsales,Clothing_1_$inv2+Clothing_1_$hourspw)
r=0.2

#we will remove hourspw and keep hoursw as it has higher coeff of cor with tsales

pairs(Clothing_1_[c(2,3,4,5,7,8)], lower.panel=NULL)

#FIRT MODEL
model0<-lm(Clothing_1_$tsales~Clothing_1_$sales+ Clothing_1_$margin
           + Clothing_1_$hoursw +Clothing_1_$inv1 + Clothing_1_$inv2)

summary(model0)
plot(Clothing_1_$tsales~Clothing_1_$sales + Clothing_1_$margin 
     + Clothing_1_$hoursw +Clothing_1_$inv1 + Clothing_1_$inv2)

standardized_residuals1<- rstandard(model0)
fitted_values1 <- fitted(model0)

plot(fitted_values1, standardized_residuals1, 
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)  

qqnorm(rstandard(model0))
qqline(rstandard(model0),lty=2)
hist(rstandard(model0))
plot(model0)

# SECOND MODEL AFTER FIXING ASSUMP

model000<-lm(sqrt(Clothing_1_$tsale)~Clothing_1_$sales+ sqrt(Clothing_1_$margin)
           + sqrt(Clothing_1_$hoursw) +sqrt(Clothing_1_$inv1) + sqrt(Clothing_1_$inv2))

summary(model000)
plot(model000)

model10<-lm(sqrt(Clothing_1_$tsales)~Clothing_1_$sales+ sqrt(Clothing_1_$margin)
             + sqrt(Clothing_1_$hoursw) +Clothing_1_$inv1 +Clothing_1_$inv2)
summary(model10)
plot(model10)

backward.model <- step(model10,direction = "backward")
summary(backward.model)
plot(backward.model)
standardized_residuals1<- rstandard(backward.model)
fitted_values1 <- fitted(backward.model)

plot(fitted_values1, standardized_residuals1, 
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)  

qqnorm(rstandard(backward.model))
qqline(rstandard(backward.model),lty=2)
hist(rstandard(backward.model))



model00<-lm(log(Clothing_1_$tsales)~log(Clothing_1_$sales) + log(Clothing_1_$margin) 
          +log(Clothing_1_$hoursw) +log(Clothing_1_$inv1)+ log(Clothing_1_$inv2))

summary(model00)

standardized_residuals1<- rstandard(model00)
fitted_values1 <- fitted(model00)

plot(fitted_values1, standardized_residuals1, 
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)  

qqnorm(rstandard(model00))
qqline(rstandard(model00),lty=2)
hist(rstandard(model00))


#BACKWARD ELEMINATION

full.model<- lm(formula = log(tsales) ~ log(Clothing_1_$sales)+ log(Clothing_1_$margin) 
                + log(Clothing_1_$hoursw) + log(Clothing_1_$inv1) + log(Clothing_1_$inv2), data = Clothing_1_)
backward.model <- step(full.model,direction = "backward")
summary(backward.model)
standardized_residuals1<- rstandard(backward.model)
fitted_values1 <- fitted(backward.model)

plot(fitted_values1, standardized_residuals1, 
     xlab = "Fitted Values", ylab = "Standardized Residuals",
     main = "Standardized Residuals vs Fitted Values")
abline(h = 0, col = "red", lty = 2)  

qqnorm(rstandard(backward.model))
qqline(rstandard(backward.model),lty=2)
hist(rstandard(backward.model))
 
plot(backward.model)
 

anova(backward.model)


newdata
predict(model00,newdata)


#FORWARD REG

null.model <- lm(log(tsales)~1, data = Clothing_1_)
forward.model <- step(null.model,direction="forward",scope=formula(full.model))
summary(forward.model)
plot(forward.model)

summary(Clothing_1_)
#prediction
newdata <- data.frame(log(sales)=2,log(hoursw)=1.9,log(margin)=1,6,log(inv1)=4) 
predict(backward.model,newdata)

new_data <- data.frame(Clothing_1_$sales = c(1, 2, 3), X2 = c(4, 5, 6), X3 = c(7, 8, 9))  
# New data for prediction
predicted_log_values <- predict(model, newdata = new_data)
predicted_values <- exp(predicted_log_values)

new_data1 <- data.frame(log(sales) = 1, log(hoursw) = 1, log(margin) = 1, log(inv1) = 3)
predicted_log_sales <- predict(model, newdata = new_data1)
predicted_sales <- exp(predicted_log_sales)


model6<-lm(log(Clothing_1_$tsales)~log(Clothing_1_$sales) + poly(Clothing_1_$margin,3) 
           +log(Clothing_1_$hoursw)+Clothing_1_$inv1+Clothing_1_$inv2)
summary(model6)

backward.model <- step(model6,direction = "backward")
summary(backward.model)
plot((backward.model))




