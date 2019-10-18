#exploratory data analysis
head(mtcars)
boxplot(mpg ~ am, data = mtcars, col = "red", ylab = "MPG", 
        xlab = "Transmission (0 = automatic,1 = manual)", 
        main = "MPG Based on Transmission Type")
pairs(mtcars)
#items that affect MPG similarly given MPG vs variable graphs
group1 <- c("disp", "hp", "wt", "carb")
group2 <- c("drat", "qsec")
group3 <- c("vs", "am")
cor(mtcars)
#values highly correlated to am (>70%): gear, and wt
#values highly correlated to each other: cyl, disp, hp, wt

#models
library(car)
fit1 <- lm(mpg ~ factor(am) - 1, data = mtcars)
summary(fit1)
fit2 <- lm(mpg ~ factor(am) + cyl + disp + hp + wt + carb + vs, data = mtcars)
summary(fit2)
sqrt(vif(fit2))
#remove disp as worst variable
fit3 <- lm(mpg ~ factor(am) + cyl + hp + wt + carb + vs, data = mtcars)
summary(fit3)
sqrt(vif(fit3))
#remove vs and cyl
fit4 <- lm(mpg ~ factor(am) + hp + wt + carb, data = mtcars)
summary(fit4)
sqrt(vif(fit4))
#everything is well balanced so will try these for the models
mdl1 <- lm(mpg ~ factor(am) - 1, data = mtcars)
mdl2 <- lm(mpg ~ factor(am) + hp - 1, data = mtcars)
mdl3 <- lm(mpg ~ factor(am) + wt - 1, data = mtcars)
mdl4 <- lm(mpg ~ factor(am) + carb - 1, data = mtcars)
mdl5 <- lm(mpg ~ factor(am) + hp + wt - 1, data = mtcars)
mdl6 <- lm(mpg ~ factor(am) + hp + carb - 1, data = mtcars)
mdl7 <- lm(mpg ~ factor(am) + wt + carb - 1, data = mtcars)
mdl8 <- lm(mpg ~ factor(am) + hp + wt + carb - 1, data = mtcars)
anova(mdl1, mdl2, mdl3, mdl4, mdl5, mdl6, mdl7, mdl8)
#anova gives the best model as mdl2 given high F value and lowest Pr(>F)
model <- mdl2
summary(model)

#diagnostics and residual plot
par(mfrow = c(2, 2))
plot(model)

par(mfrow = c(1, 1))
plot(hatvalues(model), ylab = "Hatvalue", main = "Hatvalues of Model", 
     pch = 19, col = "orange", cex = 1.5)
text(hatvalues(model), labels=rownames(mtcars), cex=0.6)

plot(dfbetas(model), main = "Dfbeta Values of Model", 
     xlab = "Automatic Transmission", ylab = "Manual Transmission", 
     pch = 19, col = "green", cex = 1.5)
text(dfbetas(model), labels=rownames(mtcars), cex=0.6)

#quantify uncertainty and interpret coefficients
#automatic transmission has 34.0 MPG on average
#manual transmission has 36.1 MPG on average
confint(model)[1,]
confint(model)[2,]
hpmn <- mean(mtcars$hp)
predict(model, newdata = data.frame(am = 0, hp = hpmn), 
        interval = "prediction")
predict(model, newdata = data.frame(am = 1, hp = hpmn), 
        interval = "prediction")


