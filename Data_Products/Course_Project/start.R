#predict black cherry tree height and volume based on girth (diameter at 4.5 ft)
#height is feet, volume is cubic feet, and girth (diameter is inches)
model1 <- lm(Height ~ Girth, data = trees)
plot(trees$Girth, trees$Height, pch = 19, col = "green", cex = 1.5, 
     xlab = "Diameter (inches)", ylab = "Height (feet)", 
     main = "Black Cherry Tree Height vs Diameter", xlim = c(8, 21))
abline(model1, col = "red", lwd = 3)

model2 <- lm(Volume ~ Girth, data = trees)
plot(trees$Girth, trees$Volume, pch = 19, col = "blue", cex = 1.5, 
     xlab = "Diameter (inches)", ylab = "Volume (cubic feet)", 
     main = "Black Cherry Tree Volume vs Diameter", xlim = c(8, 21), 
     ylim = c(0, 80))
abline(model2, col = "red", lwd = 3)





