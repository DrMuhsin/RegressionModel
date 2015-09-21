data(mtcars)

head(mtcars)

# convert qualitative data to relevant factors
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

# The problem statement is to find out whether 
# an automatic have a better MPG compared to manual transmission 
#(0 = automatic, 1 = manual)

# plot pairwise graph of mt cars
p1 = pairs(mtcars, panel = panel.smooth, main = "Pairwise plot of mtcars data")

# check the correlation of variances between every value
cov2cor(cov(sapply(mtcars, as.numeric)))

boxplot(mpg ~ am, data = mtcars,
        xlab = "Transmission type", ylab = "Miles per gallon",
        main = "MPG vs Transmission", col = c("salmon", "Green"), 
        names = c("Automatic", "Manual"))

# abline(fit1, lwd = 2, lt = "dashed")

# model using all data as predictors
everything_model = lm(mpg ~ ., data = mtcars)
summary(everything_model)


# step wise selection process
new_model <- step(lm(mpg ~ ., data = mtcars), trace = 0)
summary(new_model)$coef

# summary(selection_model)$r.squared

# compare basic model to selection model
basic_model <- lm(mpg ~ am, data = mtcars)
compare <- anova(basic_model, new_model)
compare$Pr

par(mar = rep(2, 4))
plot(new_model,col=c("green"))