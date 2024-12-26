# Install and load required package
install.packages("stargazer")  # Only run if stargazer is not installed
library(stargazer)

# Display the structure of the data
head(dat)
names(dat)

# Explore the data
head(dat)

# Generate descriptive statistics table
stargazer(dat, type = "text")

# Visualizations
plot(dat$numfatal, dat$intersectiontype, main = "NumFatal vs Intersection Type", 
     xlab = "Number of Fatalities", ylab = "Intersection Type")
plot(dat$numfatal, dat$dralc_inv, main = "NumFatal vs Alcohol Involvement", 
     xlab = "Number of Fatalities", ylab = "Driver Alcohol Involvement")

# Linear model analysis
mod1 <- lm(numfatal ~ weather + intersectiontype + hitrun_inv + 
             dralc_inv + did_inv + dr1520_inv + dr2124_inv + dr65_inv + dayweek, 
           data = dat)
summary(mod1)

#Residual plot of the model
plot(fitted(mod1), resid(mod1))
abline(h=0, col = "red", lwd = 3)

#Log regression model 
log.mod1 <- lm(dat$numfatal ~ dat$weather + dat$intersectiontype + dat$hitrun_inv + dat$dralc_inv + dat$did_inv + dat$dr1520_inv + dat$dr2124_inv + dat$dr65_inv + dat$dayweek, data = dat)
summary(log.mod1)

#Residual plot for the log model
plot(fitted(log.mod1), resid(log.mod1))
abline(h=0, col = "red", lwd = 3)