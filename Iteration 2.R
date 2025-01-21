# Load necessary libraries
required_packages <- c("zoo" , "olsrr", "MASS", "ggplot2", "dplyr", "fastDummies", "car", "lmtest")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load dataset
data <- read.csv("preprocessed_file.csv")

# Explore data
head(data)
str(data)
summary(data)
pairs(data)
cor(data)

# Boxplots for each variable
for (j in 1:ncol(data)) {
  windows(); boxplot(data[, j], pch = 19, xlab = names(data)[j], main = "")
}

# Sort data (to address potential autocorrelation)
data <- data %>% arrange(month, day)

# Index plot of response variable ( Checking the existence of a trend in service attribute)
plot(data$service, pch = 19, col = "blue", xlab = "Observation Index", ylab = "Residuals", main = "Index Plot of Residuals")

# Feature engineering
data$end_of_month <- data$day %in% c(1, 2, 3, 28, 29, 30, 31)
data$weekend <- data$weekday_Sunday | data$weekday_Monday
data$end_of_month_and_weekend <- data$weekend & data$end_of_month
data$lag_1_service <- dplyr::lag(data$service, n = 1)
data$service_day_trend <- (data$day - 15) ^ 2
data <- subset(data, select = -c(weekday_Sunday, weekday_Monday))
data <- na.omit(data)
data
# Fit regression model
reg <- lm(service ~ . , data = data)
summary(reg)

# Assumption 1: Linearity
# Fitted vs Actual plot
ggplot(data, aes(x = fitted(reg), y = service)) +
  geom_point(color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Actual Values", title = "Fitted vs Actual Values") +
  theme_minimal()

# Assumption 2: Independence of residuals (Autocorrelation)
plot(reg$residuals, pch = 19, col = "blue", xlab = "Observation Index", ylab = "Residuals", main = "Index Plot of Residuals")
acf(data$service, lag.max = 90, main = "ACF of Variable")

# Assumption 3: Homoscedasticity
# Fitted vs Residuals plot
ggplot(data, aes(x = fitted(reg), y = reg$residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals", title = "Fitted vs Residuals") +
  theme_minimal()

# Assumption 4: Normality of residuals
# Residual QQ plot
qqnorm(residuals(reg), main = "QQ Plot of Residuals")
qqline(residuals(reg), col = "red")

# Assumption 5: No multicollinearity
ols_vif_tol(reg)
ols_eigen_cindex(reg)
ols_coll_diag(reg)

# Assumption 6: Resiudals are independant of all the predictors:
p=reg$rank-1
IStudRes <- rstandard(reg)
EStudRes <- rstudent(reg)
for(j in 1:p){  # Assumes that Y is the first column of df
  windows()
  plot(data[,j+1],IStudRes,pch=19,xlab = names(data)[j+1],main=paste("Plot of IStudRes v.",names(data)[j+1]),ylab = "IStudRes")
}

# Assumption 7: No high leverage points or influential observations
op <- par(mfrow = c(2, 2))
plot(cooks.distance(reg), pch = 19, xlab = "Index", ylab = "Cook's Distance")
plot(ols_leverage(reg), pch = 19, xlab = "Index", ylab = "Leverage Values")
Hinf <- ols_hadi(reg)
plot(Hinf$hadi, pch = 19, ylab = "Hadi's Influence")
par(op)
ols_plot_resid_lev(reg)
ols_plot_resid_stand(reg)

# Variable selection
k <- ols_step_backward_aic(reg)
plot(k)
k <- ols_step_both_p(reg)
plot(k)
selected_variables <- k$model$terms
selected_variables

