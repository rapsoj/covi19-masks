#### 1 - PREPARE WORKSPACE ####

# Install packages
#install.packages("dplyr")

# Access libraries
library(dplyr)

# Load in Czech COVID-19 case data
df <- read.csv("czech_cases.csv")


#### 2 - CLEAN DATA ####

# Specify date column as date data type
df$date <- as.Date(df$date)

# Specify cases as numeric
df$total_cases <- as.numeric(df$total_cases)

# Create column for days since first case
df$days_since_first_case <- df$date - as.Date("2020-03-01")

# Specify days since first case as numeric
df$days_since_first_case <- as.numeric(df$days_since_first_case)

# Create column for new cases
df$new_cases <- df$total_cases - lag(df$total_cases, default = first(df$total_cases))

# Remove data before first cases
df <- subset(df, days_since_first_case >= 0)


#### 3 - EXPLORE DATA ####

# Visualize total cases normally
plot(df$days_since_first_case, df$total_cases, pch=16,
     main='Growth in total COVID-19 Cases, Czechia',
     xlab='Days Since First Case',
     ylab='Total Cases')

# Visualize total cases logarithmically
plot(df$days_since_first_case, log(df$total_cases), pch=16,
     main='Logarithmic Growth in total COVID-19 Cases, Czechia',
     xlab='Days Since First Case',
     ylab='Log of Total Cases')

# Visualize change in cases normally
plot(df$days_since_first_case, df$new_cases, pch=16,
     main='Growth in COVID-19 Cases, Czechia',
     xlab='Days Since First Case',
     ylab='New Cases')

# Visualize change in cases logarithmically
plot(df$days_since_first_case, log(df$new_cases), pch=16,
     main='Logarithmic Growth in total COVID-19 Cases, Czechia',
     xlab='Days Since First Case',
     ylab='Log of New Cases')


#### 4 - MODEL GROWTH ####

# Create exponential model of all cases
model <- lm(log(df$days_since_first_case) ~ df$total_cases)
summary(model)

# Find overall doubling period
model <- lm(data=df, log(total_cases) ~ days_since_first_case)
summary(model)
d <- log10(2)/summary(model)$coefficients[2]
cat("Doubling Period:", d)

# Find doubling period before policy
df_before <- subset(df, date < "2020-03-19")
model_before <- lm(data=df_before, log(total_cases) ~ days_since_first_case)
summary(model_before)
d_before <- log10(2)/summary(model_before)$coefficients[2]
cat("Doubling Period:", d_before)

# Find doubling period after policy
df_after <- subset(df, date >= "2020-03-19")
model_after <- lm(data=df_after, log(total_cases) ~ days_since_first_case)
summary(model_after)
d_after <- log10(2)/summary(model_after)$coefficients[2]
cat("Doubling Period:", d_after)


#### 5 - VISUALIZE FINDINGS ####

# Plot data points
par(mgp=c(3.25,1,0), mar=c(5,5,4,2)+0.1)
plot(df$days_since_first_case, df$total_cases, pch=21, cex=1,
     main="Total Reported COVID-19 Cases, Czechia",
     xlab="Days Since First Reported Case",
     ylab="Total Reported Cases",
     yaxt="none")

# Visualize exponential growth before policy
lines(df_before$days_since_first_case, exp(predict(model_before)), lwd=3, col="#990000")

# Visualize exponential growth after policy
lines(df_after$days_since_first_case, exp(predict(model_after)), lwd=3, col="#006699")

# Estimate exponential growth before policy extension
new <- data.frame(days_since_first_case=df$days_since_first_case)
exp(predict(model_before, newdata=new))
lines(df$days_since_first_case, exp(predict(model_before, newdata=new)),
      lwd=3, col="#990000", lty=2)

# Add line to indicate policy
abline(v = 18, lwd=2, lty=2)

# Add legend
legend(1, 3100, legend=c("Growth Before Mask Policy", "Growth After Mask Policy"),
       col=c("#990000", "#006699"), lwd=3, cex=0.9)

# Fix y-axis
axis(2, seq(0,3500,500), las=2)


#### 6 - ADDITIONAL CALCULATIONS ####

# Find doubling period after public closures
df_new <- subset(df, date >= "2020-03-14")
model_new <- lm(data=df_new, log(total_cases) ~ days_since_first_case)
summary(model_new)
d_new <- log10(2)/summary(model_new)$coefficients[2]
cat("Doubling Period:", d_new)

# Find doubling period after quarantine
df_new <- subset(df, date >= "2020-03-15")
model_new <- lm(data=df_new, log(total_cases) ~ days_since_first_case)
summary(model_new)
d_new <- log10(2)/summary(model_new)$coefficients[2]
cat("Doubling Period:", d_new)

# Find doubling period before five days after mask policy
df_before_new <- subset(df, date < "2020-03-24")
model_before_new <- lm(data=df_before_new, log(total_cases) ~ days_since_first_case)
summary(model_before_new)
d_before_new <- log10(2)/summary(model_before_new)$coefficients[2]
cat("Doubling Period:", d_before_new)

# Find doubling period after five days after mask policy
df_after_new <- subset(df, date >= "2020-03-24")
model_after_new <- lm(data=df_after_new, log(total_cases) ~ days_since_first_case)
summary(model_after_new)
d_after_new <- log10(2)/summary(model_after_new)$coefficients[2]
cat("Doubling Period:", d_after_new)

# Plot data points
par(mgp=c(3.25,1,0), mar=c(5,5,4,2)+0.1)
plot(df$days_since_first_case, df$total_cases, pch=21, cex=1,
     main="Total Reported COVID-19 Cases, Czechia",
     xlab="Days Since First Reported Case",
     ylab="Total Reported Cases",
     yaxt="none")

# Visualize normal exponential growth
lines(df$days_since_first_case,
      exp(predict(model)), lwd=3, col="#ff9900")

# Add line to indicate policy
abline(v = 23, lwd=2, lty=2)

# Add legend
legend(1, 3100, legend=c("Exponential Case Growth", "5 Days After Mask Policy"),
       col=c("#ff9900", "#000000"), lwd=3:1, lty=1:2, cex=0.9)

# Fix y-axis
axis(2, seq(0,3500,500), las=2)