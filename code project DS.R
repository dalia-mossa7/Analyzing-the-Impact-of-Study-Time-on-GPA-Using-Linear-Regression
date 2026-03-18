# Load packages
library(tidyverse)

# Import data
dat <- read.csv("C:\\Users\\Lenovo\\Desktop\\Student_performance_data _.csv")
# Select relevant columns
df <- dat %>% select(StudyTimeWeekly, GPA)
str(df)

# Check na values
sum(is.na(df))

# Scatterplot of variables
stm <- df$StudyTimeWeekly ; gpa = df$GPA

scatter.smooth(stm, gpa, main = "Scores ~ Study time", lpars =
                 list(col = "red", lwd = 3, lty = 3),
               xlab = "Study Time (Weekly)", ylab = "GPA")

# Density plots
par(mfrow = c(1, 2))

plot(density(df$StudyTimeWeekly), main = "Study Time", ylab = "Frequency")
polygon(density(df$StudyTimeWeekly), col = "green")
plot(density(df$GPA), main = "GPA", ylab = "Frequency")
polygon(density(df$GPA), col = "red")

# corrplot(cor(df), method = "number")

par(mfrow = c(1, 2))

boxplot(df$StudyTimeWeekly, main = "Study Time")
boxplot(df$GPA, main = "GPA")

# Summary statistics
summary(df)
# Correlation
cor(df)


# Regression model
m1 <- lm(GPA ~ StudyTimeWeekly, data = df)
summary(m1)

# Model diagnostics

par(mfrow = c(1, 2))
plot(m1, which = 2)
plot(m1, which = 1)


# Make some predictions
study_hours = data.frame(StudyTimeWeekly = c(12, 15, 3, 5, 19)) 
study_hours$pred_gpa = predict(m1, study_hours)
print(study_hours)

# Check model performance
Actual = df$GPA
Predicted = predict ( m1 , newdata = df)

# Calculate RMSE
RMSE =  sqrt ( sum ((Actual - Predicted) ^2, na.rm = T) /nrow(df) )
paste0("The RMSE is ", RMSE)
View(dat)
summary(dat)
