# Simple Linear Regression Analysis

# 1. Data: Load and inspect the dataset
# Assuming the dataset is named 'Salary_dataset.csv' and is in the working directory.
data <- read.csv("C:/Users/Hp/Downloads/Final Project Statprob/Salary_dataset.csv")

data <- data[ , c("YearsExperience", "Salary")]  # Select relevant columns

# Preview the data
head(data)

# 2. Assumption Testing
# 2.1 Linearity: Check if the relationship between YearsExperience and Salary is linear
plot(data$YearsExperience, data$Salary, 
     main = "Scatter Plot of YearsExperience vs Salary", 
     xlab = "Years of Experience", 
     ylab = "Salary", 
     col = "blue", 
     pch = 19)

# Add a regression line to visually inspect linearity
model <- lm(Salary ~ YearsExperience, data = data)
abline(model, col = "red", lwd = 2)

# 2.2 Normality of Residuals: Check if residuals follow a normal distribution
residuals <- residuals(model)
hist(residuals, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     breaks = 10)
shapiro.test(residuals)  # Perform Shapiro-Wilk test

# 2.3 Homoscedasticity: Check if residuals have constant variance
plot(fitted(model), residuals, 
     main = "Residuals vs Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     col = "blue", 
     pch = 19)
abline(h = 0, col = "red", lwd = 2)

# 3. Analysis: Print summary of the regression model
summary(model)

# 4. Visualization: Scatter plot with regression line
library(ggplot2)
ggplot(data, aes(x = YearsExperience, y = Salary)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Regression Line: Salary vs Years of Experience",
       x = "Years of Experience",
       y = "Salary")

# 5. Interpretation
# The output of the summary(model) function provides:
# - Intercept (beta_0): The predicted salary when YearsExperience = 0.
# - Slope (beta_1): The change in Salary for every additional year of experience.
# - R-squared: How well the model explains the variability in Salary.
# - p-value: Tests the significance of the relationship between YearsExperience and Salary.

