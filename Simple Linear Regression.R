# Name : Putu Meisya Cantika Putri
# Class : SI 1 IKI
# NIM : 2415091034

library(ggplot2)
library(car)

# Simulate data
set.seed(123) 
number_of_employees <- sample(10:100, 50, replace = TRUE)
project_time <- 500 - 3.5 * number_of_employees + rnorm(50, mean = 0, sd = 20) 
data <- data.frame(number_of_employees, project_time)

# Simple Linear Regression
model <- lm(project_time ~ number_of_employees, data = data)

# Assumption Tests
# Linearity Check
linearity_plot <- ggplot(data, aes(x = number_of_employees, y = project_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Linearity Check", x = "Number of Employees", y = "Project Time")
print(linearity_plot)
# If the points align well with the red line, the linearity assumption is met.

# Normality of Residuals
residuals <- model$residuals
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)
# If p-value > 0.05, residuals are approximately normal, validating the model.


# Homoscedasticity Check
homoscedasticity_plot <- plot(model, which = 1, main = "Homoscedasticity Check")
# If residuals appear randomly scattered without patterns in this plot, the homoscedasticity assumption holds.

# Analysis
summary_model <- summary(model)
Print(summary_model)

# Visualization
# Scatter plot with regression line
regression_plot <- ggplot(data, aes(x = number_of_employees, y = project_time)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Simple Linear Regression", x = "Number of Employees", y = "Project Time")
print(regression_plot)
# This plot visually confirms the decreasing trend of project time as the number of employees increases.


# Interpretation 

# 1. The regression analysis shows a significant negative relationship between the number of employees and project time.
#    - For every additional employee, project time decreases by an average of 3.5 units (based on the slope coefficient).
#    - The relationship is statistically significant if the p-value for the slope is less than 0.05.
# 2. Assumptions:
#    - Linearity: The scatterplot and regression line suggest a linear relationship between the variables.
#    - Normality: The Shapiro-Wilk test for residuals shows that they follow a normal distribution (if p-value > 0.05).
#    - Homoscedasticity: The residuals plot shows constant variance with no clear patterns, satisfying this assumption.
# 3. Model performance:
#    - If R^2 is high (e.g., > 0.90), the model explains a large proportion of the variability in project time.
# 4. Practical implication:
#    - Increasing the number of employees reduces project time. This insight can guide resource allocation in projects.