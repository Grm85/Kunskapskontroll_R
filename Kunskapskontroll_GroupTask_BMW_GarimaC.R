# Load libraries
library(readxl)
library(MASS)  
library(leaps)  
library(car)   
library(Metrics)
library(broom)
library(ggplot2)

# Read the Excel file
file_path <- "C:/Users/LENOVO/Downloads/BMW.xlsx"
cars <- read_excel(file_path)

# Data preprocessing
cars <- na.omit(cars)

#view data
summary(cars)

# Linear regression
logrr <- lm(Pris ~ Beskrivning, data = cars)
summary(logrr)
predd <- predict(logrr, newdata = cars)

# Generalized linear model
logr3 <- glm(Pris ~ Miltal, data = cars)
summary(logr3)
pred3 <- predict(logr3, newdata = cars)


# Backward selection
fulmdl <- lm(Pris ~ ., data = cars)
bkww <- step(fulmdl, direction = "backward", trace = 2)

# Forward selection
nulmd1 <- lm(Pris ~ 1, data = cars)
frww <- step(nulmd1, scope = list(lower = nulmd1, upper = fulmdl), direction = "forward", trace = 3)

# Stepwise selection
stepwise <- step(nulmd1, scope = list(lower = nulmd1, upper = fulmdl), direction = "both", trace = 3)

# Plot between predicted price and price 
scatter.smooth(cars$Pris ~ predd, xlab = "Selling price", main = "Predict Selling Price")
abline(lm(cars$Pris ~ predd, data = cars), col = "red")

ggplot(data = cars, aes(x = Pris, y = predd)) +
  geom_point() + geom_smooth()


# Plot between predicted selling price and milage 

ggplot(data = cars, aes(x = Miltal, y = pred3)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Mileage", y = "Predicted Selling Price") +
  ggtitle("Predicted Selling Price vs. Mileage")


# Broom package
anova(logrr)
glance(logrr)
View(augment(logrr))
