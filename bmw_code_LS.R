# Restart R session if necessary
# Restart R session...

# Install necessary packages if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load necessary packages
library(readxl)
library(dplyr)
library(ggplot2)

# Read data from Excel file
# Make sure to provide the correct file path
cars <- read_excel(path = 'C:/Users/linas/OneDrive/Dokument/R Programming/kunskapskontroll/BMW_LS.xlsx')

# View the data to verify if it's loaded correctly
View(cars)

# Handle missing values
cars <- na.omit(cars)

# Convert categorical variables to numeric
cars <- cars %>%
  mutate(Drivmenel = case_when(
    Drivmenel == "Bensin" ~ 1,
    Drivmenel == "Hybri" ~ 2,
    Drivmenel == "Diesel" ~ 3
  ),
  Växellåda = ifelse(Växellåda == "Manuell", 1, 
                     ifelse(Växellåda == "Automat", 2, NA))
  )

# Build linear regression model with mileage as a predictor
lm_model <- lm(Pris ~ Årsmodell + Miltal + Växellåda + Drivmenel + Modell, data = cars)

# Polynomial regression
poly_model <- lm(Pris ~ poly(Årsmodell, 2) + poly(Miltal, 2) + Växellåda + poly(Drivmenel, 2) + poly(Modell, 2), data = cars)

# Calculate AIC and BIC
aic_lm <- AIC(lm_model)
bic_lm <- BIC(lm_model)
aic_poly <- AIC(poly_model)
bic_poly <- BIC(poly_model)

# Make predictions
predictions_lm <- predict(lm_model, cars)
predictions_poly <- predict(poly_model, cars)

# Evaluate the models
summary(lm_model)
summary(poly_model)

# Visualize predictions against actual values
ggplot(data = cars, aes(x = Pris, y = predictions_lm)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Actual Price", y = "Predicted Price", title = "Linear Regression: Predicted Price vs Actual Price")

ggplot(data = cars, aes(x = Pris, y = predictions_poly)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Actual Price", y = "Predicted Price", title = "Polynomial Regression: Predicted Price vs Actual Price")

# Print AIC and BIC
cat("Linear Regression AIC:", aic_lm, "\n")
cat("Linear Regression BIC:", bic_lm, "\n")
cat("Polynomial Regression AIC:", aic_poly, "\n")
cat("Polynomial Regression BIC:", bic_poly, "\n")

plot(cars)
