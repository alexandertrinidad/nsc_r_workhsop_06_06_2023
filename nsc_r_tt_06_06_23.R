## Tidy Tuesday NSC-R workshop
## Title: Checking OLS regression assumptions in R

# Sources: 
#         Gelman, A., Hill, J., & Vehtari, A. (2022) Regression and Other Stories. Cambridge University Press 
#         Steenbeek, W., & Ruiter, S. (2023). Non-spatial and Spatial Linear Regression. In E.Groff and C. Haberman (Eds.), The study of crime and place: A methods handbook. Temple University Press, 2021). Temple University Press
#         Ernst, A.F. & Casper, A. J. (2017). Regression assumptions in clinical psychology research practice—a systematic review of common misconceptions.PeerJ 5:e3323; DOI 10.7717/peerj.3323 
# 
# Assumption 1: Validity. 
# Assumption 2: Representativeness. Assumed with randomization. or representative sample. 

# Assumption 3: Linearity: its deterministic component is a linear function of the separate predictors. 
# Assumption 4: Conditional distribution of errors, has a mean of 0. (Plot or hold when random assignment of X)
# Assumption 5: Independence of errors: Errors from the prediction line are independent. 
#               Residuals are independent: Durbin-Watson test (H0 no correlation AMONG RESIDUALS.)
# Assumption 6: Large outliers are unlikely

# When samples are small two additional.

# Assumption 7: Equal variance of errors: Homoscedasticity.Breusch-Pagan test. or Plot
#               Breusch-Pagan test of hetersocdascity present. H0 = heterosdascity no present (good to go)
# Assumption 8: Errors normal distributed. Conditional distribution of E given X is normal. 

library(tidyverse)
# Defining the parameters. Simple linear regression
set.seed(1234)
N <- 100
X <- runif(N, 0, 1) # Low self control
a <- 1
b <- 1
sigma <- 0.3
error <- rnorm(N, 0, sigma)

y <- a + X*b + error # crime propensity
 
# As data frame
myfakedata <- data.frame(X = X, y = y) 

# Fit the model
fitfake <- lm(y ~ X, data = myfakedata)

summary(fitfake)

# Plotting data and predicted model. To understands patterns not shown in the fitted models (Wim)
## This plot observations and abline superimpose the line of our model using the estimated coefficient
fitfake |> 
  ggplot(aes(x = X, y = y)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE)

# Add residuals error
fitfake |> 
  ggplot(aes(x = X, y = y)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = X, yend = .fitted), color = "red", 
               size = 0.5)

# Including measure of uncertainty
fitfake |> 
  ggplot(aes(x = X, y = y)) +
  geom_point() +
  stat_smooth(method = lm, se = TRUE) +
  geom_segment(aes(xend = X, yend = .fitted), color = "red", 
               size = 0.5)

# ASSUMPTIONS --------------------------------------------------------------

# ASSUMPTION 3: Linearity. residuals vs fitted:
plot(fitfake, 1)

# Extract the predict values
y_pred <- predict(fitfake)

# residuals vs predicted 
fitfake |> 
  ggplot(aes(x = y_pred, y = residuals(fitfake))) +
  geom_point() +
  geom_hline(yintercept = 0) 

# ASSUMPTION 4: Conditional distribution of errors, has a mean of 0. Hold when x is randomly assigned
# observational studies difficult and not a test whether the error is uncorrelated with unknown variables.


fitfake |> 
  ggplot(aes(x = X, y = residuals(fitfake))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sd(residuals(fitfake)), linetype = "dashed") +
  geom_hline(yintercept = -sd(residuals(fitfake)), linetype = "dashed")


# Assumption 5: Independence of the errors H0: No correlated
library(car)
durbinWatsonTest(fitfake)


# ASSUMPTION 6: Outliers & High leverage points
# Outliers: standardize residuals (residual / se) (greater thn 3 posible outlier. )
# Cook' distance scaled change in fitted values. 
# Cook' distance over 4/n

plot(fitfake, 4) # by default the 3 most influential
plot(fitfake, 4, id.n = 5)

plot(fitfake, 5)


# Formal test: Bonferroni Outlier Test
outlierTest(fitfake)

# Checking further the values
# Identify the influential cases let's say 3 times the mean. 
cooksD <- cooks.distance(fitfake)

influential <- cooksD[(cooksD > (3 * mean(cooksD)))]

# remove outliers
outliers <- myfakedata[names(influential), ]

myfakedata_without_outliers <- myfakedata |> anti_join(outliers)

fitfake_nonoutliers <-  lm(y ~ X, data = myfakedata_without_outliers)

plot(fitfake_nonoutliers, 4)
plot(fitfake_nonoutliers, 4, id.n = 5)
outlierTest(fitfake_nonoutliers)

# ASSUMPTION 7: Equal variance of errors. Scale-Location
plot(fitfake, 3)

# Formal test Breusch-Pagan test heteroscedasticity. 
library(lmtest)
bptest(fitfake) # H0 = No heteroscedasticity


# ASSUMPTION  8: Normality of residuals Q-Q
plot(fitfake, 2)


# Alternatives to plot()
library(ggfortify)

autoplot(fitfake)


# Multiple regression -----------------------------------------------------

# Defining the parameters. Simple linear regression
set.seed(1234)
N <- 100
K <- 10 # predictors
X <- array(runif(N*K, 0, 1), c(N, K)) # this is a matrix
# z <- sample(c(0,1), N, replace = TRUE)
a <- 1
b <- 1:K
# theta <- 5
sigma <- 2
error <- rnorm(N, 0, sigma)

y <- a + X %*% b + error # as X is a matrix %*% we need this operator

# As data frame
myfakedata_multiple <- data.frame(X = X, y = y)

# Fit the model
fitfake_multiple <- lm(y ~ X, data = myfakedata_multiple)

summary(fitfake_multiple)

# A3: Linearity. 
plot(fitfake_multiple, 1)

# Extract the predict values
y_pred <- predict(fitfake_multiple)

# residuals vs predicted 
fitfake_multiple |> 
  ggplot(aes(x = y_pred, y = residuals(fitfake_multiple))) +
  geom_point() +
  geom_hline(yintercept = 0) 

# A4: Conditional distribution of errors, has a mean of 0.
fitfake_multiple |> 
  ggplot(aes(x = myfakedata_multiple$X.1, y = residuals(fitfake_multiple))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sd(residuals(fitfake)), linetype = "dashed") +
  geom_hline(yintercept = -sd(residuals(fitfake)), linetype = "dashed")


# A5: Independence of errors
durbinWatsonTest(fitfake_multiple)

# A6: Outliers

plot(fitfake_multiple, 4)
plot(fitfake_multiple, 5)

# Formal test: Bonferroni Outlier Test
outlierTest(fitfake_multiple)

# Checking further the values
# Identify the influential cases let's say 3 times the mean. 
cooksD <- cooks.distance(fitfake_multiple)

influential <- cooksD[(cooksD > (3 * mean(cooksD)))]

# A7: Equal variance of errors. Scale-Location
plot(fitfake_multiple, 3)
bptest(fitfake_multiple) # H0 = No heteroscedasticity


# A8: Normality of residuals Q-Q
plot(fitfake_multiple, 2)

# Actual data -------------------------------------------------------------

# Load packages
library(tidytuesdayR)
library(tidyverse)

# Get the data
tuesdata <- tt_load('2023-03-21')

# Access data 
languages <- tuesdata[[1]]

# Exploring data 
glimpse(languages)

# Select variables of interest
languages_select <- languages |> 
  select(pldb_id, title,  number_of_users, number_of_jobs)

# Explore data 
summary(languages_select)

# Fit OLS model

fit <- lm(number_of_jobs ~ number_of_users, data = languages_select)

# Explore results
summary(fit)

# Fit of the model: 
fit |> 
  ggplot(aes(x = number_of_users, y = number_of_jobs)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = number_of_users, yend = .fitted), color = "red", 
               size = 0.5)

# A3: Linearity

plot(fit, 1)

# Extract the predict values
y_pred <- predict(fit)

# residuals vs predicted 
fit |> 
  ggplot(aes(x = y_pred, y = residuals(fit))) +
  geom_point() +
  geom_hline(yintercept = 0) 

# A4: Conditional distribution of errors, has a mean of 0 
y_pred <- predict(fit)

fit |> 
  ggplot(aes(x = y_pred, y = residuals(fit))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = sd(residuals(fit)), linetype = "dashed") +
  geom_hline(yintercept = -sd(residuals(fit)), linetype = "dashed")

# A5: Independence of errors
durbinWatsonTest(fit)

# A6: Outliers
plot(fit, 4)
plot(fit, 5)

cooksD <- cooks.distance(fit)

influential <- cooksD[(cooksD > (3 * mean(cooksD)))]

# remove outliers
outliers <- languages_select[names(influential), ]

languages_select_without_outliers <- languages_select |>
  anti_join(outliers)

fit_wiouth_outliers <- lm(number_of_jobs ~ number_of_users, data = languages_select_without_outliers)
plot(fit_wiouth_outliers, 4)
plot(fit_wiouth_outliers, 5)


# A7:vEqual variance of errors. Scale-Location
bptest(fit_wiouth_outliers)
plot(fit_wiouth_outliers, 3)

# A8: Normality of errors
plot(fit, 2)
