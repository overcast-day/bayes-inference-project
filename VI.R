suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(gridExtra))
verbose=FALSE


#load the dataset
df <- read.csv("data/heart.csv", stringsAsFactors = FALSE)
str(df)
head(df)

#Data Preprocessing

# Binary encoding
df$Sex_num <- ifelse(df$Sex == "M", 1, 0)
df$ExerciseAngina_num <- ifelse(df$ExerciseAngina == "Y", 1, 0)

# ST_Slope dummies
# Use "Up" as the reference category
df$ST_Flat <- ifelse(df$ST_Slope == "Flat", 1, 0)
df$ST_Down <- ifelse(df$ST_Slope == "Down", 1, 0)

# Standardize continuous predictors
df$Age_std <- as.numeric(scale(df$Age))
df$MaxHR_std <- as.numeric(scale(df$MaxHR))
df$Oldpeak_std <- as.numeric(scale(df$Oldpeak))

# Response
df$y <- df$HeartDisease

#data check
summary(df[, c("Age_std", "MaxHR_std", "Oldpeak_std",
               "Sex_num", "ExerciseAngina_num",
               "ST_Flat", "ST_Down", "y")])

#checking no Na
colSums(is.na(df[, c("Age_std", "MaxHR_std", "Oldpeak_std",
                     "Sex_num", "ExerciseAngina_num",
                     "ST_Flat", "ST_Down", "y")]))

# Building Model 1 Matrix
X1 <- as.matrix(df[, c("Age_std",
                       "Sex_num",
                       "MaxHR_std",
                       "Oldpeak_std",
                       "ExerciseAngina_num",
                       "ST_Flat",
                       "ST_Down")])

y <- df$y

N <- nrow(X1)
K <- ncol(X1)

dim(X1)
length(y)


#Stan Data list
stan_data1 <- list(
  N = N,
  K = K,
  X = X1,
  y = y
)

#Compiling the model
mod <- cmdstan_model("stan/VI_Model1.stan")

#Fitting the VI
fit_vi_1 <- mod$variational(
  data = stan_data1,
  seed = 123,
  algorithm = "meanfield",
  draws = 2000
)

#Extracting posteriror draws
library(posterior)

draws_df <- fit_vi_1$draws(format = "df")

head(draws_df)


#Posterior summary
summary <- data.frame(
  term = c("alpha", paste0("beta[", 1:7, "]")),
  mean = c(
    mean(draws_df$alpha),
    sapply(1:7, function(i) mean(draws_df[[paste0("beta[", i, "]")]]))
  ),
  sd = c(
    sd(draws_df$alpha),
    sapply(1:7, function(i) sd(draws_df[[paste0("beta[", i, "]")]]))
  )
)

print(summary)


#Labelling variables
coef_names <- c(
  "Intercept",
  "Age_std",
  "Sex_num",
  "MaxHR_std",
  "Oldpeak_std",
  "ExerciseAngina_num",
  "ST_Flat",
  "ST_Down"
)

summary$variable <- coef_names
summary <- summary[, c("variable", "mean", "sd")]

print(summary)


#Generating Predicted probabilities

# Extract beta matrix
beta_mat <- as.matrix(draws_df[, paste0("beta[", 1:7, "]")])

# Linear predictor
posterior_linpred <- sweep(
  beta_mat %*% t(X1),
  1,
  draws_df$alpha,
  "+"
)

# Convert to probabilities
posterior_prob <- 1 / (1 + exp(-posterior_linpred))

# Posterior mean predicted probability
p_hat_vi_1 <- colMeans(posterior_prob)

# Check
head(p_hat_vi_1)
summary(p_hat_vi_1)

#Brier score
brier <- mean((p_hat_vi_1 - y)^2)
brier

#Interpretation of Brier score
#“The VI-based model achieved a Brier score of 0.118, indicating strong 
#predictive performance and well-calibrated probability estimates.”

