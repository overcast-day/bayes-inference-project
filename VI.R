suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(gridExtra))
verbose=FALSE


#load the dataset
df <- read.csv("~/S405/bayes-inference-project/data/heart.csv", stringsAsFactors = FALSE)
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
mod <- cmdstan_model("~/S405/bayes-inference-project/stan/VI_Model1.stan", force_recompile = TRUE)

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



# --------------------------------
# Model 2 additional features
# --------------------------------
df$Age2 <- df$Age_std^2
df$Age_MaxHR <- df$Age_std * df$MaxHR_std


#New design matrix
X2 <- as.matrix(df[, c(
  "Age_std",
  "Sex_num",
  "MaxHR_std",
  "Oldpeak_std",
  "ExerciseAngina_num",
  "ST_Flat",
  "ST_Down",
  "Age2",
  "Age_MaxHR"
)])

y2 <- df$y

N2 <- nrow(X2)
K2 <- ncol(X2)

cat("N2 =", N2, "\n")
cat("K2 =", K2, "\n")

#Stan list for model 2

stan_data2 <- list(
  N = N2,
  K = K2,
  X = X2,
  y = as.integer(y2)
)

# Model 2
fit_vi_2 <- mod$variational(
  data = stan_data2,
  seed = 123,
  algorithm = "meanfield",
  draws = 2000
)

#Extracting posterior draws
draws_df2 <- fit_vi_2$draws(format = "df")
nrow(draws_df2)
head(draws_df2)

#Posterior summary for model 2
summary2 <- data.frame(
  term = c("alpha", paste0("beta[", 1:9, "]")),
  mean = c(
    mean(draws_df2$alpha),
    sapply(1:9, function(i) mean(draws_df2[[paste0("beta[", i, "]")]]))
  ),
  sd = c(
    sd(draws_df2$alpha),
    sapply(1:9, function(i) sd(draws_df2[[paste0("beta[", i, "]")]]))
  )
)

coef_names2 <- c(
  "Intercept",
  "Age_std",
  "Sex_num",
  "MaxHR_std",
  "Oldpeak_std",
  "ExerciseAngina_num",
  "ST_Flat",
  "ST_Down",
  "Age2",
  "Age_MaxHR"
)

summary2$variable <- coef_names2
summary2 <- summary2[, c("variable", "mean", "sd")]

print(summary2)

#Predicting probabilities for model 2
beta_mat2 <- as.matrix(draws_df2[, paste0("beta[", 1:9, "]")])

posterior_linpred2 <- sweep(
  beta_mat2 %*% t(X2),
  1,
  draws_df2$alpha,
  "+"
)

posterior_prob2 <- 1 / (1 + exp(-posterior_linpred2))

p_hat_vi_2 <- colMeans(posterior_prob2)

head(p_hat_vi_2)
summary(p_hat_vi_2)

#Brier score for model 2
brier2 <- mean((p_hat_vi_2 - y2)^2)
brier2

#Comparing brier scores
cat("Model 1 Brier:", brier, "\n")
cat("Model 2 Brier:", brier2, "\n")

#“The extended model including nonlinear and interaction terms resulted in only 
#marginal improvement in predictive performance, suggesting that the simpler 
#model already captures the key structure in the data.”


#Calibration plot for Model 1
library(ggplot2)

# Create bins
df_cal <- data.frame(
  p_hat = p_hat_vi_1,
  y = y
)

df_cal$bin <- cut(df_cal$p_hat, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

calibration <- df_cal %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(p_hat),
    actual = mean(y),
    count = n()
  )

# Plot
ggplot(calibration, aes(x = mean_pred, y = actual)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Plot (VI - Model 1)",
    x = "Predicted Probability",
    y = "Observed Frequency"
  ) +
  theme_minimal()


#POsterior distribution plot
library(bayesplot)

# Convert to matrix
posterior_mat <- as.matrix(draws_df[, c("alpha", paste0("beta[", 1:7, "]"))])

mcmc_areas(posterior_mat)

#Histogram of predicted probabilities
ggplot(data.frame(p_hat = p_hat_vi_1), aes(x = p_hat)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Predicted Probabilities (VI)",
    x = "Predicted Probability",
    y = "Count"
  ) +
  theme_minimal()


#Calibration all
df_cal1 <- data.frame(p_hat = p_hat_vi_1, y = y, model = "Model 1")
df_cal2 <- data.frame(p_hat = p_hat_vi_2, y = y2, model = "Model 2")

df_cal_all <- rbind(df_cal1, df_cal2)

df_cal_all$bin <- cut(df_cal_all$p_hat, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

library(dplyr)

calibration_all <- df_cal_all %>%
  group_by(model, bin) %>%
  summarise(
    mean_pred = mean(p_hat),
    actual = mean(y),
    .groups = "drop"
  )

library(ggplot2)

ggplot(calibration_all, aes(x = mean_pred, y = actual, color = model)) +
  geom_point(size = 3) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Comparison (Model 1 vs Model 2)",
    x = "Predicted Probability",
    y = "Observed Frequency"
  ) +
  theme_minimal()

