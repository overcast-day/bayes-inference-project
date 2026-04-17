suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(require(cmdstanr))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(gridExtra))
verbose=FALSE

data = read.csv("~/bayes-inference-project/data/heart.csv")
head(data)

# Clean data
heart_df <- na.omit(data)

# EDA
set.seed(1)
nvar = length(heart_df)
cat_list = list("Sex", "ChestPainType", "RestingECG", "ExerciseAngina", "ST_Slope")
plot_list <- lapply(names(heart_df)[1:nvar], function(col){
  if (col %in% cat_list){
    t <- tableGrob(table(heart_df$HeartDisease, heart_df[[col]]))
    arrangeGrob(t, 
                top = textGrob(col, gp=gpar(fontsize=12)),
                left = textGrob("Has Heart Disease?"),
                heights = unit(c(0.5, 0), "npc"),
                widths = unit(c(0.3, 0), "npc"))
  } else if(col != "HeartDisease"){
    ggplot(heart_df, aes(x = .data[[col]], y = HeartDisease)) +
      geom_point() +
      labs(
        x = col,
        y = "Has Heart Disease?")
  } else {
    return (NULL)
  }
})
plot_list <- plot_list[!sapply(plot_list, is.null)]
grid.arrange(grobs = plot_list, ncol = 2)

# Preprocess
heart_df <- heart_df %>%
  mutate(ST_Slope = case_when(
    ST_Slope == "Down"    ~ -1,
    ST_Slope == "Flat" ~ 0,
    ST_Slope == "Up"   ~ 1
  ))
# 0 is No and 1 as Yes
heart_df$ExerciseAngina <- as.numeric(as.factor(heart_df$ExerciseAngina))-1
# 0 is Female and 1 as Male
heart_df <- heart_df %>%
  mutate(Sex = case_when(
    Sex == "F" ~ 0,
    Sex == "M"   ~ 1
  ))

# Model 1 (Simple)
mod = cmdstan_model("~/bayes-inference-project/VI_Model1.stan")
fit.variational = mod$variational(
  seed = 1,
  refresh = 50,
  output_dir = "~/stan_out",
  algorithm = "meanfield",
  output_samples = 1000,
  data = list(
    n_patients = length(heart_df$HeartDisease),
    y = heart_df$HeartDisease,
    age = heart_df$Age,
    maxHR = heart_df$MaxHR,
    oldPeak = heart_df$Oldpeak,
    sex = heart_df$Sex,
    exAngina = heart_df$ExerciseAngina,
    stSlope1 = heart_df$ST_Slope,
    stSlope2 = heart_df$ST_Slope
  )        
)

# Error: Chain 1 stan::variational::advi::adapt_eta: All proposed step-sizes failed. 
# Your model may be either severely ill-conditioned or misspecified.
