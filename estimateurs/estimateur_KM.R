source("generation_echantillon/generation_echantillon.R")
source("utils.R")
fonction_KM<-function(df,t_star){
  df<-df[,c("tox_time","is_observed")]
  surv_object<-Surv(as.numeric(df$tox_time),event=df$is_observed)
  fit <- survfit(surv_object ~1, data = df)
  estimateur_survie<-1-tp.surv(fit,t_star)[1,][["surv"]]
  return(estimateur_survie)
}

##### TEST ####
set.seed(123)
df<-Generation_un_ech(n=10,lambda=0.5,p=0.33,k=1,t_star=6)
str(df)
estim_KM<-fonction_KM(df,t_star=6)
estim_KM

print("test")
# Alternative 

kaplan_meier_estimate <- function(df, t_star) {
  require(survival)
  # Subset the data frame to keep only the relevant columns
  df <- df[, c("tox_time", "is_observed")]
  # Create a survival object from the data frame
  surv_obj <- Surv(df$tox_time, df$is_observed)
  # Fit the Kaplan-Meier survival curve
  km_fit <- survfit(surv_obj ~ 1, data = df)
  # Estimate the survival probability at time t_star
  surv_prob <- 1 - summary(km_fit, times = t_star)$surv
  return(surv_prob)
}

# Example 
kaplan_meier_estimate(df, t_star = 6)


