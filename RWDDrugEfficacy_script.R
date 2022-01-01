# Install the following packages by typing at the R command prompt:
# install.packages("writexl")
# install.packages("MatchIt")
# install.packages("survival")
# install.packages("survminer")


# Import libraries
library(readxl)
library(writexl)
library(dplyr)
library(lmtest)     # Likelihood Ratio Test
library(MatchIt)    # Propensity Score Matching
library(survival)    # Survival analysis
library(survminer)


# Import data processed using Python
merged_df_dum <- read_excel("C:/Users/nanda/.spyder-py3/RWDDrugEfficacy/merged_df_dum.xlsx")


# Check data type
str(merged_df_dum)

# merged_df_dum$Bleeding_event <- as.factor(merged_df_dum$Bleeding_event)
merged_df_dum$treatment_variable_Drug_B <- as.factor(merged_df_dum$treatment_variable_Drug_B)
merged_df_dum$sex_2 <- as.factor(merged_df_dum$sex_2)

merged_df_dum$other_drugs_1_Yes <- as.factor(merged_df_dum$other_drugs_1_Yes)
merged_df_dum$other_drugs_2_Yes <- as.factor(merged_df_dum$other_drugs_2_Yes)
merged_df_dum$other_drugs_3_Yes <- as.factor(merged_df_dum$other_drugs_3_Yes)
merged_df_dum$other_drugs_4_Yes <- as.factor(merged_df_dum$other_drugs_4_Yes)
merged_df_dum$other_drugs_5_Yes <- as.factor(merged_df_dum$other_drugs_5_Yes)
merged_df_dum$other_drugs_6_Yes <- as.factor(merged_df_dum$other_drugs_6_Yes)
merged_df_dum$other_drugs_7_Yes <- as.factor(merged_df_dum$other_drugs_7_Yes)
merged_df_dum$other_drugs_8_Yes <- as.factor(merged_df_dum$other_drugs_8_Yes)

merged_df_dum$diagnosis_1_Yes <- as.factor(merged_df_dum$diagnosis_1_Yes)
merged_df_dum$diagnosis_2_Yes <- as.factor(merged_df_dum$diagnosis_2_Yes)
merged_df_dum$diagnosis_3_Yes <- as.factor(merged_df_dum$diagnosis_3_Yes)
merged_df_dum$diagnosis_4_Yes <- as.factor(merged_df_dum$diagnosis_4_Yes)
merged_df_dum$diagnosis_5_Yes <- as.factor(merged_df_dum$diagnosis_5_Yes)
merged_df_dum$diagnosis_6_Yes <- as.factor(merged_df_dum$diagnosis_6_Yes)
merged_df_dum$diagnosis_7_Yes <- as.factor(merged_df_dum$diagnosis_7_Yes)
merged_df_dum$diagnosis_8_Yes <- as.factor(merged_df_dum$diagnosis_8_Yes)
merged_df_dum$diagnosis_9_Yes <- as.factor(merged_df_dum$diagnosis_9_Yes)
merged_df_dum$diagnosis_10_Yes <- as.factor(merged_df_dum$diagnosis_10_Yes)
merged_df_dum$diagnosis_11_Yes <- as.factor(merged_df_dum$diagnosis_11_Yes)
merged_df_dum$diagnosis_12_Yes <- as.factor(merged_df_dum$diagnosis_12_Yes)
merged_df_dum$diagnosis_13_Yes <- as.factor(merged_df_dum$diagnosis_13_Yes)
merged_df_dum$diagnosis_14_Yes <- as.factor(merged_df_dum$diagnosis_14_Yes)
merged_df_dum$diagnosis_15_Yes <- as.factor(merged_df_dum$diagnosis_15_Yes)

merged_df_dum$Diag_Score_1_1 <- as.factor(merged_df_dum$Diag_Score_1_1)
merged_df_dum$Diag_Score_1_2 <- as.factor(merged_df_dum$Diag_Score_1_2)
merged_df_dum$Diag_Score_1_3 <- as.factor(merged_df_dum$Diag_Score_1_3)
merged_df_dum$Diag_Score_1_4 <- as.factor(merged_df_dum$Diag_Score_1_4)
merged_df_dum$Diag_Score_1_5 <- as.factor(merged_df_dum$Diag_Score_1_5)
merged_df_dum$Diag_Score_1_6 <- as.factor(merged_df_dum$Diag_Score_1_6)

merged_df_dum$Diag_Score_2_1 <- as.factor(merged_df_dum$Diag_Score_2_1)
merged_df_dum$Diag_Score_2_2 <- as.factor(merged_df_dum$Diag_Score_2_2)
merged_df_dum$Diag_Score_2_3 <- as.factor(merged_df_dum$Diag_Score_2_3)
merged_df_dum$Diag_Score_2_4 <- as.factor(merged_df_dum$Diag_Score_2_4)
merged_df_dum$Diag_Score_2_5 <- as.factor(merged_df_dum$Diag_Score_2_5)
merged_df_dum$Diag_Score_2_6 <- as.factor(merged_df_dum$Diag_Score_2_6)
merged_df_dum$Diag_Score_2_7 <- as.factor(merged_df_dum$Diag_Score_2_7)
merged_df_dum$Diag_Score_2_8 <- as.factor(merged_df_dum$Diag_Score_2_8)

str(merged_df_dum)    # Check data type(s) was/were changed correctly


# Scatterplots
plot(merged_df_dum$lab_1, merged_df_dum$age, main="Scatterplot", 
     xlab="lab_1", ylab="age")
abline(lm(age~lab_1, data=merged_df_dum), col="red")

plot(merged_df_dum$lab_6, merged_df_dum$age, main="Scatterplot", 
     xlab="lab_6", ylab="age")
abline(lm(age~lab_6, data=merged_df_dum), col="red")

plot(merged_df_dum$lab_7, merged_df_dum$age, main="Scatterplot", 
     xlab="lab_7", ylab="age")
abline(lm(age~lab_7, data=merged_df_dum), col="red")

plot(merged_df_dum$duration_in_years, merged_df_dum$age, main="Scatterplot", 
     xlab="duration_in_years", ylab="age")
abline(lm(age~duration_in_years, data=merged_df_dum), col="red")

plot(merged_df_dum$lab_6, merged_df_dum$lab_1, main="Scatterplot", 
     xlab="lab_6", ylab="lab_1")
abline(lm(lab_1~lab_6, data=merged_df_dum), col="red")

plot(merged_df_dum$lab_7, merged_df_dum$lab_1, main="Scatterplot", 
     xlab="lab_7", ylab="lab_1")
abline(lm(lab_1~lab_7, data=merged_df_dum), col="red")

plot(merged_df_dum$duration_in_years, merged_df_dum$lab_1, main="Scatterplot", 
     xlab="duration_in_years", ylab="lab_1")
abline(lm(lab_1~duration_in_years, data=merged_df_dum), col="red")

plot(merged_df_dum$lab_7, merged_df_dum$lab_6, main="Scatterplot", 
     xlab="lab_7", ylab="lab_6")
abline(lm(lab_6~lab_7, data=merged_df_dum), col="red")

plot(merged_df_dum$duration_in_years, merged_df_dum$lab_6, main="Scatterplot", 
     xlab="duration_in_years", ylab="lab_6")
abline(lm(lab_6~duration_in_years, data=merged_df_dum), col="red")

plot(merged_df_dum$duration_in_years, merged_df_dum$lab_7, main="Scatterplot", 
     xlab="duration_in_years", ylab="lab_7")
abline(lm(lab_7~duration_in_years, data=merged_df_dum), col="red")


# Identify outcome, exposure and covariates
# Y <- select(merged_df_dum, Bleeding_event)
# X <- select(merged_df_dum, treatment_variable_Drug_B)
# covariates <- select(merged_df_dum, -c(Bleeding_event, treatment_variable_Drug_B, patient_id, duration_in_years))


# Logistic Regression to check significance of covariate coefficient
# Hierarchical model building: Step-down/Backward selection with exit criteria: p-value>0.05
FullModel <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years))
LogReg_FullModel <- glm(treatment_variable_Drug_B~., family=binomial, data=FullModel)
summary(LogReg_FullModel)

W <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                              Diag_Score_2_1, Diag_Score_2_2, Diag_Score_2_3, Diag_Score_2_4, Diag_Score_2_5, Diag_Score_2_6, Diag_Score_2_7, Diag_Score_2_8))
LogReg_W <- glm(treatment_variable_Drug_B~., family=binomial, data=W)
summary(LogReg_W)

lrtest(LogReg_W, LogReg_FullModel)    # Likelihood Ratio Test for comparison of models

Z <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                              diagnosis_10_Yes, diagnosis_1_Yes, diagnosis_6_Yes, age, diagnosis_15_Yes))
LogReg <- glm(treatment_variable_Drug_B~., family=binomial, data=Z)
summary(LogReg)

V <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                              diagnosis_10_Yes, diagnosis_1_Yes, diagnosis_6_Yes, age, diagnosis_15_Yes, 
                              Diag_Score_1_1, Diag_Score_1_2, Diag_Score_1_3, Diag_Score_1_4, Diag_Score_1_5, Diag_Score_1_6))
LogReg_V <- glm(treatment_variable_Drug_B~., family=binomial, data=V)
summary(LogReg_V)

lrtest(LogReg_V, LogReg)    # Likelihood Ratio Test for comparison of models

Z <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                              diagnosis_10_Yes, diagnosis_1_Yes, diagnosis_6_Yes, age, diagnosis_15_Yes, diagnosis_2_Yes, 
                              other_drugs_6_Yes, other_drugs_5_Yes, sex_2, other_drugs_3_Yes, diagnosis_13_Yes, diagnosis_14_Yes))
LogReg <- glm(treatment_variable_Drug_B~., family=binomial, data=Z)
summary(LogReg)

Z_DiagScore1 <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                                         diagnosis_10_Yes, diagnosis_1_Yes, diagnosis_6_Yes, age, diagnosis_15_Yes, diagnosis_2_Yes, 
                                         other_drugs_6_Yes, other_drugs_5_Yes, sex_2, other_drugs_3_Yes, diagnosis_13_Yes, diagnosis_14_Yes, 
                                         Diag_Score_2_1, Diag_Score_2_2, Diag_Score_2_3, Diag_Score_2_4, Diag_Score_2_5, Diag_Score_2_6, Diag_Score_2_7, Diag_Score_2_8))
LogReg_Z_DiagScore1 <- glm(treatment_variable_Drug_B~., family=binomial, data=Z_DiagScore1)
summary(LogReg_Z_DiagScore1)

lrtest(LogReg_Z_DiagScore1, LogReg)    # Likelihood Ratio Test for comparison of models

Z_DiagScore2 <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                                         diagnosis_10_Yes, diagnosis_1_Yes, diagnosis_6_Yes, age, diagnosis_15_Yes, diagnosis_2_Yes, 
                                         other_drugs_6_Yes, other_drugs_5_Yes, sex_2, other_drugs_3_Yes, diagnosis_13_Yes, diagnosis_14_Yes, 
                                         Diag_Score_1_1, Diag_Score_1_2, Diag_Score_1_3, Diag_Score_1_4, Diag_Score_1_5, Diag_Score_1_6))
LogReg_Z_DiagScore2 <- glm(treatment_variable_Drug_B~., family=binomial, data=Z_DiagScore2)
summary(LogReg_Z_DiagScore2)

lrtest(LogReg_Z_DiagScore2, LogReg)    # Likelihood Ratio Test for comparison of models

Z_simple <- select(merged_df_dum, -c(Bleeding_event, patient_id, duration_in_years, 
                                     diagnosis_10_Yes, diagnosis_1_Yes, diagnosis_6_Yes, age, diagnosis_15_Yes, diagnosis_2_Yes, 
                                     other_drugs_6_Yes, other_drugs_5_Yes, sex_2, other_drugs_3_Yes, diagnosis_13_Yes, diagnosis_14_Yes, 
                                     Diag_Score_2_1, Diag_Score_2_2, Diag_Score_2_3, Diag_Score_2_4, Diag_Score_2_5, Diag_Score_2_6, Diag_Score_2_7, Diag_Score_2_8, 
                                     Diag_Score_1_1, Diag_Score_1_2, Diag_Score_1_3, Diag_Score_1_4, Diag_Score_1_5, Diag_Score_1_6))
LogReg_Z_simple <- glm(treatment_variable_Drug_B~., family=binomial, data=Z_simple)
summary(LogReg_Z_simple)

lrtest(LogReg_Z_simple, LogReg)    # Likelihood Ratio Test for comparison of models


# Propensity Score Matching
# distance: Propensity score (common support - units from both treatment groups dropped) estimated with logistic regression

# 1:1 Nearest Neighbour Matching without replacement
require(MatchIt)
NN <- matchit(treatment_variable_Drug_B~lab_1+lab_6+lab_7+other_drugs_1_Yes+other_drugs_2_Yes+other_drugs_4_Yes+other_drugs_7_Yes+other_drugs_8_Yes
              +diagnosis_3_Yes+diagnosis_4_Yes+diagnosis_5_Yes+diagnosis_7_Yes+diagnosis_8_Yes+diagnosis_9_Yes+diagnosis_11_Yes+diagnosis_12_Yes
              +Diag_Score_1_1+Diag_Score_1_2+Diag_Score_1_3+Diag_Score_1_4+Diag_Score_1_5+Diag_Score_1_6
              +Diag_Score_2_1+Diag_Score_2_2+Diag_Score_2_3+Diag_Score_2_4+Diag_Score_2_5+Diag_Score_2_6+Diag_Score_2_7+Diag_Score_2_8, 
              data=merged_df_dum, method="nearest", discard="both", ratio=1)
NN             # To check matching result
summary(NN)
plot(NN, type="jitter")    # Distribution of propensity scores
plot(NN, type="hist")
plot(NN, type="qq", which.xs=c('lab_1', 'lab_6', 'lab_7'))    # Covariate balance
plot(NN, type="ecdf", which.xs=c('other_drugs_1_Yes', 'other_drugs_2_Yes', 'other_drugs_4_Yes', 'other_drugs_7_Yes', 'other_drugs_8_Yes', 
                                 'diagnosis_3_Yes', 'diagnosis_4_Yes', 'diagnosis_5_Yes', 'diagnosis_7_Yes', 'diagnosis_8_Yes', 'diagnosis_9_Yes', 'diagnosis_11_Yes', 'diagnosis_12_Yes', 
                                 'Diag_Score_1_1', 'Diag_Score_1_2', 'Diag_Score_1_3', 'Diag_Score_1_4', 'Diag_Score_1_5', 'Diag_Score_1_6', 
                                 'Diag_Score_2_1', 'Diag_Score_2_2', 'Diag_Score_2_3', 'Diag_Score_2_4', 'Diag_Score_2_5', 'Diag_Score_2_6', 'Diag_Score_2_7', 'Diag_Score_2_8'))
plot(NN, type="density")
plot(summary(NN), var.order="unmatched")
NN_data <- match.data(NN)    # Matched data
write_xlsx(NN_data, "C:\\Users\\nanda\\R_Projects\\RWDDrugEfficacy\\PSM_NN.xlsx")    # Export matched data to excel

# Caliper Matching
ps_sd <- sd(NN_data$distance)
CM <- matchit(treatment_variable_Drug_B~lab_1+lab_6+lab_7+other_drugs_1_Yes+other_drugs_2_Yes+other_drugs_4_Yes+other_drugs_7_Yes+other_drugs_8_Yes
              +diagnosis_3_Yes+diagnosis_4_Yes+diagnosis_5_Yes+diagnosis_7_Yes+diagnosis_8_Yes+diagnosis_9_Yes+diagnosis_11_Yes+diagnosis_12_Yes
              +Diag_Score_1_1+Diag_Score_1_2+Diag_Score_1_3+Diag_Score_1_4+Diag_Score_1_5+Diag_Score_1_6
              +Diag_Score_2_1+Diag_Score_2_2+Diag_Score_2_3+Diag_Score_2_4+Diag_Score_2_5+Diag_Score_2_6+Diag_Score_2_7+Diag_Score_2_8, 
              data=merged_df_dum, method="nearest", discard="both", caliper=0.25*ps_sd)
CM             # To check matching result
summary(CM)
plot(CM, type="jitter")    # Distribution of propensity scores
plot(CM, type="hist")
plot(CM, type="qq", which.xs=c('lab_1', 'lab_6', 'lab_7'))    # Covariate balance
plot(CM, type="ecdf", which.xs=c('other_drugs_1_Yes', 'other_drugs_2_Yes', 'other_drugs_4_Yes', 'other_drugs_7_Yes', 'other_drugs_8_Yes', 
                                 'diagnosis_3_Yes', 'diagnosis_4_Yes', 'diagnosis_5_Yes', 'diagnosis_7_Yes', 'diagnosis_8_Yes', 'diagnosis_9_Yes', 'diagnosis_11_Yes', 'diagnosis_12_Yes', 
                                 'Diag_Score_1_1', 'Diag_Score_1_2', 'Diag_Score_1_3', 'Diag_Score_1_4', 'Diag_Score_1_5', 'Diag_Score_1_6', 
                                 'Diag_Score_2_1', 'Diag_Score_2_2', 'Diag_Score_2_3', 'Diag_Score_2_4', 'Diag_Score_2_5', 'Diag_Score_2_6', 'Diag_Score_2_7', 'Diag_Score_2_8'))
plot(CM, type="density")
plot(summary(CM), var.order="unmatched")
CM_data <- match.data(CM)    # Matched data
write_xlsx(CM_data, "C:\\Users\\nanda\\R_Projects\\RWDDrugEfficacy\\PSM_CM.xlsx")    # Export matched data to excel


# Kaplan-Meier Curve
attach(CM_data)
KMcurve <- survfit(Surv(duration_in_years, Bleeding_event)~treatment_variable_Drug_B)
KMcurve
summary(KMcurve, times=seq(0, 2, 0.05))
ggsurvplot(KMcurve, data=CM_data)
ggsurvplot(KMcurve, data=CM_data, title="Kaplan-Meier Curve",
           ylab="Survival Probability", surv.scale="percent",
           xlab="Time (years)", xlim=c(0, 2), break.x.by=0.1,
           risk.table=TRUE, risk.table.title="", risk.table.height=.20,
           legend.title="", legend.labs=c("Drug A", "Drug B"), palette=c("blue", "red"),
           pval=TRUE)
# Check plot
plot(KMcurve, main="Kaplan-Meier Curve", ylab="Survival Probability", xlab="Time (years)", col=c("blue", "red"), las=1, lwd=2, mark.time=TRUE)
legend(1.25,1.0, legend=c("Drug A", "Drug B"), lty=1, lwd=2, col=c("blue", "red"), bty="", cex=1)

# Log-Rank Test
# H0: Survival in both treatment groups is same
survdiff(Surv(duration_in_years, Bleeding_event)~treatment_variable_Drug_B)


# Cox Proportional Hazards Regression
CoxReg <- coxph(Surv(duration_in_years, Bleeding_event)~treatment_variable_Drug_B)
summary(CoxReg)

CoxReg_adj <- coxph(Surv(duration_in_years, Bleeding_event)~treatment_variable_Drug_B+lab_1+lab_6+lab_7+other_drugs_1_Yes+other_drugs_2_Yes+other_drugs_4_Yes+other_drugs_7_Yes+other_drugs_8_Yes
                    +diagnosis_3_Yes+diagnosis_4_Yes+diagnosis_5_Yes+diagnosis_7_Yes+diagnosis_8_Yes+diagnosis_9_Yes+diagnosis_11_Yes+diagnosis_12_Yes
                    +Diag_Score_1_1+Diag_Score_1_2+Diag_Score_1_3+Diag_Score_1_4+Diag_Score_1_5+Diag_Score_1_6
                    +Diag_Score_2_1+Diag_Score_2_2+Diag_Score_2_3+Diag_Score_2_4+Diag_Score_2_5+Diag_Score_2_6+Diag_Score_2_7+Diag_Score_2_8)
summary(CoxReg_adj)


