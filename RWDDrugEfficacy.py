# -*- coding: utf-8 -*-
"""

@author: nanda
"""


# Import libraries
import pandas as pd
import numpy as np
import scipy.stats as stats
import statsmodels.stats.proportion as prop
import statsmodels.api as sm
import matplotlib.pyplot as plot
# from sklearn.linear_model import LogisticRegression


# Import datasets
pat_char = pd.read_csv (r'C:\Users\nanda\Desktop\Patient_characteristics.csv')
pat_char.patient_id.nunique()       # Check unique patient records
event_dur = pd.read_csv (r'C:\Users\nanda\Desktop\Event_duration.csv')
event_dur.patient_id.nunique()      # Check unique patient records
event_dur.rename(columns={'Bleeding_event (1=event, 0=censored)':'Bleeding_event'}, inplace=True)


# Merge datasets
event_dur = event_dur.drop(columns=['treatment_variable'])

merged_df = pat_char.merge(event_dur, how='left', on='patient_id')


# Create dummy variables for categorical variables
merged_df.dtypes        # Data type of each variable
merged_df['sex'] = merged_df['sex'].astype(str)     # Change from integer to string in order to create dummy variables
merged_df['Diag_Score_1'] = merged_df['Diag_Score_1'].astype(str)     # Ordinal variable hence change from integer to string in order to create dummy variables
merged_df['Diag_Score_2'] = merged_df['Diag_Score_2'].astype(str)     # Ordinal variable hence change from integer to string in order to create dummy variables
merged_df['Bleeding_event'] = merged_df['Bleeding_event'].astype(np.uint8)      # For identification of binary variables later
merged_df.dtypes        # Check data type(s) was/were changed correctly

merged_df_dum = pd.get_dummies(data=merged_df, drop_first=True)


# Check missing data
print(merged_df_dum.count(axis=0))      # Check missing data
merged_df_dum = merged_df_dum.drop(['lab_2', 'lab_3', 'lab_4', 'lab_5', 'lab_8'], axis=1)       # Drop variables "lab_2", "lab_3", "lab_4", "lab_5", "lab_8" as missing data >20%
merged_df_dum = merged_df_dum.dropna()      # Drop records with missing data in any variable
merged_df_dum.to_excel("merged_df_dum.xlsx", index=False)        # Export data to excel for use in R


# Descriptive statistics
# summary = pd.DataFrame({"DataType":merged_df_dum.dtypes,
#                         "Obs":merged_df_dum.count(axis=0),
#                         "Mean":merged_df_dum.mean(axis=0, skipna=True),
#                         "Std Dev":merged_df_dum.std(axis=0, skipna=True),
#                         "Min":merged_df_dum.min(axis=0, skipna=True),
#                         "Max":merged_df_dum.max(axis=0, skipna=True)})
summary = merged_df_dum.describe()
summary.to_excel("summary.xlsx")        # Export descriptive statistics to excel


# Tabulation of binary variables
BinaryVar_list = list(merged_df_dum.select_dtypes(np.uint8).head())        # List of binary variables
tabulation = pd.DataFrame({"S/N":[0,1]})        # Create empty dataframe with two rows and column "S/N"
for i in BinaryVar_list:
    x = pd.DataFrame({i:merged_df_dum[i].value_counts()})
    tabulation = tabulation.merge(x, how='left', left_index=True, right_index=True)
    tabulation["%"+i] = tabulation[i]/tabulation[i].sum()*100
tabulation = tabulation.drop(columns=['S/N'])       # Drop column "S/N"
tabulation.to_excel("tabulation.xlsx")        # Export tabulation of binary variables to excel


# Check differences between 2 treatment groups
Drug_A = merged_df_dum.loc[merged_df_dum['treatment_variable_Drug_B']==0]       # Split into 2 treatment groups
Drug_B = merged_df_dum.loc[merged_df_dum['treatment_variable_Drug_B']==1]

# 2-sample t-test and Mann-Whitney U test for numeric variables
NumVar_list = list(merged_df_dum.select_dtypes(exclude=np.uint8).head())        # List of numeric variables
NumVar_list.remove('patient_id')
for j in NumVar_list:    
    levene_test = stats.levene(Drug_A[j], Drug_B[j], center='median', proportiontocut=0.05)     # Test for equality of variances
    if levene_test.pvalue<0.05:
        ttest = stats.ttest_ind(Drug_A[j], Drug_B[j], equal_var=False, alternative='two-sided')
    else:
        ttest = stats.ttest_ind(Drug_A[j], Drug_B[j], equal_var=True, alternative='two-sided')
    
    mannwhitneyu_test = stats.mannwhitneyu(Drug_A[j], Drug_B[j], alternative='two-sided')
       
    y = pd.DataFrame({"treatment_variable_Drug_B":merged_df_dum['treatment_variable_Drug_B'].value_counts(),
                      "Mean_Drug_A":Drug_A[j].mean(),
                      "Mean_Drug_B":Drug_B[j].mean(),
                      "Mean_Diff":Drug_A[j].mean()-Drug_B[j].mean(),
                      "ttest_pvalue":ttest.pvalue,
                      "ttest_Significance":"Significant" if ttest.pvalue<0.05 else "Not Significant",
                      "Median_Drug_A":Drug_A[j].median(),
                      "Median_Drug_B":Drug_B[j].median(),
                      "mannwhitneyu_test_pvalue":mannwhitneyu_test.pvalue,
                      "mannwhitneyu_test_Significance":"Significant" if mannwhitneyu_test.pvalue<0.05 else "Not Significant",
                      "Min_Drug_A":Drug_A[j].min(),
                      "Max_Drug_A":Drug_A[j].max(),
                      "Min_Drug_B":Drug_B[j].min(),
                      "Max_Drug_B":Drug_B[j].max()})
    y.to_excel("numeric_"+str(j)+".xlsx")      # Export 2-sample t-test and Mann-Whitney U test results for each numeric variable


# 2-sample z-test of proportions for binary variables
for k in BinaryVar_list:
    successes = np.array([Drug_A[k].sum(), Drug_B[k].sum()])
    samples = np.array([len(Drug_A.index), len(Drug_B.index)])
    prtest = prop.proportions_ztest(count=successes, nobs=samples, value=0, alternative='two-sided', prop_var=False)
       
    z = pd.DataFrame({"Drug":["A", "B"],
                      "Success":successes,
                      "Sample":samples,
                      "Proportion":successes/samples,
                      "Proportion_Diff":Drug_A[k].sum()/len(Drug_A.index)-Drug_B[k].sum()/len(Drug_B.index),
                      "pvalue":prtest[1],
                      "Significance":"Significant" if prtest[1]<0.05 else "Not Significant"})
    z.to_excel("binary_"+str(k)+".xlsx")      # Export 2-sample z-test of proportions result for each binary variable





#### Done in R ####

# Identify outcome, exposure and covariates
Y = merged_df_dum.loc[:, 'Bleeding_event']
X = merged_df_dum.loc[:, 'treatment_variable_Drug_B']
covariates = merged_df_dum.drop(columns=['Bleeding_event', 'treatment_variable_Drug_B', 'patient_id', 'duration_in_years'])


# Logistic Regression to check significance of covariate coefficient
# LogReg = LogisticRegression()
# LogReg.fit(covariates, X)
LogReg = sm.Logit(X, covariates).fit()
print(LogReg.summary())


# Propensity Score Matching


