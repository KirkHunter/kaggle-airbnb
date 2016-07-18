# R scripts for the Kaggle Airbnb competition

These scripts may not contain the most elegant R code ever written, 
and it could use some DRYing up, but they illustrate my methodology 
in the competition as well as how to build logistic regression and 
random forest models in R.

airbnb.R was used for feature engineering whereby I used 
empirical probability estimates for a given destination as a feature 
in the logistic regression model.

calculate_ndcg.R is my script which allows me to reproduce the unique 
scoring system used for this problem. More on NDCG here https://www.kaggle.com/wiki/NormalizedDiscountedCumulativeGain

train.R shows my complete model building process all the way through
creation of the submission file. The models in this script produce my 
final predictions.

