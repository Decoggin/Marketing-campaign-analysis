# Marketing-campaign-analysis


### Research Question

For this project I downloaded a dataset from kaggle
<https://www.kaggle.com/datasets/whenamancodes/customer-personality-analysis/data>
containing customer personality data. A detailed list of all 29 variables can be 
found by following the above link.


*The project aims were to answer:*

1)  Can the likelihood of a customer responding to a campaign be
    predicted by their demographic and purchasing behaviour?
2)  Is the relationship between campaign response and demographic /
    purchasing predictors moderated by income?

This project uses a logistic regression model with
moderation effects to investigate the research questions.

## Project Structure

- `campaign_analysis_script.R`: script containing the steps taken for data analysis
- `results/`: Outputs and figures
- `README.md`: Project description and instructions
- `marketing_campaign.csv`: The data 

## Requirements

To use the script in this repository:

1. Download the public domain dataset from <https://www.kaggle.com/datasets/whenamancodes/customer-personality-analysis/data>
2. Ensure the necessary environment and packages are installed (R version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit))

## Models and Methods 

### Exploratory data analysis 
Data was first explored using a variety of visualisation methods, including heatmaps, raincloud plots and proportional bar plots. 

### Data pre-processing
Steps include handling missing values, scaling numerical features and encoding categorical variables. 

### Logistic Regression
Logistic regression models were built to predict the binary response variable based on customer demographics and purchasing behaviours. 

### Model comparison
The performance package was used to evaluate model performance, using methods such as weighted AIC and BIC, and the spherical scoring rule (a measure of prediction accuracy). Model comparisons were also visualised. 

### Cross Validation 
The best performing model was cross validated. The data was split into training and testing sets with an 80:20 ratio


### Results 
#### Summary of Findings

- The final logistic regression model had an accuracy of 78% according to cross validation
- Customers income, number of web purchases and number of catalogue purchases positively influenced the likelihood they would respond to a marketing campaign. 
- The number of teens in the customers home negatively impacted the likelihood they would respond to a campaign, as did the number of store purchases they made. 
- The relationship between store and catalogue purchases and campaign response
was moderated by the customers income.

These findings could inform retailers on which customers to market
campaigns to in the future, focusing on those who make more web and
catalogue purchases. Additionally, efforts may be made to focus
campaigns towards customers with higher incomes who make more frequent
store purchases. Future market research may focus efforts on
investigating why having a lower number of teenagers in the home
influences customers to respond to marketing campaigns less.

### Limitations

Limitations of this analysis include the unbalanced sample of the data,
where only 605 customers responded to a campaign as opposed to 1610 who
did not respond. Whilst a balanced sample is not an assumption of
logistic regression, this could lead to biased parameter estimates and
inflated type 1 error rate. 

Furthermore, the accuracy of `moderating_model_2` was 78%. Whilst 78% indicates good performance, the model could not correctly predict whether a customer responded to a campaign 22% of the time. This suggests that other hidden factors may influence the likelihood of campaign response that warrant further
investigation.

### License
This project is open source and available under the MIT license

### Contact Info
Please contact me via email for any queries or contributions to the project. I would love to hear from you: daisybella13@hotmail.co.uk

