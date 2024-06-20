library(tidyverse)
library(visdat)
library(reshape2) # for melting
library(clusterSim) # for cluster gap analysis
library(vcd) # for mosaic plot
library(ggrain) # rainplots
library(psych)
library(effects)
library(performance)
library(DescTools) # for Durbin Watson test
library(car) # for VIF
library(MASS) # for extracting distribution shape for data sim
library(rockchalk) # for slopes interactions
library(broom)
library(tibble)
library(knitr)
library(pixiedust)
library(kableExtra)
library(ggokabeito)



marketing_data <- read.delim("marketing_campaign.csv")



################
# DATA TIDYING #
################

vis_miss(marketing_data)

# Income has missing values, Z_costcontact and Z revenue are both constant
# Filtered out misisng income and Z variables, chr variables to factors
# also mutating new vars: total spend and campaign response

tidied_marketing_data <- marketing_data %>%
  filter(!is.na(Income)) %>%
  dplyr::select(!c(Z_CostContact, Z_Revenue)) %>%
  mutate(Education = factor(Education), Marital_Status = factor(Marital_Status)) %>%
  mutate(total_spend = MntWines + MntFruits + MntMeatProducts + MntFishProducts +MntSweetProducts +MntGoldProds) %>%
  mutate(campaign_response = ifelse(AcceptedCmp1 == 1 | AcceptedCmp2 == 1 |
                                      AcceptedCmp3== 1 | AcceptedCmp4 == 1 |
                                      AcceptedCmp5 == 1 | Response == 1, 1, 0))



#############################
# EXPLORATORY DATA ANALYSIS #
#############################

heatmap_data <- tidied_marketing_data %>%
  select_if(is.numeric) %>%
  cor() %>%
  melt()


numeric_data <- select_if(tidied_marketing_data, is.numeric) %>%
  cor() %>%
  melt()


ggplot(numeric_data, aes(x= Var1, y = Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(high = "blue", low = "white") +
  labs(title = "Heatmap for Numeric Variables") +
  theme(axis.text.x = element_text(angle = 90))


# can we predict campaign response by purchasing and spending patterns?




##################################
# PURCHASING BEHAVIOUR VARIABLES #
##################################

summary_data <- summary(tidied_marketing_data[c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth", "total_spend")])
summary_data


# Exploring the relationship of campaign response and purchasing types 

purchasing <- (c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth", "total_spend"))

plot_list_purchasing <-list()

for (p in purchasing) {
  plot <- tidied_marketing_data %>%
    mutate(campaign_response = factor(campaign_response)) %>%
    ggplot(aes(x = campaign_response, y = !!sym(p), colour = campaign_response)) +
    geom_rain(point.args = rlang::list2(size = 1, alpha = 0.2)) +
    guides(colour = FALSE) +
    coord_flip() +
    labs(title = paste("Distribution of", p, "by campaign response")) +
    theme_minimal() +
    scale_colour_okabe_ito()
  
  
  plot_list_purchasing[[p]] <- plot
}

plot_list_purchasing

#########################
# DEMOGRAPHIC VARIABLES #
#########################

# exploring the relationship of campaign response and demographic variables
# most respondents have 0 kids or teens at home so boxplots / rain plots are less informative

# for teen home
count_data_teen <- tidied_marketing_data %>%
  mutate(campaign_response = factor(campaign_response)) %>%
  group_by(Teenhome, campaign_response) %>%
  summarise(count = n()) %>%
  ungroup()


count_data_teen %>%
ggplot(aes(x = Teenhome, y = campaign_response, size = count, fill = count)) +
  geom_point(shape = 21, color = "black") +
  scale_size_continuous(range = c(1, 10)) +
  scale_fill_viridis_c(name = "Count") +
  theme_minimal() +
  labs(x = "Teenhome", y = "Campaign Response",
  title = "Number of Teens at Home vs Customers Campaign Response")

# for kidhome
count_data_kid <- tidied_marketing_data %>%
  mutate(campaign_response = factor(campaign_response)) %>%
  group_by(Kidhome, campaign_response) %>%
  summarise(count = n()) %>%
  ungroup()

count_data_kid %>%
ggplot(aes(x = Kidhome, y = campaign_response, size = count, fill = count)) +
  geom_point(shape = 21, color = "black") +
  scale_size_continuous(range = c(1, 10)) +
  scale_fill_viridis_c(name = "Count") +
  theme_minimal() +
  labs(x = "Kidhome", y = "Campaign Response", 
       title = "Number of Kids at Home vs Customers Campaign Response")


# year of birth
tidied_marketing_data %>%
  mutate(campaign_response = factor(campaign_response)) %>%
  ggplot(aes(x = campaign_response, y = Year_Birth, colour = campaign_response)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  geom_jitter(alpha = 0.2, width = 0.2) +
  guides(colour = FALSE) +
  coord_flip() +
  labs(title = "Distribution of Birth Year across Campaign Response") +
  theme_minimal() + 
  scale_colour_okabe_ito()

# year of birth = little difference
# Kidhome = those who said no to all campiagns had more children on average
# Teenhome = those who said no to all campaigns had more children on average
# Id like to include kid and teen home in the analysis


#########################
# CATEGORICAL VARIABLES #
#########################

# Tidying levels of Education and Marital Status

tidied_marketing_data_categorical <- tidied_marketing_data %>%
  mutate(Marital_Status = dplyr::recode(Marital_Status, "Alone" = "Single")) %>%
  mutate(Education = dplyr::recode(Education, "2n Cycle" = "Master")) %>%
  filter(Marital_Status != "Absurd" & Marital_Status != "YOLO") %>%
  droplevels()

unique(tidied_marketing_data_categorical$Marital_Status)


# Mosaic Plot

mosaic(
  xtabs(~ Marital_Status + Education, data = tidied_marketing_data_categorical),
  main = "Mosaic Plot of Marital Status and Education",
  highlighting = "Education",  # Color by Education
  highlighting_fill = c("lightblue", "lightgreen", "lightyellow", "lightpink"),
  labeling_args = list(las = 2, cex = 0.8)
)


# How does Campaign Response change with the levels of Education and Marital Status?

describeBy(tidied_marketing_data_categorical$campaign_response, 
           group = tidied_marketing_data_categorical$Education, 
           mat = TRUE)


describeBy(tidied_marketing_data_categorical$campaign_response, 
           group = tidied_marketing_data_categorical$Marital_Status, 
           mat = TRUE)


# Proportion Bar Graphs for both variables with Campaign Response 

categorical <- (c("Education", "Marital_Status"))

plot_list3 <-list()

for (c in categorical) {
  plot <- tidied_marketing_data_categorical %>%
    mutate(campaign_response = factor(campaign_response)) %>%
    ggplot(aes(x = !!sym(paste0(c)), fill = campaign_response)) +
    geom_bar(position = "fill") + 
    labs(title = paste("Proportion of Campaign Response by",c,"Level")) +
    scale_fill_manual(values = c("0" = "pink", "1" = "blue"))  +
    theme_minimal()
  
  plot_list3[[c]] <- plot
}

plot_list3



###################################
# INCOME AS A MODERATING VARIABLE #
###################################


boxplot(tidied_marketing_data_categorical$Income)

# THERE IS AN EXTREME OUTLIER IN INCOME

tidied_marketing_data_categorical %>%
  summarise(max_income = max(Income))
# 666666 is an outlier 

tidied_marketing_data_666666removed <- tidied_marketing_data_categorical %>%
  filter(Income!= 666666)



# visualising Income and Campaign Response 

tidied_marketing_data_666666removed %>%
  mutate(campaign_response = factor(campaign_response)) %>%
  ggplot(aes(x = Income, y = campaign_response, colour = campaign_response)) +
  geom_jitter(alpha= 0.2, height = 0.2) +
  geom_boxplot(alpha = 0.2) +
  guides(colour = FALSE) +
  coord_flip() +
  labs(title = "Distribution of Income Across Campaign Response") +
  theme_minimal() +
  scale_colour_okabe_ito()


# people responding to campaigns have on average higher incomes
# Is this due to more spending?
  
tidied_marketing_data_666666removed %>%
  ggplot(aes(x= Income, y = total_spend)) +
  geom_jitter(alpha = 0.5, colour = "pink") +
  geom_smooth(method = "lm", colour = "blue") +
  labs(title = "The Relationship Between Customer's Total Spend and Income") +
  theme_minimal()



# Visualising Income and Purchasing Behaviours

purchasing <- (c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases", "NumWebVisitsMonth", "total_spend"))

plot_list_purchasing_income <-list()

for (p in purchasing) {
  plot <- tidied_marketing_data_666666removed %>%
    mutate(campaign_response = factor(campaign_response)) %>%
    ggplot(aes(x = Income, y = !!sym(p), colour = campaign_response)) +
    geom_jitter(alpha = 0.2) +
    scale_colour_manual(name = "Response",
                          values = c("1" = "blue", "0" = "red"),
                          labels = c("1" = "Responded", "0" = "Did not Respond")) +
    labs(title = paste("The Relationship Between",p,"and Income Across Campaign Response Groups")) +
    theme_minimal()
  
  plot_list_purchasing_income[[p]] <- plot
}

plot_list_purchasing_income

# Moderating effect of Income observed
# higher income suggests less web visits - and more likely to respond to campaign
# higher income suggests more store purchases, catalog purchases, web purchases -  and more likely to respond to campaign (to be expected)
# u shaped relationship with income and deal purchases


# Visualising demographic variables and Income

demographic <- (c("Teenhome", "Kidhome", "Education", "Marital_Status"))

plot_list_demographic_income <-list()

for (d in demographic) {
  plot <- tidied_marketing_data_666666removed %>%
    mutate(campaign_response = factor(campaign_response),
           Teenhome = factor(Teenhome),
           Kidhome = factor(Kidhome)) %>%
    ggplot(aes(x = Income, y = campaign_response, colour = !!sym(d))) +
    geom_jitter(alpha = 0.5, height = 0.2) +
    labs(title = paste("The Relationship Between",d,"and Income Across Campaign Response Groups"))
  
  
  plot_list_demographic_income[[d]] <- plot
}

plot_list_demographic_income


#####################################
# Preparing data for model building #
#####################################


# centring and scaling variables - due to looking at interaction terms 


predictors <- c("NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", 
                "NumStorePurchases", "Kidhome", "Teenhome", "Income")


scaled_data <- tidied_marketing_data_categorical

scaled_data[predictors] <- scale(scaled_data[predictors])

# Check summary statistics of the scaled predictor variables
summary(scaled_data[predictors])




# contrast coding of categorical variables

contrasts(scaled_data$Marital_Status) # reference category is single
contrasts(scaled_data$Education) 

# changing reference category of Education to Basic 

scaled_data$Education <- relevel(scaled_data$Education, ref = "Basic")




##################
# Model building #
##################


# predictors: 
# purchasing behaviours: "NumDealsPurchases", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases"
# demographic: "Kid home" Teen home" "Education" "Marital-status"
# moderating variable: Income



# splitting into test and train

set.seed(123)
test_size <- 0.2
train_index <- createDataPartition(scaled_data$campaign_response, p = 1 - test_size, list = FALSE)
train_data <- scaled_data[train_index, ]
test_data <- scaled_data[-train_index, ]


# building a maximal model 

Maximal_model <- glm(campaign_response ~ NumDealsPurchases + NumWebPurchases + 
                NumCatalogPurchases + NumStorePurchases + Kidhome + Teenhome 
              + Education + Marital_Status + Income, data = train_data, 
              family = binomial)
                
summary(Maximal_model)

# null model

null_model <- glm(campaign_response ~ 1, data = train_data, family = binomial)

summary(null_model)


# step-wise selection of variables to include in the model 

stepAICmodel <- step(null_model, scope = list (upper = Maximal_model), direction = "both")

# catalog purchases, web purchases, store purchases, teen home, income and marital status
# so deals purchased, education, kid home have been removed based on AIC values 

summary(stepAICmodel)


# Assumption check for stepAICmodel


# Assumptions of a Binomial model:
# the data is independent 
# the dependent variable is binomial 
# linear relationship between the transformed expected response and explanatory variables
# independent errors (but not normally distributed)
# multicollinearity
# parameter estimated needs to use Maximum Likelihood Estimation (MLE) not OLS (oridnary least squares)



# Residuals vs. Fitted Values Plot -  checks for linearity
plot(stepAICmodel, which = 1)  

# Q-Q Plot
plot(stepAICmodel, which = 2) 

# Obtain residuals from the model
residuals <- residuals(stepAICmodel)

# Plot autocorrelation
acf(residuals)

# Durbin-Watson test
DurbinWatsonTest(stepAICmodel) # no significant evidence of autocorrelation

# checking for multi-collinearity
vif(stepAICmodel)




# MODERATING MODEL

# building a model with interaction terms for each predictor

moderating_model <- glm(campaign_response ~  NumWebPurchases * Income + 
                          NumCatalogPurchases * Income + 
                          NumStorePurchases * Income + 
                          Teenhome * Income 
                          + Marital_Status * Income, data = train_data, 
                          family = binomial)

summary(moderating_model)


# significant interactions:
# income and catalog purchases
# income and number of store purchases
# income and teen home
# NOT web purchases or marital status 



# Assumption Check

# Residuals vs. Fitted Values Plot
plot(moderating_model, which = 1)  

# Q-Q Plot
plot(moderating_model, which = 2)

# Obtain residuals from the model
mod_residuals <- residuals(moderating_model)

# Plot autocorrelation function
acf(mod_residuals)

# Durbin-Watson test
DurbinWatsonTest(moderating_model) 

# checking for multicollinearity
vif(moderating_model)



# MODERATING MODEL 2 

# interaction effects included for only the significant interactions
# main effects included for web purchases and teen home

moderating_model_2 <- glm(campaign_response ~ Income 
                          + NumWebPurchases
                          + Teenhome 
                          + NumCatalogPurchases * Income 
                          + NumStorePurchases * Income, 
                          data = train_data, 
                          family = binomial)

summary(moderating_model_2)

                          
# all three interactions are significant
# Income and number of web purchases significant main effects
# Only one level of Marital status significant



# Assumption Check 

# Residuals vs. Fitted Values Plot
plot(moderating_model_2, which = 1) 

# Q-Q Plot
plot(moderating_model_2, which = 2) 

# Obtain residuals from the model
mod_residuals <- residuals(moderating_model_2)

# Plot autocorrelation function (ACF)
acf(mod_residuals)

# Durbin-Watson test
DurbinWatsonTest(moderating_model_2) 

# checking for multicollinearity
vif(moderating_model_2) 


# MODERATING MODEL 3 
# contains only the interaction terms for income and number of store and catalogue purchases
# No main effects - does the moderating effect of Income better predict campaign response?

moderating_model_3 <- glm(campaign_response ~ Income +
                            NumCatalogPurchases * Income + 
                            NumStorePurchases * Income, 
                          data = train_data, 
                          family = binomial)

summary(moderating_model_3) 


# Assumption check


# Residuals vs. Fitted Values Plot 
plot(moderating_model_3, which = 1)  

# Q-Q Plot
plot(moderating_model_3, which = 2) 

# Obtain residuals from the model
mod_residuals <- residuals(moderating_model_3)

# Plot autocorrelation function (ACF) of the residuals
acf(mod_residuals)

# Durbin-Watson test
DurbinWatsonTest(moderating_model_3) 

# checking for multicollinearity
vif(moderating_model_3)



####################
# MODEL COMPARISON #
####################

# using performance package 

performance <- compare_performance(stepAICmodel, moderating_model, moderating_model_2,
                                   moderating_model_3)
performance

plot(performance)

# together this shows the moderating model with interactions and main effects 
# (moderating_model_2) explains the data best while maintaining the trade off between overfitting
# and explaining variation - weighted AIC is more robust and reliable than AIC 




################
# INTERACTIONS #
################

# Interaction plots

interactions <- c("NumCatalogPurchases", "NumStorePurchases")


for (i in interactions) {
  interaction_effect <- allEffects(moderating_model_2)[paste("Income:", i, sep = "")]
  
  plot(interaction_effect, main = paste("Interaction Effect between Income and", i),
       xlim = c(0, max(interaction_effect$Income1)))
}



# Plotting slopes
  plotSlopes(moderating_model_2, plotx = "NumStorePurchases", modx = "Income", 
             modxVals = "std.dev")
  
  plotSlopes(moderating_model_2, plotx = "NumCatalogPurchases", modx = "Income", 
             modxVals = "std.dev")


###########
# RESULTS #
###########
  

# Interpretation of effect sizes

# Calculate confidence intervals for coefficients
conf_intervals <- confint(moderating_model_2)

# Extract model coefficients, p-values, and confidence intervals
model_summary <- tidy(moderating_model_2)

# combine model_summary and confidence intervals
table_data <- bind_cols(model_summary, conf_intervals)

# adjusting the column names
colnames(table_data) <- c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")




# table of coefficients and confidence intervals

dust(table_data) %>%
  sprinkle(cols = c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high"), round = 3) %>%
  sprinkle(col = "p.value", fn = quote(pvalString(value))) %>%
  sprinkle_colnames(estimate = "Estimte",
                    std.error = "Std Error",
                    statistic = "Statistic",
                    p.value = "P value",
                    conf.low = "conf. low",
                    conf.high = "conf. high") %>%
  kable() %>%
  kable_styling()




# Odds Ratios

# Extract the coefficient estimates from the summary output
coef_summary <- summary(moderating_model_2)$coefficients

# Extract the coefficient estimates and their corresponding names
coef_estimates <- coef_summary[, "Estimate"]

# Exponentiate the coefficients to obtain the odds ratios
odds_ratios <- exp(coef_estimates)

# Print the odds ratios
print(odds_ratios)


####################
# CROSS-VALIDATION #
####################

# Predict using the test data
predictions <- predict(moderating_model_2, newdata = test_data, type = "response")

binary_predictions <- ifelse(predictions > 0.5, 1, 0)


# obtain a vector with campaign response for the test data
campaign_response <- test_data$campaign_response

# Calculate accuracy, finding the mean of were the prediction = the true response value
accuracy <- mean(binary_predictions == campaign_response)

# Print accuracy
cat("Accuracy:", accuracy, "\n")




