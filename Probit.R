library(tidyverse)
library(dplyr)
library(psych)
library(mfx)
library(fastDummies)
# ----
# Import gathered data
head(data)
# ----
# Define customer data
individuals_data <- data %>% dplyr::select(customer_id, time_entered_model, threshold_value,
                                           number_of_neighbours, change_group, state_init, 
                                           state_after, times_adopted, negative_experiences)
rownames(individuals_data) <- individuals_data[,1]
individuals_data[,1] <- NULL
str(individuals_data)
head(individuals_data)
customer_data <- individuals_data %>% filter(individuals_data$state_init=="susceptible")
customer_data <- customer_data %>% dplyr::select(time_entered_model, threshold_value,
                                                 number_of_neighbours, change_group,
                                                 state_after, times_adopted, negative_experiences)
# Prepare for probit model
head(customer_data)
# Change continuous variables to categories and then to dummy variables
## time_entered_model
customer_data$time_entered_model_cat <- cut(customer_data$time_entered_model, 
                                            seq(0,20,4), right=FALSE,labels=c(1:5))

## threshold_value
customer_data$threshold_value_cat <- cut(customer_data$threshold_value, 
                                         c(0,0.2,0.4,0.6,0.8,1),labels=c(1:5))
## number_of_neighbours
customer_data$number_of_neighbours_cat <- cut(customer_data$number_of_neighbours, 
                                              c(0,5,7,14), right=FALSE, labels=c(1:3))
## negative_experiences
customer_data$negative_experiences_cat <- cut(customer_data$negative_experiences, 
                                              c(0,1,50,100), right=FALSE, labels=c(1:3))
summary(customer_data$negative_experiences_cat)
head(customer_data,2)
# Create dummy variables from categorical ones
customer_data <- dummy_cols(customer_data, select_columns = 'time_entered_model_cat')
customer_data <- dummy_cols(customer_data, select_columns = 'threshold_value_cat')
customer_data <- dummy_cols(customer_data, select_columns = 'number_of_neighbours_cat')
customer_data <- dummy_cols(customer_data, select_columns = 'negative_experiences_cat')
customer_data$change_group <- recode(customer_data$change_group,
                                     "cannot_change" = 1,
                                     "can_change" = 0)
customer_data$state_after <- recode(customer_data$state_after,
                                    "not_changed" = 0,
                                    "changed" = 1)
map(customer_data, ~sum(is.na(.)))
customer_data_binary <- customer_data %>% dplyr::select(change_group, state_after,
                                                        time_entered_model_cat_1,time_entered_model_cat_2,
                                                        time_entered_model_cat_3, time_entered_model_cat_4,
                                                        time_entered_model_cat_5, threshold_value_cat_1,
                                                        threshold_value_cat_2, threshold_value_cat_3,
                                                        threshold_value_cat_4, threshold_value_cat_5,
                                                        number_of_neighbours_cat_1, number_of_neighbours_cat_2,
                                                        number_of_neighbours_cat_3, negative_experiences_cat_1,
                                                        negative_experiences_cat_2, negative_experiences_cat_3)
colnames(customer_data_binary) <- c('V09', 'state_after','V06_1','V06_2','V06_3','V06_4','V06_5',
                                    'V07_1','V07_2','V07_3','V07_4','V07_5',
                                    'V08_1','V08_2','V08_3',
                                    'V16_1','V16_2','V16_3')
# Find probit model coefficients
probit1 <- glm(state_after ~ V09+V06_2+V06_3+V06_4+V06_5+
                 V07_1+V07_2+V07_3+V07_4+V08_1+V08_3+V16_1+V16_3, 
               family = binomial(link = "probit"), data = customer_data_binary)
summary(probit1)
# Calculate marginal effects for model 1
marginal_effect1s <- probitmfx(state_after ~ V09+V06_2+V06_3+V06_4+V06_5+
                                 V07_1+V07_2+V07_3+V07_4+ V08_1+V08_3+V16_1+V16_3, 
                               data = customer_data_binary,  atmean = FALSE)
# Calculate McFadden's Pseudo R-squared of model 1
probit0<- update(probit1, formula=customer_data_binary$state_after~1)
McFadden1<-1-as.vector(logLik(probit1)/logLik(probit0))
# Find probit model 2 coefficients with interaction effect added
probit2<-glm(state_after ~ V09+V06_2+V06_3+V06_4+V06_5+
               V07_1+V07_2+V07_3+V07_4+V08_1+V08_3+V16_1+V16_3+V07_3:V09, 
             family = binomial(link = "probit"), data = customer_data_binary)
summary(probit2)
# Calculate marginal effects for model 2
marginal_effects2 <- probitmfx(state_after ~ V09+V06_2+V06_3+V06_4+V06_5+
                                 V07_1+V07_2+V07_3+V07_4+ V08_1+V08_3+V16_1+V16_3+
                                 V07_3:V09, data = customer_data_binary, atmean = FALSE)
# Calculate McFadden's Pseudo R-squared of model 2
probit<- update(probit2, formula=customer_data_binary$state_after~1)
McFadden2<-1-as.vector(logLik(probit2)/logLik(probit))
