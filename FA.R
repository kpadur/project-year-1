library(dplyr)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(psych)
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
head(customer_data)
# ----
# Calculate factor analysis of mixed data (FAMD)
res <- FAMD(customer_data, ncp = 3, graph = FALSE)
# Get the proportion of variances retained by the different dimensions
eig.val <- get_eigenvalue(res)
# Use scree plot to confirm results
fviz_screeplot(res)
# Separate quantitative and qualitative variables for analysis
## Quantitative variables: PCA
quanti.var <- get_famd_var(res, "quanti.var")
round(quanti.var$coord, 2)
round(quanti.var$cos2, 2)
round(quanti.var$contrib, 2)
# Qualitative variables: MCA
quali.var <- get_famd_var(res, "quali.var")
round(quali.var$coord, 2)
round(quali.var$cos2, 2)
round(quali.var$contrib, 2)
round(quali.var$v.test, 2)
# Visualise the results of the variables
fviz_famd_var(res, axes=c(1,2), repel = TRUE)
fviz_famd_var(res, axes=c(1,3), repel = TRUE)
## Contribution to the first dimension
fviz_contrib(res, "var", axes = 1)
## Contribution to the second dimension
fviz_contrib(res, "var", axes = 2)
## Contribution to the third dimension
fviz_contrib(res, "var", axes = 3)
## Visualise the results for quantitative variables
fviz_famd_var(res, "quanti.var", axes=c(1,2), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_famd_var(res, "quanti.var", axes=c(1,3), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
# Visualise the results for qualitative variables
fviz_famd_var(res, "quali.var", axes=c(1,2), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_famd_var(res, "quali.var", axes=c(1,3), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
