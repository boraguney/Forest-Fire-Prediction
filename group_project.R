#Load required packages
library(tidyverse)
library(repr)
library(broom)
library(car)
library(factoextra)
library(corrplot)
library(caret)
library(pROC)

#Read in the data for Bejaia region
forest_fires_b <- read.csv("Algerian_forest_fires_dataset_UPDATE(1).csv", 
                           header = TRUE, skip = 1,  nrow = 122, sep = ",")
head(forest_fires_b)

#This code deals with the label column (Classes) of the forest fire data set.
#The following steps are involved:
# - Firstly, two for loops are used to remove unnecessary spaces from the label 
#column to ensure the consistency of label values. Secondly, a third for loop is 
#used to check whether there are any values in the label column that are not 
#"not fire" or "fire". If there is any such value, it is printed for inspection 
#and handling.
#Finally, specific assignment operations are used to change the label values of 
#row 87 and 89 to "fire" and the label value of row 93 to "not fire", to ensure 
#that the label values of these data points are correct and consistent with 
#their actual conditions.
for (i in 1:122){
  if(forest_fires_b$Classes[i] == "fire   "){
    forest_fires_b$Classes[i] = "fire"
  }
}
for (i in 1:122){
  if(forest_fires_b$Classes[i] == "not fire   "){
    forest_fires_b$Classes[i] = "not fire"
  }
}

for (i in 1:122){
  if(forest_fires_b$Classes[i] != "not fire" & forest_fires_b$Classes[i] != "fire"){
    print(forest_fires_b$Classes[i])
  }
}
forest_fires_b$Classes[87] = "fire"
forest_fires_b$Classes[89] = "fire"
forest_fires_b$Classes[93] = "not fire"

#Add column for region, 0 for Bejaia
df_rb <- data.frame(region = rep(0,122))
data_rb <- cbind(forest_fires_b, df_rb )


#Read the data of Sidi Bel-Abbes Region
forest_fires_s <-read.csv("Algerian_forest_fires_dataset_UPDATE(1).csv", 
                          header = TRUE, skip = 126, nrow = 122, sep = ",")

#Fix the Classes column like previously
for (i in 1:122){
  if(forest_fires_s$Classes[i] == "fire   "){
    forest_fires_s$Classes[i] = "fire"
  }
}
for (i in 1:122){
  if(forest_fires_s$Classes[i] == "not fire   "){
    forest_fires_s$Classes[i] = "not fire"
  }
}
for (i in 1:122){
  if(forest_fires_s$Classes[i] != "not fire" & forest_fires_s$Classes[i] != "fire"){
    print(i)
  }
}
forest_fires_s$Classes[30] = "not fire"
forest_fires_s$Classes[122] = "not fire"

#Add column for region, 1 for Sidi Bel-Abbes
df_rs <- data.frame(region  = rep(1,122))
data_rs <- cbind(forest_fires_s,df_rs)
# remove the data at row 44 since there is a missing entry
data_rs <- data_rs[-44,]

# combine the data of Bejaia and Sidi region vertically for ease of analysis.
data_comb <- rbind(data_rb, data_rs)

#Factor categorical variables
data_comb$Classes <- factor(data_comb$Classes)
data_comb$month <- factor(data_comb$month)
data_comb$region <- factor(data_comb$region)

#Convert to numerical variables
data_comb$Rain <- as.numeric(data_comb$Rain)
data_comb$Ws <- as.numeric(data_comb$Ws)
data_comb$RH <- as.numeric(data_comb$RH)
data_comb$Temperature <- as.numeric(data_comb$Temperature)
data_comb$FWI<- as.numeric(data_comb$FWI)
data_comb$DC<- as.numeric(data_comb$DC)

#Remove year column out since it is constant throughout the observations
data_comb <- data_comb[,!names(data_comb) %in% c("year")]

#View data
head(data_comb)

#Create correlation matrix and plot for variables
cor_matrix <- cor(data_comb[, c("Temperature", "RH", "Ws", "Rain", "FFMC", 
                                "DMC", "DC", "ISI", "BUI", "FWI")])
heatmap(cor_matrix)

#Take the numerical features for PCA
X <- data_comb[, 3:12]

#Standardize to avoid most weight going to larger measurements
p_comp <- princomp(X, cor = TRUE)
summary(p_comp)
p_comp$sdev

#Create 2D plot of the data based on PCA
biplot(p_comp)

#Create scatterplots of the data based on PCA
par(mfrow=c(2,3))
scatterplot(y = data_comb$BUI, x = data_comb$DMC, xlab = "DMC", ylab = "BUI", 
            data = data_comb, smooth = FALSE, boxplot = "")
scatterplot(y = data_comb$BUI, x = data_comb$DC, xlab = "DC", ylab = "BUI", 
            data = data_comb, smooth = FALSE, boxplot = "")
scatterplot(y = data_comb$ISI, x = data_comb$FFMC, xlab = "FFMC", ylab = "ISI", 
            data = data_comb, smooth = FALSE, boxplot = "", regLine = FALSE)
scatterplot(y = data_comb$ISI, x = data_comb$Ws, xlab = "Wind Speed(Ws)", 
            ylab = "ISI", data = data_comb, smooth = FALSE, boxplot = "")

#Since they are all related by definition and evidence we gathered by our plots.
scatterplot(y = data_comb$FWI, x = data_comb$ISI, xlab = "ISI", ylab = "FWI", 
            data = data_comb, smooth = FALSE, boxplot = "")
scatterplot(y = data_comb$FWI, x = data_comb$BUI, xlab = "BUI", ylab = "FWI", 
            data = data_comb, smooth = FALSE, boxplot = "")
p_comp_new <- princomp(X[,c(1,2,3,4,5,6,7,8,9,10)], cor = TRUE)
summary(p_comp_new)

#BUI vs DMC, they seem to be linearly dependent.
#BUI vs DC, they seem to be linearly dependent.
#ISI vs FFMC, there seems to be quadratic or exponential relationship
#ISI vs Ws, have not noticed any linear dependency here
#Since BUI seems to be linearly dependent on DMC and DC, and ISI is linearly 
#dependent on FFMC, it is reasonable to drop DMC, DC, and ISI because including 
#them would give misleading results due to collinearity.


#Plots of contributions from each component
#We actually will not use PCA components but the contributions of variables to 
#the variance explained helps us to derive the relevant variables in our 
#prediction. Based on the contributions given by the plot, we decided to use 
#FFMC, Temperature, and Wind Speed variables in our analysis.
plot(p_comp_new, main = "")
fviz_pca_var(p_comp_new,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Boxplots to examine and compare our selected variables
scatterplot(Ws ~ month, data = data_comb,xlab = "Month", ylab = "Wind Speed")
scatterplot(Temperature ~ month, data = data_comb,xlab = "Month", 
            ylab = "Temperature" )
scatterplot(Temperature ~ region, data = data_comb,xlab = "Region", 
            ylab = "Temperature" )
scatterplot(Ws ~ region, data = data_comb,xlab = "Region", ylab = "Wind Speed")
scatterplot(Ws ~ Classes, data = data_comb,xlab = "Region", ylab = "Wind Speed")
scatterplot(FFMC ~ Classes, data = data_comb,xlab = "Classes", ylab = "FFMC" )
scatterplot(Ws ~ Classes, data = data_comb,xlab = "Classes", ylab = "Wind Speed")
boxplot(Temperature ~ Classes, data = data_comb,xlab = "Classes", 
        ylab = "Temperature" )
ext_temp <- data_comb$Temperature >= 34

#Create logistic regression model
log_model <- glm(Classes ~ FFMC + Ws + Temperature, data = data_comb, 
                 family = binomial(link = "logit"))
summary(log_model)

#Set the random seed for reproducibility
set.seed(2343)
#Split the data set into training and testing sets using 70:30 split
train_index <- sample(nrow(data_comb), 0.7 * nrow(data_comb))
train_data <- data_comb[train_index, ]
test_data <- data_comb[-train_index, ]

#Then, fit the logistic regression model using glm()
log_model_train <- glm(Classes ~ FFMC + Ws + Temperature, data = train_data, 
                 family = binomial(link = "logit"))
summary(log_model_train)

#Perform 10-fold cross-validation
#Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)
model <- train(Classes ~ FFMC + Ws + Temperature, data = train_data, 
               method = "glm", family = binomial(link = "logit"), trControl = ctrl)
print(model) # Print the results of cross-validation

#Make predictions on the testing set and calculate accuracy
test_pred <- predict(log_model_train, newdata = test_data, type = "response", 
                     trControl = ctrl)
test_pred_class <- ifelse(test_pred > 0.5, "not fire", "fire")
test_acc <- mean(test_pred_class == test_data$Classes)
print(paste0("Testing accuracy: ", test_acc))

#Generate an ROC curve object using the roc() function
test_prob <- predict(log_model_train, newdata = test_data, type = "response")
roc_obj <- roc(test_data$Classes, test_prob)
plot(roc_obj, main = "ROC Curve for Logistic Regression Model", 
     col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 2)), 
       col = "blue", lwd = 2, cex = 0.8)

#New nodel without temperature since the term was not significant
log_model_woT <- glm(Classes ~ FFMC + Ws, data = data_comb, 
                     family = binomial(link = "logit"))
summary(log_model_woT)

#Create training model based on new model
model_woT <- train(Classes ~ FFMC + Ws, data = train_data, method = "glm", 
                   family = binomial(link = "logit"), trControl = ctrl)
print(model_woT)

#Make predictions on the testing set and calculate accuracy using new model
test_pred_woT <- predict(log_model_woT, newdata = test_data, type = "response", 
                         trControl = ctrl)
test_pred_class_woT <- ifelse(test_pred_woT > 0.5, "not fire", "fire")
test_acc_woT <- mean(test_pred_class_woT == test_data$Classes)
print(paste0("Testing accuracy: ", test_acc_woT))

