library(multiUS)
library(dplyr)
library(psych)
library(olsrr)
library(car)
library(caret)





# 1. Data exploration
train=read.csv('https://raw.githubusercontent.com/anishkapeter/Stat1Project/main/train.csv')


# 2. Looking at how many NAs are there in each columns
colSums(is.na(train))

#Continuous： LotFrontage , MasVnrArea , GarageYrBlt

#Categorical: Alley, BsmtQual, BsmtCond,BsmtFinType1,BsmtFinType2,FireplaceQu,GarageType,GarageYrBlt,GarageFinish,GarageQual,GarageCond,PoolQC,Fence,MiscFeature

#Special: MasVnrType


# 3. Using Knn imputation to impute all missing Continuous values
train$LotFrontage=as.numeric(train$LotFrontage)
train$MasVnrArea=as.numeric(train$MasVnrArea)
train$GarageYrBlt= as.numeric(train$GarageYrBlt)

tmp <- data.frame(LotFrontage = train$LotFrontage, MasVnrArea = train$MasVnrArea , GarageYrBlt = train$GarageYrBlt)
imputed_data <- KNNimp(tmp)

train$LotFrontage <- imputed_data$LotFrontage
train$MasVnrArea <- imputed_data$MasVnrArea
train$GarageYrBlt = imputed_data$GarageYrBlt


# 4. Convert all character columns to factors
train <- mutate_if(train, is.character, function(x) addNA(as.factor(x)))


# 5. Plot the data, checking for co linearity (Did this in SAS)

# Observation 1299 is influential, choose to delete
train = train[-1299,]

# 6. Log X and Y


trainlog <- log1p(train[sapply(train, is.numeric)])


#combine logged data to orginal data

# Selecting the non-numeric columns from the original dataset
train_non_numeric <- train[sapply(train, is.factor)]

# Combining the log-transformed numeric columns and non-numeric columns
train_combined <- cbind(trainlog, train_non_numeric)


write.csv(train_combined,file = 'trainlog')





# Subset the data frame to only include numeric columns
numeric_columns <- sapply(train, is.numeric)
train_numeric <- train[, numeric_columns]


pairs(train_numeric[,2:6])
pairs(train_numeric[,7:13])
pairs(train_numeric[,14:20])
pairs(train_numeric[,21:30])
pairs(train_numeric[,31:38])

#Checking for multi-linearity and VIF


model1 <- lm(SalePrice ~ ., data = train_combined)
residualPlot(model1)


# First, generate the histogram
hist_resid <- hist(model1$residuals, probability = TRUE, main = "Histogram of Residuals", xlab = "Residuals")

# Generate density data
dens_resid <- density(model1$residuals)

# Add the density line
lines(dens_resid, col = "blue")



qqPlot(model1)

stud_resid <- rstudent(model1)




# Forward Selection

# Fit a full model
model <- lm(SalePrice ~ ., data = train)
residualPlot(model)
# Perform forward selection

result <- ols_step_forward_p(model)
summary(result)
forward_predictors=result$predictors

model_final_forward = lm(SalePrice ~., data = train[, c(forward_predictors, "SalePrice")])

summary(model_final_forward)
# Backward Selection


# Perform forward selection
Backword_model <- ols_step_backward_p(model)
summary(Backword_model)



#Step Wise Selection

Step_model = ols_step_both_p(model)






train2= read.csv(file = 'processed_data_nominal.csv')



# Examen relationship in Sales price in neighborhood 'NAmes', 'Edwards', 'BrkSide' 

options(scipen = 999)

filtered_neighborhood = data %>% filter (Neighborhood %in% c('NAmes', 'Edwards', 'BrkSide'))

filtered_neighborhood_model= glm(SalePrice ~ GrLivArea, data = filtered_neighborhood)
summary(filtered_neighborhood_model)
residualPlot(filtered_neighborhood_model)
histogram(filtered_neighborhood_model$residuals)



# Residual plot clustered, using log-log method to transform the dataset

data3=data
data3$SalePrice = log(data$SalePrice)
data3$GrLivArea = log(data$GrLivArea)

filtered_data2 <- data3 %>% filter(Neighborhood %in% c('NAmes', 'Edwards', 'BrkSide'))

filtered_data2_model = glm(SalePrice ~ GrLivArea, data = filtered_data2)

histogram(filtered_data2_model$residuals)
plot(filtered_data2_model)


#Observation 337 seems to be very influential, sales price is low for a huge area house, choosing to delete


filtered_data2 <- filtered_data2 [ -c(337),]

filtered_data2_model = glm(SalePrice ~ GrLivArea, data = filtered_data2)

histogram(filtered_data2_model$residuals)
plot(filtered_data2_model)
summary(filtered_data2_model)

plot(filtered_data2$GrLivArea, filtered_data2$SalePrice, 
     main="Scatterplot with Regression Line", 
     xlab="Above grade (ground) living area square feet (GrLivArea)", 
     ylab="Sale Price (SalePrice)", pch=19, frame=FALSE, col="blue")

# Add the regression line
abline(filtered_data2_model, col="red")

#### Therefore the regression model predicting SalePrice using GrlivArea is 
logSalePrice = 7.58437 + 0.59230 * logGrlivArea


# Adding Interaation Variables

# Fit the model
filtered_data2_model2 <- glm(SalePrice ~  GrLivArea + GrLivArea * Neighborhood, data = filtered_data2)

# Summary of the model
model_summary <- summary(filtered_data2_model2)
model_summary

# Confidence intervals
model_confint <- confint(filtered_data2_model2)
model_confint

# Residuals histogram
histogram <- ggplot(filtered_data2_model2, aes(x=residuals)) +
  geom_histogram(binwidth=1, color="black", fill="white") +
  theme_minimal() +
  labs(x="Residuals", y="Frequency", title="Histogram of Residuals")

# Model diagnostics
plot_diag <- plot(filtered_data2_model2, diagnostic = TRUE)


# Model Coefficients
model_coef <- coef(filtered_data2_model2)






