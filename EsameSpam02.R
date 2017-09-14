################################################################################
##
## File: EsameSpam02.R
##                
## Purpose: Spam email analysis.
##
## Created: 2017.09.03
##
## Version: 2017.09.13
##
## Remarks: 
##
################################################################################

################################################################################
## Clean
################################################################################

####
rm(list=ls(all=TRUE))


################################################################################
## Libraries and functions
################################################################################
library(caret)
library(MASS)
library(glmnet)
library(mgcv)
library(rpart)
library(ROCR)
library(Matrix)
source("d:/scripts/MBD2016-Functions-20160503.R")


################################################################################
## Inputs
################################################################################

####
## set working directory
setwd("D:/Data/")    

## input


train <- read.table(file = "Spam-Training.txt", header= TRUE, sep = "\t", 
                    na.strings = "NA", colClasses = NA, check.names = FALSE, comment.char = "")


test <- read.csv (file="Spam-Test.txt",header = TRUE, sep = "\t", 
                  na.strings = "NA", colClasses = NA, check.names = FALSE, comment.char = "")


str(train)
head(train)
summary(train)
mod1 <- lm(spam ~ ., data = train)
summary(mod1)

trainClean <- train
trainClean$pct_all <- NULL
trainClean$pct_mail <- NULL
trainClean$pct_receive <- NULL
trainClean$pct_people <- NULL
trainClean$pct_report <- NULL
trainClean$pct_addresses <- NULL
trainClean$pct_hpl <- NULL
trainClean$pct_650 <- NULL
trainClean$pct_lab <- NULL
trainClean$pct_telnet <- NULL
trainClean$pct_857 <- NULL
trainClean$pct_415 <- NULL
trainClean$pct_85 <- NULL
trainClean$pct_technology <- NULL
trainClean$pct_parts <- NULL
trainClean$pct_pm <- NULL
trainClean$pct_direct <- NULL
trainClean$pct_cs <- NULL
trainClean$pct_original <- NULL
trainClean$pct_parenthesis <- NULL
trainClean$pct_bracket <- NULL
trainClean$pct_hash <- NULL
trainClean$crl_average <- NULL
trainClean$crl_longest <- NULL
mod1 <- lm(spam ~ ., data = trainClean)
summary(mod1)

# levels(as.factor(trainClean$spam))
# trainClean$spam[trainClean$spam == "0"] <- "non-spam"
# trainClean$spam[trainClean$spam == "1"] <- "spam"
# trainClean$spam <- factor(trainClean$spam)

# file.data <- trainClean







################################################################################
## Data
################################################################################


#### Read full dataset
#data <- read.table(file = trainClean, header = TRUE, sep = "\t", 
#                   na.strings = "NA", colClasses = NA, check.names = FALSE, comment.char = "")

#### Manage variables
## Dependent
#data$y <- ifelse(data$y == "no", 0, 1)
## pdays


################################################################################
## Modeling settings (useful to avoid repetitions across approaches)
################################################################################

#### Variables
yVar <- "spam"
xVar <- c("pct_make" , "pct_address" , "pct_3d" , "pct_our" ,"pct_over", "pct_remove","pct_internet",
          "pct_order", "pct_will" , "pct_free", "pct_business" , "pct_email" , "pct_you" , "pct_credit" ,
          "pct_your" , "pct_font" , "pct_000", "pct_money" , "pct_hp" , "pct_george" , "pct_labs" , "pct_data" ,
          "pct_1999" , "pct_meeting" , "pct_project" , "pct_re", "pct_edu" , "pct_table" , "pct_conference" , 
          "pct_semicolon" , "pct_exclamatory" , "pct_dollar", "crl_total")



################################################################################
## Stepwise selection
################################################################################

#### Null model
formula <- as.formula( paste0( yVar, " ~ 1" ) )
null <- glm(formula = formula, data = train, family = "binomial")
#### Full model
formula <- as.formula( paste0( yVar, " ~ 1 + ", paste0(xVar, collapse = " + ")) )
full <- glm(formula = formula, data = train, family = "binomial")

#### FORWARD
#### Use k = 2 for AIC, k = log(NROW(data)) for BIC
forward <- step(object = null, scope = list(lower = null, upper = full), 
                direction = "forward", k = 2)

# #### BACKWARD
# #### Use k = 2 for AIC, k = log(NROW(data)) for BIC
# backward <- step(object = full, scope = list(lower = null, upper = full), 
#   direction = "backward", k = 2)
# 
# #### BOTH
# #### Use k = 2 for AIC, k = log(NROW(data)) for BIC
# both <- step(object = null, scope = list(lower = null, upper = full), 
#   direction = "both", k = 2)

#### Look at:
## -> anova components; summary; 
## -> Comment: same final model in all cases
## -> age, campaigns, may deserve a better treatment (group it in categories?)
## -> Some clashes between pdays / previous and pdays / poutcome cast doubts on 
##    pdays


################################################################################
## GAM 
################################################################################

#### Fit
fx <- FALSE    ## Try TRUE and FALSE; explain
k  <- 12       ## Try different choices; explain
y  <- trainClean$spam
formula <- y ~ pct_make + pct_address + pct_3d+pct_our+pct_over+pct_remove+pct_internet+
  pct_order+pct_will+pct_free+pct_business+pct_email+pct_you+pct_credit+pct_your+pct_font+pct_000+pct_money+pct_1999+
  pct_meeting+pct_project+pct_re+pct_edu+pct_table+pct_conference+pct_semicolon+pct_exclamatory+
  pct_dollar+crl_total

  # formula <- y ~ job + marital + education + default + housing + contact + month + day_of_week + 
  #  pdays + nr.employed + emp.var.rate + cons.price.idx + cons.conf.idx + previousF +
  # s(age,            bs = "cr", k = k, fx = fx) + 
  # s(campaign,       bs = "cr", k = k, fx = fx) + 
  # s(euribor3m,      bs = "cr", k = k, fx = fx)  
fit <- gam(formula = formula, family = "binomial", data = train, method = "P-ML",
           scale = 0)
fit.gam <- fit

#### Give a look to summary(); a look to plot
plot(x = fit, residuals = TRUE, rug = TRUE, se = TRUE, pages = 4, scale = -1)




#### So tedious?
# test.all <- .gam.ftest.all(fit = fit)

#### Some model cleaning could be done


################################################################################
## Ridge Regression
################################################################################

#### Ridge
alpha <- 0

#### Data
## Dependent variable
y <- train[, yVar]
## Independent variables (needed since 'glmnet' wants numerical matrices!)
x <- .indVars(data = train, xVar = xVar, constant = FALSE)

# #### Fit (more options omitted)
# fit <- glmnet(x = x, y = y, family = "binomial", alpha = alpha, 
#   nlambda = 100, standardize = TRUE)
# fit.ridge <- fit
# #### Print output (Explain columns Df and %Dev)
# # print(fit)
# 
# #### Plot trajectories of coefficients
# par(mfrow = c(1,3))
# plot(x = fit, xvar = "lambda", label = TRUE) ## lambda
# plot(x = fit, xvar = "norm",   label = TRUE) ## L1-norm
# plot(x = fit, xvar = "dev",    label = TRUE) ## R^2
# 
# #### Print coefficients (A lot of numbers... why?)
# # print(coef(fit))
# 
# #### Selecting coefficients
# ## 's' = lambda... but 'exact'?
# # print( coef(fit, s = 0.1, exact = TRUE) )

#### Which one is the best lambda?
cvfit <- cv.glmnet(x = x, y = y, alpha = alpha,             # lambda = lambda, 
                   family = "binomial", type.measure = "deviance", nfolds = 20)
par( mfrow = c(1,1) )
plot(cvfit)

## Print best lambdas
cat("min(lambda) = ", cvfit$lambda.min, "1se(lambda) = ", cvfit$lambda.1se, "\n")

#### Estimate best
lambda <- cvfit$lambda.1se 
fit.1 <- glmnet(x = x, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda, standardize = TRUE)

#### Store
lambda.best.ridge <- lambda
fit.best.ridge <- fit.1

table(actual=trainClean$ispam,predicted=predict(fit.best.ridge,newdata=trainClean)$class)
pred1 <- predict(fit.1, trainClean, type="class")

################################################################################
## Lasso Regression
################################################################################

#### Lasso
alpha <- 1

#### Data
## Dependent variable
y <- train[, yVar]
## Independent variables (needed since 'glmnet' wants numerical matrices!)
x <- .indVars(data = train, xVar = xVar, constant = FALSE)

# #### Fit (more options omitted)
# # lambda <- exp( seq(from = -8, to = 2, by = 0.1) )
# fit <- glmnet(x = x, y = y, family = "binomial", alpha = alpha, 
#   nlambda = 100, standardize = TRUE)
# fit.lasso <- fit
# 
# #### Plot trajectories of coefficients
# par(mfrow = c(1,2))
# plot(x = fit.lasso, xvar = "lambda", label = TRUE, main = "Lasso") ## lambda Lasso
# plot(x = fit.ridge, xvar = "lambda", label = TRUE, main = "Ridge") ## lambda Ridge

#### Select the best lambda
cvfit <- cv.glmnet(x = x, y = y, alpha = alpha, family = "binomial", 
                   type.measure = "deviance", nfolds = 20)
plot(cvfit)
## Print best lambdas
cat("min(lambda) = ", cvfit$lambda.min, "1se(lambda) = ", cvfit$lambda.1se, "\n")

#### Estimate best
lambda <- cvfit$lambda.1se 
fit <- glmnet(x = x, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda, standardize = TRUE)

#### Store
lambda.best.lasso <- lambda
fit.best.lasso <- fit

pred2 <- predict(fit, trainClean, type="class")

################################################################################
## Recursive Partitioning: 1st try
################################################################################

#### Formula
formula <- as.formula( paste0( yVar, " ~ ", paste0(xVar, collapse = " + ") ) )

#### First try using default settings
## Here a selection of the main control parameters
control <- rpart.control(
  minsplit = 20, ## Minimum number of observations in a node (group)
  cp = 0.01,     ## Minimum cp decrease: any split not decreasing "rel error" 
  ##  by a factor of cp is not attempted
  ## With "anova", the overall R-squared must increase by cp 
  ##  at each step. 
  xval = 10)     ## Number of cross-validations to compute xerror
## Fit
fit <- rpart(formula = formula, data = train, method = "class", control = control, 
             model = TRUE)  ## model = TRUE useful for kfold-cv

#### Plot
#### Try options uniform = TRUE and branch = 0.5
par(mfrow = c(1,1))
plot(x = fit, uniform = FALSE, branch = 1, compress = FALSE, margin = 0, minbranch = 0.3) 
text(fit)        ## Adds text, values and labels
#### Print
print(fit)
#### A deeper look to the main summary table
## Columns:
##  cp        = Complexity Parameter = Decrease in rel error (below) 
##  rel error = 1 - R^2
##  xerror    = PRESS statistics (cf slide XXX)
##  xstd      = To compute the 1-se minimum rule (it gives nsplit = 4 here) 
printcp(fit)


pred3 <- predict(fit, trainClean, type="class")



plot(pred3, trainClean$spam)


## Where to stop the tree? min(xerror) or (better) 1-se minimum rule 
## Plot cp vs rel error
# plotcp(fit, minline = FALSE)

#### Store last model
fit.rpart1  <- fit


################################################################################
## Recursive Partitioning: 2nd try
################################################################################

#### Second try: cp decreased from 0.01 (default) to 0.001
control <- rpart.control(
  minsplit = 20, ## Minimum number of observations in a node
  cp = 0.001,    ## Minimum cp decrease
  xval = 10)     ## Number of cross-validations for xerror
fit <- rpart(formula = formula, data = train, method = "class", control = control,
             model = TRUE, x = FALSE, y = TRUE)

#### Plot cp vs rel error
par(mfrow = c(1,1))
plotcp(fit, minline = TRUE)
#### Look to the main summary table
par(mfrow = c(1,2))
rsq.rpart(fit) ## Another useful plot (Relative vs Apparent)

#### Could the tree be pruned?
## How to chose the best:
## 1) min(xerror)
ind <- which.min(fit$cptable[, "xerror"])
cp.best <- fit$cptable[ind, "CP"]
## 2) largest xerror s.t xerror <= min(xerror - xstd) (called 1se-rule)
ind  <- fit$cptable[, "xerror"] <= min(fit$cptable[, "xerror"] + fit$cptable[, "xstd"])
ind1 <- which.max(fit$cptable[ind, "xerror"])
cp.best <- fit$cptable[ind, "CP"][ind1]
#### Prune
fit.rpart2 <- prune(tree = fit, cp = cp.best)   ## Prune

#### Plot
#### Try options uniform = TRUE and branch = 0.5
par(mfrow = c(1,1))
plot(x = fit, uniform = FALSE, branch = 1, compress = FALSE, margin = 0, minbranch = 0.3) 
text(fit)        ## Adds text, values and labels
#### Print
print(fit)
#### A deeper look to the main summary table
## Columns:
##  cp        = Complexity Parameter = Decrease in rel error (below) 
##  rel error = 1 - R^2
##  xerror    = PRESS statistics (cf slide XXX)
##  xstd      = To compute the 1-se minimum rule (it gives nsplit = 4 here) 
printcp(fit)
pred4 <- predict(fit, trainClean, type="class")

################################################################################
## Final check: k-fold cv
################################################################################

#### Settings
k <- 10
seed <- 100000

#### Linear Model
cv.forward  <- .kfoldcv(k = k, model = forward,  seed = seed)
# cv.backward <- .kfoldcv(k = k, model = backward, seed = seed)
# cv.both     <- .kfoldcv(k = k, model = both,     seed = seed)

#### GAM Model
# cv.gam <- .kfoldcv(k = k, model = fit.gam, seed = seed)

#### glmnet Based Models
y <- train[, yVar]
x <- .indVars(data = train, xVar = xVar, constant = FALSE)
cv.ridge <- .kfoldcv.glmnet(k = k, x = x, y = y, lambda = lambda.best.ridge, 
                            family = "binomial", alpha = 0, seed = seed)
cv.lasso <- .kfoldcv.glmnet(k = k, x = x, y = y, lambda = lambda.best.lasso, 
                            family = "binomial", alpha = 1, seed = seed)

#### rpart models
cv.rpart1 <- .kfoldcv(k = k, model = fit.rpart1, seed = seed)
cv.rpart2 <- .kfoldcv(k = k, model = fit.rpart2, seed = seed)

#### Join
x1 <- c("forward", "ridge", "lasso", "rpart1", "rpart2")
x2 <- rbind(cv.forward, cv.ridge, cv.lasso, cv.rpart1, cv.rpart2)

kfold <- data.frame(Model = x1, x2, check.names = FALSE)
cat("k-fold CV", "\n")
print(kfold)






confusionMatrix(pred1, trainClean$spam)
confusionMatrix(pred2, trainClean$spam)
confusionMatrix(pred3, trainClean$spam)
confusionMatrix(pred4, trainClean$spam)

