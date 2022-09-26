
#---------------------MLR-------------------------------------------------------------

attach(datasciencegroup)
df = datasciencegroup


df$sub_sex = as.factor(df$sub_sex)
df$sub_shift = as.factor(df$sub_shift)
df$sub_role = as.factor(df$sub_role)
df$sub_team = as.factor(df$sub_team)
df$sub_workstyle_h = as.factor(df$sub_workstyle_h)
df$sup_sex = as.factor(df$sup_sex)
df$sup_role = as.factor(df$sup_role)


data = df[,-c(1,2,3,4,20,21,22,30,32)]
str(data)

dataf = data[,-c(1,2,3,4,5,15,16,18,19,23)]

data_scaled =scale(dataf)

data = cbind(data[,c(1,2,3,4,5,15,16,18,19,23)],data_scaled)

set.seed(123)
n=sample(nrow(data),size=nrow(data)*0.8)
train=data.frame(data[n,])
test=data.frame(data[-n,])


model = lm(actual_efficacy_h ~.,data=train)
model
summary(model)
 
library(MASS)
# stepAIC(model,direction='both')

m1 = lm(formula = actual_efficacy_h ~ sub_age + sub_sex + sub_shift + 
     sub_team + sub_role + sub_colls_same_sex_prtn + sub_health_h + 
     sub_commitment_h + sub_perceptiveness_h + sub_dexterity_h + 
     sub_sociality_h + sub_goodness_h + sub_strength_h + sub_workstyle_h + 
     sup_age + sup_sex + sup_commitment_h + sup_perceptiveness_h + 
     sup_goodness_h + event_weekday_num, data = train)

summary(m1)

sqrt(mean(m1$residuals^2))

#----------------------Regression Tree----------------------------------------------------

library(rpart)
library(rpart.plot)
reg.tree = rpart(actual_efficacy_h ~ ., data = train)
reg.tree
rpart.plot(reg.tree, type = 1)
reg.tree$variable.importance
summary(reg.tree)

str(data)
printcp(reg.tree)
plotcp(reg.tree)

tree.pruned = prune(reg.tree, cp=0.01)
rpart.plot(tree.pruned)

test_pred = predict(tree.pruned,test[,-24])
test_y = test$actual_efficacy_h
plot(test_pred,test_y)
abline (0,1)

mean((test_pred -test_y)^2)
plot(test_pred)
plot(test_y)

summary(tree.pruned)
rpart.plot(tree.pruned)

#---------------------------------Ridge Regression--------------------------------------------

library(glmnet)

x = model.matrix(actual_efficacy_h~.,train)[,-24]
y = train$actual_efficacy_h


fit_ridge = glmnet(x, y, alpha = 0)
plot(fit_ridge)
plot(fit_ridge, xvar = "lambda", label = TRUE)

fit_ridge_cv = cv.glmnet(x, y, alpha = 0)
plot(fit_ridge_cv)

fit_ridge_cv$lambda.min
coef(fit_ridge_cv, s = "lambda.min")

predict(fit_ridge_cv, x, s = "lambda.min")
mean((y - predict(fit_ridge_cv, x)) ^ 2)
sqrt(fit_ridge_cv$cvm)

sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.min])


#-----------------------------Lasso Regression-----------------------------------------------

library(glmnet)

x = model.matrix(actual_efficacy_h~.,train)[,-24]
y = train$actual_efficacy_h


fit_ridge = glmnet(x, y, alpha = 1)
plot(fit_ridge)
plot(fit_ridge, xvar = "lambda", label = TRUE)

fit_ridge_cv = cv.glmnet(x, y, alpha = 1)
plot(fit_ridge_cv)

fit_ridge_cv$lambda.min
coef(fit_ridge_cv, s = "lambda.min")

predict(fit_ridge_cv, x, s = "lambda.min")
mean((y - predict(fit_ridge_cv, x)) ^ 2)
sqrt(fit_ridge_cv$cvm)

sqrt(fit_ridge_cv$cvm[fit_ridge_cv$lambda == fit_ridge_cv$lambda.min])


#------------------------------------Support Vector Machine -----------------------------------------------

library(e1071)
classifier = svm(formula = actual_efficacy_h ~ .,
                 data = train,
                 kernel = 'linear')





#---------------------------------Neural Network------------------------------------------------

library(neuralnet)
n = neuralnet(actual_efficacy_h ~ .,
               data = train)


########################################################################################################################


data = df[,-c(1,2,3,4,20,21,22,30,32)]
str(data)

m2 = lm(formula = actual_efficacy_h ~ sub_workstyle_h + sup_sub_age_diff +
         event_weekday_num + sup_age )

summary(m2)



















