# Read File
Project1 = read.csv("emis7357proj1fl18-100ids.csv")
str(Project1)
summary(Project1)

# Random Data
set.seed(88)

# Split Data Training and testing
library(caTools)
spl = sample.split(Project1$click, SplitRatio = 2/3)
Train = subset(Project1, spl==TRUE)
Test = subset(Project1, spl==FALSE)

# Check Data
str(Train)
str(Test)

# Second Spliting
spl2 = sample.split(Test$click, SplitRatio = 0.5)
Validation = subset(Test, spl2==TRUE)
Test2 = subset(Test, spl2==FALSE)

#Check ALL Data
str(Train)
str(Test2)
str(Validation)

#Write files to PC
#write.csv(Train, "/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Train.csv")
#write.csv(Test2, "/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Test2.csv")
#write.csv(Validation, "/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Validation.csv")

#Average Click-Though Rate on the Training Set
mean(as.numeric(Project1$click)) #0.1686866
mean(as.numeric(Train$click)) #0.1686866
mean(as.numeric(Test2$click)) #0.168706
mean(as.numeric(Validation$click)) #0.1686672


# Initial case
#Original Equation: TrainModel = glm(click ~ device_model + device_type + device_conn_type + C14 + C15 + C16 + C17 + C18 + C19 + C20 + C21 + id + hour + C1 + banner_pos + site_id + site_domain + site_category + app_id + app_domain + app_category + device_id + device_ip, data = Train, family=binomial)

str(Train)
head(Train)

#Create new Column called app_domain_factor, replacing thhe original column app_domain, and if its value was 
#equal to 7801e8d9, the most frequent number, then app_domain ==1, otherwirse == 0
Train$app_domain_factor <- 0
for(i in 1:length(Train$app_domain)){
  if(Train$app_domain[i] =="7801e8d9"){
    Train$app_domain_factor[i] <- 1
  } 
  else {
    Train$app_domain_factor[i] <- 0
  }
}

#Create new Column called app_category_factor, replacing thhe original column app_category, and if its value was 
#equal to 07d7df22, the most frequent number, then app_category ==1, otherwirse == 0
Train$app_category_factor <- 0
for(i in 1:length(Train$app_category)){
  if(Train$app_category[i] =="07d7df22"){
    Train$app_category_factor[i] <- 1
  } 
  else {
    Train$app_category_factor[i] <- 0
  }
}

#Create new Column called app_id_factor, replacing thhe original column app_id, and if its value was 
#equal to ecad2386, the most frequent number, then app_id ==1, otherwirse == 0
Train$app_id_factor <- 0
for(i in 1:length(Train$app_id)){
  if(Train$app_id[i] == "ecad2386"){
    Train$app_id_factor[i] <- 1
  } 
  else {
    Train$app_id_factor[i] <- 0
  }
}

#Create new Column called device_id_factor, replacing thhe original column device_id, and if its value was 
#equal to a99f214a, the most frequent number, then device_id ==1, otherwirse == 0
Train$device_id_factor <- 0
for(i in 1:length(Train$device_id)){
  if(Train$device_id[i] =="a99f214a"){
    Train$device_id_factor[i] <- 1
  } 
  else {
    Train$device_id_factor[i] <- 0
  }
}

#Create new Column called C16_factor, replacing thhe original column C16, and if its value was 
#equal to 50, the most frequent number, then C16 ==1, otherwirse == 0
Train$C16_factor <- 0
for(i in 1:length(Train$C16)){
  if(Train$C16[i] == 50){
    Train$C16_factor[i] <- 1
  } 
  else {
    Train$C16_factor[i] <- 0
  }
}

#Create new Column called C20_factor, replacing thhe original column C20, and if its value was 
#equal to -1, the most frequent number, then C20 ==1, otherwirse == 0Train$C20_factor <- 0
Train$C20_factor <- 0
for(i in 1:length(Train$C20)){
  if(Train$C20[i] == -1){
    Train$C20_factor[i] <- 1
  } 
  else {
    Train$C20_factor[i] <- 0
  }
}

#Create new column from hour called hour.1 that only includes the last two digits from hour

#Create new Column from hour.1 called hour.1_factor, and depending on its range of hours assing a "1", "2", or "3"  
#“1” from 0 to 8, “2” from 8 to 16, and “3” from 16 to 24. 
Train$hour_factor <- 0
for(i in 1:length(Train$hour.1)){
  if(Train$hour.1[i] <= 8 && Train$hour.1[i] >= 0){
    Train$hour_factor[i] <- 1
  } 
  else if(Train$hour.1[i] > 8 && Train$hour.1[i] <= 16) {
    Train$hour_factor[i] <- 2
  }
  else if(Train$hour.1[i] > 16 && Train$hour.1[i] <= 23){
    Train$hour_factor[i] <- 3
  }
}

str(Train)
write.csv(Train, "Train2.csv")
Train = read.csv("Train2.csv")



#LOGISTIC REGRESSION

# Optimization 1 (Does not include: site_id, site_domain, device_ip, device_model, C14, C15, C17, C19, C21, site_category and app_id)
TrainModel = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C18 + C20_factor + hour_factor 
                 + banner_pos +  app_domain_factor + app_category_factor + device_id_factor, data = Train, family=binomial)
summary(TrainModel)

#Optimization 2 (eliminating app_domain_factor)
TrainModel = glm(click ~ device_type + C15 + device_conn_type + C16_factor + C18 + C20_factor + hour_factor 
                 + banner_pos + app_category_factor 
                 + device_id_factor, data = Train, family=binomial)
summary(TrainModel)

#Optimization 3 (eliminating device_id_factor)
TrainModel = glm(click ~ device_type + C15 + device_conn_type + C16_factor + C18 + C20_factor + hour_factor 
                 + banner_pos + app_category_factor,data = Train, family=binomial)
summary(TrainModel)

#Optimization 4 (eliminating C15)
TrainModel = glm(click ~ device_type + device_conn_type + C16_factor + C18 + C20_factor + hour_factor 
                 + banner_pos + app_category_factor, data = Train, family=binomial)
summary(TrainModel)

#All our Coefficients are not statistically signifact over 95%
#Logit = - 0.771195(Intercept) - 0.170419(device_type) - 0.218965(device_conn_type) - 1.114126(C16_factor) + 0.019513(C18) + 0.288042(C20_factor) - 0.028877(hour_factor) + 0.105372(banner_pos) + 0.415994(app_category_factor)


##### Performance/Plot ####

library(gplots)
library(stats)
library(caTools)
library(ROCR)
predict_Train = predict(TrainModel, type="response")
ROCR_pred = prediction(predict_Train, Train$click)
ROCR_Curve = performance(ROCR_pred, "tpr", "fpr")
#def.off()

#Curve
plot(ROCR_Curve)
#Color Curve
plot(ROCR_Curve, colorize=TRUE, print.cutoffs.at=seq(0.1,0.2,0.01), text.adj=c(-0.2,0.7))

as.numeric(performance(ROCR_pred, "auc")@y.values)
#0.6209874

#### ROC, AUC, accuracy ####

predict_Train = predict(TrainModel, type="response")
table(Train$click, predict_Train > 0.17)
# Accuracy for cutoff =0.1 (47405+10722)/(47405+10722+6704+38473) = 0.5626791
#Sensitivity = 10722 / (10722+6704) = 0.6152875
#Specificity = 47405 / (47405+38473) = 0.552004 = 1 - 0.552004 = 0.447996 = FPR
#baseline= (47405+38473)/(47405+38473+6704+10722) = 0.8313134

table(Train$click, predict_Train > 0.1)
# Accuracy for cutoff =0.1 (11934+16649)/(11934+16649+777+73944) = 0.2766882 ~ 27.6
#Sensitivity = 16649 / (16649+777) = 0.9554115 ~ 95.5%
#Specificity = 11934 / (11934+73944) = 0.1389646--> 1 - 0.1389646 = 0.8610354 = FPR
#baseline= (11934+73944)/(11934+73944+777+16649) = 0.8313134

table(Train$click, predict_Train > 0.2)
# Accuracy for cutoff =0.2 (66494+6314)/(66494+6314+11112+19384) = 0.70
#Sensitivity = 6314 / (6314+11112) = 0.3623321
#Specificity = 66494 / (66494+19384) = 0.7742845; 1 - 0.7742845 = 0.2257155 = FPR
#baseline= (66494+19384)/(66494+19384+11112+6314) = 0.8313134

table(Train$click, predict_Train > 0.3)
# Accuracy for cutoff =0.3 (83017+2027)/(83017+2027+15399+2861) = 0.8232401
#Sensitivity = 2027 / (2027+15399) = 0.1163204
#Specificity = 83017 / (83017+2861) = 0.9666853  --> 1 - 0.9666853 = 0.0333147 = FPR  ~ 3.3%
#baseline= (83017+2861)/(83017+2861+15399+2027) = 0.8313134
# Best Threshold based on Accuracy and lowest FPR is at t = 0.3

#### Classification Trees #####
library(rpart)
library(rpart.plot)
sink("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Project_Tree.txt",append=TRUE,split=TRUE)
Train_Tree = rpart(click ~ app_id_factor + device_type   + C16_factor  + C20_factor + hour_factor  +  app_domain_factor + app_category_factor + device_id_factor, data=Train, method="class",minbucket=200, cp=-1)
summary(Train_Tree)
# n= 103304 
# 
prp(Train_Tree)
rpart.plot(Train_Tree, tweak=1.4)


print(Train_Tree)
printcp(Train_Tree)
plotcp(Train_Tree)
jpeg("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Train_Tree1.jpeg") # preparing to save the next tree as a JPEG
dev.off()


# Presentation 3: Rather than using cost directly, we bucket costs and consider everyone in the group equalCreates a multi-class classification problem

#Gini Index
#Train_Tree = rpart(click ~ app_id_factor + device_type  + C16_factor  + C20_factor + hour_factor +  app_domain_factor + app_category_factor + device_id_factor, data = Train, method="class", minbucket=200, cp=-1, parms=list(split='information'))
Train_Tree = rpart(click ~ device_type + device_conn_type + C16_factor + C18 + C20_factor + hour_factor + banner_pos + app_category_factor, data = Train, method="class", minbucket=5000, cp=-1, parms=list(split='information'))

prp(Train_Tree, box.palette = "Blues", tweak = 1.6)
rpart.plot(Train_Tree, tweak=1.8)
print(Train_Tree)
summary(Train_Tree)

rpart.plot(Train_Tree,tweak=1.4,fallen.leaves=FALSE)
print(Train_Tree)
summary(Train_Tree)

dev.off()

# Make predictions on training set
PredictTrain = predict(Train_Tree, type = "class")
MyTableTrain=table(Train$click, PredictTrain)
MyTableTrain
#   PredictTrain
#0     1
#0 85878     0
#1 17426     0
AccuTrain=(MyTableTrain[1,1]+MyTableTrain[2,2])/sum(MyTableTrain)
AccuTrain


## Not possible to be calculated since the conditional one's are all zero.
library(ROCR)

Predict_Train = predict(Train_Tree, newdata = Train)
Predict_Train

pred_Train = prediction(Predict_Train[,2], Train$click)
# prediction turns input data into a standardized format for ROCR
perf_Train = performance(pred_Train, "tpr", "fpr")
plot(perf_Train)
plot(perf_Train, colorize=TRUE, print.cutoffs.at=seq(0.1,0.2,0.01), text.adj=c(-0.2,0.7))
abline(h=0.5)

as.numeric(performance(pred_Train, "auc")@y.values)
# 0.6745448


##### Random Forest ######
library(randomForest)
Train_Forest = randomForest(click ~ device_type + device_conn_type + C16_factor + C18 + C20_factor + hour_factor + banner_pos + app_category_factor, data = Train, ntree=2000, nodesize=25 )

# Make predictions on testing set
PredictForest = predict(Train_Forest, newdata = Train)
table(Train$click, PredictForest)

#getting 1st tree
getTree(Train_Forest,1)
#getting 2nd tree
getTree(Train_Forest,2)

importance(Train_Forest)
varImpPlot(Train_Forest)

#Try to have more than 2500 for Random Forests
### we have to do importance & varImpPlot 

######## (6) Subset Data --> Validation ################################################
str(Validation)

Validation$app_domain_factor <- 0
for(i in 1:length(Validation$app_domain)){
  if(Validation$app_domain[i] =="7801e8d9"){
    Validation$app_domain_factor[i] <- 1
  } 
  else {
    Validation$app_domain_factor[i] <- 0
  }
}

#Create new Column called app_category_factor, replacing thhe original column app_category, and if its value was 
#equal to 07d7df22, the most frequent number, then app_category ==1, otherwirse == 0
Validation$app_category_factor <- 0
for(i in 1:length(Validation$app_category)){
  if(Validation$app_category[i] =="07d7df22"){
    Validation$app_category_factor[i] <- 1
  } 
  else {
    Validation$app_category_factor[i] <- 0
  }
}

#Create new Column called app_id_factor, replacing thhe original column app_id, and if its value was 
#equal to ecad2386, the most frequent number, then app_id ==1, otherwirse == 0
Validation$app_id_factor <- 0
for(i in 1:length(Validation$app_id)){
  if(Validation$app_id[i] == "ecad2386"){
    Validation$app_id_factor[i] <- 1
  } 
  else {
    Validation$app_id_factor[i] <- 0
  }
}

#Create new Column called device_id_factor, replacing thhe original column device_id, and if its value was 
#equal to a99f214a, the most frequent number, then device_id ==1, otherwirse == 0
Validation$device_id_factor <- 0
for(i in 1:length(Validation$device_id)){
  if(Validation$device_id[i] =="a99f214a"){
    Validation$device_id_factor[i] <- 1
  } 
  else {
    Validation$device_id_factor[i] <- 0
  }
}

#Create new Column called C16_factor, replacing thhe original column C16, and if its value was 
#equal to 50, the most frequent number, then C16 ==1, otherwirse == 0
Validation$C16_factor <- 0
for(i in 1:length(Validation$C16)){
  if(Validation$C16[i] == 50){
    Validation$C16_factor[i] <- 1
  } 
  else {
    Validation$C16_factor[i] <- 0
  }
}

#Create new Column called C20_factor, replacing thhe original column C20, and if its value was 
#equal to -1, the most frequent number, then C20 ==1, otherwirse == 0Train$C20_factor <- 0
Validation$C20_factor <- 0
for(i in 1:length(Validation$C20)){
  if(Validation$C20[i] == -1){
    Validation$C20_factor[i] <- 1
  } 
  else {
    Validation$C20_factor[i] <- 0
  }
}

#Create new column from hour called hour.1 that only includes the last two digits from hour

#Create new Column from hour.1 called hour.1_factor, and depending on its range of hours assing a "1", "2", or "3"  
#“1” from 0 to 8, “2” from 8 to 16, and “3” from 16 to 24. 
Validation$hour_factor <- 0
for(i in 1:length(Validation$hour.1)){
  if(Validation$hour.1[i] <= 8 && Validation$hour.1[i] >= 0){
    Validation$hour_factor[i] <- 1
  } 
  else if(Validation$hour.1[i] > 8 && Validation$hour.1[i] <= 16) {
    Validation$hour_factor[i] <- 2
  }
  else if(Validation$hour.1[i] > 16 && Validation$hour.1[i] <= 23){
    Validation$hour_factor[i] <- 3
  }
}

str(Validation)
write.csv(Validation, "/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Validation2.csv")
Validation = read.csv("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Validation2.csv")



# Logistic Regression
# Optimization 1 (Does not include: site_id, site_domain, device_ip, device_model, C14, C15, C17, C19, C21, site_category and app_id)
ValidationModel = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C18 + C20_factor + hour_factor 
                 + banner_pos +  app_domain_factor + app_category_factor + device_id_factor, data = Validation, family=binomial)
summary(ValidationModel)

#Optimization 2 (Eliminating hour_factor)
ValidationModel = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C18 + C20_factor 
                      + banner_pos +  app_domain_factor + app_category_factor + device_id_factor, data = Validation, family=binomial)
summary(ValidationModel)

#Optimization 3 (Eliminating C18)
ValidationModel = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C20_factor 
                      + banner_pos +  app_domain_factor + app_category_factor + device_id_factor, data = Validation, family=binomial)
summary(ValidationModel)

#Optimization 4 (Eliminating device_id_factor)
ValidationModel = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C20_factor 
                      + banner_pos +  app_domain_factor + app_category_factor, data = Validation, family=binomial)
summary(ValidationModel)

#Optimization 5 (Eliminating app_domain_factor)
ValidationModel = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C20_factor 
                      + banner_pos + app_category_factor, data = Validation, family=binomial)
summary(ValidationModel)

#Optimization 6 (Eliminating C15)
ValidationModel = glm(click ~ device_type + device_conn_type + C16_factor + C20_factor 
                      + banner_pos + app_category_factor, data = Validation, family=binomial)
summary(ValidationModel)

#final model:-0.69875 -0.20422(device_type) - 0.24299(device_conn_type) - 1.19660(C16_factor) + 0.30090(C20_factor) 
# +  0.13256(banner_pos) + 0.41580(app_category_factor)

## Now, we will start the prediction model on the validation
library(stats)
library(gplots)
library(caTools)
library(ROCR)
predict_validation = predict(ValidationModel, type="response")
ROCR_pred_validation = prediction(predict_validation, Validation$click)
ROCR_Curve_Validation = performance(ROCR_pred_validation, "tpr", "fpr")
#def.off()

#Curve
plot(ROCR_Curve_Validation)
#Color Curve
#plot(ROCR_Curve_Validation, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
plot(ROCR_Curve_Validation, colorize=TRUE, print.cutoffs.at=seq(0.1,0.2,0.01), text.adj=c(-0.2,0.7))

as.numeric(performance(ROCR_pred_validation, "auc")@y.values)
#0.6244868

#### ROC, AUC, accuracy ####

predict_validation = predict(ValidationModel, type="response")

table(Validation$click, predict_validation > 0.179)
#  FALSE  TRUE
#0 14399   7071
#1  2267   2089

# Accuracy for cutoff =0. (14399+2089)/(14399+2089+2267+7071) = 0.6384264        vs.    63.46(Train)
#Sensitivity = 2089 / (2089+2267) = 0.4795684            vs.     48.63%(Train)
#Specificity = 14399 / (14399+7071) = 0.6706567 --> 1 - 0.6706567  = 0.3293433 = FPR          vs. 0.3353129%(Train)
#baseline= (14399+7071)/(14399+7071+2267+2089) = 0.8313328                 vs.        0.8313134(Train)
# Best Threshold based on Accuracy and lowest FPR is at t = 0.179

table(Validation$click, predict_validation > 0.178)

table(Validation$click, predict_validation > 0.1)
#   FALSE  TRUE
#0  3168 18302
#1   192  4164

# Accuracy for cutoff =0.1 (3168+4164)/(3168+4164+192+18302) = 0.2838999            vs.   0.2766882(Train)

#Sensitivity = 4164 / (4164+192) = 0.9559229          vs.     0.9554115(Train)


#Specificity = 3168 / (3168+18302) = 0.1475547--> 1 - 0.1475547 =0.8524453 = FPR       vs. 0.8610354(Train)

#baseline= (3168+18302)/(3168+18302+192+4164) = 0.8313134

table(Validation$click, predict_validation > 0.2)
#FALSE  TRUE
#0 15040  6430
#1  2444  1912

# Accuracy for cutoff =0.2 (15040+1912)/(15040+1912+2444+6430) = 0.6563928          vs.     0.70(Train)

#Sensitivity = 1912 / (1912+2444) = 0.4389348                   vs.     0.3623321(Train)

#Specificity = 15040 / (15040+6430) = 0.7005123; 1 - 0.7005123 = 0.2994877  = FPR      vs. 0.2257155(Train)

#baseline= (15040+6430)/(15040+6430+2444+1912) = 0.8313134

#### CLASSIFICATION TREE ON THE VALIDATION SET USING GINI-INDEX METHOD

library(rpart)
library(rpart.plot)
sink("/Users/jaimegarcia/Desktop/DESKTOP_101/code/R_code/SMU/Assigments/Project1/Project_Tree_Validation.txt",append=TRUE,split=TRUE)
Validation_Tree = rpart(click ~ device_type + device_conn_type + C16_factor + C18 + C20_factor + hour_factor + banner_pos + app_category_factor, data = Validation, method="class", minbucket=50, cp=-1, parms=list(split='information'))

prp(Validation_Tree, box.palette = "Blues", tweak = 1.6)
rpart.plot(Validation_Tree, tweak=1.8)
print(Validation_Tree)
summary(Validation_Tree)

rpart.plot(Validation_Tree,tweak=1.4,fallen.leaves=FALSE)
print(Validation_Tree)
summary(Validation_Tree)



predict_validation = predict(Validation_Tree, type = "class")
MyTableValidation=table(Validation$click, predict_validation)
MyTableValidation
#predict_validation
#0     1
#0 21366   104
#1  4250   106
AccuValidation=(MyTableValidation[1,1]+MyTableValidation[2,2])/sum(MyTableValidation)
AccuValidation
#0.8314102


library(ROCR)

predict_validation = predict(Validation_Tree, newdata = Validation)
predict_validation

predict_validation = prediction(predict_validation[,2], Validation$click)
# prediction turns input data into a standardized format for ROCR
perf_Validation = performance(predict_validation, "tpr", "fpr")
plot(perf_Validation)
plot(perf_Validation, colorize=TRUE, print.cutoffs.at=seq(0.1,0.2,0.01), text.adj=c(-0.2,0.7))
abline(h=0.5)

as.numeric(performance(predict_validation, "auc")@y.values)
# 0.6794211

##### Random Forest ######
library(randomForest)
Validation_Forest = randomForest(click ~ device_type + device_conn_type + C16_factor + C18 + C20_factor + hour_factor + banner_pos + app_category_factor, data = Validation, ntree=2000, nodesize=25 )

# Make predictions on testing set
PredictForest_validation = predict(Validation_Forest, newdata = Validation)
table(Validation$click, PredictForest_validation)

#getting 1st tree
getTree(Validation_Forest,1)
#getting 2nd tree
getTree(Validation_Forest,2)

importance(Validation_Forest)
varImpPlot(Validation_Forest)
####### (8)  NOW, Testing our model on the Testing set

ValidationModel = glm(click ~ device_type + device_conn_type + C16_factor + C20_factor 
                      + banner_pos + app_category_factor, data = Validation, family=binomial)
summary(ValidationModel)

#final model:-0.69875 -0.20422(device_type) - 0.24299(device_conn_type) - 1.19660(C16_factor) + 0.30090(C20_factor) 
# +  0.13256(banner_pos) + 0.41580(app_category_factor)

################################# Now, we will start the prediction model on the validation

# Split Data Training and testing
library(caTools)
set.seed(88)
spl3 = sample.split(Validation$click, SplitRatio = 2/3)
Validation2 = subset(Validation, spl3==TRUE)
Test2 = subset(Validation, spl3==FALSE)

# Check Data
str(Validation2)
str(Test2)


library(stats)
library(gplots)
library(caTools)
library(ROCR)

Test2_Model = glm(click ~ device_type + device_conn_type + C15 + C16_factor + C20_factor 
                      + banner_pos +  app_domain_factor + app_category_factor, data = Test2, family=binomial)
summary(Test2_Model)
predict_test2 = predict(Test2_Model, type="response")
ROCR_pred_test2 = prediction(predict_test2, Test2$click)
ROCR_Curve_test2 = performance(ROCR_pred_test2, "tpr", "fpr")
#def.off()

#Curve
plot(ROCR_Curve_test2)
#Color Curve
#plot(ROCR_Curve_Validation, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
plot(ROCR_Curve_test2, colorize=TRUE, print.cutoffs.at=seq(0.1,0.2,0.01), text.adj=c(-0.2,0.7))
abline(h=0.5)


as.numeric(performance(ROCR_pred_test2, "auc")@y.values)
#0.625625

#### ROC, AUC, accuracy ####

predict_test2 = predict(Test2_Model, type="response")

table(Test2$click, predict_test2 > 0.179)
#
#FALSE TRUE
#0  4812 2345
#1   762  690






