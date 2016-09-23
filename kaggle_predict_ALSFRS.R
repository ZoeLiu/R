##################################################################
## Impute and roll up metrics using relationships between metrics
## use correlation to keep the scores

# Data used in the analysis were obtained from the Pooled Resource Open-Access ALS Clinical Trials (PRO-ACT) Database.
# In 2011, Prize4Life, in collaboration with the Northeast ALS Consortium, and with funding from the ALS Therapy Alliance, formed the 
# Pooled Resource Open-Access ALS Clinical Trials (PRO-ACT) Consortium. The data available in the PRO-ACT Database has been volunteered
# by PRO-ACT Consortium members."

#########################################################################################
getwd()

# link to competition: https://inclass.kaggle.com/t/239550/talk-data-to-me

# Change this to your data directory
data.dir <- "/Users/ZoeL/Google Drive/Data Mining and Analysis/Kaggle/"
setwd(data.dir)
library(plyr)
library(tree)
library(psych)
library(randomForest)
library(caret)
library(RANN)
library(ipred)

# Read in each data files into a data frame
training.target <- read.csv("training_target.csv")
training.features <- read.csv("training_features.csv",na.strings = "NA")
validation.features <- read.csv("validation_features.csv",na.strings = "NA")
validation.target <- read.csv("validation_target.csv")
leaderboard.features<- read.csv("leaderboard_features.csv",na.strings = "NA")
leaderboard.predictions <- read.csv("leaderboard_predictions-example.csv") 
test.features <- read.csv("test_features.csv",na.strings = "NA")

training.features <- training.features[training.features$subject.id!=525450,]


# combine all features
all.features <- rbind(training.features, validation.features, leaderboard.features, test.features)
all.targets <- rbind(training.target, validation.target)


# some dp
all.features <- within(all.features, {
  family.hist <- ifelse(family_als_hist.Y ==0 & family_als_hist.N==0, 1, 0)
  riluzole.use <- ifelse(if_use_riluzole.Yes==0 & if_use_riluzole.No==0, 1,0)
  age_40 <- as.integer(age >= 40)
  age_60 <- as.integer(age >= 60)
  na.uric_acid <- as.integer(!is.na(max.uric_acid))
})
table(all.features$na.uric_acid)
table(all.features$na.uric_acid[1:2524])

# combine svc_percent and fvc_percent scores
svc_pnt <- grep('svc_percent',names(all.features), value=T)
fvc_pnt <- grep('fvc_percent',names(all.features), value=T)
svc <- grep('svc',names(all.features), value=T)
fvc <- grep('fvc',names(all.features), value=T)
svc_fvc <- gsub('svc_percent','svc_fvc_percent', svc_pnt)

for (i in 1:length(svc_pnt)){
  all.features[,svc_fvc[i]] <- rowMeans(all.features[,c(svc_pnt[i],fvc_pnt[i])], na.rm=T)
}

#tail(all.features[, c('meansquares.svc_fvc_percent', 'meansquares.fvc_percent','meansquares.svc_percent')])

#write.csv(all.features,'all.features.csv')

# Fill in diag_delta using regression
# cor(all.features$diag_delta,all.features$last.date.svc, use="pairwise.complete.obs")
lm <-lm(diag_delta ~ onset_delta, data=all.features)
diag_delta.pred <- predict(lm, all.features)
all.features[is.na(all.features$diag_delta), 'diag_delta'] <- diag_delta.pred[is.na(all.features$diag_delta)]
all.features$diag_delta[all.features$diag_delta>0] <- 0
#summary(all.features$diag_delta)


#library(plyr)
#count(all.features, onset_site.Limb ~ onset_site.Bulbar + onset_site.Limb_and_Bulbar + onset_site)


# Estimate height using weight, gender
lm.max <- lm(max.height~max.weight + gender.M + age, all.features)
lm.min <- lm(min.height~min.weight + gender.M + age, all.features)
lm.last <- lm(last.height~last.weight + gender.M + age, all.features)
lm.mean <- lm(mean.height~mean.weight + gender.M + age, all.features)

max.height.pred <- predict(lm.max, all.features)
min.height.pred <- predict(lm.min, all.features)
last.height.pred <- predict(lm.last, all.features)
mean.height.pred <- predict(lm.mean, all.features)

height.na <- is.na(all.features$max.height)
all.features$max.height[height.na] <- max.height.pred[height.na]
all.features$min.height[height.na] <- min.height.pred[height.na]
all.features$last.height[height.na] <- last.height.pred[height.na]
all.features$mean.height[height.na] <- mean.height.pred[height.na]

# Estimate BMI using height and weight
bmi.na <- is.na(all.features$max.bmi)
all.features <- within(all.features, {
                       max.bmi[bmi.na] <- max.weight[bmi.na]/ (max.height[bmi.na])^2   
                       min.bmi[bmi.na] <- min.weight[bmi.na]/ (min.height[bmi.na])^2   
                       last.bmi[bmi.na] <- last.weight[bmi.na]/ (last.height[bmi.na])^2  
                       mean.bmi[bmi.na] <- mean.weight[bmi.na]/ (mean.height[bmi.na])^2 
                       })


# summary(all.features$min.height)
# summary(all.features$min.weight)
# cor(max.height.pred, all.features$max.height, use="pairwise.complete.obs")
#plot(all.features$onset_diag_delta [1:2424], training.target$ALSFRS_slope)

## impute the q5a, q5b scores 
# hands <- grep('hands', names(all.features), value=T)
# q4 <- grep('q4', names(all.features), value=T)
# q5a <- grep('q5a', names(all.features), value=T)
# q5b <- grep('q5b', names(all.features), value=T)
# for (i in 1:length(hands)) {
#   na.row_a <- is.na(all.features[, q5a[i]])
#   all.features[na.row_a, q5a[i]] <- all.features[na.row_a, hands[i]] - all.features[na.row_a, q4[i]]
#   
#   na.row_b <- is.na(all.features[, q5b[i]])
#   all.features[na.row_b, q5b[i]] <- all.features[na.row_b, hands[i]] - all.features[na.row_b, q4[i]]
# }

#head(all.features[1000:1020,c('max.hands','max.q4_handwriting','max.q5a_cutting_without_gastrostomy','max.q5b_cutting_with_gastrostomy')])

# remove unwatned variables, svc, fvc, normal variables; first date, last date, sum/num visits, q,respiratory, temperature variables,
# meansquares
remove.list <- c('race.Black', 'race.White', 'race.Unknown', 'race.Hispanic', 'race.Asian', 
                 'race.Other', 'race.American_Indian', 'gender.F',
                 'slope.height.slope','slope.bmi.slope', 
                  svc, fvc, 
                  grep('first\\.date|last\\.date|normal', names(all.features), value=T),
                  grep('^(sum\\.|num.visits\\.)', names(all.features), value=T),
                  grep("q[1-9]|r[2-3]|respiratory$", names(all.features), value=T),
                  grep("temperature|meansquares|sd\\.", names(all.features), value=T))
all.features<- subset(all.features, select = !(names(all.features) %in% remove.list))

 
# remove variables with all missing in test_feature data (removed bmi/height slope)
na.check1 <- sapply(test.features,function(x){sum(is.na(x))/length(x)})
na.check2 <- sapply(test.features,function(x){sum(is.na(x))})
na.check <-data.frame(na.check2, na.check1)
test0 <- row.names(na.check[na.check1==1,])
all.features <- subset(all.features, select = !(names(all.features) %in% test0))

## Identify Near-Zero Variance variables 
# nzv <- nearZeroVar(all.features, saveMetrics= TRUE)
# nzv <- nearZeroVar(all.features)
# all.features <- all.features[,-nzv]
# summary(all.features$mouth.slope)


# fill the missing data with median
all.features.fill <- na.roughfix(all.features)

# create onset age, log of onset_delta, onset and dig time difference
all.features.fill <- within(all.features.fill, {
  onset_age <- age + onset_delta/365
  log_onset_delta <- log(-onset_delta)
  onset_diag_delta <- diag_delta - onset_delta
  onset_site <- factor((onset_site.Limb==1)*1 + (onset_site.Bulbar==1)*2 + (onset_site.Limb_and_Bulbar)*3)
})


# create gender*uric_acid
uric <- grep('uric_acid', names(all.features.fill), value=T)
male.uric <- gsub("uric_acid","uric_acid.male", uric)
for (i in 1:length(uric)){
  all.features.fill[,male.uric[i]] <- all.features.fill[,uric[i]]*all.features$gender.M
}

all.features.fill <- all.features.fill[, -grep("\\.Limb|^(diag_delta|age|onset_delta|gender.M)$", names(all.features.fill))]


## Check linear dependencies
# comboInfo <- findLinearCombos(all.features.fill)
# comboInfo
# names(all.features.fill[,comboInfo$linearCombos[[1]]])
# cors <- cor(all.features.fill[,comboInfo$linearCombos[[1]]])
# Note: variables with linear dependencies are removed

## Check variables with high correlations
cors <- cor(all.features.fill[,-grep('onset_site',names(all.features.fill))])
highlyCor <- findCorrelation(cors, 0.8)
names(all.features.fill[,highlyCor[-c(62,66)]])
#all.features.fill <- all.features.fill[,-highlyCor[-c(62,66)]] #keep mean.bmi and log_onset_delta


# merge the target with the features
all.dat <- merge(x=all.features.fill, y=all.targets, by="subject.id", all.x=T)
# create model data
model.dat <- all.dat[all.dat$subject.id %in% c(training.features$subject.id,validation.features$subject.id),]
#summary(model.dat$ALSFRS_slope)
model.dat <- model.dat[,-1]
#grep('ALSFRS_slope', names(model.dat))
leaderboard.dat <- all.dat[all.dat$subject.id %in% c(leaderboard.features$subject.id),]



## Fit model without uric acid
model.dat1 <- model.dat[,-grep("uric_acid", names(model.dat))[-11]]
## model for those with uric acid data
model.dat2 <- model.dat[model.dat$na.uric_acid==1,]

### -----------------  Recursive Feature Elimination using random Forest--------------------------
# library(caret)
# set.seed(1)
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# results <- rfe(model.dat[,-214], model.dat[,214], sizes=c(1:5, 10, 15,20, 25,30,35,50), rfeControl = control)
# plot(results, type=c("g","o"))
# 
# print(results)
# predictors <- predictors(results)  # 15 variables selected
# names(results)
# plot(results$results$Variables,results$results$RMSE)
# # Note: including uric acid in the total sample model would have uric acid showing up everywhere

set.seed(1)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results1 <- rfe(model.dat1[,-203], model.dat1[,203], sizes=c(1:5, 10, 15:20, 25,30,40), rfeControl = control)
plot(results1, type=c("g","o"))
print(results1)
predictors1 <- predictors(results1)  # 40 variables selected
results1$fit

# second test
set.seed(1)
results2 <- rfe(model.dat1[,-203], model.dat1[,203], sizes=c(40:60), rfeControl = control)
plot(results2, type=c("g","o"))
print(results2)
predictors2 <- predictors(results2)  # 56 variables selected
results2$fit

# test without max,min variables
model.dat3 <- model.dat1[, -grep("max|min", names(model.dat1))]
results3 <- rfe(model.dat3[,-129], model.dat1[,129], sizes=c(5, 10, 12:25,30,40), rfeControl = control)
plot(results3, type=c("g","o"))
print(results3)
names(results3)
results3$fit
predictors3<-predictors(results3)

# test with updated model.dat1 (use highcor function to remove highly correlated metrics)
set.seed(1)
results4 <- rfe(model.dat1[,-131], model.dat1[,131], sizes=c(10:20,25,30), rfeControl = control)
plot(results4, type=c("g","o"))
print(results4)
predictors4 <-predictors(results4)  # 20 variables selected
results4$fit



## -------------   Model development using the variables selected -------------------------
#install.packages('doMC')
library(doMC)
registerDoMC(cores = 2)

# Tune mtry to find the best # of predictors used in each tree of RF
tune.rf <- tuneRF(x=model.dat1[,predictors4],y=model.dat1[,131], mtryStart=2, nTreeTry=500, stepFactor = 1, doBest=T)
str(tune.rf)

## random forest
set.seed(99)
fitControl <- trainControl(method ="repeatedcv", number=10, repeats=10)
rfFit <- train(ALSFRS_slope ~ ., data=model.dat1[,c(predictors4,'ALSFRS_slope')], method = "rf", trControl = fitControl,verbose = FALSE,tuneGrid = data.frame(mtry= c(2,3,4,5,6)))
rfFit$results
imp <- importance(rfFit$finalModel)
names(rfFit)
#predictions
pred <- predict(rfFit$finalModel, model.dat1)
pred2 <- predict(rfFit, model.dat1)
mean((pred-model.dat1$ALSFRS_slope)^2)
mean((pred2-model.dat1$ALSFRS_slope)^2)
leaderboard.dat$ALSFRS_slope <- predict(rfFit,leaderboard.dat)
summary(leaderboard.dat$ALSFRS_slope)
summary(pred)
summary(model.dat1$ALSFRS_slope)
leaderboard.prediction <- leaderboard.dat[order(leaderboard.dat$subject.id),c('subject.id','ALSFRS_slope')]
write.csv(leaderboard.prediction,'leaderboard.prediction_randomForest_selectedVars_newImpute.csv',row.names = F)

## svm
set.seed(88)
svmFit <- train(ALSFRS_slope ~ ., data=model.dat1[,c(predictors4,'ALSFRS_slope')], method = "svmRadial", trControl = fitControl,preProcess=c("center","scale"),tuneLength=8)
svmFit
pred.svm <- predict(svmFit$finalModel,model.dat1)
mean((pred.svm-model.dat1$ALSFRS_slope)^2)
names(svmFit$finalModel)
resamps <- resamples(list(RF = rfFit,
                          SVM = svmFit))
resamps
summary(resamps)
lpred.svm <-predict(svmFit,leaderboard.dat)
summary(lpred.svm)

# re-run rf
set.seed(77)
train <- sample(1:nrow(model.dat1), nrow(model.dat1)*0.7)
set.seed(66)
rf1 <- randomForest(ALSFRS_slope ~ ., data=model.dat1, subset=train)
imp.new <-importance(rf1)
pred.test1 <-predict(rf1, newdata=model.dat1[-train,])
mean((pred.test1-model.dat1[-train,'ALSFRS_slope'])^2)

# use data including uric acid
set.seed(66)
rf2 <- randomForest(ALSFRS_slope ~ ., data=model.dat, subset=train)
imp.new2 <-importance(rf2)
pred.test2 <-predict(rf2, newdata=model.dat[-train,])
mean((pred.test2-model.dat[-train,'ALSFRS_slope'])^2)
# Test MSE: 0.3220937
leaderboard.dat$ALSFRS_slope <- predict(rf2, newdata=leaderboard.dat)
write.csv(leaderboard.dat[,c("subject.id","ALSFRS_slope")], 'leaderboard_prediction_best_RF.csv', row.names=F)
######### Final Prediction on test data ########
test.dat <- all.dat[all.dat$subject.id %in% c(test.features$subject.id),]
test.dat$ALSFRS_slope <- predict(rf2, newdata=test.dat)
write.csv(test.dat[,c("subject.id","ALSFRS_slope")], 'test.feature_predictions_best_RF.csv', row.names=F)


# use top rf selected variables to run svm (last rf result)
library(e1071)
regressors1 <- c('mean.alsfrs_total','mean.svc_fvc_percent','mean.hands','mean.leg','mean.slope.alsfrs_total','mean.slope.svc_fvc_percent',
                'mean.slope.hands','mean.slope.weight','mean.trunk','log_onset_delta','slope.svc_fvc_percent.slope','weight.slope')
fml <- as.formula(paste('ALSFRS_slope', paste(regressors1,collapse=' + '), sep='~'))
svm.mod1 <- svm(fml, model.dat[train,], kernel="radial", gamma=0.5, cost=1, scale=T)
svm.pred1 <- predict(svm.mod1, model.dat[-train,])
mean((svm.pred1-model.dat[-train,'ALSFRS_slope'])^2)

# tune the parameters
set.seed(1)
tune.out1 <- tune(svm, fml, data=model.dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,50,100), gamma=c(0.5,1,2,3,4)))
summary(tune.out1)


### Use all variables for svm
regressors <- names(model.dat[,-209])
fml <- as.formula(paste('ALSFRS_slope', paste(regressors,collapse=' + '), sep='~'))
svm.mod <- svm(fml, model.dat[train,], kernel="radial", gamma=1, cost=1, scale=T)
svm.pred <- predict(svm.mod1, model.dat[-train,])
mean((svm.pred1-model.dat[-train,'ALSFRS_slope'])^2)

## polynomial regression




# use top rf selected (current rf result)
regressors2 <- c('log_onset_delta','onset_diag_delta','svc_fvc_percent.slope','mean.slope.weight','mean.slope.alsfrs_total',
                  'last.slope.svc_fvc_percent', 'mean.slope.svc_fvc_percent')
fml <- as.formula(paste('ALSFRS_slope', paste(regressors2,collapse=' + '), sep='~'))
svm.mod2 <- svm(fml, model.dat[train,], kernel="radial", gamma=0.5, cost=1)
svm.pred2 <- predict(svm.mod2, model.dat[-train,])
mean((svm.pred2-model.dat[-train,'ALSFRS_slope'])^2)

install.packages('FactoMineR')
library(FactoMineR)
model.dat$onset_site.Limb <- as.numeric(model.dat$onset_site==1)
pca.dat <- model.dat
pca.dat$onset_site <- NULL
pr.out <- prcomp(pca.dat, scale=T)
summary(pr.out)
plot(pr.out)
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(pve, type="o")

pca <- PCA(pca.dat, scale.unit=TRUE, ncp=10, graph=T)
pca.list <- dimdesc(pca)

# PCA regression
install.packages('pls')
library(pls)
pcr.fit <- pcr(ALSFRS_slope~., data=pca.dat, subset=train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP") 
summary(pcr.fit)
pcr.pred <- predict(pcr.fit, pca.dat[-train,], ncomp=56)
mean((pcr.pred-pca.dat$ALSFRS_slope[-train])^2)
# MSE: 0.348631

# SVM using PCA top variables:
topvars <- c('last.alsfrs_total','min.alsfrs_total','mean.alsfrs_total','max.alsfrs_total','last.trunk',
'mean.trunk','min.trunk','max.trunk','last.hands','last.leg','last.alsfrs_r_total','max.weight',
'mean.weight','min.weight','last.weight','max.uric_acid.male','last.uric_acid.male','mean.uric_acid.male',
'min.uric_acid.male','max.height','mean.height','min.height','last.height','mean.slope.alsfrs_total',
'mean.slope.alsfrs_r_total','alsfrs_total.slope','alsfrs_r_total.slope','max.slope.alsfrs_total',
'max.slope.alsfrs_r_total','mean.slope.mouth','mean.slope.leg','mean.slope.hands',
'log_onset_delta','onset_diag_delta','onset_age')
fml <- as.formula(paste('ALSFRS_slope', paste(topvars,collapse=' + '), sep='~'))
svm.mod3 <- svm(fml, model.dat[train,], kernel="radial", gamma=0.5, cost=1)
svm.pred3 <- predict(svm.mod3, model.dat[-train,])
mean((svm.pred3-model.dat[-train,'ALSFRS_slope'])^2)


topvars2 <- c('last.alsfrs_total','last.trunk','last.hands','last.leg','last.alsfrs_r_total','max.weight','max.uric_acid.male','max.height',
  'max.slope.uric_acid.male','mean.slope.alsfrs_total','mean.slope.alsfrs_r_total','log_onset_delta','onset_age','onset_diag_delta')
fml <- as.formula(paste('ALSFRS_slope', paste(topvars2,collapse=' + '), sep='~'))
svm.mod4 <- svm(fml, model.dat[train,], kernel="radial", gamma=1, cost=1)
svm.pred4 <- predict(svm.mod4, model.dat[-train,])
mean((svm.pred4-model.dat[-train,'ALSFRS_slope'])^2)

install.packages('gam')
library(gam)
gam.fit <- gam (ALSFRS_slope ~ s(last.alsfrs_total,7) + s(last.hands,1) + s(last.leg,8) + s(last.alsfrs_r_total,1) +
               s(max.weight,1) + s(max.uric_acid.male,1) + s(max.height, 1) + s(max.slope.uric_acid.male,1) + s(mean.slope.alsfrs_total,7)
               + s(mean.slope.alsfrs_r_total,4) + s(log_onset_delta,1) + s(onset_age,3), data=model.dat[train,])
summary(gam.fit)
#plot.gam(gam.fit, se=T, col="blue")
pred <- predict(gam.fit, model.dat[-train,])
mean((pred-model.dat[-train,'ALSFRS_slope'])^2)
test.dat$ALSFRS_slope <- predict(gam.fit, test.dat)
write.csv(test.dat[, c('subject.id','ALSFRS_slope')], 'test_feature_predictions_splines.csv', row.names=F)

gam.fit2 <- gam (ALSFRS_slope ~ ns(last.alsfrs_total,7) + ns(last.hands,1) + ns(last.leg,8) + ns(last.alsfrs_r_total,1) +
                  ns(max.weight,1) + ns(max.uric_acid.male,1) + ns(max.height, 1) + ns(max.slope.uric_acid.male,1) + ns(mean.slope.alsfrs_total,7)
                + ns(mean.slope.alsfrs_r_total,4) + ns(log_onset_delta,1) + ns(onset_age,3), data=model.dat[train,])
summary(gam.fit2)
#plot.gam(gam.fit, se=T, col="blue")
pred2 <- predict(gam.fit2, model.dat[-train,])
mean((pred2-model.dat[-train,'ALSFRS_slope'])^2)
test.dat$ALSFRS_slope <- predict(gam.fit, test.dat)



##  Random Forest for the uric acid
set.seed(77)
train <- sample(1:nrow(model.dat2), nrow(model.dat2)*0.7)
set.seed(66)
rf.uric_acid <- randomForest(ALSFRS_slope ~ ., data=model.dat2, subset=train, mtry=40)
imp.uric_acid <-importance(rf.uric_acid)
pred.test.uric_acid <-predict(rf.uric_acid, newdata=model.dat2[-train,])
mean((pred.test.uric_acid-model.dat2[-train,'ALSFRS_slope'])^2)

set.seed(1)
results.u <- rfe(model.dat2[,-143], model.dat2[,143], sizes=c(15:25,30,35), rfeControl = control)
plot(results.u, type=c("g","o"))
print(results.u)
predictors4 <-predictors(results4)  # 20 variables selected
results4$fit



# 
# set.seed(99)
# rfFit2 <- train(ALSFRS_slope ~ ., data=model.dat1, method = "rf", trControl = fitControl,verbose = FALSE,tuneGrid = data.frame(mtry= 15))
# rfFit2$results
# imp2 <- importance(rfFit2$finalModel)
# 
# grep("svc", names(model.dat3),value=T)


