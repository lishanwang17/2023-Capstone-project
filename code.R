
df<-read.csv("/Users/wanglishan/Desktop/uw/591/impute_1.csv")


df[,c("AGE"         ,               "ADI_STATEDECILE"     ,       "ADI_NATIONALDECILE"    ,     "NO_SCHOOL_PCT"   ,          
       "HIGH_SCHOOL_PCT"   ,         "SOME_COLLEGE_PCT"       ,    "ASSOCIATE_DEGREE_PCT"  ,     "BACHELOR_DEGREE_PCT"  ,     
       "MASTERS_DEGREE_PCT"   ,      "PROFESSIONAL_DEGREE_PCT" ,   "DOCTRATE_DEGREE_PCT"   ,     "MARRIED_CHILD_UNDER18PCT"  ,
       "MARRIED_NOCHILD_UNDER18PCT", "SINGLE_PARENT_PCT"     ,     "PUBLIC_ASSISTANCE_PCT"  ,    "LABOR_CIVILIAN_ALL_PCT"    ,
       "LABOR_CIVILIAN_UNEMP_PCT" ,  "LABOR_ARMED_PCT"       ,     "NO_LABOR_PCT"           ,    "NO_VEHICLE_PCT"          ,  
       "ONE_VEHICLE_PCT"   ,         "UNDER100FPL_PCT"      ,      "UNDER200FPL_PCT"     ,       "FOODSTAMPS_SNAP_PCT"    ,   
       "VACANT_PCT"      ,           "RISK_SCORE"          ,       "NONRX_PAID"         ,        "RX_PAID"          ,         
       "EXPENSE"        ,            "REVENUE"             ,       "MER_RATE"           ,        "PCP_VST"          ,         
       "PCP_VST_6MO"     ,           "ER_CT_YR"            ,       "ER_CT"             ,         "IP_CT"            ,         
       "HCC_CT"             ,        "BP_COC_INDEX"         ,      "BP_VST"            ,         "SPEC_CT"         ,          
       "AV_ER_CT_YR"        ,        "AV_ER_CT"              ,     "START_MONTH"         ,       "CALENDAR_YM"       ,        
       "NO_OF_CALLS"       ,         "N_TICKETS_COMPLAIN"   ,      "N_TICKETS_ENQUIRY"    ,      "NO_CS_CAT"        ,         
       "MM_CT")] <- lapply(df[,c("AGE"         ,               "ADI_STATEDECILE"     ,       "ADI_NATIONALDECILE"    ,     "NO_SCHOOL_PCT"   ,          
                                  "HIGH_SCHOOL_PCT"   ,         "SOME_COLLEGE_PCT"       ,    "ASSOCIATE_DEGREE_PCT"  ,     "BACHELOR_DEGREE_PCT"  ,     
                                  "MASTERS_DEGREE_PCT"   ,      "PROFESSIONAL_DEGREE_PCT" ,   "DOCTRATE_DEGREE_PCT"   ,     "MARRIED_CHILD_UNDER18PCT"  ,
                                  "MARRIED_NOCHILD_UNDER18PCT", "SINGLE_PARENT_PCT"     ,     "PUBLIC_ASSISTANCE_PCT"  ,    "LABOR_CIVILIAN_ALL_PCT"    ,
                                  "LABOR_CIVILIAN_UNEMP_PCT" ,  "LABOR_ARMED_PCT"       ,     "NO_LABOR_PCT"           ,    "NO_VEHICLE_PCT"          ,  
                                  "ONE_VEHICLE_PCT"   ,         "UNDER100FPL_PCT"      ,      "UNDER200FPL_PCT"     ,       "FOODSTAMPS_SNAP_PCT"    ,   
                                  "VACANT_PCT"      ,           "RISK_SCORE"          ,       "NONRX_PAID"         ,        "RX_PAID"          ,         
                                  "EXPENSE"        ,            "REVENUE"             ,       "MER_RATE"           ,        "PCP_VST"          ,         
                                  "PCP_VST_6MO"     ,           "ER_CT_YR"            ,       "ER_CT"             ,         "IP_CT"            ,         
                                  "HCC_CT"             ,        "BP_COC_INDEX"         ,      "BP_VST"            ,         "SPEC_CT"         ,          
                                  "AV_ER_CT_YR"        ,        "AV_ER_CT"              ,     "START_MONTH"         ,       "CALENDAR_YM"       ,        
                                  "NO_OF_CALLS"       ,         "N_TICKETS_COMPLAIN"   ,      "N_TICKETS_ENQUIRY"    ,      "NO_CS_CAT"        ,         
                                  "MM_CT")], numeric)




df[,c("sex","race",
      "Ethnicity","Language","ZIPCLASS","RSA_Region","HOMELESS_IND",
      "SMOKE_DX","RISK","general_health","palliative","hospice",
      "DD","Eye","Hematological","Skeletal","Psychiatric","Cardiovascular",
      "Gastro","CNS","Infectious","Cancer","Metabolic","SA","Genital",
      "Pulmonary","Cerebrovascular","Diabetes","Pregnancy","Skin",
      "BH_IND","RX_IND","SUD_IND","SMI_IND","dementia_ind",
      "Mbr_No","case_management","health_coach","IsHealthHome_Eligible",
      "HealthHome_Status","MHIP","Pref_Email",
      "Pref_Call","Pref_mail","Pref_text")] <- lapply(df[,c("sex","race",
                                                            "Ethnicity","Language","ZIPCLASS","RSA_Region","HOMELESS_IND",
                                                            "SMOKE_DX","RISK","general_health","palliative","hospice",
                                                            "DD","Eye","Hematological","Skeletal","Psychiatric","Cardiovascular",
                                                            "Gastro","CNS","Infectious","Cancer","Metabolic","SA","Genital",
                                                            "Pulmonary","Cerebrovascular","Diabetes","Pregnancy","Skin",
                                                            "BH_IND","RX_IND","SUD_IND","SMI_IND","dementia_ind",
                                                            "Mbr_No","case_management","health_coach","IsHealthHome_Eligible",
                                                            "HealthHome_Status","MHIP","Pref_Email",
                                                            "Pref_Call","Pref_mail","Pref_text")], factor)




df1$NEW_LANGUAGE=ifelse(df1$LANGUAGE=="English","English",ifelse(df1$LANGUAGE==" Spanish","Spanish","Other"))
df<-df[,-c(1,6,7,8,9,10,11,12,13,17,18,19,21,23,54,57,59,61,63,73,75,76,91,92,27,28,29,30)]#57 revunue
df<-df[,-c(1,2)]

options(java.parameters = "-Xmx5g")

sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.75,0.25))
train  <- dat[sample, ]
test   <- dat[!sample, ]

set.seed(100)

library(rJava)
library(bartMachine)
bartMachineCV(X = train[,-4],  y =  train$ENGAGED, 
              num_tree_cvs = c(50, 100,200), k_cvs = c(2,  5), 
              nu_q_cvs = NULL, k_folds = 5, folds_vec = NULL, verbose = FALSE)

bart_machine = build_bart_machine(X = train[,-4],  y =  train$ENGAGED )
 ##get estimated probabilities
phat = bart_machine$p_hat_train
##look at in-sample confusion matrix
bart_machine$confusion_matrix

dev_data$pred <- predict.glm(reg, type = 'response')
val_data$pred <- predict.glm(reg, newdata = val_data, type = 'response')

calibration_plot(data = dev_data, obs = "y", pred = "pred", title = "Calibration plot for development data")
#> $calibration_plot

calibration_plot(data = val_data, obs = "y", pred = "pred", y_lim = c(0, 0.6),
                 title = "Calibration plot for validation data", group = "sex")

#rf

dim(df)
sum(complete.cases(df) == FALSE)

df <- df[, -c(1, 2, 3, 91, 92)]
dim(df)

df$ZIPCLASS <- as.factor(df$ZIPCLASS)
summary(df$ZIPCLASS)
df$RISK <- as.factor(df$RISK)
summary(df$RISK)
df$SEX <- as.factor(df$SEX)
summary(df$SEX)
df$ETHNICITY <- as.factor(df$ETHNICITY)
summary(df$ETHNICITY)
levels(df$ETHNICITY)[levels(df$ETHNICITY) == " Declined"] <- "Declined"
summary(df$ETHNICITY)
df$PLAN.NAME <- as.factor(df$PLAN.NAME)
summary(df$PLAN.NAME)
df$RSA_REGION <- as.factor(df$RSA_REGION)
summary(df$RSA_REGION)
df$RACE <- as.factor(df$RACE)
summary(df$RACE)
levels(df$RACE)[levels(df$RACE) == "Declined "] <- "Declined"
summary(df$RACE)
df$LANGUAGE <- as.factor(df$LANGUAGE)
df$P_LANGUGE <- as.factor(df$P_LANGUGE)
summary(df$LANGUAGE)
summary(df$P_LANGUGE)

df <- df[, -82]

str(df)
library(dplyr)
df<-select(df,-c(ASSOCIATE_DEGREE_PCT,SOME_COLLEGE_PCT,BACHELOR_DEGREE_PCT,BACHELOR_DEGREE_PCT,
                 MASTERS_DEGREE_PCT,PROFESSIONAL_DEGREE_PCT,DOCTRATE_DEGREE_PCT,MARRIED_CHILD_UNDER18PCT,
                 LABOR_ARMED_PCT,LABOR_CIVILIAN_ALL_PCT,LABOR_CIVILIAN_UNEMP_PCT,
                 NO_VEHICLE_PCT,UNDER100FPL_PCT,NONRX_PAID,REVENUE,PCP_VST,ER_CT_YR,
                 IP_CT,AV_ER_CT,AV_ER_CT_YR,N_TICKETS_COMPLAIN,N_TICKETS_ENQUIRY,PLAN.NAME))

df$ENGAGED<-as.factor(df$ENGAGED)

library(ranger)
rf_model <- ranger(ENGAGED ~ ., data = training, importance = "permutation", classification = TRUE)

pred_test0 <- predict(rf_model, data = test[, -1], type = "response")
pred_test <- pred_test0$predictions
obs_test <- test[, 1]
err_rf <- mean((obs_test - pred_test)^2)
err_rf

library(caret)

obs_test_factor <- as.factor(obs_test)
pred_test_factor <- as.factor(pred_test)
confusionMatrix(data = pred_test_factor, reference = obs_test_factor)$table
sensitivity(data = pred_test_factor, reference = obs_test_factor)
specificity(data = pred_test_factor, reference = obs_test_factor)

set.seed(1)
importance_pvalues(rf_model, method = "altmann", formula = ENGAGED ~., data = training)






#lasso
# Prepare for Training
df$ENGAGED = ifelse(df$ENGAGED==1, 'Yes', 'No')
#no run below
trainIndex <- createDataPartition(df$ENGAGED, p = .7, 
                                  list = FALSE, 
                                  times = 1)

# New version
a = sample(1: 44109, 35287, replace = FALSE)
train = df[a,]
test = df[-a,]

train <- df[ trainIndex,]
test  <- df[-trainIndex,]
# Old method (Not Used)
trainControl <- trainControl(method = "cv",
                             number = 10,
                             # Compute Recall, Precision, F-Measure
                             summaryFunction = prSummary,
                             # prSummary needs calculated class probs
                             classProbs = T)
modelFit <- train(ENGAGED ~ ., data = train, 
                  method = "glmnet", 
                  trControl = trainControl,
                  metric = "F", # Optimize by F-measure
                  tuneGrid = tuneGrid,
                  family="binomial")

# New approach
tuneGrid <- expand.grid(
  .alpha=1,
  .lambda=seq(0, 0.08, by = 0.0001))
ctrl <- trainControl(method = "cv",
                     number =10, 
                     returnResamp = 'none',
                     summaryFunction = twoClassSummary,
                     classProbs = T,
                     savePredictions = T,
                     verboseIter = F)
modelFit <- train(ENGAGED ~ ., data = train, 
                  method = "glmnet", 
                  trControl = ctrl,                  
                  metric = "ROC",
                  tuneGrid = tuneGrid,
                  family="binomial")

# Find the Best Threshold 
probs <- seq(.1, 0.9, by = 0.02)
ths <- thresholder(modelFit,
                   threshold = probs,
                   final = TRUE,
                   statistics = "all")
thresh_prob <- ths[which.max(ths$F1),'prob_threshold']
# Prediction on the Testing Data
pred <- predict(modelFit, newdata = test, type = "prob")
real <- as.numeric(factor(test$ENGAGED))-1
out <- ifelse(pred>=thresh_prob, 1, 0)
sum(real==out)/length(real)
sen <- ModelMetrics::sensitivity(real, pred$Yes, cutoff = thresh_prob)
spe <- ModelMetrics::specificity(real, pred$Yes, cutoff = thresh_prob)
ModelMetrics::kappa(real, pred$Yes, cutoff = thresh_prob)
ModelMetrics::mcc(real, pred$Yes, cutoff = thresh_prob)
ModelMetrics::auc(real, pred$Yes)
ModelMetrics::precision(real, pred$Yes, cutoff = thresh_prob)
ModelMetrics::recall(real, pred$Yes, cutoff = thresh_prob)
2*(sen*spe)/(sen+spe)

# Detect
for (i in seq(0.25, 0.75, by = 0.05)) {
  sen <- ModelMetrics::sensitivity(real, pred$Yes, cutoff = i)
  spe <- ModelMetrics::specificity(real, pred$Yes, cutoff = i)
  print(paste0(i, round(spe,4)))
}
# Alternative Way
roc.obj <- roc(real, pred$Yes)
plot(roc.obj, print.thres = "best")

# Important Variables
plot(varImp(modelFit), top=20)

# Tuning performance plot
ggplot(modelFit) + theme(legend.position = "top")
box = modelFit$results
subset(box, lambda==modelFit$bestTune$lambda)


#######################################################################
#######################################################################
#table1

b<-table1(~ AGE     +              ADI_STATEDECILE    +     ADI_NATIONALDECILE    +NO_SCHOOL_PCT+          
          HIGH_SCHOOL_PCT+SOME_COLLEGE_PCT+ASSOCIATE_DEGREE_PCT+BACHELOR_DEGREE_PCT+MASTERS_DEGREE_PCT+
          PROFESSIONAL_DEGREE_PCT+DOCTRATE_DEGREE_PCT+MARRIED_CHILD_UNDER18PCT+
          MARRIED_NOCHILD_UNDER18PCT+SINGLE_PARENT_PCT+PUBLIC_ASSISTANCE_PCT+LABOR_CIVILIAN_ALL_PCT+
          LABOR_CIVILIAN_UNEMP_PCT+LABOR_ARMED_PCT+NO_LABOR_PCT+NO_VEHICLE_PCT+
          ONE_VEHICLE_PCT+UNDER100FPL_PCT+UNDER200FPL_PCT+FOODSTAMPS_SNAP_PCT+
          VACANT_PCT+RISK_SCORE+NONRX_PAID+RX_PAID+EXPENSE+REVENUE+MER_RATE+PCP_VST+PCP_VST_6MO+ER_CT_YR+ER_CT+IP_CT+HCC_CT+
          AV_ER_CT_YR+AV_ER_CT+START_MONTH++NO_OF_CALLS+N_TICKETS_COMPLAIN+N_TICKETS_ENQUIRY+NO_CS_CAT+MM_CT+
            factor(SEX)+factor(RACE)+factor(ETHNICITY)+factor(LANGUAGE)+factor(ZIPCLASS)+factor(RSA_REGION)+
            factor(HOMELESS_IND)+factor(SMOKE_DX)+factor(RISK)+factor(PALLIATIVE)+
            factor(HOSPICE)+factor(DD)+factor(EYE)+factor(HEMATOLOGICAL)+factor(SKELETAL)+factor(PSYCHIATRIC)+
            factor(CARDIOVASCULAR)+factor(GASTRO)+factor(CNS)+factor(INFECTIOUS)+factor(CANCER)+factor(METABOLIC)+
            factor(SA)+factor(GENITAL)+factor(PULMONARY)+factor(CEREBROVASCULAR)+factor(DIABETES)+factor(PREGNANCY)+
            factor(SKIN)+factor(BH_IND)+factor(RX_IND)+factor(SUD_IND)+factor(SMI_IND)+factor(DEMENTIA_IND)+
            factor(ISHEALTHHOME_ELIGIBLE)+factor(HEALTHHOME_STATUS)+
            factor(MHIP) |factor(ENGAGED), data=df)

b<-as.data.frame(b)
write_csv(b,"/Users/wanglishan/Desktop/uw/591/table1.csv")




