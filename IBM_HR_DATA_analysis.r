library(dplyr)        # Data Wrangling
library(tidyr)        # Data Wrangling
library(fastDummies)  # Data Wrangling
library(ggplot2)      # Data Visualization
library(plotly)       # Data Visualization
library(EFAutilities) # PCA
library(nFactors)     # PCA
library(corrplot)     # Correlation matrix
library(NbClust)      # cluster analysis
library(cluster)      # cluster analysis
library(factoextra)   # cluster analysis
library(ROCR)         # Plot the ROC curve

# Predict the Monthly Income with Loyalty Factor.
# Predict who left the company wint GLM using Kmeans cluster.

#################
#   Read data   #
#################

# read the csv file:
hrdata <- read.csv("C:/Users/molna/OneDrive/Desktop/ibm_hr_data.csv")

str(hrdata)
head(hrdata)
summary(hrdata)
dim(hrdata) # 1470   36


# check for na values:
sapply(hrdata, function(x) sum(is.na(x)))
# no null values in the dataset

# clean the column names:
colnames(hrdata)[1] <- "Age"
colnames(hrdata)

#########################
#   Data preparation    #
#########################

# dummy variables: 
hr_dummy <- dummy_cols(hrdata)

colnames(hr_dummy)
# remove categorical variables (non coded):
#hr_dummy[, c("BusinessTravel","Department","EducationField","Gender","JobRole",
#               "MaritalStatus","Over18","OverTime")]=NULL

colnames(hr_dummy) <- gsub(" ", "_", colnames(hr_dummy))
colnames(hr_dummy) <- gsub("_&_", "_", colnames(hr_dummy))
colnames(hr_dummy) <- gsub("-", "_", colnames(hr_dummy))

colnames(hr_dummy)                           

str(hr_dummy)

# remove non informative variables:
hr_dummy[,c("EmployeeCount","EmployeeNumber","StandardHours", "Over18_Y")] = NULL


###########################
#   Data visualizations   #
###########################

par(mfrow = c(1,1)) 

# histograms for the main continous variables:
hist(hr_dummy$Age, xlab="age", ylab="count",
     breaks=20, main="Age variability in the company", 
     col="lightblue", freq=FALSE) 

hist(hr_dummy$MonthlyIncome, xlab="MonthlyIncome", ylab="count",
     breaks=20, main="MonthlyIncome", col="lightblue", ylim=c(0,400))

hist(hr_dummy$YearsAtCompany, xlab="YearsAtCompany", ylab="count",
     breaks=20, main="YearsAtcompany", col="lightblue", ylim=c(0,400))

hist(hr_dummy$RelationshipSatisfaction, xlab="RelationshipSatisfaction", ylab="count",
     breaks=20, main="RelationshipSatisfaction", col="lightblue", ylim=c(0,400))

hist(hr_dummy$JobSatisfaction, xlab="JobSatisfaction", ylab="count",
     breaks=20, main="JobSatisfaction", col="lightblue", ylim=c(0,400))

hist(hr_dummy$EnvironmentSatisfaction, xlab="EnvSatisfaction", ylab="count",
     breaks=20, main="EnvSatisfaction", col="lightblue", ylim=c(0,400))

# visualize the correlation matrix:

# select numerical variables:
numcols <- unlist(lapply(hrdata, is.numeric))
numdf <- hrdata[,numcols]
numdf[,c("EmployeeCount", "Attrition","EmployeeNumber","StandardHours", "Over18_Y")] = NULL
data.cor <- cor(numdf, use = "pairwise.complete.obs")
corrplot(data.cor)

# Age correlated with TotalWorkingYears
# TotalWorkingYears correlated with MonthlyIncome and Joblevel
# YearsWithCurrManger correlated with YearsInCurrentRole
# YearsInCurrentRole correlated with YearsAtCompany 



#####################################################################
#   PCA (Principal component analysis) on quantitative variables    #
#####################################################################

std_num <- scale(numdf)
std_num <- as.data.frame(std_num) 

# Kaiser-Meyer-Olkin (KMO) Test is a measure of how suited your data is for Factor Analysis.
# Overall MSA =  0.79 -> The data is good for PCA

KMO(std_num)
data.cor2 <- cor(std_num, use = "pairwise.complete.obs")
cortest.bartlett(data.cor2, 100) 
# If the P-value < 0.05 then it is ideal case for dimention reduction.


# based on kmo values reduce dimensions (KMO < 0,5):
rel_std <- std_num[, c("Age", "JobLevel", "NumCompaniesWorked", "TotalWorkingYears", "YearsAtCompany",
                     "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")] 


A <- eigen(cor(rel_std))
EV <- A$values
EV

CM <- cor(rel_std, method = "spearman", use="complete.obs")

ev <- eigen(CM) 
ap <- parallel(subject=nrow(rel_std),var=ncol(rel_std), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

# Using Scree plot to find out how many factor we can build:
plotnScree(nS)
# The screeplot offers 2 factor modell.

# Two factor model with fa() function:
TwoFactor = fa(r= rel_std, nfactors = 2, rotate ="varimax", fm ="pa")
TwoFactor$loadings
fa.diagram(TwoFactor)

# PA 1: latent variable: loyality
# PA 2: latent variable: age, work experience

# PA 1: Contains the 34% of the total variance in rel_std data.
# PA 2: Contains the 30% of total variance in rel_std data.


# Two factor model with prcomp() function:
fact<-prcomp(rel_std, rank. = 2)
fact

summary(fact)
# PC1: Contains the 50% of the total variance in rel_std data.
# PC2: Contains the 20% of total variance in rel_std data.

# the correlations between the factors ~ 0. (Diagonal matrix)
cor(fact$x)


# factor scores to variables:
loyalty_fact <- fact$x[,1]
age_fact <- fact$x[,2]


# model.0 with loyalty PC:
model.0 <- lm(hr_dummy$MonthlyIncome ~ loyalty_fact)
summary(model.0)
# the loyalty factor is significant, it explains the 51% (R-square) of Monthly Income.

# model.1 with the age PC:
model.1 <- lm(hr_dummy$MonthlyIncome ~ age_fact)
summary(model.1)
# The age factor is significant too, but it explains the 13% of Monthly Income.


########################################################
#   ANOVA and Linear Regression on nominal variables   #
########################################################

aov_mar <-  aov(hr_dummy$MonthlyIncome ~ hr_dummy$MaritalStatus_Single + hr_dummy$MaritalStatus_Divorced + hr_dummy$MaritalStatus_Married)
summary(aov_mar)
# the maritalstatus_single is more singnificant than the divorced.

single <- lm(hr_dummy$MonthlyIncome ~ hr_dummy$MaritalStatus_Single)
summary(single)
# The single effect is negativ for the Monthly Income. 

satis <- lm(hr_dummy$MonthlyIncome ~ as.factor(hr_dummy$EnvironmentSatisfaction) 
                            + as.factor(hr_dummy$WorkLifeBalance)
                            + as.factor(hr_dummy$JobSatisfaction)
                            + as.factor(hr_dummy$RelationshipSatisfaction))
summary(satis)
# the satisfaction indexes are not significat for the Monthly Income, because of the <0.05 P-values.

#########################
#   Cluster analysis    #
#########################

colnames(std_num)

# determine the number of clusters:
wss <- (nrow(std_num)-1)*sum(apply(std_num,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(std_num, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# decision of cluster number based on NbClust plot: the slope of the line changes near 5th step.
NbClust(std_num, method = 'complete', index = 'dindex') # 6 clusters


# K-means clustering:
k.means <- kmeans(std_num, 6)
str(k.means)

# table the cluster groups:
table(k.means$cluster)

# visualize the clusters:
fviz_cluster(k.means, data = std_num)

# cbind cluster groups to dataframe:
hrdata["kmeans"] <- k.means$cluster

hr_dummy["kmeans"] <- k.means$cluster
std_num["kmeans"] <- k.means$cluster

# Which cluster variable is the most significant:
# ANOVA analysis on the cluster groups (On standardized data): 
# the F - value must be significant

aov_age <- summary(aov(std_num$MonthlyIncome ~ std_num$Age))[[1]]["std_num$Age" , "F value"]
# 483.7621
aov_edu <- summary(aov(std_num$MonthlyIncome ~ std_num$Education))[[1]]["std_num$Education" , "F value"]
# 13.35819
aov_drate <- summary(aov(std_num$MonthlyIncome ~ std_num$DailyRate))[[1]]["std_num$DailyRate" , "F value"]
# 0.08720255
aov_dist <- summary(aov(std_num$MonthlyIncome ~ std_num$DistanceFromHome))[[1]]["std_num$DistanceFromHome" , "F value"]
# 0.4250963
aov_envsat <- summary(aov(std_num$MonthlyIncome ~ std_num$EnvironmentSatisfaction))[[1]]["std_num$EnvironmentSatisfaction" , "F value"]
# 0.05751288
aov_hrate <- summary(aov(std_num$MonthlyIncome ~ std_num$HourlyRate))[[1]]["std_num$HourlyRate" , "F value"]
# 0.3662987
aov_jobinv <- summary(aov(std_num$MonthlyIncome ~ std_num$JobInvolvement))[[1]]["std_num$JobInvolvement" , "F value"]
# 0.3424445
aov_joblev <- summary(aov(std_num$MonthlyIncome ~ std_num$JobLevel))[[1]]["std_num$JobLevel" , "F value"]
# 13676.94
aov_jobsat <- summary(aov(std_num$MonthlyIncome ~ std_num$JobSatisfaction))[[1]]["std_num$JobSatisfaction" , "F value"]
# 0.07519329
aov_mrate <- summary(aov(std_num$MonthlyIncome ~ std_num$MonthlyRate))[[1]]["std_num$MonthlyRate" , "F value"]
#  1.781358
aov_numcomp <- summary(aov(std_num$MonthlyIncome ~ std_num$NumCompaniesWorked))[[1]]["std_num$NumCompaniesWorked" , "F value"]
# 33.56723
aov_salhike <- summary(aov(std_num$MonthlyIncome ~ std_num$PercentSalaryHike))[[1]]["std_num$PercentSalaryHike" , "F value"]
# 1.092382
aov_perfrate <- summary(aov(std_num$MonthlyIncome ~ std_num$PerformanceRating))[[1]]["std_num$PerformanceRating" , "F value"]
# 0.4303957
aov_relsat <- summary(aov(std_num$MonthlyIncome ~ std_num$RelationshipSatisfaction))[[1]]["std_num$RelationshipSatisfaction" , "F value"]
# 0.9833885
aov_stockopt <- summary(aov(std_num$MonthlyIncome ~ std_num$StockOptionLevel))[[1]]["std_num$StockOptionLevel" , "F value"]
# 0.04292993
aov_workyear <- summary(aov(std_num$MonthlyIncome ~ std_num$TotalWorkingYears))[[1]]["std_num$TotalWorkingYears" , "F value"]
# 2177.973
aov_training <- summary(aov(std_num$MonthlyIncome ~ std_num$TrainingTimesLastYear))[[1]]["std_num$TrainingTimesLastYear" , "F value"]
# 0.6939075
aov_worklife <- summary(aov(std_num$MonthlyIncome ~ std_num$WorkLifeBalance))[[1]]["std_num$WorkLifeBalance" , "F value"]
# 1.383353
aov_yearsatcom <- summary(aov(std_num$MonthlyIncome ~ std_num$YearsAtCompany))[[1]]["std_num$YearsAtCompany" , "F value"]
# 527.891
aov_currole <- summary(aov(std_num$MonthlyIncome ~ std_num$YearsInCurrentRole))[[1]]["std_num$YearsInCurrentRole" , "F value"]
# 223.9524
aov_lastprom <- summary(aov(std_num$MonthlyIncome ~ std_num$YearsSinceLastPromotion))[[1]]["std_num$YearsSinceLastPromotion" , "F value"]
# 198.3064
aov_ywithman <- summary(aov(std_num$MonthlyIncome ~ std_num$YearsWithCurrManager))[[1]]["std_num$YearsWithCurrManager" , "F value"]
# 197.1359

# Based on the ANOVa, in the cluster groups Joblevel, YearsatCompany, Age are the most important variables! (Highter F-statistics)


str(k.means)
k.means$centers



###############################
#   GLM-Logistic Regression   #
###############################


# exploratory data analysis:
ggplot(data=hrdata, aes(x=DistanceFromHome, fill = Attrition)) + geom_bar(position="fill")
# the distance and the travel frequency are affect on attrition, but it is the part of the cluster 

# recode the target variable:
hr_dummy['attr'] <- ifelse(hr_dummy$Attrition == "Yes", 1, 0)


# Molel and Predictor selection based on AIC:
logmodel.0 <- glm(attr ~  BusinessTravel_Travel_Frequently 
                + EducationField_Marketing + OverTime_Yes + kmeans
                + NumCompaniesWorked + JobSatisfaction + WorkLifeBalance
                + YearsSinceLastPromotion, family=binomial(link="logit"),  data = hr_dummy)
summary(logmodel.0)
# AIC: 1120

log_pred_prob <- predict(logmodel.0, newdata=hr_dummy ,type='response')
summary(log_pred_prob)
log_pred <- ifelse(log_pred_prob>=0.5,1,0) # set the treshhold 


# merge the calculated vectors to the database:
hr_dummy['logreg_prob'] <- log_pred_prob
hr_dummy['logreg_pred'] <- log_pred

hr_dummy[,c("attr", "logreg_prob", "logreg_pred")]


# Pint the confusion matrix:
caret::confusionMatrix(as.factor(log_pred),as.factor(hr_dummy$attr))

# Based on the the Accuracy the modell is better than random, but based the confusion matrix:
# the False Positive class is big (228). So the modell predicted 0, but the actual value was 1 in this cases.


# roc curve:
pred <- prediction(log_pred_prob, hr_dummy$attr)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="red", main="ROC Curve")
abline(a = 0, b = 1)

auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR # 0.733515

# Based on the ROC curve the modell is better than random, because the Roc curve is highter than the diagonal.
# The Under the curve (AUC) is 73%.
