library(dplyr)        # Data Wrangling
library(tidyr)        # Data Wrangling
library(fastDummies)  # Data Wrangling
library(ggplot2)      # Data Visualization
library(plotly)       # Data Visualization
library(EFAutilities) # PCA
library(nFactors)     # PCA
library(corrplot)     # Correlation matrix

#Kérdések:

# fizetés becslése lineáris regresszióval!
# prediktálni, hogy kik mennek el a cégtől? glm
# melyek azok a dimenziók, amelyek a felmondást befolyojásolják?
# a munkahelyi performanciát mely dimenziók magyarázzák?

# lca kategorikusokra, kmeans numerikusra
# lineárisnál a célváltozót nem kell standardizálni

# read the csv file:
hrdata <- read.csv("C:/Users/molna/Desktop/Egyetem/KRE/Többváltozós statisztika/ibm_hr_data.csv")

# watch the data:
str(hrdata)
head(hrdata)
summary(hrdata)
dim(hrdata) # 1470   36

# hány paraméterrel becslünk? ne nagyon menjünk 5 fölé a kis megfigyelés miatt! 

# glm-nél nominálsan is bele lehet tenni (nem dummyzva)

# check for na values:
sapply(hrdata, function(x) sum(is.na(x)))

# clean the column names:
colnames(hrdata)[1] <- "Age"
colnames(hrdata)

hrdata$MonthlyRate



# dummy variables: 
hr_dummy <- dummy_cols(hrdata)

colnames(hr_dummy)
# remove categorical variables (non coded):
hr_dummy[, c("BusinessTravel","Department","EducationField","Gender","JobRole",
               "MaritalStatus","Over18","OverTime")]=NULL

colnames(hr_dummy) <- gsub(" ", "_", colnames(hr_dummy))
colnames(hr_dummy) <- gsub("_&_", "_", colnames(hr_dummy))
colnames(hr_dummy) <- gsub("-", "_", colnames(hr_dummy))

colnames(hr_dummy)                           

head(hr_dummy)



str(hr_dummy)

# remove non informative variables:
hr_dummy[,c("EmployeeCount","EmployeeNumber","StandardHours", "Over18_Y")] = NULL


# histograms for the main continous variables:
hist(hr_dummy$Age, xlab="age", ylab="count",
     breaks=20, main="Age variability in the company", 
     col="lightblue", freq=FALSE) 

hist(hr_dummy$MonthlyIncome, xlab="MonthlyIncome", ylab="count",
     breaks=20, main="MonthlyIncome", col="lightblue", ylim=c(0,400))

hist(hr_dummy$YearsAtCompany, xlab="YearsAtCompany", ylab="count",
     breaks=20, main="YearsAtcompany", col="lightblue", ylim=c(0,400))


# visualize the correlation matrix:
colnames(hrdata)

#numerikus oszlopok leválogatása:
numcols <- unlist(lapply(hrdata, is.numeric))
numdf <- hrdata[,numcols]
numdf[,c("EmployeeCount", "Attrition","EmployeeNumber","StandardHours", "Over18_Y")] = NULL
data.cor <- cor(numdf, use = "pairwise.complete.obs")
corrplot(data.cor)

# Age correlated with TotalWorkingYears
# TotalWorkingYears correlated with MonthlyIncome and Joblevel
# YearsWithCurrManger correlated with YearsInCurrentRole
# YearsInCurrentRole correlated with YearsAtCompany 


# Chech correlations with Bartlett Test 
cortest.bartlett(data.cor, 100) # p = 0
# If the P-value > 0.05 then it is ideal case for dimention reduction.

#####################################################################
#   PCA (Principal component analysis) on quantitative variables    #
#####################################################################
std_num <- scale(numdf)
std_num <- as.data.frame(std_num) 

str(std_num)
dim(std_num)

KMO(std_num)

# based on kmo values reduce dimensions:
rel_std <- numdf[, c("Age", "JobLevel", "MonthlyIncome", "NumCompaniesWorked", "TotalWorkingYears", "YearsAtCompany",
                     "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")] 
colnames(std_num)
colnames(rel_std)


A <- eigen(cor(std_num))
EV <- A$values
EV

CM <- cor(std_num, method = "spearman", use="complete.obs")

ev <- eigen(CM) 
ap <- parallel(subject=nrow(std_num),var=ncol(std_num), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

# Using Scree plot to find out how many factor we can build:
plotnScree(nS)

# Two factor:
TwoFactor = fa(r= rel_std, nfactors =2, rotate ="varimax", fm ="pa")
print(TwoFactor)
 
Loading <- print(TwoFactor$loadings)

fa.diagram(TwoFactor)

# PCA 1: loyality
# PCA 2: age 


# educationt's loading was low so drop it.

# income = joblevel, so drop one of them.


# model.0 with loyalty PC:
model.0 <- lm(hr_dummy$MonthlyIncome ~ hr_dummy$YearsAtCompany + hr_dummy$YearsInCurrentRole + hr_dummy$YearsSinceLastPromotion + hr_dummy$YearsWithCurrManager)
summary(model.0)

# the loyalty PC explains the 27% of Monthly Income.



###########################################
#   ANOVA analysis on nominal variables   #
###########################################
# Which categorical variables affect on the monthly income?
# https://psu-psychology.github.io/r-bootcamp-2018/talks/anova_categorical.html
aov_df <- aov(hrdata$MonthlyIncome ~ hrdata$Education + hrdata$Department + hrdata$MaritalStatus + hrdata$JobRole)
summary(aov_df)


aov_dummy <- aov(hr_dummy$MonthlyIncome ~ hr_dummy$MaritalStatus_Single + hr_dummy$MaritalStatus_Married + hr_dummy$MaritalStatus_Divorced)
summary(aov_dummy)

aov_mar <-  aov(hr_dummy$MonthlyIncome ~ hr_dummy$MaritalStatus_Single + hr_dummy$MaritalStatus_Divorced + hr_dummy$MaritalStatus_Married)
summary(aov_mar)
# pozitív vagy negatív hatás?


colnames(hr_dummy)


aov_d_dep <- aov(hr_dummy$MonthlyIncome ~ hr_dummy$Department_Sales + hr_dummy$Department_Research_Development 
                + hr_dummy$Department_Human_Resources + hr_dummy$EducationField_Life_Sciences  
                + hr_dummy$EducationField_Other + hr_dummy$EducationField_Medical          
                + hr_dummy$EducationField_Marketing + hr_dummy$EducationField_Technical_Degree 
                + hr_dummy$EducationField_Human_Resources)
summary(aov_d_dep)
hr_dummy$Department_Sales 


aov_perf <- aov(hr_dummy$PerformanceRating ~ hr_dummy$RelationshipSatisfaction + hr_dummy$JobSatisfaction + hr_dummy$TrainingTimesLastYear)
summary(aov_perf)

inc_model.0 <- lm(hr_dummy$MonthlyIncome  ~ hr_dummy$Age + hr_dummy$YearsInCurrentRole)
summary(inc_model.0)

#########################
#   Cluster analysis    #
#########################

library(NbClust)
library(cluster)
library(factoextra)

# determine the number of clusters:
wss <- (nrow(std_num)-1)*sum(apply(std_num,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(std_num, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


NbClust(std_num, method = 'complete', index = 'dindex') # 4 clusters


# K-means clustering:
k.means <- kmeans(std_num, 4)
str(k.means)

# table the cluster groups:
table(k.means$cluster)

# visualize the clusters:
fviz_cluster(k.means, data = std_num)

# cbind cluster groups to dataframe:
hrdata["kmeans"] <- k.means$cluster

hr_dummy["kmeans"] <- k.means$cluster
std_num["kmeans"] <- k.means$cluster
colnames(std_num)


# ANOVA analysis on the cluster groups (On standardized data): 
# the F - value must be significant
clust_aov <- aov(std_num$MonthlyIncome ~ std_num$kmeans)
summary(clust_aov)

# 2e-16 significant!


###############################
#   GLM-Logistic Regression   #
###############################

# exploratory data analysis:
ggplot(data=hrdata, aes(x=DistanceFromHome, fill = Attrition)) + geom_bar(position="fill")
# the distance and the travel frequency are affect on attrition.

# recode the target variable:
hr_dummy['attr'] <- ifelse(hr_dummy$Attrition == "Yes", 1, 0)


# Molel and Predictor selection based on AIC:
# https://www.kaggle.com/rohitkumar06/let-s-reduce-attrition-logistic-reg-acc-88

logmodel.0 <- glm(attr ~ Age + BusinessTravel_Travel_Frequently 
                + EducationField_Marketing + OverTime_Yes + kmeans
                + NumCompaniesWorked + JobSatisfaction + WorkLifeBalance
                + YearsSinceLastPromotion, family=binomial(link="logit"),  data = hr_dummy)
summary(logmodel)
# AIC: 1121.2

# without kmeans and worklifebalance:
logmodel.1 <- glm(attr ~ Age + BusinessTravel_Travel_Frequently 
                  + EducationField_Marketing + OverTime_Yes 
                  + NumCompaniesWorked + JobSatisfaction 
                  , family=binomial(link="logit"),  data = hr_dummy)
summary(logmodel.1)
# AIC: 1125

# without EducationField_Marketing:
logmodel.2 <- glm(attr ~ Age + BusinessTravel_Travel_Frequently 
                  + OverTime_Yes 
                  + NumCompaniesWorked + JobSatisfaction 
                  , family=binomial(link="logit"),  data = hr_dummy)
summary(logmodel.2)
# AIC: 1129.2

# replace Jobsatisfaction with RelationshipSatisfaction:
logmodel.3 <- glm(attr ~ Age + BusinessTravel_Travel_Frequently 
                  + OverTime_Yes 
                  + JobLevel + YearsSinceLastPromotion 
                  , family=binomial(link="logit"),  data = hr_dummy)
summary(logmodel.3)
# AIC: 1146.8
# with + YearsSinceLastPromotion the AIc raises > 2! ~ 1.5
# with + kmeans AIc raises > 2! ~ 1
# based on the number of observations don't raise the predictors above 5!
# After add other preictor the AIC increased.

# the Age/JobLevel/YearsSinceLastPromotion are part of the K-means clustering
# change it to kmeans

# chisquare for dependence/independence:
table_kmeans <- table(hr_dummy$kmeans, hr_dummy$attr)
chisq.test(table_kmeans)

logmodel.4 <- glm(attr ~ BusinessTravel_Travel_Frequently 
                  + OverTime_Yes 
                  + kmeans
                  , family=binomial(link="logit"),  data = hr_dummy)
summary(logmodel.4)
# AIC: 1207.8

set.seed(123)
train <- sample(1:nrow(hr_dummy),size=nrow(hr_dummy)*0.7)
test <- - train

train_df <- hr_dummy[train,]
test_df <- hr_dummy[test,]

table(train_df$attr)
table(test_df$attr)

logmodel.4 <- glm(attr ~ BusinessTravel_Travel_Frequently 
                  + OverTime_Yes 
                  + kmeans
                  , family=binomial(link="binomial"),  data = train_df)
summary(logmodel.4)

log_pred <- predict(logmodel.4,newdata=test_df ,type='response')
log_pred <- ifelse(log_pred>=0.5,1,0)
caret::confusionMatrix(as.factor(log_pred),as.factor(test_df$attr))

# így pont arra az arányra tanult rá, ami az eredetiben volt :(
# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
# https://rpubs.com/abhaypadda/smote-for-imbalanced-data

as.data.frame(table(train_df$attr))

install.packages("DMwR")
library(DMwR)

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
balanced.data <- SMOTE(attr ~., train_df, perc.over = 4800, k = 5, perc.under = 1000)

as.data.frame(table(balanced.data$Class))