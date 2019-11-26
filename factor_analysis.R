#########################################
# Analysis of customer reviwev data     #
# Factor analysis                       #
#########################################

#######################
# 1. Import packages: #
#######################

library(EFAutilities)
library(nFactors)
library(corrplot)       # for making correlation plot.
library(ggplot2)        # for plotting
library(psych)          # for Factor Analysis
library(car)            # for calculating VIF, to check multicollinearity
library(caTools)        # for partitioning the data
library(reshape2)       # for data visualization


#######################
# 2. Import dataset:  #
#######################

# The data is about product customer survey about satisfaction:
setwd('C:/Users/molna/Desktop/Egyetem/KRE/Többváltozós statisztika')
df <- read.csv('Factor-Hair-Revised.csv', header = TRUE)
head(df)

dim(df) #100, 13

colnames(df)

#####################
# 3. Data cleaning: #
#####################

# Rename variables:
variables <- c("Customer_ID","Product_Quality" , "E_Commerce" , "Technical_Support" , 
               "Complaint_Resolution" , "Advertising" , "Product_Line" , "Salesforce_Image",
               "Competitive_Pricing" , "Warranty_Claims" , "Order_Billing" , 
               "Delivery_Speed" , "Customer_Satisfaction")

colnames(df) <- variables 
colnames(df)

# Remove ID column:
df <- df[, -1]
# Check:
dim(df)

#attach(df)

#################################
# 4. Exploratory Data Analysis: #
#################################

df$Competitive_Pricing

  # Dataset structure:
str(df)

# Summary:
summary(df)

# check null values:
sum(is.na(df))      # 0

# Variable distributions:

# Customer Satisfaction:
hist(df$Customer_Satisfaction, breaks = c(0:10), labels = T,
      include.lowest=T, right=T, 
      col=8, border=1, 
      main = paste("Histogram of Customer Satisfaction"),
      xlab= "Customer Satisfaction", ylab="COUNT", 
      xlim = c(0,10), ylim = c(0,35))

# Histogram of the independent Variables:
gg <- melt(df)
ggplot(gg, aes(x=value, fill=variable)) +
  geom_histogram(binwidth=5)+
  facet_wrap(~variable)

# Histogram of the independent Variables:
par("mar") 
par(mar=c(1,1,1,1))
par(mfrow = c(3,4)) #Convert Plotting space in 12
for (i in (1:11)) {
  
  h = round(max(df[,i]),0)+1
  
  l = round(min(df[,i]),0)-1
  
  n = variables[i]
  
  hist (df[,i], breaks = seq(l,h,((h-l)/6)), labels = T,
        include.lowest=T, right=T, 
        col=8, border=1, 
        main = NULL, xlab= n, ylab=NULL, 
        cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
        xlim = c(0,11), ylim = c(0,70))
}

# Create correlation matrix:
par(mfrow = c(1,1))
corrplot(cor(df[,-12], use = "pairwise.complete.obs"))
# We can find multiple correlations, for example between:
# Order_Billing and Technical_Support
# Delivery_Speed/Order_Billing and Complaint_Resolution
# Salesforce_Image and E_Commerce

# Chech correlations with Bartlett Test 
cortest.bartlett(cor(df[,-12]), 100)
# If the P-value > 0.05 then it is ideal case for dimention reduction.

# Kaiser-Meyer-Olkin (KMO) Test is a measure of how suited your data is for Factor Analysis.
KMO(df[,-12])

#ide kell dimenziócsökkentés!!!!!
#Alacsonyakat

# The global KMO is 0.65
# Which variable can be part of factor building?
# Value Description
# KMO < 0.5 Unacceptable
# 0.5 < KMO < 0.6 Miserable
# 0.6 < KMO < 0.7 Mediocre
# 0.7 < KMO < 0.8 Middling
# 0.8 < KMO < 0.9 Meritorious
#	KMO > 0.9 Marvelous

# Calculate the Eigen values for the variables:
A <- eigen(cor(df[,-12]))
EV <- A$values
EV

CM <- cor(df[,-12], method = "spearman", use="complete.obs")

ev <- eigen(CM) # get eigenvalues/sajátérték
ap <- parallel(subject=nrow(df[,-12]),var=ncol(df[,-12]), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

# Using Scree plot to find out how many factor we can build:
plotnScree(nS)

# Building the 4 factor model:
FourFactor = fa(r= df[,-12], nfactors =4, rotate ="none", fm ="pa")
print(FourFactor)

Loading <- print(FourFactor$loadings,cutoff = 0.3)

fa.diagram(FourFactor)
