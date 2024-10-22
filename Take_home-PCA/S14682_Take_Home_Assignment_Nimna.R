#BT 4019 – Statistics for Bioinformatics
#Name : Nimna Alupotha Gamage(Nimna A. G. T.)
#Index No. : S14682
#Reg. No. : 2019s17241

#Take-home Assignment
#This contains only the commented R code. The complete document with answers and explanations is 
#the "S14682_NimnaA.G.T._Take_Home_Assignment" file.
#Question 1

#Set working directory
setwd("D:/4th_yr_sem1/BT 4019 - Statistical Methods in Bioinformatics/TH")

#Get working directory
getwd()

#Load the data set
#Read the 'breast-cancer-data.csv' file

data_breast_cancer = read.csv("breast-cancer-data.csv", header = T)


#1-1. Carry out a principal component analysis and identify the important principal components.

#Extract numeric variables

data_breast_cancer_numeric = data_breast_cancer[, 3:32]

#Standardize data

scaled_data = scale(data_breast_cancer_numeric)

#perform PCA

##1 - Using prcomp()
pca_pr_breast_cancer = prcomp(scaled_data) 

##2 - Using princomp()
pca_prin_breast_cancer = princomp(scaled_data) 


#1-2. How many principal components would you choose? Justify your answer.

##1 - Using prcomp()

#summary of the sample-shows the variance explained by each PC
summary(pca_pr_breast_cancer)

#structure of the sample
str(pca_pr_breast_cancer)

#plot the data - Bar chart
plot(pca_pr_breast_cancer, main = "The plot of PCA result of 'breast-cancer-data'_pr")

#Scree plot
screeplot(pca_pr_breast_cancer, main = "The screeplot of PCA result of 'breast-cancer-data'_pr", type = "lines")

##2 - Using princomp()

#summary of the sample-shows the variance explained by each PC
summary(pca_prin_breast_cancer)

#structure of the sample
str(pca_prin_breast_cancer)

#plot the data - Bar chart
plot(pca_prin_breast_cancer, main = "The plot of PCA result of 'breast-cancer-data'_prin")

#Scree plot
screeplot(pca_prin_breast_cancer, main = "The screeplot of PCA result of 'breast-cancer-data'_prin", type = "lines")



#2-1. Select the first two principal components and identify whether there are any potential 
#groupings through a suitable visualization technique.
#2-2. How many groups can you identify? 
#2-3. What are they?

##1 - Using Bi plot

###without labels
biplot(pca_pr_breast_cancer, main = "The Biplot between PC1 and PC2")

###with labels
fac = as.factor(paste(data_breast_cancer[,2]))
biplot(pca_pr_breast_cancer, main = "The Biplot between PC1 and PC2-with labels", xlabs = as.numeric(fac))


##2 - Using Scatter plot

#Select the first two principal components
#Extract PC1
pc1 = pca_pr_breast_cancer$x[,1]

#Extract PC2
pc2 = pca_pr_breast_cancer$x[,2]

###without labels
plot(pc1, pc2, main = "The Scatterplot between PC1 and PC2", xlab = "PC1", ylab = "PC2")

###with labels
#plot between PC1 and PC2
plot(pc1, pc2, main = "The Scatterplot between PC1 and PC2-with labels", xlab = "PC1", ylab = "PC2")

#label the points
grps = as.factor(data_breast_cancer$diagnosis)
points(pc1, pc2, col=grps)


