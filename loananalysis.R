# This is the R code for loan data  analysis
# Patrick Rotzetter, November 2019

#used libraries
library(C50)
library(ggplot2)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(kohonen)
library(corrplot)
require('randomForest')
require('party')
library(caret)
library(pROC)
library(stringr)
library(tidyverse)
library(DescTools)

# read csv file
lines <- readLines("LoanStats_2017Q1 2.csv")
lines <-str_replace_all(lines,"[^[:graph:]]", " ") 
lines <- gsub('(^"|"$)', "", lines)
lines <- gsub("\"","" ,lines)
loans<-read.csv(textConnection(lines), sep=',',stringsAsFactors = FALSE)

# lines <- read_csv("LoanStats_2017Q1 2.csv",sep=',')
# remove some unwanted columns like id, member_id, address, zip

loans<-select(loans, -c(id,member_id,emp_title, annual_inc,zip_code,addr_state))

# remove some unwanted columns like url, desc

loans<-select(loans, -c(url,desc,purpose,title,
                        sec_app_earliest_cr_line,
                        sec_app_inq_last_6mths,
                        sec_app_mths_since_last_major_derog.))

# convert percentages columns in numbers

loans$int_rate<-as.numeric(gsub("[\\%]", "", loans$int_rate))
loans$revol_util<-as.numeric(gsub("[\\%]", "", loans$revol_util))
loans$total_acc<-as.numeric(gsub("[\\%]", "", loans$total_acc))
loans$emp_length<-gsub("[^0-9+<]","",loans$emp_length)
loans$emp_length<-as.factor(loans$emp_length)
numCols=c("loan_amnt","funded_amnt", "out_prncp","out_prncp_inv","dti","delinq_2yrs",
        "mths_since_last_delinq","mths_since_last_record","earliest_cr_line",
        "inq_last_6mths","total_pymnt","last_pymnt_amnt", "annual_inc_joint","dti_joint","mths_since_last_major_derog")
          
for (col in numCols) {
  loans[[col]]<-as.numeric(loans[[col]])
}

# remove NA values from known columns so far and replace by 0
loans$num_accts_ever_120_pd[is.na(loans$num_accts_ever_120_pd)]<-0  
loans$num_tl_120dpd_2m[is.na(loans$num_tl_120dpd_2m)]<-0  
loans$num_tl_30dpd[is.na(loans$num_tl_30dpd)]<-0  
loans$num_tl_90g_dpd_24m[is.na(loans$num_tl_90g_dpd_24m)]<-0 
loans$num_tl_op_past_12m[is.na(loans$num_tl_op_past_12m)]<-0 
loans$delinq_2yrs[is.na(loans$delinq_2yrs)]<-0 

# figure out which columns are numeric so that we can look at the distribution
numeric_cols <- sapply(loans, is.numeric)



#
# 'bad' statuses
bad_indicators <- c("Charged Off",
                    "Default",
                    "Does not meet the credit policy. Status:Charged Off",
                    "In Grace Period", 
                    "Default Receiver",
                    "Late (16-30 days)",
                    "Late (31-120 days)")

# assign certain statuses to a 'bad' ('0') group
loans$is_bad <- ifelse(loans$loan_status %in% bad_indicators, 1,
                          ifelse(loans$loan_status=="", NA, 0)
)

loans$is_bad<-as.factor(loans$is_bad)



# Let us lok at missing values
# Determine percentage  of NAs in each column
naPercentage <-sapply(loans, function(y) sum(length(which(is.na(y))))/length(y))
hist(naPercentage[naPercentage>0], col = "blue", xlab = "Percentange of null values",
     main = "Frequency of null values")

# let us remove all columns with more than 80% NAs
AllNA <- which(naPercentage>0.80)
loans<-loans[,-AllNA]
naPercentage <-sapply(loans, function(y) sum(length(which(is.na(y))))/length(y))
hist(naPercentage[naPercentage>0], col = "blue", xlab = "Percentange of null values",
     main = "Frequency of null values")


# convert char to factors
loans <- mutate_if(loans, is.character, as.factor)

# impute mean values for the remaining NAs
#for(i in 1:ncol(loans)) {
#  if (is.numeric(loans[,i])) {
#    loans[ , i][is.na(loans[ , i])] <- mean(loans[ , i], na.rm = TRUE)
#  }
#}


# let us look at a few variables to understand the data better


hist(loans$loan_amnt,col = 'blue', xlab='Loan Amount', main = 'Distribution by loan amount')
hist(loans$installment,col = 'blue', xlab='Installment Amount', main = 'Distribution by installment amount')

# let us look at interest rate by grade

plot(loans$int_rate~loans$grade, loans)


fdc<- loans%>% group_by(sub_grade) %>% summarize(count=n())
ggplot(fdc,aes(x=sub_grade,y=count))+geom_col()

# let us look at the past due as function of loan grade
fdc<- loans%>% group_by(sub_grade) %>% summarize(PDMean=sum(num_accts_ever_120_pd)/n()) %>% arrange(PDMean)
ggplot(fdc, aes(x=sub_grade, y=PDMean))+geom_col()+theme_minimal()
fdc<- loans%>% group_by(sub_grade) %>% summarize(PDMean=sum(delinq_2yrs)/n()) %>% arrange(PDMean)
ggplot(fdc, aes(x=sub_grade, y=PDMean))+geom_col()+theme_minimal()

fdc<- loans%>% group_by(emp_length) %>% summarize(PDMean=sum(num_accts_ever_120_pd+num_tl_120dpd_2m
                                                            +num_tl_30dpd)/n()) %>% arrange(PDMean)
ggplot(fdc, aes(x=emp_length, y=PDMean))+geom_col()+theme_minimal()

ggplot(loans,aes(y=loans$num_accts_ever_120_pd,x=loans$sub_grade))+geom_col()
ggplot(loans,aes(y=loans$mths_since_recent_revol_delinq,x=loans$sub_grade))+geom_col()

fdc<- loans%>% group_by(emp_length) %>% summarize(badloan=sum(as.numeric(is_bad)))                                 
ggplot(fdc, aes(x=emp_length, y=badloan))+geom_col()+theme_minimal()


# figure out which columns are numeric so that we can look at the distribution
numericCols <- sapply(loans, is.numeric)
nonumeric_cols<- sapply(loans, is.factor)
loansnum<-loans[, numericCols]
loansnonum<-loans[,nonumeric_cols]
# experiment k-means
fullTrainMatrix<-as.matrix(as.data.frame(lapply(loansnum, as.numeric)))
fullTrainMatrix<-scale(fullTrainMatrix)


# pca analysis
res.PCA<-PCA(fullTrainMatrix, scale.unit = TRUE, ncp = 5, graph = TRUE)

var <- get_pca_var(res.PCA)
var
corrplot(var$cos2, is.corr=FALSE)
fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(res.PCA,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)


# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(fullTrainMatrix, centers = i, nstart=20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:7, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


cluster<-kmeans(fullTrainMatrix, centers=2, iter.max = 20)
print(cluster)
fviz_cluster(cluster, data = fullTrainMatrix,
             #palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot")

# random forest
loansnum$isBad<-loans$is_bad
loansnum[ , "isBad"][is.na(loansnum[ , "isBad"])]<-0
loansnonum$isBad<-loansnum$isBad
set.seed(101)
modelRF <- randomForest(isBad ~ ., data=loansnum, ntree=20, importance = TRUE)
plot(modelRF$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")
preds<-predict(modelRF, select(loansnum,-isBad))
confusionMatrix(preds,loansnum$isBad)
modelRF.getTree()
plot(modelRF)
important <- importance(modelRF, type=1 )
Important_Features <- data.frame(Feature = row.names(important), Importance = important[, 1])
Important_Features <-Important_Features[order(-Important_Features$Importance),]
plot_ <- ggplot(Important_Features[1:10,], 
                aes(x= reorder(Feature,
                               Importance) , y = Importance) ) +
  geom_bar(stat = "identity", 
           fill = "#800080") +
  coord_flip() +
  theme_light(base_size = 20) +
  xlab("") + 
  ylab("Importance")+
  ggtitle("Important Features in Random Forest\n") +
  theme(plot.title = element_text(size=18))
ggsave("important_features.png", 
       plot_)
plot_

loansnonum<-select(loansnonum,-c("verification_status","policy_code"))
loansnonum<-select(loansnonum,-c("next_pymnt_d"))
loansnonum<-select(loansnonum,-c("initial_list_status"))
loansnonum<-select(loansnonum,-is_bad)
loansnonum<-loansnonum[rowSums(is.na(loansnonum)) == 0,])
modelRF <- randomForest(isBad ~ ., data=loansnonum, ntree=20, importance = TRUE)
plot(modelRF$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")
preds<-predict(modelRF, select(loansnonum,-isBad))
confusionMatrix(preds,loansnonum$isBad)
modelRF.getTree()
plot(modelRF)
important <- importance(modelRF, type=1 )
Important_Features <- data.frame(Feature = row.names(important), Importance = important[, 1])
Important_Features <-Important_Features[order(-Important_Features$Importance),]
plot_ <- ggplot(Important_Features[1:5,], 
                aes(x= reorder(Feature,
                               Importance) , y = Importance) ) +
  geom_bar(stat = "identity", 
           fill = "#800080") +
  coord_flip() +
  theme_light(base_size = 20) +
  xlab("") + 
  ylab("Importance")+
  ggtitle("Important Features in Random Forest\n") +
  theme(plot.title = element_text(size=18))
ggsave("important_features.png", 
       plot_)
plot_
