# This is the R code for loan data  analysis
# Patrick Rotzetter, November 2019

#used libraries
library(C50)
library(ggplot2)
library(dplyr)
library(factoextra)
library(FactoMineR)
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

loans<-select(loans, -c(id,member_id,emp_title, zip_code,addr_state))

# remove some unwanted columns like url, desc

loans<-select(loans, -c(url,desc,purpose,title,
                        sec_app_earliest_cr_line,
                        sec_app_inq_last_6mths,
                        sec_app_mths_since_last_major_derog.))

# convert percentages columns in numbers

loans$int_rate<-as.numeric(gsub("[\\%]", "", loans$int_rate))
loans$revol_util<-as.numeric(gsub("[\\%]", "", loans$revol_util))
loans$total_acc<-as.numeric(gsub("[\\%]", "", loans$total_acc))

# get employment length and remove years to get cleaner data
loans$emp_length<-gsub("[^0-9+<]","",loans$emp_length)
loans$emp_length<-as.factor(loans$emp_length)


# identify numeric columns and convert them to numeric

numCols=c("annual_inc","loan_amnt","funded_amnt", "out_prncp","out_prncp_inv","dti","delinq_2yrs",
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
loans$mths_since_last_delinq[is.na(loans$mths_since_last_delinq)]<-0
loans$mths_since_last_major_derog[is.na(loans$mths_since_last_major_derog)]<-0
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

# assign certain statuses to a 'bad' ('1') group
loans$is_bad <- ifelse(loans$loan_status %in% bad_indicators, 1,0)
loans$is_bad<-as.factor(loans$is_bad)
loans<-select(loans,-loan_status)

ggplot(loans, aes(x=is_bad)) + geom_bar(fill = "slateblue")+labs(x="Bad loan flag", y="Count")



# Let us look at missing values
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

colnames(loans)[colSums(is.na(loans)) > 0]
# convert char to factors
loans <- mutate_if(loans, is.character, as.factor)

# impute mean values for the remaining NAs
#for(i in 1:ncol(loans)) {
#  if (is.numeric(loans[,i])) {
#    loans[ , i][is.na(loans[ , i])] <- mean(loans[ , i], na.rm = TRUE)
#  }
#}


# let us look at a few variables to understand the data better


hist(loans$loan_amnt,col = 'staleblue', xlab='Loan Amount', main = 'Distribution by loan amount')
hist(loans$installment,col = 'staleblue', xlab='Installment Amount', main = 'Distribution by installment amount')

# let us look at interest rate by grade

plot(loans$int_rate~loans$grade, loans,col='blue',xlab = "Loan grade", ylab="Interest rate")

# let us look at th enumber of loans by credit sub-grade
fdc<- loans%>% group_by(sub_grade) %>% summarize(count=n())
ggplot(fdc,aes(x=sub_grade,y=count))+geom_col(fill = "slateblue")+labs(x="Loan sub-grade", y="Count")+theme(
      panel.border = element_blank(),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "grey")
    )

# let us look at the past due as function of loan grade averaged by number of loans
fdc<- loans%>% group_by(sub_grade) %>% summarize(PDMean=sum(num_accts_ever_120_pd)/n()) %>% arrange(PDMean)
ggplot(fdc, aes(x=sub_grade, y=PDMean))+geom_col()+geom_col(fill = "slateblue")+labs(x="Loan sub-grade", y="Past due more than 2 years",title="Percentage of past due by loan sub-grade")+theme(
  panel.border = element_blank(),  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey")
)



fdc<- loans%>% group_by(sub_grade) %>% summarize(delinq=sum(delinq_2yrs)/n()) %>% arrange(delinq)
ggplot(fdc, aes(x=sub_grade, y=delinq))+geom_col()+geom_col(fill = "slateblue")+labs(x="Loan sub-grade", y="Deliquency in past 2 years",title="Percentage of deliquency by loan sub-grade")+theme(
  panel.border = element_blank(),  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey")
)

fdc<- loans%>% group_by(emp_length) %>% summarize(badloan=sum(as.numeric(is_bad==1))/n())                                 
ggplot(fdc, aes(x=emp_length, y=badloan))+geom_col(fill = "slateblue")+labs(x="Length of employment", y="Bad loans",title="Number of bad loans by employment legth")+
  theme(
  panel.border = element_blank(),  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"))

# remove rows with employee length = 911 or 1954

loans<-loans %>% filter(!emp_length=="911") 
loans<-loans %>% filter(!emp_length=="1954") 

fdc<- loans%>% group_by(annual_inc) %>% summarize(badloan=sum(as.numeric(is_bad==1)))                                 
ggplot(subset(fdc,badloan>0), aes(x=annual_inc, y=badloan))+geom_point(col = "slateblue")+labs(x="Annual income", y="Number of bBad loans",title="Number of bad loans by annual income")+
  theme(
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))+
    scale_x_continuous(labels = scales::comma)


# Figure out which columns are numeric so that we can do some modeling with numeric features
numericCols <- sapply(loans, is.numeric)
nonumeric_cols<- sapply(loans, is.factor)
#loans<-loans[rowSums(is.na(loans)) == 0,]
loansnum<-loans[, numericCols]
loansnonum<-loans[,nonumeric_cols]

colnames(loansnum)[colSums(is.na(loansnum)) > 0]
loansnum[is.na(loansnum)] <- 0
# experiment k-means

fullTrainMatrix<-as.matrix(as.data.frame(lapply(loansnum, as.numeric)))
fullTrainMatrix<-scale(fullTrainMatrix)


# pca analysis
res.PCA<-PCA(fullTrainMatrix, scale.unit = TRUE, ncp = 5, graph = FALSE)

fviz_eig(res.PCA, addlabels = TRUE, ylim = c(0, 50))

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
for (i in 1:10) {
  km.out <- kmeans(fullTrainMatrix, centers = i, nstart=20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


cluster<-kmeans(fullTrainMatrix, centers=3, iter.max = 20)
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
           fill = "slateblue") +
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
loansnonum<-loansnonum[rowSums(is.na(loansnonum)) == 0,]
modelRF <- randomForest(isBad ~ ., data=loansnonum, ntree=20, importance = TRUE)
plot(modelRF$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")
preds<-predict(modelRF, select(loansnonum,-isBad))
confusionMatrix(preds,loansnonum$isBad)
modelRF.getTree()
plot(modelRF)
important <- importance(modelRF, type=1 )
Important_Features <- data.frame(Feature = row.names(important), Importance = important[, 1])
Important_Features <-Important_Features[order(-Important_Features$Importance),]
plot_ <- ggplot(Important_Features, 
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


# correlations to drop

indexesToDrop <- findCorrelation(cor(select(loansnum,-isBad)), cutoff = 0.8)
corrplot(cor(select(loansnum,-isBad)[,indexesToDrop]))

set.seed(101)
modelRF <- randomForest(isBad ~ ., data=loansnum[,-indexesToDrop], ntree=20, importance = TRUE)
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

