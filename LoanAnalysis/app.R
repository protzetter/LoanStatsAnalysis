#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

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


loans<-select(loans, -c(id,member_id,emp_title))

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
# let us remove all columns with more than 80% NAs
naPercentage <-sapply(loans, function(y) sum(length(which(is.na(y))))/length(y))
AllNA <- which(naPercentage>0.80)
loans<-loans[,-AllNA]
naPercentage <-sapply(loans, function(y) sum(length(which(is.na(y))))/length(y))
loans$annual_inc<-as.numeric(loans$annual_inc)
loans[is.na(loans)] <- 0

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Loan data set Q1 2017"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         h4('Select the variable to analyze'),
        selectInput("variable", "Variable:",
                    list("Loan amount" = "loan_amnt", 
                         "Installment" = "installment", 
                         "Interest rate" = "int_rate",
                         "Annual income" = "annual_inc")),
        h4('Select the number of bins for the histogram'),        
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
      ),


      # Show a plot of the generated distribution
      mainPanel(
         withSpinner(plotOutput("distPlot"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  

  output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      xl= switch(input$variable, "loan_amnt" = "Loan amount","installment"="Installment", "int_rate" = "Interest rate", "annual_inc" = "Annual income")
      x    <-  loans[[input$variable]]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'slateblue', border = 'white',xlab=xl, main = paste('Distribution by ',xl))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

