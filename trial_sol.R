library(readxl)
library(ggplot2)
library(DescTools)

setwd("D:/Personal/IIIT-B/Course2/Case Study/working/loan")
loan_initial <- read.csv("loan.csv",header = TRUE,stringsAsFactors = TRUE)
loan <- loan_initial
loan <- loan[,!apply(is.na(loan),2,all)]
dim(loan)
colnames(loan)
str(loan)
summary(loan)
Desc(loan$loan_amnt,main="Loan Amount Distribution",plotit=TRUE)
## FROM GGPLOT
# ggplot(loan,aes(loan$loan_amnt)) + geom_histogram(fill="light blue") + ggtitle("Number of loans Vs Loan amounts") + labs(x="Loan Amount",y="Number of Loans")
# ggplot(loan,aes(loan$loan_amnt)) + geom_histogram(aes(fill=..count..)) + scale_fill_gradient("Count",low="blue",high="red") + ggtitle("Number of loans Vs Loan amounts") + labs(x="Loan Amount",y="Number of Loans")
# ggplot(loan,aes(x=factor(loan$loan_status))) + geom_histogram(fill="light blue",stat="count") + geom_text(stat="count",aes(label=abs(..count..)),vjust=0.7) + labs(title="Loan Status Summary",x="Loan Status",y="Number of Loans")
# ggplot(loan,aes(x=loan$loan_status,y=loan$loan_amnt)) + geom_boxplot(aes(fill=loan$loan_status)) + labs(title="Loan status against Amount",x="Loan Status",y="Loan Amount")
loan <- subset(loan,select=-c(1,2))
loan <- unique(loan)
## Removing the columns which has only one value or only one value with some NAs.
loan <- loan[,-which(names(loan) %in% c("collections_12_mths_ex_med","policy_code","application_type","acc_now_delinq","chargeoff_within_12_mths","delinq_amnt","pub_rec_bankruptcies","tax_liens"))]
loan$annual_inc <- format(loan$annual_inc,nsmall = 2)
#ggplot(loan,aes(loan$loan_status)) + geom_histogram(stat="count",aes(fill=loan$verification_status))
#ggplot(loan,aes(loan$loan_status)) + geom_histogram(stat="count",aes(fill=loan$verification_status)) + geom_text(stat="count",aes(label=abs(..count..),fill=loan$verification_status),vjust=0.4,position=position_stack(0.4)) + labs(title="Status Vs Verification vs Amount",x="Loan_status",y="Count") + guides(fill=guide_legend("Verification Status"))
#ggplot(loan,aes(loan$grade)) + geom_histogram(stat="count",aes(fill=loan$loan_status)) + labs(title="Grade Vs Loan status",x="Grade",y="Loan Count") + guides(fill=guide_legend("Loan Status")) + geom_text(stat="count",aes(label=abs(..count..),fill=loan$loan_status),vjust=0.4,position=position_stack(0.4))

###QUANTILE READING
quantile(loan$loan_amnt)
quantile(loan$funded_amnt)
cor(loan$loan_amnt,loan$funded_amnt)

P <- ecdf(loan$annual_inc)
plot(P)

