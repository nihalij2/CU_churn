
library("lubridate")
library("zipcode")

member = read.csv("Member Dataset.csv")

#creating age groups
member$Age_bin = ifelse(member$Age < 14, "Kids", ifelse(member$Age < 22, "Teens",
                                                        ifelse(member$Age<35, "Millenials",
                                                               ifelse(member$Age<55, "Middel_Aged","55+"))))

#combining diffrent loan and savings accounts to one of each kind
member[is.na(member)] <- 0
member$Loan_jul <- member$Visa + member$Mortgage +member$Home.Equity+member$Other.Loan+member$Vehicle
member$Savings_jul <- member$Checking +member$Savings+member$CD+member$IRA+member$Money.Market
member$Loan_aug <- member$Visa.1 + member$Mortgage.1 +member$Home.Equity.1+member$Other.Loan.1+member$Vehicle.1
member$Savings_aug <- member$Checking.1 +member$Savings.1+member$CD.1+member$IRA.1+member$Money.Market.1
member$Loan_sep <- member$Visa.2 + member$Mortgage.2 +member$Home.Equity.2+member$Other.Loan.2+member$Vehicle.2
member$Savings_sep <- member$Checking.2 +member$Savings.2+member$CD.2+member$IRA.2+member$Money.Market.2

#Adding type of customer
member$Cust_type = ifelse(member$ClosedDate == "", "Active", ifelse(mdy(member$ClosedDate) < ymd("2018-07-31"),
                                                                    "Closed_bfr_jul","Churned_Recently"))
#Duration
member$loyalty_months = ifelse(member$Cust_type== "Active",
                               interval(mdy(member$EarliestMemAcctDate), mdy("10-01-2018")) %/% months(1),
                               interval(mdy(member$EarliestMemAcctDate), mdy(member$ClosedDate)) %/% months(1))


#Zip correction
Zip_correct = function(zip_old){
  zip_new = rep(paste0(rep('0',5-nchar(zip_old)),zip_old,collapse = ""))
  return(zip_new)
}
member$ZipCode_Validated = as.character(member$ZipCode_Validated)

for(i in 1:nrow(member)){
  member$zip[i] = Zip_correct(member$ZipCode_Validated[i])
}


EDA_variables = member[,c("Member_ID","zip","Age_bin","Cust_type","loyalty_months",
                          "July.Trans", "August.Trans","September.Trans",
                          "Loan_jul","Savings_jul","Loan_aug","Savings_aug","Loan_sep","Savings_sep"
                          )]
#adding city and state from zipcode library
data(zipcode)
EDA_variables = merge(EDA_variables,zipcode[,c("zip","city","state")],"zip",all.x = T)
write.csv(EDA_variables,"CU_EDA_variables.csv", row.names = F)
