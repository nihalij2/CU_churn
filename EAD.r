x<- read.csv("Member Dataset.csv",stringsAsFactors = F)
x[is.na(x)] <- 0
x$negative_bal_jul <- x$Visa + x$Mortgage +x$Home.Equity+x$Other.Loan+x$Vehicle
x$positive_bal_jul <- x$Checking +x$Savings
x$negative_bal_aug <- x$Visa.1 + x$Mortgage.1 +x$Home.Equity.1+x$Other.Loan.1+x$Vehicle.1
x$positive_bal_aug <- x$Checking.1 +x$Savings.1
x$negative_bal_sep <- x$Visa.2 + x$Mortgage.2 +x$Home.Equity.2+x$Other.Loan.2+x$Vehicle.2
x$positive_bal_sep <- x$Checking.2 +x$Savings.2


x$positive_bal_jul
