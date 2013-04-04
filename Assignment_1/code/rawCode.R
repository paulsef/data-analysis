# Read in the Data, change to lower case
loan <- read.csv('./loansData.csv')
names(loan) <- tolower(names(loan))

# convert interest rate, to numeric class
loan$interest.rate <- as.numeric(sub(
  '%', "", as.character(loan$interest.rate)))
#convert debt:income to numeric
loan$debt.to.income.ratio <- as.numeric(sub(
  '%','', as.character(loan$debt.to.income.ratio)))
#convert state employment length to numeric
loan$employment.length <- gsub(
  ' year| years','', as.character(loan$employment.length))
loan$employment.length <- gsub(
  '< ',"", loan$employment.length)
loan$employment.length <- gsub(
  '10\\+',"10", loan$employment.length)
loan$employment.length <- as.numeric(loan$employment.length)
#convert FICO score to numeric
loan$fico.range <- as.character(loan$fico.range)
loan$fico.range <- as.numeric(gsub('-[0-9]+','',loan$fico.range))
#exploratory plots

#histrogram of interest rates
hist(loan$interest.rate)
#interest rate vs. FICO range, shows higher score means lower IR
plot(loan$interest.rate ~ loan$fico.range)
#scatter interest rate vs. FICO range
plot(loan$interest.rate, loan$fico.range)
#interest rate vs.debt:income
plot(loan$interest.rate ~ loan$debt.to.income.ratio)
#fico score vs. interest rate, by loan length
lm1 <- lm(loan$interest.rate ~ loan$fico.range + 
            loan$loan.length + loan$fico.range*loan$loan.length)
plot(loan$interest.rate ~ loan$fico.range, 
     pch = 19)
points(loan$fico.range, loan$interest.rate, pch = 19, 
       col = loan$loan.length)
abline(c(lm1$coeff[1], lm1$coeff[2]), col = "green", lwd = 3)
abline(c(lm1$coeff[1] + lm1$coeff[3], 
         lm1$coeff[2] + lm1$coeff[4]), col = "blue", lwd = 3)


legend(0, 0, legend = levels(loan_factor$Loan.Purpose), 
       col = loan_factor$Loan.Purpose)

#final figures

pdf(file = "./finalfigure.pdf")
par(mfrow=c(1,3))

#Histogram of FICO scores
hist(loan$fico.range, breaks = 30, xlab = "FICO Score", ylab = "Frequency", main = "")
mtext(text = "(a)", side = 3, line = 1)

#Boxplot of loan length vs interest rate
boxplot(loan$interest.rate ~ loan$loan.length, xlab = "Loan Length", ylab = "Interest Rate", main = "",  varwidth = TRUE)
mtext(text = "(b)", side = 3, line = 1)

#plot of fico range and interest rate colored by loan length
plot(loan$fico.range, loan$interest.rate, col = loan$loan.length, xlab = "FICO score", ylab = "interest.rate", main = "")
abline(c(lm1$coeff[1], lm1$coeff[2]), col = "black", lwd = 3)
abline(c(lm1$coeff[1] + lm1$coeff[3], 
         lm1$coeff[2] + lm1$coeff[4]), col = "red", lwd = 3)
dev.off()