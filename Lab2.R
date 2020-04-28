#Q1------Load the Bank.CSV into a data frame and print the first 5 lines of the data-------------

setwd('C:/Users/neilr/OneDrive/Documents/R')
bank_data = read.csv('bank.csv')
head(bank_data,5)

#Q2------Create a subset with people who are between 40 and 50 years old (40<age<=50), 
#and then get the minimum, mean and maximum value of the "balance" variable from that subset.---------

sub1_bank = subset(bank_data, age > 40)
sub2_bank = subset(sub1_bank, age <= 50)
# Getting min,max and mean of sub2_bank data with balance veriable
print(mean(sub2_bank$balance))
print(min(sub2_bank$balance))
print(max(sub2_bank$balance))

#Q3------------Sort the data frame in ascending order by the variable "duration," 
#and show the first 6 lines of the sorted data. --------------------------------------------------------

# Sort in asending order
sorted_bank = order(sub2_bank$duration)
sorted1 = sub2_bank[sorted_bank,]
print(head(sorted1))

#Q4---------------------------Add (append) a new column "married" with value 1 if the person is married and 0 
#otherwise, which is derived from the variable "marital," and save the new data frame as Bank2.CSV on your 
#computer. Please take a screenshot of the new data------------------------------------------------------

#Adding a new column
sorted1$married = 0
for (i in 1:nrow(sorted1)){
if (sorted1$marital[i] == 'married'){
  sorted1$married[i] = 1
}
}
head(sorted1)
#export to new csv
write.csv(sorted1,'Bank2.csv')