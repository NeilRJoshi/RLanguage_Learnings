install.packages("sqldf")
library(sqldf)

setwd('C:/Users/neilr/OneDrive/Documents/R')
fd = read.csv('flightdata.csv')
View(fd)

# Q1.
  q1 = sqldf("SELECT * FROM fd 
             WHERE origin = 'OKC' and dest = 'SFO' LIMIT 5")
  q1
  
  q1_r = subset(fd, origin == 'OKC' & dest == 'SFO')[1:5,]
  q1_r

# Q2.
q2 = sqldf("SELECT count(*), origin,dest FROM fd
           GROUP BY origin,dest
           LIMIT 5")
q2

# Q3.
q3 = sqldf("SELECT * FROM fd
           WHERE origin LIKE 'D%'
           ORDER BY depdelay DESC
           LIMIT 5")
q3

# Q4.
q4 = sqldf("SELECT * FROM fd
           WHERE depdelay is not null;
           ")

write.csv(q4,"C:\\Users\\neilr\\OneDrive\\Documents\\R\\SQL_LAB4.csv", row.names = FALSE)



