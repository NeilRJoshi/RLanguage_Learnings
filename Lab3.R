# Call required packages
library(nycflights13)
library(dplyr)
View(flights)

# Manipulate data using pipe operatoe
ans1 = flights %>% filter(month %in% c(2,3)) %>% arrange(year, month, day)

head(ans1,10)

#Q2)
# Grouping and then summurizing the given conditions
by_carrier = group_by(flights, carrier)
ans2 =summarise(by_carrier, AD_Mean = mean(arr_delay, na.rm = TRUE), AD_SD = sd(arr_delay, na.rm = TRUE), DD_Mean = mean(dep_delay, na.rm = TRUE), DD_SD = sd(dep_delay, na.rm = TRUE))
print(ans2)

# Q3)
#abc = mutate(flights, combo = paste(origin,dest, sep = '-')) 
#by_combo = group_by(abc, combo)
#  ans3 = summarise(by_combo, AD_median = median(arr_delay, na.rm = TRUE), DD_median = median(dep_delay, na.rm = TRUE))
 # ans3
  
ans3 = flights %>% group_by(origin,dest) %>% summarise(AD_median = median(arr_delay, na.rm = TRUE), DD_median = median(dep_delay, na.rm = TRUE)) %>% arrange(desc(AD_median))
ans3