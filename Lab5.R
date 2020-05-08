library(sqldf)
library(nycflights13)
View(flights)
View(planes)

# Q1.
q1a = sqldf("SELECT median(distance) AS mid_dist, AVG(distance) AS avg_dist FROM flights")


q1 = sqldf("SELECT flights.* FROM flights, q1a
           WHERE distance BETWEEN q1a.mid_dist AND q1a.avg_dist")
q1


# Q2.
q2 = sqldf("SELECT * FROM flights
           WHERE distance BETWEEN (SELECT median(distance) FROM flights) AND (SELECT AVG(distance) FROM flights)")
q2

# Q3.

q3a = sqldf("SELECT AVG(dep_delay) AS avg_dep, origin FROM flights 
            GROUP BY origin")


q3b = sqldf("SELECT flights.* FROM flights, q3a
           WHERE flights.origin = q3a.origin AND flights.dep_delay > q3a.avg_dep ")


q3 = sqldf("SELECT * FROM q3b
           ORDER BY dep_delay DESC
           LIMIT 10")
q3

# Q4.

q4 = sqldf("SELECT flights.* FROM flights, (SELECT origin, AVG(dep_delay) AS avg_dep FROM flights GROUP BY origin) as q3
           WHERE flights.origin = q3.origin AND flights.dep_delay > q3.avg_dep
           ORDER BY dep_delay DESC 
           LIMIT 10")
q4


# Q5.

q5 = sqldf("SELECT model, SUM(distance) FROM (SELECT flights.*, planes.model FROM flights 
           INNER JOIN planes ON flights.tailnum = planes.tailnum)
           GROUP BY model
           ORDER BY sum(distance) DESC
           LIMIT 10")

q5
