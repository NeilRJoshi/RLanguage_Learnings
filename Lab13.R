
#install.packages("rvest")

library(rvest)
library(ggplot2)

url="https://www.tripadvisor.com/Hotel_Review-g42159-d90019-Reviews-East_Lansing_Marriott_at_University_Place-East_Lansing_Ingham_County_Michigan.html"

#Q1-----------------------------------------------------------------------------------
#lets get the first 5 reviews 
reviews <- url %>% read_html() %>% html_nodes(".hotels-community-tab-common-Card__section--4r93H")
reviews=reviews[1:5]
review <- reviews %>% html_node(".location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE") %>% html_node("span") %>% html_text()
result1=data.frame(review,stringsAsFactors = FALSE)

#Lets get other 15 reviews
for(i in 1:3){
  Sys.sleep(10)
  url=paste("https://www.tripadvisor.com/Hotel_Review-g42159-d90019-Reviews-or",i*5,"-Marriott_East_Lansing_at_University_Place-East_Lansing_Ingham_County_Michigan.html#REVIEWS",sep="")
  reviews <- url %>% read_html() %>% html_nodes(".hotels-community-tab-common-Card__section--4r93H")
  reviews=reviews[1:5]
  review <- reviews %>% html_node(".location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE") %>%
    html_node("span") %>% html_text()
  
  result1=rbind(result1,data.frame(review, stringsAsFactors = FALSE))
}
View(result1)

#Q2---------------------------------------------------------------------------------------------
#Lets get first 5 reviews and other details
reviews <- url %>%
  read_html() %>%
  html_nodes(".hotels-community-tab-common-Card__section--4r93H")
reviews=reviews[1:5]
id <- reviews %>%
  html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC") %>%
  html_text()
quote <- reviews %>%
  html_node(".location-review-review-list-parts-ReviewTitle__reviewTitle--2GO9Z") %>%
  html_node("span") %>%
  html_text()
date <- reviews %>%
  html_node(".location-review-review-list-parts-EventDate__event_date--1epHa")%>%
  html_text() %>%
  sub(pattern="Date of stay: ",replacement="")
date=unlist(strsplit(date,split=" "))
date=matrix(date,ncol=2,byrow=TRUE)
colnames(date)=c("month","year")
month=date[,1]
year=date[,2]
review <- reviews %>%
  html_node(".location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE") %>%
  html_node("span") %>%
  html_text()
#obtain reviewer's profile pages
userpage <- reviews %>%
  html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC") %>%
  html_attr("href")

#obtain bubble rating
bubbles=reviews %>%
  html_node(".location-review-review-list-parts-RatingLine__bubbles--GcJvM") 

rating=numeric(5)
for (j in 1:5){
  if (length(bubbles[j]%>%html_nodes(".bubble_50"))==1){rating[j]=5
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_40"))==1){rating[j]=4
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_30"))==1){rating[j]=3
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_20"))==1){rating[j]=2
  next}
  rating[j]=1
}
result2=data.frame(id, quote, year, month, review,userpage, rating,stringsAsFactors = FALSE)


#Lets get other 15 reviews
for(i in 1:3){
  Sys.sleep(3)
  url=paste("https://www.tripadvisor.com/Hotel_Review-g42159-d90019-Reviews-or",i*5,"-Marriott_East_Lansing_at_University_Place-East_Lansing_Ingham_County_Michigan.html#REVIEWS",sep="")
  reviews <- url %>%
    read_html() %>%
    html_nodes(".hotels-community-tab-common-Card__section--4r93H")
  reviews=reviews[1:5]
  id <- reviews %>%
    html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC") %>%
    html_text()
  quote <- reviews %>%
    html_node(".location-review-review-list-parts-ReviewTitle__reviewTitle--2GO9Z") %>%
    html_node("span") %>%
    html_text()
  date <- reviews %>%
    html_node(".location-review-review-list-parts-EventDate__event_date--1epHa")%>%
    html_text() %>%
    sub(pattern="Date of stay: ",replacement="")
  date=unlist(strsplit(date,split=" "))
  date=matrix(date,ncol=2,byrow=TRUE)
  colnames(date)=c("month","year")
  month=date[,1]
  year=date[,2]
  review <- reviews %>%
    html_node(".location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE") %>%
    html_node("span") %>%
    html_text()
  userpage <- reviews %>%
    html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC") %>%
    html_attr("href")
  
  #obtain bubble rating
  bubbles=reviews %>%
    html_node(".location-review-review-list-parts-RatingLine__bubbles--GcJvM") 
  rating=numeric(5)
  for (j in 1:5){
    if (length(bubbles[j]%>%html_nodes(".bubble_50"))==1){rating[j]=5
    next}
    if (length(bubbles[j]%>%html_nodes(".bubble_40"))==1){rating[j]=4
    next}
    if (length(bubbles[j]%>%html_nodes(".bubble_30"))==1){rating[j]=3
    next}
    if (length(bubbles[j]%>%html_nodes(".bubble_20"))==1){rating[j]=2
    next}
    rating[j]=1
  }
  result2=rbind(result2 ,data.frame(id, quote, year, month, review,userpage,rating, stringsAsFactors = FALSE))
}

View(result2)

#Q3-----------------------------------------------------------------------------------------

q3 = ggplot(data=result2)+geom_bar(mapping=aes(x=rating),stat="count")

q3
