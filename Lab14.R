require(dplyr)
require(twitteR)
require(jsonlite)
require(httr)

# Authentication. 
consumer_key <- "9H7jm6KJxgMeJcVyPaQJFUXsI" 
consumer_secret <- "wkaLONHFeHrP24NeAE8T9w0KytpZwbxyfAcj7ldCGxnaw99HrK"
access_token <- "867062604040155136-OZjpWfXepqI6On08ghfVXHd4tiY6yqx"
access_secret <- "3AMZ0z06s8Xizfd9Sg9rSl1hMA1pftkNzeLkBDWEDKzWK"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#check rate limit
getCurRateLimitInfo()




# Q1---------------------------------------------------------------------------------

#get more information
user= twListToDF(lookupUsers("TheRock"))
View(user)

# Q2---------------------------------------------------------------------------------

timeline=twListToDF(userTimeline(user$screenName,n=10))
View(timeline)

# Q3---------------------------------------------------------------------------------

tw = searchTwitter("lionking", n = 10, since ="2018-01-01", retryOnRateLimit = 100)
d = twListToDF(tw)
View(d)

#Using httr package - authentication
myapp = oauth_app("twitter", key=consumer_key, secret=consumer_secret)
sig = sign_oauth1.0(myapp, token=access_token, token_secret=access_secret)
tweets=GET("https://api.twitter.com/1.1/search/tweets.json?q=lion%20king&src=typd&lang=en",sig)

setwd("C:/Users/neilr/OneDrive/Documents/R/LAB/Lab14")
write(as.character(tweets),file="output.json")


t=GET(paste("https://api.twitter.com/1.1/statuses/show/",d$id[1],sep=""),sig)
write(as.character(t),file="output2.json")

#parse the json
w=fromJSON(as.character(t))
View(w)

#extract content
id=w$id_str
screen_name=w$user$screen_name
user_location=w$user$location
user_description=w$user$description
tweet_text=w$text

d3=data.frame(id,screen_name,user_description, user_location,tweet_text)

# parse rest of the tweets
for (i in 2:10){
  print(i)
  t=GET(paste("https://api.twitter.com/1.1/statuses/show/",d$id[i],sep=""),sig)
  w=fromJSON(as.character(t))
  id=w$id_str
  screen_name=w$user$screen_name
  user_location=w$user$location
  user_description=w$user$description
  tweet_text=w$text
  d3=rbind(d3,data.frame(id,screen_name,  user_description, user_location,tweet_text))
}
View(d3)

