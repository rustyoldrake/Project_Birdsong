######################################################
### This is some R code that scrapes TWITTER to get data to use for IBM Watson ENRICHMENT in other Blogs
### The Watson services here http://www.ibm.com/smarterplanet/us/en/ibmwatson/developercloud/services-catalog.html
### Uses: https://github.com/geoffjentry/twitteR
### Support: http://thinktostart.wordpress.com/2013/05/22/twitter-authentification-with-r/
### there is also an IBM Watson ASK - Appliation Starter Kit as another tool set
######################################################

## TWITTER
library(twitteR)
library(RJSONIO)
library(stringr)
library(tm)
library(plyr)
library(ROAuth)  # if you get this error - need this lib Error: object 'OAuthFactory' not found


## WATSON
library(RCurl) # General Network Client Interface for R
library(httr) # Tools for URLs and HTTP
library(rjson) # JSON for R
library(jsonlite) # JSON parser
library(XML) # XML parser
library(data.table) # data shaping
library(reshape2) # data shaping
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(png) # for the presenting of images



######### Housekeeping And Authentication
setwd("/Users/ryan/Documents/Project_Birdsong")
getwd()
source("keys.R") # this files is where you put your Access Credentials from Bluemix (username and password)

## This next line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
Sys.setlocale(locale="C") # error: input string 1 is invalid in this locale
options(warn=-1) # careful - turns off warnings


### SET UP THE ACCESS
reqURL <- "https://api.twitter.com/oauth/request_token/"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
base_url_TON = "https://gateway.watsonplatform.net/tone-analyzer/api"  ## Tone Analyzer (also can do as part of alchemy combined call)

## CHECK WE GOT THESE FROM KEYS.R
consumer_key # check we got it
consumer_secret # check we got it
access_token # check we got it
access_secret # check we got it
username_password_TON # check we got it

## http://www.r-bloggers.com/setting-up-the-twitter-r-package-for-text-analytics/
## https://apps.twitter.com/app/
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


############  FUNCTION DECLARATIONS - UTILITY FUNCTIONS - CLEAN DATA

### FUNCTION tolower 
try.tolower = function(x)
{ y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y) }

### FUNCTION ## Clean up junk from text
clean.text <- function(some_txt)
{
  #some_txt = gsub("https://t.co/", " ", some_txt)
  some_txt = gsub('http.* *', " ", some_txt)
  #some_txt = gsub("http\\w+", " ", some_txt)
  #some_txt = gsub("<", "", some_txt)
  #some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  #some_txt = gsub("@\\w+", " ", some_txt)
  some_txt = gsub("[][!$%()*,.:;<=>^_`|~.{}]", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt) # kills our # and @ in tweets
  some_txt = gsub("[[:digit:]]", " ", some_txt)
  #some_txt = gsub("[ \t]{2,}", " ", some_txt)
  #some_txt = gsub("^\\s+|\\s+$", " ", some_txt)
  some_txt = gsub("\\\n", " ", some_txt)
  #some_txt = gsub("amp", "", some_txt)
  #some_txt = sapply(some_txt, try.tolower)
  #some_txt = some_txt[some_txt != ""]
  some_txt <- iconv(some_txt, "latin1", "ASCII", sub="") # kills the unicode characters like <U+2XXX>
  names(some_txt) = NULL
  return(some_txt)
}


### FUNCTION to post data to Tone Analyzer and return results
process_data_to_tone <- function(text)
{
  response <- POST(url="https://gateway.watsonplatform.net/tone-analyzer/api/v3/tone?version=2016-05-19",
                   authenticate(username_TON,password_TON),
                   add_headers("Content-Type"="text/plain","charset"="UTF-8"),
                   body=text)
  
  response_text <- httr::content(response, "text", encoding="UTF-8")  # or encoding = "ISO-8859-1"
  # https://github.com/hadley/httr/issues/330
  
  abc <- tidyResponse(response_text)
  return(abc)
}


### Function to process output from API and table
tidyResponse <- function(data)
{
  data <- as.data.frame(strsplit(as.character(data),"\"score\""))
  data <- data[-c(1), ] # remove dud first row
  data  <- gsub("\"tone_id\":","",data)
  data  <- gsub(":","",data)
  data  <- gsub("\"","",data)
  data  <- gsub("_big5","",data)
  data <- data.frame(data)
  data <- data.frame(do.call('rbind', strsplit(as.character(data$data),',',fixed=TRUE)))
  data <- data[,-c(3:6), ] # remove dud first row
  data <- data[c("X2", "X1")]
  data$X1 <- as.character.numeric_version(data$X1) # not sure why, but coercing to numbers requires this
  data$X1 <- as.numeric(data$X1)
  data$X1 <- round((data$X1),3)
  setnames(data,c("trait","signal"))
  return(data)
}

# process_data_to_tone("I'm angry about the speeding ticket, but confident It's my last one")


#### CODE !! CODE !!

#### DATA ! - GO GET SOME TWEETS !
tweets = searchTwitter("#ZCash",200, lang="en") # something nice and friendly
length(tweets) #how many? 1000?  Sometimes you get less!
head(tweets)
backup <- tweets
head(backup)
tweets <- backup
tweets = laply(tweets, function(t) t$getText())
tweets
tweets = clean.text(tweets)
head(tweets,10)


###############################  ALCHEMY - COMBINED CALL - RETREIVE AND STORE INFO
alchemy_url <- "http://gateway-a.watsonplatform.net/calls/text/"
api_feature <- "TextGetCombinedData"
api_key #check we got this from keys.R
output_mode <- "json"
#text <- read.csv("data.csv") # old method - csv input - we'll use tweets we just pulled
text <- tweets
Encoding(text) <- "UTF=8"
head(text)


########## METHOD 2 - NEW JSON WAY   ############

text <- data.frame(tweets)
text
len <- dim(text)[1] # how tall is our data frame?
len
output_mode <- "json"
catchers_mitt <- as.data.frame(matrix(ncol=26, nrow=len))
setnames(catchers_mitt,c("index","text",
                         "keyword1","keyword2","keyword3",
                         "concept1","concept2","concept3",
                         "entity1","entity2","entity3",
                         "anger","disgust","fear","joy","sadness",
                         "analytical","confident","tentative","openness",
                         "conscientiousness","extraversion","agreeableness","emotional_range",
                         "taxonomy",
                         "knowledgegraph"))

catchers_mitt$text <- text[,1] # text column makes up the text
catchers_mitt


## OK! Let's enrich now with Alchemy Combined call PLUS a Tone/EMotion Call
for (i in 1:len){
  text_to_send <- URLencode(paste(text[i,]))
  query <- paste(alchemy_url,api_feature,"?extract=keyword,entity,concept,taxonomy,doc-sentiment&apikey=",api_key,"&text=",text_to_send,"&outputMode=",output_mode,"&knowledgeGraph=1", sep="")
  query
  response <- POST(query)
  reply <- fromJSON(paste(response))
  reply
  
  catchers_mitt$index[i] <- i
 
  catchers_mitt$keyword1[i] <- paste(reply$keywords$text[1],"",sep="")
  catchers_mitt$keyword2[i] <- paste(reply$keywords$text[2],"",sep="")
  catchers_mitt$keyword3[i] <- paste(reply$keywords$text[3],"",sep="")
  
  catchers_mitt$concept1[i] <- paste(reply$concepts$text[1],"",sep="")
  catchers_mitt$concept2[i] <- paste(reply$concepts$text[2],"",sep="") # in case Null, forces to no character string
  catchers_mitt$concept3[i] <- paste(reply$concepts$text[3],"",sep="")
  
  catchers_mitt$entity1[i] <- paste(reply$entities$text[1],"",sep="")
  catchers_mitt$entity2[i] <- paste(reply$entities$text[2],"",sep="")
  catchers_mitt$entity3[i] <- paste(reply$entities$text[3],"",sep="")
  
  catchers_mitt$taxonomy[i] <- paste(reply$taxonomy$label[1],"")
  catchers_mitt$knowledgegraph[i] <- paste(reply$keywords$knowledgeGraph[1,],"")
  
  ## Tone and EMotional signal
  response_emo <- process_data_to_tone(paste(text[i,])) # this function does its own URL Encode %20
  response_emo$signal <- as.numeric(response_emo$signal)
  catchers_mitt$anger[i] <- response_emo$signal[1]
  catchers_mitt$disgust[i] <- response_emo$signal[2]
  catchers_mitt$fear[i] <- response_emo$signal[3]
  catchers_mitt$joy[i] <- response_emo$signal[4]
  catchers_mitt$sadness[i]  <- response_emo$signal[5]
  catchers_mitt$analytical[i]  <- response_emo$signal[6]
  catchers_mitt$confident[i]  <- response_emo$signal[7]
  catchers_mitt$tentative[i]  <- response_emo$signal[8]
  catchers_mitt$openness[i]  <- response_emo$signal[9]
  catchers_mitt$conscientiousness[i]  <- response_emo$signal[10]
  catchers_mitt$extraversion[i]  <- response_emo$signal[11]
  catchers_mitt$agreeableness[i]  <- response_emo$signal[12]
  catchers_mitt$emotional_range[i] <- response_emo$signal[13]
  
  print(catchers_mitt[i,]) # print the line out
}


## Clean up and WRITE ENRICHED Data Frame to A File
# only do this to non numerics!
#catchers_mitt <- data.frame(lapply(catchers_mitt, as.character), stringsAsFactors=FALSE)

dim(catchers_mitt)
catchers_mitt
write.csv(catchers_mitt,"enriched_tweets6_#ZCash.csv")

# Emotions
m <- rbind(c(1,2,3), c(4,5,6)); layout(m); layout.show(max(m)) # plotty mc plotface
hist(catchers_mitt$anger,breaks=10,xlim = range(0,1),col="red",main="Anger")
hist(catchers_mitt$joy,breaks=10,xlim = range(0,1),col="yellow",main="Joy")
hist(catchers_mitt$fear,breaks=10,xlim = range(0,1),col="purple",main="Fear")
hist(catchers_mitt$disgust,breaks=10,xlim = range(0,1),col="green",main="Disgust")
hist(catchers_mitt$sadness,breaks=10,xlim = range(0,1),col="blue",main="Sadness")

colors = c("red", "green", "purple", "yellow", "blue") 
plot(catchers_mitt[12:16],main="Emotions",col=colors) # Emotions

# Social Tendencies
m <- rbind(c(1,2,3), c(4,5,6), c(7,8,9)); layout(m); layout.show(max(m)) # plotty mc plotface
hist(catchers_mitt$analytical,breaks=100,main="Analytical")
hist(catchers_mitt$confident,breaks=100,main="Confident")
hist(catchers_mitt$tentative,breaks=100,main="Tentative")
hist(catchers_mitt$openness,breaks=100,main="Openness")
hist(catchers_mitt$conscientiousness,breaks=100,main="Conscientiousness")
hist(catchers_mitt$extraversion,breaks=100,main="Extraversion")
hist(catchers_mitt$agreeableness,breaks=100,main="Agreeableness")
hist(catchers_mitt$emotional_range,breaks=100,main="Emotional_range")

plot(catchers_mitt[17:24],main="Social Tendencies") # www.ibm.com/watson/developercloud/tone-analyzer.html








