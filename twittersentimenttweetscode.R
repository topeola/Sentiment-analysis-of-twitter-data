#Initialise libraries
library(twitteR)
library(stringr)
library(plyr)

#Accessing twitter API 
api_key <- "XXXXXXXXXXX"

api_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXX"

access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#search for rackspace tweets 
rackspace.tweets = searchTwitter('rackspace', n=1000, lang = "en")

#check for how many tweets were obtained
length(rackspace.tweets)
head(rackspace.tweets, 10)

#remove retweets 
strip_retweets(rackspace.tweets, strip_manual = TRUE, strip_mt = TRUE)
no_retweets = strip_retweets(rackspace.tweets)
length(no_retweets)

#convert to dataframe and de-duplicate tweets
tweets1.df = twListToDF(no_retweets)
tweets2.df <- tweets1.df [!duplicated(tweets1.df$text),]

#write tweets to a .csv file
write.csv(tweets2.df, file='C:/Users/owner/Documents/rackspacenoretweets20150419.csv', row.names=F)

#sentiment scoring function is defined here 

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}

# Load sentiment word lists
hu.liu.pos = scan('C:/Users/owner/Downloads/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:/Users/owner/Downloads/negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'great', 'fantastic', 'excellent', 'good','upgrade','thanks', 'terrific','outstanding')
neg.words = c(hu.liu.neg, 'regret', 'expensive','terrible', 'worst', 'bad','hard','fuck', 'shit', 'suck')

#import the csv file containing our tweets
datasetrackspace <- read.csv("C:/Users/owner/Documents/rackspacenoretweets20150419.csv")
datasetrackspace$text <- as.factor(datasetrackspace$text)

#score all tweets
rackspace.scores = score.sentiment(datasetrackspace$text, pos.words,neg.words, .progress='text')
path <- "C:/Users/owner/Desktop"
write.csv(rackspace.scores, file = paste(path,"rackspacescores.csv", sep = ""), row.names = TRUE)

#qplots showing distribution of scores of tweets
qplot(awscloud.scores$score, xlab = 'Score of Amazon Web Services Cloud Tweets')
qplot(rackspace.scores$score, xlab = 'Score of Rackspace Tweets')

rackspace.scores$cloud = 'Rackspace'
rackspace.scores$code = 'RS'

awscloud.scores$cloud = 'Amazon Web'
awscloud.scores$code = 'AWS'

#combine results into a single "all.scores" data.frame 
all.scores = rbind(rackspace.scores, awscloud.scores)

#focus on only positive and negative tweets
all.scores$pos = as.numeric( all.scores$score >= 1 )
all.scores$neg = as.numeric( all.scores$score <= -1 )

#calculating overall sentiment scores
twitter.df2 = ddply(all.scores, c('cloud', 'code'), summarise,
                    pos.count = sum(pos ), neg.count = sum(neg ) )
twitter.df2$all.count = twitter.df2$pos.count + twitter.df2$neg.count
twitter.df2$score = round( 100 * twitter.df2$pos.count /
                             twitter.df2$all.count )
library(doBy)
orderBy(~-score, twitter.df2)

qplot(awscloud.scores$score, xlab = 'Score of Amazon Web Services Cloud Tweets')



