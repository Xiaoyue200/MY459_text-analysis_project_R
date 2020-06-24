---
title: "Rcode"
output:
  html_document:
    df_print: paged
  word_document: default
  pdf_document: default
---
  
### Current Projuect: Sentiment Analysis of Trump's tweets on China (contrast with Japan, north Korea, south Korea)
  
get all tweets from 2009 to the present(2020.5.30)
the data includes: source, id, tweets texts, the created time, the count of retweets, the count of favorites and whether the tweet is retweet
```{r}
library(tidyverse)
library(lubridate)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
all_tweets <- map(2009:2020, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  mutate(created_at = parse_date_time(created_at, "a b! d! H!:M!:S! z!* Y!")) %>%
  tbl_df()
```

motivation 1:
Between July 2011 and November 2012 (Obama’s re-election), a full 7% of Trump’s tweets mentioned China(China, Chinese, President Xi, Beijing).
After he was inaugurated, there are several months each year when over 5% of his tweets mentioned China. 
Besides, he mentioned China more frequently after 2016.

```{r}
library(stringr)
all_tweets %>%
  filter(!str_detect(text, "^(\"|RT)")) %>%
  group_by(month = round_date(created_at, "month")) %>%
  summarize(tweets = n(),
            hashtag = sum(str_detect(str_to_lower(text), "china|chinese|president xi|beijing")),
            percent = hashtag / tweets) %>%
  ungroup() %>%
  filter(tweets >= 10) %>%
  ggplot(aes(as.Date(month), percent)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.05,color = "red", lty = 2)+
  #geom_vline(xintercept = as.integer(as.Date("2011-04-30")), color = "red", lty = 2) +
  #geom_vline(xintercept = as.integer(as.Date("2012-11-06")), color = "blue", lty = 2) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Time",
       y = "% of Trump's tweets that mention China",
       subtitle = paste0("Summarized by month; only months containing at least 10 tweets.\n",
                        ""),
       title = "Trump's tweets mentioning China")+
  theme_bw()
```
```{r}
all_tweets %>%
  filter(!str_detect(text, "^(\"|RT)")) %>%
  group_by(month = round_date(created_at, "year")) %>%
  summarize(tweets = n(),
            hashtag = sum(str_detect(str_to_lower(text), "china|chinese|president xi|beijing")),
            percent = hashtag / tweets) %>%
  ungroup() %>%
  #filter(created_at >= "2015-01-01") %>%
  ggplot(aes(as.Date(month), percent)) +
  geom_line() +
  geom_point() +
  #geom_hline(yintercept = 0.05,color = "red", lty = 2)+
  geom_vline(xintercept = as.integer(as.Date("2016-01-01")), color = "blue", lty = 2) +
  #geom_vline(xintercept = as.integer(as.Date("2012-11-06")), color = "blue", lty = 2) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Time",
       y = "% of Trump's tweets that mention China",
       subtitle = paste0("Summarized by year.\n",
                        ""),
       title = "Trump's tweets mentioning China")+
  theme_bw()
```

```{r}
between <- all_tweets %>%
  filter(created_at >= "2011-04-30", created_at < "2012-11-07") %>%
  mutate(china = str_detect(str_to_lower(text), "china|chinese|president xi|beijing"))

percent_mentioned <- mean(between$china)
```

motivation 2:
Compared with other Asian countries like Japan, North and South Korea, the number of Trump's tweets about China is more than twice the sum of the others .
```{r}
paste("China: ",length(grep("china|chinese|president xi|beijing", all_tweets$text, ignore.case = TRUE)))
paste("North Korea: ",length(grep("north korea+", all_tweets$text, ignore.case = TRUE)))
paste("Japan: ",length(grep("japan+", all_tweets$text, ignore.case = TRUE)))
paste("South Korea: ",length(grep("south korea+", all_tweets$text, ignore.case = TRUE)))
paste("倍数: ",969/(199+112+50))
```

motivation 3:
very different tones on China

description of the corpus:
China-related tweets from 2011-01-27 to 2020-05-30: texts containing any word in the set {China, Chinese, President Xi, Beijing}
```{r}
# get the China-related tweets
china_tweets <- all_tweets[grep('china|chinese|president xi|beijing', all_tweets$text, ignore.case=TRUE),]
```

remove punctuation, numbers, stopwords,urls, words and symbols which are not of interest to our data('rt', 'amp', etc). keep Twitter features…
document-feature matrix is of 969 documents and 3480 features.
```{r}
library(quanteda)
twcorpus <- corpus(china_tweets)
twdfm <- dfm(twcorpus, verbose=TRUE, 
             remove=c(stopwords("english"),"@realdonaldtrump", "just", "rt", "amp", "now", "one", "can", "u","cont"), remove_punct=TRUE, remove_url=TRUE,
             remove_numbers=TRUE)
twdfm
```

take a look at a wordcloud of the most frequent 50 features
we could find that he often mention some economic stuff like money,jobs, tariffs, etc when mentioning China.
```{r}
textplot_wordcloud(twdfm, rotation=0, min_size=2, max_size=4.5, max_words=50)
```


sentiment analysis1: sentiment detection using dictionary methods
I will use the positive and negative categories in the augmented General Inquirer dictionary to measure the extent to which Trump adopted a positive or negative tone forsna.
```{r}
library(quanteda.dictionaries)
data(data_dictionary_geninqposneg)
pos.words <- data_dictionary_geninqposneg[['positive']]
neg.words <- data_dictionary_geninqposneg[['negative']]
mydict <- dictionary(list(positive = pos.words,
                          negative = neg.words))

```

the average sentiment score of Trump's tweet on China is 0.32
```{r}
sent <- dfm(twcorpus, verbose=TRUE, 
             remove=c(stopwords("english"),"@realdonaldtrump", "just", "rt", "amp", "now", "one", "can", "u","cont"), remove_punct=TRUE, remove_url=TRUE,
             remove_numbers=TRUE, dictionary = mydict)
china_tweets$score <- as.numeric(sent[,1]) - as.numeric(sent[,2])
#average sentiment score
mean(china_tweets$score)
```

the most positive tweet:
the most negative tweet:
the proportion of positive, neutral, and negative tweets:
```{r}
#the most pos. tweet
china_tweets[which.max(china_tweets$score),]
#the most neg. tweet
china_tweets[which.min(china_tweets$score),]
# what is the proportion of positive, neutral, and negative tweets?
china_tweets$sentiment <- "neutral"
china_tweets$sentiment[china_tweets$score<0] <- "negative"
china_tweets$sentiment[china_tweets$score>0] <- "positive"
table(china_tweets$sentiment)
```

compared with other countries:
after normalize words by text length, the sentiment score for the four countries:
China: 0.8926272
North Korea: 3.1616648
South Korea: 5.4787506
Japan: 5.9535573
```{r}
twchina_tweets <- all_tweets[grep('china|chinese|president xi|beijing', all_tweets$text, ignore.case=TRUE),]
twchina_tweets$country <- 'China'
nk_tweets <- all_tweets[grep("north korea+", all_tweets$text, ignore.case=TRUE),]
nk_tweets$country <- 'North Korea'
sk_tweets <- all_tweets[grep("south korea+", all_tweets$text, ignore.case=TRUE),]
sk_tweets$country <- 'South Korea'
jp_tweets <- all_tweets[grep("japan+", all_tweets$text, ignore.case=TRUE),]
jp_tweets$country <- 'Japan'
countrytweets <- rbind(twchina_tweets, nk_tweets,sk_tweets,jp_tweets)

```

```{r}
tmpdfm <- dfm(corpus(countrytweets), groups = "country") 
tmpdfm <- dfm_weight(tmpdfm, scheme="prop")
tmpsent <- dfm_lookup(tmpdfm, dictionary = mydict)
(tmpsent[,1]-tmpsent[,2])*100
```

sentiment score changes every year:
```{r}
china_tweets %>%
  group_by(month = round_date(created_at, "month")) %>%
  summarize(average_sentiment = mean(score)) %>%
  ggplot(aes(as.Date(month), average_sentiment)) +
  geom_line() +
  geom_hline(color = "red", lty = 2, yintercept = 0) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  labs(x = "Time",
       y = "Average Sentiment Score",
       title = "@realDonaldTrump sentiment on China over time",
       subtitle = "Dashed line represents a 'neutral' sentiment average.")+
  theme_bw()
```


after his inauguration, his tweets turned towards postive.
recently(2020-02~2020-05)
```{r}
china_tweets %>%
  group_by(month = round_date(created_at, "month")) %>%
  summarize(average_sentiment = mean(score)) %>%
  ggplot(aes(as.Date(month), average_sentiment)) +
  geom_line() +
  geom_vline(xintercept = as.integer(as.Date("2015-06-16")), color = "red", lty = 2) +
  geom_vline(xintercept = as.integer(as.Date("2017-01-20")), color = "blue", lty = 2)+
  geom_vline(xintercept = as.integer(as.Date("2020-02-01")), color = "grey", lty = 2)+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  labs(x = "Time",
       y = "Average Sentiment Score",
       title = "@realDonaldTrump sentiment on China over time",
       subtitle ="Red line is when Trump launched his campaign , blue is inauguration day, grey is 2020-02.")+
  theme_bw()
```

run an LDA model: topics 
```{r}
#subset it to pos, neg and neutral 
#pos.tweets <- filter(china_tweets,sentiment=="positive")
neg.tweets <- filter(china_tweets,sentiment=="negative")
#neu.tweets <- filter(china_tweets,sentiment=="neutral")
```


```{r}
# for negative tweets
neg.twdfm <- dfm(corpus(neg.tweets), verbose=TRUE, 
             remove=c(stopwords("english"),"@realdonaldtrump", "just", "rt", "amp", "now", "one", "can", "u","cont"), remove_punct=TRUE, remove_url=TRUE,
             remove_numbers=TRUE)
neg.cdfm <- dfm_trim(neg.twdfm, min_docfreq = 2)

# estimate LDA with K topics
K <- 5
lda <- LDA(neg.cdfm, k = K, method = "Gibbs", 
                control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))
terms <- get_terms(lda, 15)
topics <- get_topics(lda, 1)
```

```{r}
# A few labels:
paste(terms[,1], collapse=", ")
paste(terms[,2], collapse=", ")
paste(terms[,3], collapse=", ")
paste(terms[,4], collapse=", ")
paste(terms[,5], collapse=", ")
sample(neg.tweets$text[topics==1], 1)
sample(neg.tweets$text[topics==2], 1)
sample(neg.tweets$text[topics==3], 1)
sample(neg.tweets$text[topics==4], 1)
sample(neg.tweets$text[topics==5], 1)
```

Pick a topic whose prevalence you think may have evolved over time and plot it. (For example, topic2: trade). What do you find?
We see a spike in the lead-up during his campaign
```{r}
# Topic 2
paste(terms[,2], collapse=", ")

# add probability to df
neg.tweets$prob_topic <- lda@gamma[,2]
# creating month variable
neg.tweets$month <- substr(neg.tweets$created_at, 1, 7)
# now aggregate at the month level
agg <- aggregate(prob_topic~month, data=neg.tweets, FUN=mean)
#months <- c(seq(as.Date('2011-07-01'), as.Date("2013-10-01"), by="month"),as.Date('2013-12-01'),seq(as.Date('2014-02-01'), as.Date("2014-04-01"), by="month"),as.Date('2014-06-01'))
# and plot it
library(xts)
PCP <- ts(agg$prob_topic, frequency = 12, start = 2012)
plot(as.xts(PCP), major.format = "%Y",xlab="Month", ylab="Avg. prob. of tweets about topic 2",
     main="Estimated proportion of tweets about trade")
```
