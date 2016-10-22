setwd("/Users/colinleverger/Documents/Experiments/R/wordcloud")

#### Word Cloud generator ####
# Install packages
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML"))

# Load libs
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)

### Parameters ###
# Site URL
url = 'http://colinleverger.fr'
# Words to delete from the cloud
unwanted.words = c("use","currently")

# Call to the function
generatewordcloud(url, unwanted.words, min.freq.user = 3)

# Function to generate a word cloud from a website
# Params: url, unwanted.words, stem.doc (default FALSE), min.freq.user: minimal freq of words to display
generatewordcloud <- function(url, unwanted.words, stem.doc = FALSE, min.freq.user){
  # Download HTML
  html <- getURL(url, followlocation = TRUE)

  # Parse html
  doc = htmlParse(html, asText = TRUE)
  plain.text <- xpathSApply(doc, "//p", xmlValue)
  write(plain.text, "text/plaintext.txt")
  
  # Prepare the corpus...
  corpus <- Corpus(DirSource("text/"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  if(stem.doc){
    corpus <- tm_map(corpus, stemDocument)
  }
  
  # Remove unwanted words from corpus
  corpus <- tm_map(corpus, removeWords, unwanted.words)
  
  # Generate wordcloud
  wordcloud(
    corpus,
    min.freq = min.freq.user,
    random.order = FALSE,
    rot.per = 0.35,
    use.r.layout = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
  return(1)
}
