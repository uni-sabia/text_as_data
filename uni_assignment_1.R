##################################
### Code Completion Assignment ###
##################################

# Your goal is to complete the following skeleton code so that the script can be executed
# the three *** indicate where you need to to insert the correct code 

# stringr & regular expressions -------------------------------------------

addresses <-c("221B Baker St., London", "1600 Pennsylvania Avenue, Washington D.C.", 
              "742 Evergreen Terrace, Springfield")

products <- c("TV  ", " laptop", "portable  charger", "Wireless Keybord",
              "   HeadPhones   ")

sentences <- stringr::sentences[1:20]

field_names <- c("order_number", "order_date", "customer_email", "product_title", "amount")

email <- c("tom@hogwarts.com",
           "tom.riddle@hogwarts.com",
           "tom@hogwarts.eu.com",
           "potter@hogwarts.com",
           "harry@hogwarts.com",
           "hermione+witch@hogwarts.com")

files <- c(".bash_profile",
           "workspace.doc",
           "img0912.jpg",
           "updated_img0912.png",
           "documentation.html",
           "favicon.gif",
           "img0912.jpg.tmp",
           "access.lock")

#convert addresses to lower-case.

library(stringr)
str_to_lower(string = addresses)

#extract digits from addresses

str_extract(string = addresses, pattern = "[[:digit:]]+")

#split addresses into two parts: street & city

str_split(string = addresses, pattern = ",", simplify = T)

#split addresses into three parts: house number, street & city
str_split(string = addresses, pattern = "(?<=[0-9]{3,4}[A-Z]?)\\s|,\\s", simplify = T, n=3)

#For sentences that end with the letter “t” extract the last word
str_extract(string = sentences, pattern = "\\w+t.$") 

#Extract the first 30 characters from each sentence

str_trunc(string = sentences, width=30, ellipsis = "...", "right")

#replace all underscores in products with spaces and 
#capitalize the first letter of each word

str_replace(field_names, "_", " ") %>%
  str_to_title()
  
#extract names appearing before @ from email 
  
str_extract(email, "[^@]+")

#extract the three images (.jpg, .png, .gif) from files
str_extract(files, pattern="\\w+.png|\\w+.gif|\\w+.jpg") 



# web scraping & tidytext -------------------------------------------------

library(rvest)
library(tidytext)
library(tibble)
library(dplyr)

#extract the text of J.K. Rowling's commencement speech at Harvard University

url <- "https://news.harvard.edu/gazette/story/2008/06/text-of-j-k-rowling-speech/"
ccs <- "p" # This selector conveniently puts one paragraph per row. 

speech <- url %>% 
  read_html() %>%
  html_nodes(ccs) %>%
  html_text
  
  # convert the text to a tibble, remove the first line ("Text as Delivered") 
  # and the 15th line ("Sign up for daily emails to get the latest Harvard news.")
  # and add a column with a number for each paragraph
  
  speech_df <- tibble(text = speech) %>% 
  slice(-(1:12)) %>% # My text data has ads from 1st row until the 12th, unlike the instruction
  rownames_to_column()
  
  #tokenize the text into words and remove stop words
  
  data("stop_words") # get stopwords data

  speech_df_tokens <- speech_df %>% 
  unnest_tokens(words, text) %>% 
  rename(word=words) %>%
  anti_join(stop_words)


#list the 5 most frequent words
  top_five <- speech_df_tokens %>%
    group_by(word) %>%
    summarize(freq=n()) %>%
    arrange(desc(freq)) %>%
    top_n(5)
  top_five ## Life, failure, day, people, parents are top-five frequently-occurring words

  # Reading in text ---------------------------------------------------------
library(readtext)
library(quanteda)

#read in the UK manifestos from the course's data folder, 
#create document names for party and year from file names
manifestos <- readtext("data/UK_manifestos/*.txt", 
                       docvarsfrom= "filenames",
                       dvsep = "-",
                       docvarnames = c("Party", "Year"))

# create a corpus out of the documents
manifesto_corpus <- corpus(manifestos)
summary(manifesto_corpus)

# overwrite the party column with these new labels: Conservatives, Labour, LibDems
manifesto_corpus$Party <- factor(manifesto_corpus$Party, 
                                  labels=c("Conservatives", "Labour", "LibDems"))
summary(manifesto_corpus)

#tokenize the text into words, remove numbers and punctuations, and convert it to lower case
#and find out in which context the parties mention words containing europe

manifesto_tokens <- tokens(manifesto_corpus,
                           what = "word", 
                           remove_punct = TRUE,
                           remove_numbers = TRUE) %>%
  tokens_tolower()

europe_context <- kwic(manifesto_tokens, "*europe*")
europe_context
nrow(europe_context) # 27 matches
 
#now remove stop words from the text using the smart source 
#and tokenize the manifestos into bigrams 
manifesto_bigrams<- manifesto_tokens %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_ngrams(n=2)

#create a document feature matrix from the manifesto bigrams 
#and keep only the party manifestos from the Liberal Democrats
manifesto_dfm <- dfm(manifesto_bigrams) %>%
  dfm_subset(Party=="LibDems")
  
#find out how many bigrams appear only once

once_bigram <- manifesto_dfm %>% 
  dfm_trim(min_termfreq=1,
           max_termfreq=1)
once_bigram # 3893 bigrams (features) appear only once 
