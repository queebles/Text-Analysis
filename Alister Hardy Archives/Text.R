# Clean text and prepare data for analysis

library(hunspell)
library(qdap)
library(SentimentAnalysis)
library(stringr)
library(tidyverse)
library(tm)
library(utf8)

# Load data ----

hardy <- read.csv("data/original/ReligiousExperienceD.csv")
hardy <- hardy[1:11]

# Light cleaning ----

clean_vector <- function(vector) {
        vector <- tolower(vector)
        vector <- removeNumbers(vector)
        vector <- replace_contraction(vector)
        vector <- removeWords(vector, stopwords('en'))
        vector <- stripWhitespace(vector)
        return(vector)
}

words <- clean_vector(hardy$text)
rm(hardy)

# Create/clean corpus ----

words <- Corpus(VectorSource(words))
clean_corpus <- function(corpus) {
        # convert to lower case
        corpus <- tm_map(corpus, content_transformer(tolower))
        # convert hypens to spaces
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "-",
                         replace = " ")
        # convert to neardeath
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "near death| nde| ndes| nde's",
                         replace = " neardeath ")
        # convert to lifetime
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "life time|life times",
                         replace = "lifetime")
        # convert to outofbody
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "out of body| oob| oobs| oob's",
                         replace = " outofbody ")
        # convert to dejavu
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "deja vu",
                         replace = "dejavu")
        # convert to inlaw
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "in law|in laws|in law's",
                         replace = "inlaw")
        # convert to experience
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "experiences|experienced|experiencing",
                         replace = "experience")
        # convert to feel
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "felt|feeling|feels",
                         replace = "feel")
        # convert to know
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "knew|knowing|knows",
                         replace = "know")
        # convert to seem
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "seeming|seemed",
                         replace = "seem")
        # convert to believe
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "believes|believed|believing",
                         replace = "believe")
        # convert to come
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "came|coming|comes",
                         replace = "come")
        # convert to department
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " dept| depts",
                         replace = " department ")
        # convert to minutes
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " mins|minute",
                         replace = " minutes ")
        # convert to time
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "oclock",
                         replace = "time")
        # convert to hours
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " hrs|hour",
                         replace = " hours ")
        # convert to weeks
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " wks|week ",
                         replace = " weeks ")
        # convert to hospital
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "hosp |hospitals",
                         replace = " hospital ")
        # convert to doctor
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "dr | drs|doc |doctor|doctors",
                         replace = " doctor ")
        # convert to building
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " bldg| bldgs|buildings",
                         replace = " building ")
        # convert to think
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "thought|thinking|thoughts|thinks",
                         replace = "think")
        # convert to yards
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " yds|yard",
                         replace = " yards ")
        # convert to years
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " yrs| yr|year ",
                         replace = " years ")
        # convert to tell
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "said|told|tell|tells|telling",
                         replace = "tell")
        # remove words between < >
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "<[^[:space:]]*",
                         replace = "")
        # remove @ (usernames)
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "@[^[:space:]]*",
                         replace = "")
        # remove what would be emojis
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "\\W",
                         replace = " ")
        # remove URLs
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "http[^[:space:]]*",
                         replace = "")
        # remove numbers
        corpus <- tm_map(corpus, removeNumbers)
        # remove anything other than English letters or space
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = "[^[:alpha:][:space:]]*",
                         replace = "")
        # remove punctuation
        corpus <- tm_map(corpus, removePunctuation)
        # remove stopwords
        corpus <- tm_map(corpus, removeWords, c("the", "and", "my", "alasdair",
                                                "you", "your", "yours",
                                                "we ", "ours", "me", "allister",
                                                "etc", "mine ", "us", "alister",
                                                "he", "she", "his", "alistair",
                                                "hers", "they", "theirs",
                                                "those", "or", "sic", "alester",
                                                "it", "its", "surname",
                                                "sir", "alister", "hardy",
                                                "mrs", "ive", "im", "lbs",
                                                "esq", "mph ", "monday",
                                                "tuesday", "wednesday",
                                                "thursday", "friday", "saturday",
                                                "sunday", "mon", "tue",
                                                "wed", "thurs", "thur",
                                                "thu", "fri", "jan", "aleister",
                                                "feb", "mar", "apr", "alaister",
                                                "jun", "jul", "aug", "alastor",
                                                "sep", "oct", "nov","allasdair",
                                                "dec", "xmas", "weds","alaster",
                                                "tues", "january", "february",
                                                "april", "june", "july",
                                                "august", "september", "october",
                                                "november", "december",
                                                "weekday", "weekend","alastir",
                                                "sundays", "mondays", "tuesdays",
                                                "wednesdays", "thursdays",
                                                "fridays", "saturdays",
                                                stopwords("english")))
        # remove single letter words
        corpus <- tm_map(corpus, content_transformer(gsub),
                         pattern = " . ",
                         replace = " ")
        # remove extra whitespace
        corpus <- tm_map(corpus, stripWhitespace)
        # reduce words to basic forms
        # corpus <- tm_map(corpus, stemDocument)
        return(corpus)
}

words <- clean_corpus(words)

# document-term matrix (DTM) - lists all occurrences of words in corpus
words_dtm <- TermDocumentMatrix(words)
rm(words)

# convert dtm to matrix
words_m <- as.matrix(words_dtm)

# convert to data frame
words_df = data.frame(sort(rowSums(as.matrix(words_dtm)),
                             decreasing = T))
words_df <- rownames_to_column(words_df,
                           var = "term")
names(words_df)[2] <- "num"

# Spell check  ----

# coerce encoding to UTF-8
words_df$term <- utf8_encode(words_df$term)

# generate a sorted frequency table, drop terms that are 2 characters or less
freq <- words_df %>%
        group_by(term) %>%
        summarise(num = sum(num)) %>%
        filter(nchar(term) > 2 ) %>%
        arrange(-num)

# use qdap to check words one by one and make corrections
# change length to a subset of rows if checking smaller parts of the whole
out <- check_spelling_interactive(freq$term[1:length(freq$term)])
preprocessed(out)
fixit <- attributes(out)$correct
freq$term[1:length(freq$term)] = fixit(freq$term[1:length(freq$term)])

# split strings of 2 or more words into new rows
freq <- freq %>%
mutate(term = strsplit(as.character(term), " ")) %>%
  unnest(term)

# rearrange columns
freq <- freq[c(3,1)]

# format words to be processed by hunspell
freq$term <- tolower(freq$term)

# spell check using british english
check <- hunspell_check(freq$term, dict = "en_GB")

# create a new column with results of the check
freq$check <- as.logical(as.vector(check))

# split words into correct and incorrect groups
correct <- freq %>%
  filter(check == T)
incorrect <- freq %>%
  filter(check == F)

# format results for another round of checks
correct <- correct[1:2]
incorrect <- incorrect[1:2]
rm(freq)

# spell check using american english
check <- hunspell_check(incorrect$term, dict = "en_US")
incorrect$check <- as.logical(as.vector(check))

# split groups
correct2 <- incorrect %>%
  filter(check == T)
incorrect2 <- incorrect %>%
  filter(check == F)

# merge results
incorrect <- incorrect2[1:2]
rm(incorrect2)
correct2 <- correct2[1:2]
correct <- rbind(correct, correct2)
rm(correct2)
correct <- correct %>%
  group_by(term) %>%
  summarise(num = sum(num)) %>%
  filter(nchar(term) > 2 ) %>%
  arrange(-num)
incorrect <- incorrect %>%
  group_by(term) %>%
  summarise(num = sum(num)) %>%
  filter(nchar(term) > 2 ) %>%
  arrange(-num)

# go back to the qdap spell check to ensure that nothing had been missed
# run through the spell check sequence until you get back to this point
# now review the incorrect words dataframe and move anything mislabeled to the
# correct words dataframe (names, places, etc.)
correct2 <- incorrect %>%
  filter(str_detect(term, "hindu|arab|eden|singapore|holland|kenya"))
incorrect2 <- incorrect %>%
  filter(!str_detect(term, "hindu|arab|eden|singapore|holland|kenya"))

# if desired, make any final corrections
correct2$term <- gsub("etcetera", "etc", correct2$term)

# merge results
incorrect <- incorrect2
rm(incorrect2)
correct <- rbind(correct, correct2)
rm(correct2)
correct <- correct %>%
  group_by(term) %>%
  summarise(num = sum(num)) %>%
  filter(nchar(term) > 2 ) %>%
  arrange(-num)

# determine percentage of correct words overall
# this should be greater than 90%
nrow(correct)/(nrow(correct) + nrow(incorrect))

# stem the most frequent terms for analysis
correct$stem <- stemDocument(correct$term)
stem_freq <- correct %>%
  group_by(stem) %>%
  summarise(num = sum(num))%>%
  filter((num) > 50 )%>%
  arrange(-num)

# prepare a dataframe for sentiment analysis
sentiment <- analyzeSentiment(words_dtm)
sentiment <- analyzeSentiment(as.character(words_freqs$term))
summary(sentiment$SentimentLM)
table(convertToDirection(sentiment$SentimentLM))

# prepare data for cluster analysis
# compute distance between document vectors
# this takes a long time
words_d <- dist(words_m)

# run hierarchical clustering using Ward's method
words_groups <- hclust(words_d,
                       method = "ward.D")
