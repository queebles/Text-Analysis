### Text Mining and Analysis
### Alister Hardy Religious Experience Research Centre Archive
### in part adapted from Gaston Sanchez in his work with sentiment analysis 
### of Twitter data.

## get data ----

hardy <- read.csv("~/alisterhardyarchive.csv")
hardy <- hardy[1:11]

## create corpus ----

library(tm)

subjects <- Corpus(VectorSource(hardy$Subjects))

clean_corpus <- function(corpus) {
        # convert to lower case
        corpus <- tm_map(corpus, 
                         content_transformer(tolower))
        # convert hypens to spaces
        corpus <- tm_map(corpus, 
                         content_transformer(gsub),
                         pattern = "-", 
                         replace = " ")
        # remove words between < >
        corpus <- tm_map(corpus, 
                         content_transformer(gsub), 
                         pattern = "<[^[:space:]]*", 
                         replace = "")  
        # remove numbers
        corpus <- tm_map(corpus, 
                         removeNumbers)
        # remove anything other than English letters or space
        corpus <- tm_map(corpus, 
                         content_transformer(gsub), 
                         pattern = "[^[:alpha:][:space:]]*",
                         replace = "")
        # remove stopwords
        corpus <- tm_map(corpus, 
                         removeWords, 
                         c("the", "and", stopwords("english")))
        # remove single letter words
        corpus <- tm_map(corpus, 
                         content_transformer(gsub),
                         pattern = " . ", 
                         replace = " ") 
        # remove extra whitespace
        corpus <- tm_map(corpus, 
                         stripWhitespace)
        # reduce words to basic forms
        corpus <- tm_map(corpus, 
                         stemDocument)
        return(corpus)
}

subjects <- clean_corpus(subjects)

# document-term matrix (DTM) - lists all occurrences of words in corpus
subjects_dtm <- TermDocumentMatrix(subjects)
rm(subjects)

# remove less frequent words so that the sparsity is less than 0.99
subjects_dtm = removeSparseTerms(subjects_dtm, 0.999)
# remove less frequent words so that the sparsity is less than 0.95 (if desired)
# subjects_dtm = removeSparseTerms(subjects_dtm, 0.98)

# convert to matrix
subjects_m <- as.matrix(subjects_dtm)

## term frequency ----

subject_freq <- data.frame(
        term = names(rowSums(as.matrix(subjects_dtm))), 
        num = rowSums(as.matrix(subjects_dtm))
        )

subject_freq<- subject_freq[order(-subject_freq$num),] 

 # identify most frequent terms
 subject_top50 <- findFreqTerms(subjects_dtm, 50)

 ## word cloud ----

 library(wordcloud)

 wordcloud(subject_freq$term, subject_freq$num,
           max.words = 50,
           colors=brewer.pal(3, "Dark2"),
           scale = c(4, 0.2))

 ## bar plots ----

 library(viridis)

 # top 10 descending frequency
 barplot(subject_freq[1:10, ]$num,
         las = 2,
         names.arg = subject_freq[1:10, ]$term,
         col = viridis(10),
         cex.axis = 0.8,
         main ="Most frequent words",
         ylab = "Word frequencies")

 library(ggplot2)

 # top 30 descending alpha
 ggplot(subject_freq[1:30,],
        aes(x = term,
            y = num,
            fill = term)) +
         geom_bar(stat = "identity") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 90,
                                          hjust = 1)) +
         ylab("Word frequencies") +
         xlab("Most frequent words") +
         guides(fill = FALSE)

 ## correlations ----

 # find correlations with term "death"
 findAssocs(subjects_dtm, terms = "death",
            corlimit = 0.2)

 # find correlations with term "spirit"
 findAssocs(subjects_dtm, terms = "spirit",
            corlimit = 0.2)

 # find correlations with term "demon"
 findAssocs(subjects_dtm, terms = "demon",
            corlimit = 0.1)

## sentiment analysis ----

devtools::install_github("cran/Rstem")
devtools::install_github("queebles/sentiment")

library(sentiment)

# classify emotion
class_emo = classify_emotion(subject_freq$term, 
                             algorithm = "bayes", 
                             prior = 1.0)
emotion = class_emo[, 7]

# classify polarity
class_pol = classify_polarity(subject_freq$term, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]
sent_df = data.frame(text = , 
                     emotion = emotion,
                     polarity = polarity, 
                     stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, 
                                   levels=names(sort(table(emotion), 
                                                     decreasing = TRUE))))

# graph emotions and polarities
library(ggplot2)
ggplot(data = na.omit(subset(sent_df)), 
       aes(x = emotion)) +
        geom_bar(aes(y = ..count.., 
                     fill = emotion)) +
        labs(x = "emotion categories", 
             y = "")

ggplot(sent_df, aes(x = polarity)) +
        geom_bar(aes(y = ..count.., 
                     fill = polarity), color = "black") +
        scale_fill_brewer(palette = "RdGy") +
        labs(x = "polarity categories", 
             y = "")

# separate text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
some_txt <- subject_freq$term
for (i in 1:nemo)
{
        tmp = some_txt[emotion == emos[i]]
        emo.docs[i] = paste(tmp, collapse =" ")
}

# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(2.5,0.5), 
                 random.order = FALSE, 
                 title.size = 1)

## clustering ----

# compute distance between document vectors
subjects_d <- dist(subjects_m)

# hierarchical clustering - Ward's method
subjects_groups <- hclust(subjects_d,
                          method = "ward.D")

# plot dendogram with hang so that labels fall below tree
par(cex = 0.5)
plot(subjects_groups,
     hang = -1,
     xlab = "Subjects/Terms", 
     main = "Cluster Dendogram", 
     sub = "Alister Hardy Religious Experience Research Centre Archive")

# cut into 5 subtrees (optional)
rect.hclust(subjects_groups, 5)

# kmeans - determine the optimum number of clusters (elbow method) by looking for "elbow" in plot of summed intra-cluster distances (withinss) as a function of k
elbow <- 2:29
for (i in 2:29) elbow[i] <- sum(kmeans(subjects_d,
                                       centers = i,
                                       nstart = 25)$withinss)
plot(2:29, 
     elbow[2:29],
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# kmeans algorithm, 5 clusters (determined to be optimal), 100 starting configurations
subjects_kfit <- kmeans(subjects_d, 
                        5,
                        nstart = 100)

# plot cluster

library(cluster)

clusplot(as.matrix(subjects_d),
         subjects_kfit$cluster,
         color = T,
         shade = T,
         labels = 2,
         lines = 0)

devtools::install_github("cran/Rstem")
devtools::install_github("cran/sentiment")

library(sentiment)

class_emo = classify_emotion(subject_freq$term, 
                             algorithm = "bayes", 
                             prior = 1.0)
emotion = class_emo[, 7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(subject_freq$term, 
                              algorithm = "bayes")
polarity = class_pol[,4]


sent_df = data.frame(text = subject_freq$term, 
                     emotion = emotion,
                     polarity = polarity, 
                     stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, 
                                   levels=names(sort(table(emotion), 
                                                     decreasing = TRUE))))

# graph the emotions and polarities.

ggplot(sent_df, 
       aes(x = emotion)) +
        geom_bar(aes(y = ..count.., 
                     fill = emotion)) +
        scale_fill_brewer(palette = "Dark2") +
        labs(x = "emotion categories", 
             y = "")
