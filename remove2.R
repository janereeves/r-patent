# for text mining functions
library(NLP)
library(tm)
# for word stemming
library(SnowballC)
# for plot
library(Rgraphviz)

#library(corrplot)
# for drawing the wordcloud
library(wordcloud)
# for calculating the tf-idf
library(textir)
# flexible prodedures for clustering
library(fpc)
#read the csv file
library(lsa)
title <- read.csv2("title2.csv")
abstract <- read.csv2("abstract.csv")
#choose the column
titles <- title$title
abstracts <- abstract$x
#titleabstract <- cbind.data.frame(titles = titles,abstracts = abstracts)
# paste to one column
titleabstract <- data.frame(paste(titles,abstracts))
#titleabstract <- data.frame(lapply(titleabstract, as.character), stringsAsFactors=FALSE)
write.csv2(titleabstract,"titleabstract2.csv")

#interpret each element of a row as a document
#S3 class Corpus, PlainTextDocument

corpus <- Corpus(DataframeSource(titleabstract))
# transfer "-", "/" to spaces
corpus <- tm_map(corpus, content_transformer(function(x) chartr("-/","  ", x)))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes=FALSE)
#tolower is not S3 method in tm package, maybe useful as a S3 method. 
#content_transformer, functions which modify the content of an R object.
corpus <- tm_map(corpus, content_transformer(tolower))
sw <- read.csv2("stopwords.csv", head=FALSE, col.names="word")
sw <- sapply(sw,as.character)
corpus <- tm_map(corpus, removeWords, sw)
# another way: can use tm stopwords predefined in the package
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)
# define the stopwords
corpus <- tm_map(corpus, stemDocument)
# output a csv file
# ?this sapply needs to be understood
# convert Corpus to data frame
outputcorpus <- unlist(sapply(corpus,"[","content"))
outputcorpus2 <- data.frame(outputcorpus)
write.csv(outputcorpus2, "corpus.csv")

#Use the regular expression symbol \\W to match non-word characters, using + to indicate one or more in a row, 
#along with gregexpr to find all matches in a string. 
#Words are the number of word separators plus 1.
# count the number of terms per document
countterm <- sapply(gregexpr("\\W+", outputcorpus), length) + 1
# total number of terms
nterm <- sum(countterm)
# total number of documents
ndoc <- length(corpus)
# average number of terms per document
norm <- nterm/ndoc

# transform the corpus to document-term matrix
doctermmatrix <- DocumentTermMatrix(corpus)
#write.csv2(as.matrix(doctermmatrix),"doctermmatrix.csv")
#A term-document matrix where those terms from x are removed which have at least a sparse percentage of empty (i.e., terms occurring 0 times in a document) elements. I.e., the resulting matrix contains only terms with a sparse factor of less than sparse. 
#doctermmatrix2 <- removeSparseTerms(doctermmatrix, 0.95)


#termdocmatrix <- TermDocumentMatrix(corpus)
tfidf <- tfidf(doctermmatrix,FALSE)
#tfidf <- weightTfIdf(doctermmatrix,FALSE)
# tfidf to ntfidf
ntfidf <- tfidf*norm/countterm
# get the top 100 terms
matrix.ntfidf <- as.matrix(ntfidf)
sortterm <- sort(colSums(matrix.ntfidf), decreasing=TRUE)
write.csv2(sortterm[1:100],"sortterm.csv")
freqterm <-  names(sortterm[1:100])
write.csv2(freqterm,"freqterm.csv")
# select the key terms, stem the key terms manually and choose the matrix
# 100 is enough?
freqtermselect <- read.csv2("freqtermselect.csv")
freqtermselect <- freqtermselect$x
freqtermselect <- as.vector(freqtermselect)
freqterm.ntfidf <- matrix.ntfidf[,freqtermselect]
#cordoc <- cor(t(freqterm.ntfidf))
cosdoc <- cosine(t(freqterm.ntfidf))
costerm <- cosine(freqterm.ntfidf)
cosdoc[is.nan(cosdoc)] = 0
costerm[is.nan(costerm)] = 0
corrplot(costerm)
corrplot(cosdoc)
#cordoc2 <- cor(t(matrix.ntfidf))

clusterresult <- kmeans(ntfidf,3)
plotcluster(matrix.ntfidf, clusterresult$cluster)


#visualize the correlation of words
#do not plot correlations below corThreshold 
#in tm package, need Rgraphviz package
plot(doctermmatrix, terms = findFreqTerms(doctermmatrix, lowfreq = 30), corThreshold = 0.1, weighting=TRUE)
wordcloud(corpus,max.words=100,random.order=FALSE)

# system and method not included, lose some values,why?
#findAssocs(doctermmatrix,findFreqTerms(doctermmatrix,lowfreq = 30),0.3)
findAssocs(doctermmatrix,"method",0.1)
freqword <- findFreqTerms(doctermmatrix, lowfreq = 30)
n <- length(freqword)
assocterms <- list()
for (i in 1:n){
	assocterm<- findAssocs(doctermmatrix,freqword[i],0.3)
	assocterms[[i]] <- assocterm
	}


m <- as.matrix(doctermmatrix)
freqword <- findFreqTerms(doctermmatrix, lowfreq = 30)
freqdocmatrix <- m[,colnames(m)%in%freqword]
corterm <- cor(freqdocmatrix)
corrplot(corterm, method="circle")





















