#FUNCTIONS
# Similarity function of lexRank 
simil_lexRank <- function(x,y){
  c <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(c)
}

#MAIN FRAME
lexRank <-function(){
  
  library(NLP)
  library(tm)
  library(openNLP)
  library(SnowballC)
  
  doc <- readLines("sentences.txt")
  doc <- as.String(doc)
  
  # Word and sentence token annotator 
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  
  doc_annotations <- annotate(doc, list(sent_ann, word_ann))
  bio_doc <- AnnotatedPlainTextDocument(doc, doc_annotations)
  
  # Sentence boundaries in text
  sentence.boundaries<-annotate(doc,list(sent_ann))
  sentences<-doc[sentence.boundaries]
  
  # Pre-processing
  corp <- Corpus(VectorSource(sents(bio_doc)))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removeWords, stopwords("english"))
  corp <- tm_map(corp, stemDocument)
  
  # Create list from corpus elements to use in document term matrix
  j<-length(sents(bio_doc))
  corp_list<-list()
  
  for (i in 1:j){
    corp_list[[length(corp_list)+1]]<-corp[[i]]
  }
  
  # Document term matrix   
  newsCorpus = Corpus(VectorSource(corp_list))
  dtm <- DocumentTermMatrix(newsCorpus, control = list(weighting = weightTf, normalize = TRUE))
  
  
  # similarity matrix 
  k <- nrow(dtm)
  p <- length(sents(bio_doc))
  m <- matrix(NA, nrow=k, ncol=k)
  cos <- as.data.frame(m)
  
  for (i in 1:k){
    for(j in 1:k){
      cos[i,j]= simil_lexRank(dtm[i,],dtm[j,])
    }
  }
  
  # pagerank algorithm 
  M = t(cos / rowSums(cos))
  n = nrow(M)
  
  U = matrix(data=rep(1/n, n^2), nrow=n, ncol=n) # U is the square matrix which all elements equal to 1\N
  beta=0.85
  A = beta*U+(1-beta)*M # A is the irreducible aperidic stochastic matrix, we find its eigenvalue to calculate pagerank
  e = eigen(A) 
  v <- e$vec[,1]

  highest<- order(v, decreasing = T)[1:10]
  a <- v[highest]
  
  # Create list from highest eigenvalue sentences
  summ_list<- list()
  m<-length(highest)
  
  for(i in 1:m){
    x<-highest[i] 
    summ_list[[length(summ_list)+1]]<- sentences[x]
  }
  
  d<-lapply(summ_list, cat,"", file="lexRank_summary.txt", append=TRUE)
}
