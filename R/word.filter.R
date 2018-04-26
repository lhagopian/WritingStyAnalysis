#' @title word.filter
#' @description Provides a list of all of the words in the .txt file.
#' @name word.filter
#' @param w.freq This should be the list of word frequencies (word.freq function).
#' @param include.sw This is a boolean indicating whether stopwords should be included: TRUE=stopwords removed, FALSE= stopwords only
#' @examples 
#' word.freq(w.freq, include.sw=TRUE)
#' @return a list of the approriate words and their frequencies
#' @export

#given a list of words and frequencies and a boolean indicating mode, return a list of the approriate words and their frequencies
# modes: TRUE=stopwords removed, FALSE= stopwords only
word.filter<-function(w.freq, include.sw=TRUE){
  stopword <- readLines("stopwords.txt", encoding="UTF-8") #imports stopwords from .txt
  stopwords<-strsplit(stopword, ", ") # sepeates text into individual values - seperated by ", "
  sw.df<-data.frame(stopwords) #convert stopwords into data frame
  names(sw.df)[1]<-paste("stopword") # change cloumn name to "stopword"
  wf.sw<-(w.freq$Var1 %in% sw.df$stopword) #print TRUE if stopword are found in Var1
  remove.sw.t<-subset(w.freq, !wf.sw) #creates a subset of words without stopwords
  remove.sw.f<-subset(w.freq, wf.sw) #creates a subset of only stopwords
  ifelse(include.sw == FALSE, return(remove.sw.f), return(remove.sw.t)) #if FALSE is indicated print only stopwords, else print all other words
}  
