#' @title word.freq
#' @description Provides a list of all of the words in the .txt file.
#' @name word.freq
#' @param text This should be a text (.txt) file.
#' @examples 
#' word.freq(text)
#' @return a list of word frequencies
#' @export

#calculate word frequency for words in a given text (string), return list with words (Var1) and word frequencies (Freq)
word.freq<-function(text){
  txt<-gsub("[^[:alnum:][:space:]']",'', text) # remove punctuations
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  txt<-casefold(txt, upper = FALSE)
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  w.freq<-as.data.frame(table(unlist(w))) # converts w into a data frame with frequency of words
  library(dplyr)
  w.freq.s<-arrange(w.freq,desc(Freq)) #orders words
  return(w.freq.s)
}

