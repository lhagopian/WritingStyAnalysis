#' @title punct.freq
#' @description Provides a list of all of the punctuation in the .txt file.
#' @name punct.freq
#' @param text This should be a text (.txt) file.
#' @examples 
#' punct.freq(text)
#' @return a list of punctuation frequencies
#' @export

#calculate punctuation frequency for punctuations in a given text (string), return list with punctuations and punctuations frequencies
punct.freq<-function(text){
  txt<-gsub("[^[:punct:][:space:]']",'', text) # remove letters and numbers
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  w.freq<-as.data.frame(table(unlist(w))) # converts w into a data frame with frequency of words
  library(dplyr)
  w.freq.s<-arrange(w.freq,desc(Freq)) #orders words
  return(w.freq.s)
}
