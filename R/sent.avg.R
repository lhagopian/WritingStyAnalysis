#' @title sent.avg
#' @description Provides an average sentence length of the words in the .txt file.
#' @name sent.avg
#' @param text This should be a text (.txt) file.
#' @examples 
#' sent.avg(text)
#' @return a double indicating the average sentence length used in the text
#' @export

#given a text (string), return average sentence length (double)
sent.avg<-function(text){
  s<-strsplit(text, split="[.!?]") # sepeates text into individual sentences - seperated by ".!?"
  sentences<-unlist(s)
  split.sentences<-gsub("[^[:alnum:][:space:]'.!?]",'', sentences) # remove punctuations
  split.sentences<-gsub("[[:space:]]+",' ', split.sentences) # replace double spaces with single space
  ss.words<-trimws(split.sentences, which="both") #removes empty spaces before and after words
  ss.words<-strsplit(ss.words, " ") #seperates text into indivuduail values
  avg<-sum(lengths(ss.words))/length(ss.words)
  return(avg) 
}
