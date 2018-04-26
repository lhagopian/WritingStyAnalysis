#' @title word.avg
#' @description Provides an average word length of the words in the .txt file.
#' @name word.avg
#' @param text This should be a text (.txt) file.
#' @examples 
#' word.avg(text)
#' @return a double indicating the average word length used in the text
#' @export

#given a text (string), return average word length (double)
word.avg<-function(text){
  txt<-gsub("[^[:alnum:][:space:]']",'', text) # remove punctuations
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  words<-unlist(w)
  avg<-sum(nchar(words))/length(words)
  return(avg)
}
