text<-readLines("Gra.Maga.txt", encoding="UTF-8")

#' Requred packages:
install.packages("dplyr")

#' @title word.freq
#' @description Provides a list of all of the words in the .txt file.
#' @name word.freq
#' @param text This should be a text (.txt) file.
#' @examples 
#' word.freq(text)
#' @return a list of word frequencies

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


#' @title punct.freq
#' @description Provides a list of all of the punctuation in the .txt file.
#' @name punct.freq
#' @param text This should be a text (.txt) file.
#' @examples 
#' punct.freq(text)
#' @return a list of punctuation frequencies

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


#' @title word.filter
#' @description Provides a list of all of the words in the .txt file.
#' @name word.filter
#' @param w.freq This should be the list of word frequencies (word.freq function).
#' @param include.sw This is a boolean indicating whether stopwords should be included: TRUE=stopwords removed, FALSE= stopwords only
#' @examples 
#' word.freq(w.freq, include.sw=TRUE)
#' @return a list of the approriate words and their frequencies

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


#' @title word.avg
#' @description Provides an average word length of the words in the .txt file.
#' @name word.avg
#' @param text This should be a text (.txt) file.
#' @examples 
#' word.avg(text)
#' @return a double indicating the average word length used in the text

#given a text (string), return average word length (double)
word.avg<-function(text){
  txt<-gsub("[^[:alnum:][:space:]']",'', text) # remove punctuations
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  words<-unlist(w)
  avg<-sum(nchar(words))/length(words)
  return(avg)
}


#' @title sent.avg
#' @description Provides an average sentence length of the words in the .txt file.
#' @name sent.avg
#' @param text This should be a text (.txt) file.
#' @examples 
#' sent.avg(text)
#' @return a double indicating the average sentence length used in the text

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


#' @title word.hist
#' @description Given a list of words and frequencies, an integer n, and a boolean indicating whether to print the n values, plot a histogram of top n words.
#' @name word.hist
#' @param w.freq This should be the list of word frequencies (3 frequency functions).
#' @param n This indicates the top x most frequently occuring words that will be graphed
#' @param print.val This indicates whether a list of the top n words should be printed out
#' @examples 
#' word.hist(w.freq, 50, TRUE,)
#' @return a histogram of the top n frequently occuring words in a text (can also return a list of the top n frequently occuring words)

#given a list of words and frequencies, an integer n, and a boolean indicating whether to print the n values, plot a histogram of top n words
word.hist<-function(w.freq, n=20, print.val=FALSE, title="Word Frequencies in Text"){
  if (n>length(w.freq$Var1)) n<-length(w.freq$Var1) 
  words<-w.freq[1:n,] #select subset of n values of w.freq
  if (print.val) print(words) #specifies print.val
  num.steps<-10 # number of steps for y-axis
  curr.max<-max(words$Freq)/num.steps #creates current max value for y-axis
  
  div.fac<-0 #dividing factor
  while (curr.max>=10) { # as long as curr.max is greater than or equal to 10
    curr.max<-curr.max/10  #divide curr.max by 10
    div.fac<-div.fac+1  #and add 1 to div.fac
  }
  curr.max<-round(curr.max, 0.5) #round curr.max up to nearest 0.5 value
  curr.max<-curr.max*10^div.fac # multiply curr.max by 10 to the power of div.fac
  curr.max.fin<-curr.max*num.steps+curr.max
  plot<-barplot(words$Freq,   #create a barplot using word requency
                names.arg=words$Var1, #create the x values using words Val1
                cex.names=0.75, #word size
                las=2, #word rottation on x-axis
                yaxp=c(0,curr.max.fin,5), #specifies max y-axis value
                ylim=c(0,curr.max.fin), #specifies max y-axis value
                xlab="Words", ylab="Frequencies", #x and y axis lables
                main=title) #plot title
}


#' @title punct.hist
#' @description Given a list of punctuations and frequencies and an integer n, plot a histogram of top n punctuations.
#' @name punct.hist
#' @param w.freq This should be the list of punctuations frequencies (frequency function).
#' @param n This indicates the top x most frequently occuring punctuations that will be graphed
#' @param print.val This indicates whether a list of the top n punctuations should be printed out
#' @examples 
#' punct.hist(p.freq, 50, TRUE,)
#' @return a histogram of the top n frequently occuring punctuations in a text (can also return a list of the top n frequently occuring words)

#given a list of punctuations and frequencies and an integer n, plot a histogram of top n punctuations
punct.hist<-function(p.freq, n=20, print.val=FALSE, title="Punctuation Frequencies in Text"){
  if (n>length(p.freq$Var1)) n<-length(p.freq$Var1) 
  punct<-p.freq[1:n,] #select subset of n values of p.freq
  if (print.val) print(punct) #specifies print.val
  num.steps<-10 # number of steps for y-axis
  curr.max<-max(punct$Freq)/num.steps #creates current max value for y-axis
  
  div.fac<-0 #dividing factor
  while (curr.max>=10) { # as long as curr.max is greater than or equal to 10
    curr.max<-curr.max/10  #divide curr.max by 10
    div.fac<-div.fac+1  #and add 1 to div.fac
  }
  curr.max<-round(curr.max, 0.5) #round curr.max up to nearest 0.5 value
  curr.max<-curr.max*10^div.fac # multiply curr.max by 10 to the power of div.fac
  curr.max.fin<-curr.max*num.steps+curr.max
  plot<-barplot(punct$Freq,   #create a barplot using punctuations requency
                names.arg=punct$Var1, #create the x values using punctuations Val1
                cex.names=1.4, #punctuations size
                las=1, #word rottation on x-axis
                yaxp=c(0,curr.max.fin,5), #specifies max y-axis value
                ylim=c(0,curr.max.fin), #specifies max y-axis value
                xlab="Punctuations", ylab="Frequencies", #x and y axis lables
                main=title) #plot title
}
