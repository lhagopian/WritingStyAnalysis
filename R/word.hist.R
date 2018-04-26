#' @title word.hist
#' @description Given a list of words and frequencies, an integer n, and a boolean indicating whether to print the n values, plot a histogram of top n words.
#' @name word.hist
#' @param w.freq This should be the list of word frequencies (3 frequency functions).
#' @param n This indicates the top x most frequently occuring words that will be graphed
#' @param print.val This indicates whether a list of the top n words should be printed out
#' @examples 
#' word.hist(w.freq, 50, TRUE,)
#' @return a histogram of the top n frequently occuring words in a text (can also return a list of the top n frequently occuring words)
#' @export

#given a list of words and frequencies, an integer n, and a boolean indicating whether to print the n values, plot a histogram of top n words
word.hist<-function(w.freq, n=20, print.val=FALSE){
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
                main="Word Frequencies in Text") #plot title
}
