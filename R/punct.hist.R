#' @title punct.hist
#' @description Given a list of punctuations and frequencies and an integer n, plot a histogram of top n punctuations.
#' @name punct.hist
#' @param p.freq This should be the list of punctuations frequencies (frequency function).
#' @param n This indicates the top x most frequently occuring punctuations that will be graphed
#' @param print.val This indicates whether a list of the top n punctuations should be printed out
#' @examples 
#' punct.hist(p.freq, 50, TRUE,)
#' @return a histogram of the top n frequently occuring punctuations in a text (can also return a list of the top n frequently occuring words)
#' @export

#given a list of punctuations and frequencies and an integer n, plot a histogram of top n punctuations
punct.hist<-function(p.freq, n=20, print.val=FALSE){
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
                main="Punctuation Frequencies in Text") #plot title
}