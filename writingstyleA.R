text<-"This is a test test, test. test test sentence to? see see if! if, if this - parses and! and and. time cat line five five five five five nicely.  Let's see if it works.  Blah blah blah"
text<-readLines("Gra.Maga.txt", encoding="UTF-8")

#calculate word frequency for words in a given text (string), return list with words (Var1) and word frequencies (Freq)
word.freq<-function(text){
  txt<-gsub("[^[:alnum:][:space:]']",'', text) # remove punctuations
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  txt<-casefold(txt, upper = FALSE)
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  w.freq<-as.data.frame(table(unlist(w))) # converts w into a data frame with frequency of words
  w.freq.s<-arrange(w.freq,desc(Freq)) #orders words
  return(w.freq.s)
}

#calculate punctuation frequency for punctuations in a given text (string), return list with punctuations and punctuations frequencies
punct.freq<-function(text){
  txt<-gsub("[^[:punct:][:space:]']",'', text) # remove letters and numbers
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  w.freq<-as.data.frame(table(unlist(w))) # converts w into a data frame with frequency of words
  w.freq.s<-arrange(w.freq,desc(Freq)) #orders words
  return(w.freq.s)
}

#given a list of words and frequencies and a boolean indicating mode, return a list the approriate words and their frequencies
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

#given a text (string), return average word length (double)
word.avg<-function(text){
  txt<-gsub("[^[:alnum:][:space:]']",'', text) # remove punctuations
  txt<-gsub("[[:space:]]+",' ', txt) # replace double spaces with single space
  w<-strsplit(txt, " ") # sepeates text into individual values - seperated by " "
  words<-unlist(w)
  avg<-sum(nchar(words))/length(words)
  return(avg)
}

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



# store word.freq in a variable (w.freq)
w.freq<-word.freq(text) 
# using text calculate punct.freq - store punct.freq in a variable (p.freq)
p.freq<-punct.freq(text) 
# using w.freq run word.filter with stopwords removed (TRUE) - store in w.filt.T
w.filt.T<-word.filter(w.freq, include.sw=TRUE)
# using w.freq run word.filter with only stopwords (FALSE) - store in w.filt.F
w.filt.F<-word.filter(w.freq, include.sw=FALSE)
# using text caluclate average word length
word.avg(text)
# using text calcualte average sentence length
sent.avg(text)
# using stored variables (w.freq, w.filt.T, w.filt.F) create histogram
word.hist(w.freq, 50, title="Word Frequencies in Text (all)") 
word.hist(w.filt.T, 50, title="Word Frequencies in Text (no stopwords)")
word.hist(w.filt.F, 50, title="Word Frequencies in Text (only stopwords)")
# using stored variable (p.freq) create histogram
punct.hist(p.freq) 
