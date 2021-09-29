# Group 11: Tianyu Bao, ID:s2140490
            xxx
            xxx
setwd("C:\\Users\\ASUS\\Documents\\Stastical programming\\SP-Practical-1-A-Markov-Text-Model")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

split_punct<- function(x,y){
  ii<- grep(y, x,fixed = FALSE)
  x[ii]<- strsplit(x[ii],"(\\s+)|(?=[[:punct:]])",perl = TRUE)
  x<- unlist(x)
return(x)
}

a<-split_punct(a,":")
a<-split_punct(a,",")
a<-split_punct(a,".")
a<-split_punct(a,";")
a<-split_punct(a,"?")
a<-split_punct(a,"!")
a
a1<-tolower(a)
U<-unique(a1)
match(a,a1)










