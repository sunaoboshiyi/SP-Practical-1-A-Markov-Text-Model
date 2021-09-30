# Group 11: Tianyu Bao, ID:s2140490
#           Aobo Sun, ID:s2164956
#           xxx
            

setwd("C:\\Users\\ASUS\\Documents\\Stastical programming\\SP-Practical-1-A-Markov-Text-Model")
a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

a <- strsplit(a," ")

#Q4
split_punct <- function(x,y){
  i_pc <- grep(y,x,fixed = TRUE)
  x_new <- gsub(y,'',x,fixed = TRUE)
  xs <- rep('',length(i_pc)+length(x))
  ii <- i_pc+1:length(i_pc)
  xs[ii] <- rep(y,length(i_pc))
  xs[-ii] <- x_new
  return(xs)
}

split_punct(a,",")

#Q5
punc <- c(',','.',';','!',':','?')
for (y in punc) a <- split_punct(a,y)
a

#Q6
ta <- tolower(a) #
u <- unique(ta) #
ma <- match(ta,u) #
nm <- c(tabulate(ma)) # 
c_list<-sort(nm,decreasing = TRUE)
threshold<-c_list[1000]
b_list<- c()
p1<-0
for (i in nm){
  p1<- p1+1
  if (i >= threshold){
    b_list<-c(b_list,p1)
  }
}
b<-c()
for (i in b_list) {
  b<-c(b,u[i])
}
b
