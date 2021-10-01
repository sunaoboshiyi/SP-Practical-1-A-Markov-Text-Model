# Group 11: Tianyu Bao, ID:s2140490
#           Aobo Sun, ID:s2164956
#           Honghao Ruan, ID:s2140412 
            

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

#Q5
punc <- c(',','.',';','!',':','?')
for (y in punc) a <- split_punct(a,y)

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

# Q7
d <- match(ta,b)
d2 <- cbind(d[1:(length(d)-1)],d[2:length(d)])
s <- rowSums(d2)
ii <- which(is.na(s))
d2 <- d2[-ii,1:2]
A <- matrix(0,length(b),length(b))
for (y in 1:dim(d2)[1]){
  i <- d2[y,1]
  j <- d2[y,2]
  A[i,j] <- A[i,j]+1
}
rs_A <- rowSums(A)
Rs_A <- matrix(rep(rs_A,length(rs_A)),length(rs_A),length(rs_A))
A_sd <- A/Rs_A

#Q8
L<-length(b)
s_w <- sample(1:L,1)
s<- c()
for (i in 1:50) {
  w <- sample(1:L,1,replace = TRUE,prob = A[s_w,])
  s <- c(s,b[w])
}

s<- cat(s,sep=" ")
