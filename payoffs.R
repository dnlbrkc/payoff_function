payoffs <- function(N,K){
require(gtools)
#List all permutations of binary digits of length N
LS <- permutations(2,N,v=c(0,1),repeats.allowed=TRUE) 
if (K==0){
  depends <- as.vector(1:N)
  values <- replicate(N,round(runif(2,0,1),1))
  fitness <- values
  fitness.score <- vector()
  for (i in 1:nrow(LS)){
  rows<-as.numeric(LS[i,])+1
  values<-sapply(1:N, function(y) fitness[rows[y],y] )
  fitness.score[i]<-mean(values)
  }
} else {
  depends <- rbind(1:N,replicate(N,sample(c(1:N),K,replace=F)))
  combinations <- permutations(2,K+1,v=c(0,1),repeats.allowed=TRUE)
  values <- replicate(N,round(runif(nrow(combinations),0,1),1))
  fitness <- cbind(combinations,values)
  indx1<-do.call(`paste0`,as.data.frame(fitness[,c(1:(K+1))]))
  indx2<-sapply(1:N, function(y) do.call(`paste0`,as.data.frame(LS[,depends[,y]])))
  fitness.score <- sapply(1:nrow(LS), function(o) mean(diag(sapply(indx2[o,], function(x) fitness[which(indx1 %in% x),(K+2):ncol(fitness)]))))
}
#Normalize and scale payoffs
fitness.score <- (fitness.score/max(fitness.score))^8
#Store solutions and corresponding payoffs
#landscape <- cbind(LS,fitness.score)
return(as.vector(fitness.score))
}