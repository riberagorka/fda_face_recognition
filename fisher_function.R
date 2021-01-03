fda <- function(data, labels){
  
  
  W <- data[,1:length(levels(as.factor(labels)))-1]
  data.labels = data.frame(W,
                           labels = as.integer(labels)) 
  mean  <- colMeans(W) # Total mean vector
  
  # mean of each class
  means <- sapply(levels(as.factor(data.labels$labels)), 
                  FUN = function(class) {
                    colMeans(W[data.labels$labels==class,
                               1:length(unique(data.labels$labels))-1])})
  means <- t(means)
  
  #Creation of Sw matrix with 0 values
  SW = matrix(0, ncol(W), ncol(W))
  
  #Computing the Sw
  #TRAIN DATA
  #for (i in 1:nrow(SW)){
  # for (j in which(as.character(data.labels$labels) == i)){
  #  SW = SW + t(as.matrix(W[j,] - means[i,])) %*% W[j,] - means[i,]
  #}        
  #}
  
  for (i in 1:length(unique(labels))){
    SW = SW + cov(data.labels[data.labels$labels==as.character(i),
                              1:ncol(W)])*
      (table(data.labels$labels)[1]-1)
  }
  
  #ALL DATA
  #for (i in 1:nrow(SW)){
  # for (j in which(as.character(data.labels$labels) == i)){
  #  SW = SW + as.matrix(W[j,] - means[i,]) %*% t(W[j,] - means[i,])
  #}        
  #}
  
  SB = matrix(0, ncol(W), ncol(W))
  for (i in levels(as.factor(data.labels$labels))){
    SB <- SB + (table(data.labels$labels)[i]) * (means[i,]-mean)%*%t(means[i,]-mean)
  }
  
  SW_1 <- solve(SW)
  eig <- eigen(SW_1%*%SB)
  eigenvalues <- eig$values
  P_ <- eig$vectors
  
  D <- eigenvalues / sum(eigenvalues)
  Cumulative.Var <- cumsum(eigenvalues) / sum(eigenvalues) #Cummulative variance of each principal axis
  threshold <- min(which(Cumulative.Var > 0.95)) # threshold in 95% in order to reduce the number of principal components
  P <- P_[,1:threshold]
  return(list(mean = mean, P = P, D = D))
}
