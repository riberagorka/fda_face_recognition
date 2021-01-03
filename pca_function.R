pca<- function(data){
  
  G <- scale(data, center = T, scale = F) # Data centration to mean = 0
  
  dispersion.matrix <- (G%*%t(G))/(nrow(G)-1) #Computing the dispersion matrix with the traspose
  
  eig <- eigen(dispersion.matrix) #Computing the eigenvalues and the eigenvector of the matrix
  lambda <- eig$values #eigenvalues
  P_ <- eig$vectors #eigenvectors
  
  
  D <- lambda / sum(lambda) #Proportion of variance explained by each principal axis
  Cumulative.Var <- cumsum(lambda) / sum(lambda) #Cummulative variance of each principal axis
  threshold <- min(which(Cumulative.Var > 0.95)) # threshold in 95% in order to reduce the number of principal components
  D_scale <- diag(D[1:threshold]^(-1/2)) / (sqrt(nrow(G)-1)) #we apply the threshold to the eigenvalues
  P <- t(G)%*%P_[,1:threshold]%*%D_scale #we apply the threshold to the eigenvectors
  mean = apply(G, 2, mean) #mean face
  
  return(list(mean = mean, P_ = P, D =D))
}
