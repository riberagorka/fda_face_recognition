#Authors: Gorka Ribera & Pablo Santos

classifier_image = function(image, data){
  
  labels = rep(1:25,each=6)
  data = image_loading(data) 
  #pca
  data.pca = pca(data) 
  mean = data.pca$media
  eigenfaces = data.pca$P
  eigenvalues = data.pca$D
  #projection
  eigen_data = data %*% eigenfaces
  #fda
  data.fda = fda(eigen_data, labels)
  P = data.fda$P
  
  fisher_data = as.matrix(eigen_data[,1:nrow(P)]) %*% P
  image = image_loading(image)
  
  image_scale = scale(image, center= T, scale=F)
  eigen_image = data.matrix(image) %*% eigenfaces
  eigen_image = eigen_image[,1:nrow(P)]
  fisher_image = t(data.matrix(eigen_image)) %*% P
  
  target = knn(fisher_data, fisher_image, labels, "euclidean", 3,1000)
  
  return(target)
}
setwd("~/Documents/UC3M/2.Year/Statistical Learning/Lab2.Fisherfaces")
names <- list.files(pattern = ".jpg") #Assign to the pattern of the image to be loaded

image = names[1]
data = names[1:144]
classifier_image(image, data)
