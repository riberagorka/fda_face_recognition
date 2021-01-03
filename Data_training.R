

classifier <- function(names){
  data = image_loading(names) 
  
  n_folds = 6 #number of folds
  folds = sample(rep(1:n_folds, length.out = dim(data)[1])) #indexing the folds from 1 to 5 randomly
  labels = rep(1:25, each = 6) #labels, we repeat each number 6 times
  
  data = data.frame(labels = labels,
                    data) # assigning the indexes to the data
  
  distance = c("euclidean", "manhattan", "minkowski", "canberra") #types of distances
  k = c(3,5,7,9) #number of k
  accs = 0 #variable definition
  parameters = as.data.frame(expand.grid(distance = distance, k = k)) #distance and k parameters as data.frame
  
  for (each in 1:(n_folds)){
    parameters = cbind(parameters, accs)
  } #we add 'acc' columns depending on the number of folds we have created
  ccrm = 0
  parameters = cbind(parameters, ccrm) # we add 'ccrm' as last column in order to compute the mean of accuracies
  
  for (i in 1:n_folds){
    train.data = data[folds!=i,-1] #80% of the original data without labels
    test.data = data[folds==i,-1] #20% of the original data without labels
    
    train.labels = factor(data[folds!=i, 1]) #train.data labels
    test.labels = factor(data[folds==i,1]) #test.data labels
    
    pca.data <- pca(train.data)
    P.pca = pca.data$P #eigenvectors
    mean.pca = pca.data$mean # mean face vector
    
    #projection of the train data to the new space created
    pca.data1= data.frame(t(t(P.pca) %*% t(train.data))) 
    
    fisher.data <- fda(pca.data1, train.labels) #función de fisher
    
    P = fisher.data$P #eigenvectors
    mean.fda = fisher.data$mean # mean face vector fiher
    variance_explained = fisher.data$D #varianción explicada por cada eigenvector (eigenvalue)
    
    #proyectar los datos originales en nuestra nueva dimension de fisher
    projection = data.frame(labels = train.labels,
                            t(t(P) %*% t(pca.data1[,1:nrow(P)])))
    
    test_scaled = scale(test.data, center = mean.pca, scale = F)
    
    
    pca.test.data = data.frame(as.matrix(test_scaled) %*% P.pca)
    pca.test.data = pca.test.data[,1:length(mean.fda)]
    #pca.test.data_scaled = scale(pca.test.data, center = mean.fda, scale = F)
    test.projection = data.frame(labels = test.labels,
                                 as.matrix(pca.test.data) %*% P)
    
    
    #accuracy based on the predictions that has done the knn function
    #the loop changes the parameters in each iteration
    for (h in 1:nrow(parameters)){
      dist = NULL
      acc = 0
      #for loop that in each iteration changes the row of the projection
      for (j in 1:nrow(test.data)){
        #knn function application, we add the train data, one row of the test data, a distance and a k
        dist[j] = knn(projection, test.projection[j,], projection$labels, parameters[h,1], parameters[h,2], threshold = 5000)
      }
      #the loop computes the accuracy of each parameters selection
      for (j in 1:nrow(test.data)){
        if (dist[j] == test.labels[j]){
          acc = acc + 1
        }
      }
      acc = acc/nrow(test.data)
      parameters[h, i+2] = round(acc,2)
    }
    #computes the mean accuracy for each parameter
    for (j in 1:nrow(parameters)){
      mean_acc = 0
      for (i in 1:(ncol(parameters)-3)){
        mean_acc[i] = parameters[j,i+2]
      }
      parameters$ccrm[j] = round(sum(mean_acc)/length(mean_acc),2)
    }
    
  }
  return(parameters)
}


### PROGRAM

setwd("~/Documents/UC3M/2.Year/Statistical Learning/Lab2.Fisherfaces")
names <- list.files(pattern = ".jpg") #Assign to the pattern of the image to be loaded

classifier(names)


parameters







