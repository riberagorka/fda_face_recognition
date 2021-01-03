# knn function to predict the label of the image
knn = function(train.data, test.data, labels, distance, k, threshold = 10000){
  target = NULL
  #Computing the distance between rows
  dmatrix=dist(rbind(test.data,train.data),
               method = distance, 
               diag = TRUE, 
               upper = TRUE)
  dmatrix=as.matrix(dmatrix) #distances as matrix
  dmatrix=dmatrix[1,(nrow(test.data)+1):nrow(dmatrix)] #1st rows distances as a vector
  ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE) #Indexing the distances from lower to higher
  if (ordenados$x[1] > threshold){
    return(0)
  } else{
    labels_sel = labels[ordenados$ix[1:k]] #choosing the labels depending on the distance from 1 to 'k'
    uniqv = unique(labels_sel)
    target = uniqv[which.max(tabulate(match(labels_sel, uniqv)))] #Selecting the label that is the most repeated
    #If they are two values with the same frecuency, it returs the first
    return(target)
  }
}
