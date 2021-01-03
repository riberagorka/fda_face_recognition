## Function to load all the images and insert them in a matrix
image_loading = function(images){
  
  library(OpenImageR)
  img <- readImage(images[1])
  
  data <- matrix(0, nrow = nrow(as.data.frame(images)), ncol = prod(dim(img))) #Matrix of '0's with images dimensions
  
  
  counter <- 1
  for (image in images){
    im <- readImage(image)
    
    vector <- NULL
    red  <- im[,,1] #vector with red values
    green  <- im[,,2] #vector with green values
    blue  <- im[,,3] #vector with blue values
    
    
    vector <- cbind(vector, as.vector(red)) #add the red vector as a columns
    vector <- cbind(vector, as.vector(green)) #add the green vector as a columns
    vector <- cbind(vector, as.vector(blue)) #add the blue vector as a columns
    vector <- as.vector(vector) #Matrix to vector
    
    data[counter,] <- vector #add the vector in the 'n' row of the data matrix
    counter <- counter + 1
  }
  return(data)
}
