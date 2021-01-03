### MAIN PROGRAM

setwd("~/Documents/UC3M/2.Year/Statistical Learning/Lab2.Fisherfaces")
names <- list.files(pattern = ".jpg") #Assign to the pattern of the image to be loaded

#Training our model...
classifier(names)

#Classifying an image: 
image = names[1]
data = names
classifier_image(image, data)
