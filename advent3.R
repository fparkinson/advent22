####part 1
backpacks <- advent3input$backpack
for (i in seq(length(backpacks))) {
  comp1 <- as.data.frame(strsplit(substr(backpacks[i], 1, str_length(backpacks[i])/2), split = "")) 
  comp2 <- as.data.frame(strsplit(substr(backpacks[i], (str_length(backpacks[i])/2)+1, str_length(backpacks[i])), split = ""))
  colnames(comp1)[1] <- "items"
  colnames(comp2)[1] <- "items"
  duplicate <- intersect(comp1,comp2)
  duplicate <- duplicate$items

 points[i] <-  if(duplicate %in% letters) {
    match(duplicate,letters)
  } else {
    match(duplicate, LETTERS) + 26
  }
  
}
Reduce('+',points)
####part 2
new_points <-c(length(100))
for (i in seq(1,100)) {

  comp1 <- as.data.frame(strsplit(backpacks[i*3],split=""))
  comp2 <- as.data.frame(strsplit(backpacks[i*3-1],split=""))
  comp3 <- as.data.frame(strsplit(backpacks[i*3-2],split=""))
  
  colnames(comp1)[1] <- "items"
  colnames(comp2)[1] <- "items"
  colnames(comp3)[1] <- "items"
  
  duplicate <- Reduce(intersect,list(comp1,comp2,comp3))
  
  new_points[i] <-  if(duplicate %in% letters) {
    match(duplicate,letters)
  } else {
    match(duplicate, LETTERS) + 26
  }
  
}
Reduce('+',new_points)
