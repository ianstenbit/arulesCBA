print.CBA <- function(x, ...){
  
  print("CBA Object")

  print("Number of rules:")
  print(length(x[[1]]))

  print("First rules:")
  inspect(head(x[[1]]))

  print("Default class:")
  print(x[[2]])
  
}