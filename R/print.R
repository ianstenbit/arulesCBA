print.CBA <- function(x, ...){
  
  print("CBA Object\n")

  print("Number of rules:\n")
  print(length(x[[1]]))

  print("First rules:\n")
  print(inspect(head(x[[1]])))

  print("Default class:\n")
  print(x[[2]])
  
}