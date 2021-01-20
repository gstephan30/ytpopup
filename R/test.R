
if(file.exists(".Renviron")){
  print(1)
  readRenviron(".Renviron")
} else {
  print(2)
  readRenviron("../.Renviron")
}
  
Sys.getenv()