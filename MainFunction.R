args = commandArgs(trailingOnly = TRUE)


  fname<-args[1]
  rname<-args[2]


if (length(args)==2) {
  
  print("Initiating auto-detection algorithm")
  source('QCMplotfuntion.R')
  
}
if (length(args)>2){
  
  print("Initiating single plot algorithm")
  
  ranname1<- args[3]
  ranname2<- args[4]
  
  source('QCMplotfuntionsolo.R')
  
}
  if (length(args)<2){
    print("Invalid arguments, please try again!")
    
  }