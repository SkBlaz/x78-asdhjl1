sink("outputs/brutefoutputr.txt")
args = commandArgs(trailingOnly = TRUE)

fname<-args[1]
wsize<- args[2]
rsq<- as.numeric(args[3])
from<- args[4]
to<-args[5]

if (length(args)> 3){
  
  sc3<- read.delim2(paste("datafiles/",fname, sep=""), header=FALSE)
  datavector<-sc3$V3
  datavector<- datavector-min(datavector)
  #datavector<-sapply(datavector, function(x) ifelse(x>mean(datavector)*.96, x<- mean(datavector)*.96, x))
  
  window<- as.integer(wsize)
  
  
  print("Commencing brute force..")
  
  unlink("plotsb/testplotb.png", recursive = FALSE, force = TRUE)
  plotname<- paste("testplotb.png")
  png(paste("plotsb/", plotname, sep=""), width = 1000, height = 800)
  
  
  
  
  plotall<- function(mainset, gname){
    par(mar = c(6, 7, 3, 5)) 
    plot(seq(1,length(mainset),1), mainset, las=2, xaxt="n",
         xlab="", 
         ylab="",
         main=gname)
    
    grid(40,20,lwd=2) # grid only in y-direction
    mtext("Time of reaction (s)", side=1, line=4)
    mtext("Frequency (Hz)", side=2, line=5)
    
    axis(side=1,at=seq(1,length(mainset),length(mainset)/20),lwd=4,lwd.ticks=2,las=2,col.ticks="black")
    lines(mainset, col="green")
  }
  
  plotall(datavector,"main")
  
  for (mark in from:to){
    
    sub<- datavector[mark:(mark+window)]
    subx<- seq(mark,(mark+window),1)
    curlm<- lm(sub ~ subx)
    #print(summary(curlm)$r.squared)
    #print("Hit found!")
    if(curlm$coefficients[2]<0 && summary(curlm)$r.squared > rsq){
      print("############################################################################")
      print(paste("Startpoint: ", mark, " endpoint: ", (mark+window)))
      print (summary(curlm))
      abline(curlm, col="red")
      
    }
    
  }
  
  dev.off()
  
  
  
}else{
  
  sc3<- read.delim2(paste("datafiles/",fname, sep=""), header=FALSE)
  datavector<-sc3$V3
  datavector<- datavector-min(datavector)
  #datavector<-sapply(datavector, function(x) ifelse(x>mean(datavector)*.96, x<- mean(datavector)*.96, x))
  
  window<- as.integer(wsize)
  
  
  print("Commencing brute force..")
  
  unlink("plotsb/testplotb.png", recursive = FALSE, force = TRUE)
  plotname<- paste("testplotb.png")
  png(paste("plotsb/", plotname, sep=""), width = 1000, height = 800)
  
  
  
  
  plotall<- function(mainset, gname){
    par(mar = c(6, 7, 3, 5)) 
    plot(seq(1,length(mainset),1), mainset, las=2, xaxt="n",
         xlab="", 
         ylab="",
         main=gname)
    
    grid(40,20,lwd=2) # grid only in y-direction
    mtext("Time of reaction (s)", side=1, line=4)
    mtext("Frequency (Hz)", side=2, line=5)
    
    axis(side=1,at=seq(1,length(mainset),length(mainset)/20),lwd=4,lwd.ticks=2,las=2,col.ticks="black")
    lines(mainset, col="green")
  }
  
  plotall(datavector,"main")
  
  for (mark in 1:(length(datavector)-window)){
    
    sub<- datavector[mark:(mark+window)]
    subx<- seq(mark,(mark+window),1)
    curlm<- lm(sub ~ subx)
    #print(summary(curlm)$r.squared)
    #print("Hit found!")
    if(curlm$coefficients[2]<0 && summary(curlm)$r.squared > rsq){
      print("############################################################################")
      print(paste("Startpoint: ", mark, " endpoint: ", (mark+window)))
      print (summary(curlm))
      abline(curlm, col="red")
      
    }
    
  }
  
  dev.off()
  
}


sink()
