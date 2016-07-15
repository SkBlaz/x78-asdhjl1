### Main algorithm!

sink("CustomAlgorithmsolo.txt")

currentmax<-vector()
currentlocation<-vector()
currentcoef<-vector()
fitvalues<-vector()
linexdata<-vector()

startrange<- as.numeric(ranname1)
endrange<- as.numeric(ranname2)

sc3<- read.delim2(paste("datafiles/",fname, sep=""), header=FALSE)
rangeparam<-as.numeric(rname)



############################   CODE   #########################
datavector<-sc3$V3
datavector<- datavector-min(datavector)
datavector<-sapply(datavector, function(x) ifelse(x>mean(datavector)*.96, x<- mean(datavector)*.96, x))



#Plot algorithm


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


findextrema<- function(datavector1, gname, rprm){
  plotall(datavector1,paste(gname))
  xval<-c(0)
  remove<-c(0)
  for (i in 1:length(datavector1)){
    
    if (i>15 && i < length(datavector1)-15){
      
      #DevMeasure<-(mean(datavector1)+sd(datavector1))/sd(datavector1)
      
      Ncrit<-sd(datavector)/sd(datavector1)
      low<-0.95
      high<-1.05
      
      ifelse(Ncrit<1,low<-low-Ncrit, low<-low)
      ifelse(Ncrit<1,high<-high+Ncrit, high<-high)
      
      
      ifelse(Ncrit>4,low<-0.995, low<-low)
      ifelse(Ncrit>4,high<-1.02, high<-high)
      
      
      
      if (datavector1[i]>mean(datavector1)*low && datavector1[i]<mean(datavector1)*high){
        print("candidate point found!")
        xval<-i
        fitvalues <- c(fitvalues, i)
        print(paste("Current maximum derivative found at time: ", i))
        for (i in 1:length(fitvalues)){
          linexdata[i] <- fitvalues[i]
        }
      }
    } 
  }
  
  linexdata<-as.numeric(linexdata)
  linexdata<-linexdata[linexdata<100]
  #print(linexdata)
  #print(mean(linexdata))
  #   for (g in linexdata){if (g< mean(linexdata)-2 || g> linexdata[3]+20){remove<-c(remove,g)}}
  #   linexdata<-setdiff(linexdata, remove)
  #   
  #print(linexdata)
  print("Region range>>")
  print((round(mean(linexdata), 0)-3): (round(mean(linexdata), 0)+3))
  linexdata<-(round(mean(linexdata), 0)-rprm): (round(mean(linexdata), 0)+rprm)
  from<-min(linexdata)
  to<-max(linexdata)
  lineydata<-as.numeric(datavector1[linexdata])
  print(lineydata)
  lfit<- lm(lineydata ~ linexdata)
  print(lfit)
  abline(lfit, col="orange")
  points(linexdata,lineydata, pch=20, cex=2, col="lightblue")
  coefficient<-round(lfit$coefficients[2],2)
  return (as.numeric(coefficient))
}



### Plot of detected lines..
unlink("plots2/testplot.png", recursive = FALSE, force = TRUE)
plotname<- paste("testplot2.png")
png(paste("plots2/", plotname, sep=""), width = 1000, height = 800)
par(mfrow=c(1,1))
sub<-datavector[startrange:endrange]
findextrema(sub, paste("from: ", startrange, "to", endrange), rangeparam)
dev.off()
sink()