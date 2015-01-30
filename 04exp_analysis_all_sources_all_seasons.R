#calculate the lambda for each source (S,C,P) regardless of seasons (4seasons), since control X is not real pathogen
Sum1_new<-0
select.mean_week_total_new1<-NULL
hislook_new<-NULL
for(Source in c('Cattle','Poultry','Swine')){
  pdf(paste(root.path,"./newmeanofcon_EXDdecaymodelplotID_", Source, "_", method, ".pdf", sep=""))
  par(mfrow=c(2,4))
  for (method in unique(UnewRunoff5$method)){
    indxnew2 <- which(Source==UnewRunoff5$Source & method == UnewRunoff5$method)
    if(length(indxnew2) > 0){
      select.data.new2 <- UnewRunoff5[indxnew2,]
      print(sprintf("%s--%s--%d", Source, method,length(unique(select.data.new2$week))))
      select.mean_week_new1 <- NULL
      for (week in unique(select.data.new2$week)){
        for (PlotID in unique(select.data.new2$PlotID)){
          indxnew31<-which(select.data.new2$week == week & select.data.new2$PlotID == PlotID)
          #Sum1 <- Sum1 + length(indx3)
          if(length(indxnew31) > 0) {
            #detectionfreq1<- paste(sum(as.numeric(select.data[indx3,9])),Sum1, sep="/")
            select.mean_week_new1 <- rbind(select.mean_week_new1, cbind(mean(as.numeric(select.data.new2[indxnew31,]$proxy)), week,PlotID,method,Source))  
            select.mean_week_new1<-unique(select.mean_week_new1)
          }
        }
      }
      if (!is.null(select.mean_week_new1)){
        #nexp.estimate<- my.fitdist.exp.estimate(as.numeric(select.mean_week[,1]))
        #print(nexp.estimate)
        #nexp.estimate1<-format(nexp.estimate,digits=3)
        #nexp.sd<-my.fitdist.exp.sd(as.numeric(select.mean_week[,1]))
        #print(nexp.sd)
        #nexp.sd<-format(nexp.sd,digits=3)
        #plot(select.mean_week[,1]~select.mean_week[,2],type="p",xlab="week", ylab="concentration",
          #main=paste("concentration vs week_",Source,"\n",method,"\n","lambda=",nexp.estimate1,",","sd=",nexp.sd), sub=season)
        #curve(1/nexp.estimate*exp(-x/nexp.estimate),col = "blue",add = TRUE)
        
        lambda<- my.glm.exp.lambda(as.numeric(select.mean_week_new1[,2]),as.numeric(select.mean_week_new1[,1]))
        lambda.intercept<- my.glm.exp.lambda.intercept(as.numeric(select.mean_week_new1[,2]),as.numeric(select.mean_week_new1[,1]))
        lambda1<-format(as.numeric(log(2)/-lambda),digits=3)
        print(lambda1)
        #temp.look <- cbind(select.mean_week1[,1],select.mean_week1[,2])
        hislook_new<- rbind(hislook_new,cbind(as.numeric(lambda1), lambda,lambda.intercept,method,Source))
        #print(paste(dim(temp.look)[[1]],dim(temp.look)[[2]],season,method))
        plot(jitter(as.numeric(select.mean_week_new1[,1]))~select.mean_week_new1[,2],type="p",xlab="week", ylab="concentration",
          main=paste(Source,"\n",method,"\n","half-life=",as.numeric(lambda1)*7, "days"))
          curve(exp(lambda.intercept)*exp(x*lambda),add=TRUE,col="blue")
          #curve(exp(lambda.intercept)*exp(x*lambda1),col = "blue",add = TRUE)
          #lines(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),lty=3)
          #select.mean_week_total<-rbind(select.mean_week_total,select.mean_week1) 
      } else {
        print("There is no data!!!")
      }
    }
  }
  dev.off()
}


hislook_new<-as.data.frame(hislook_new,stringsAsFactors=FALSE)
names(hislook_new)[1]<-"half-life"







#scatter plot for mean of the concentration at each plotID for each week
#Sum2<-0
select.mean_week_total_new<-NULL
#hislook<-NULL

select.data111<-NULL
 for(Source in c('Cattle','Poultry','Swine'))
{
indx111<- grep(Source,UnewRunoff5[,3], fixed=TRUE,ignore.case = FALSE)
select.data111<-rbind(select.data111,UnewRunoff5[indx111,])
}






pdf(paste(root.path,"./newmeanofcon_EXDdecaymodelplotID_", method, ".pdf", sep=""))
  
 for (method in unique(select.data111$method)) {

  
   indxnew1<- which(method == select.data111$method)
     if(length(indxnew1) > 0)
     {
       select.data.new<- select.data111[indxnew1,]
       print(sprintf("%s--%d", method, length(unique(select.data$week))))
       select.mean_week_new <- NULL
       for (week in unique(select.data.new$week))
       {
         for (PlotID in unique(select.data.new$PlotID))
         {
           indxnew3<-which(select.data.new$week == week & select.data.new$PlotID == PlotID)
                   #Sum1 <- Sum1 + length(indx3)
           if(length(indxnew3) > 0) {
             #detectionfreq1<- paste(sum(as.numeric(select.data[indx3,9])),Sum1, sep="/")
              select.mean_week_new <- rbind(select.mean_week_new, cbind(mean(as.numeric(select.data.new[indxnew3,]$proxy)), week,PlotID,method,select.data.new[indxnew3,3]))  
              select.mean_week_new1<-unique(select.mean_week_new)
             
             }
         }
       }
       if (!is.null(select.mean_week_new1))
       {
         
         #nexp.estimate<- my.fitdist.exp.estimate(as.numeric(select.mean_week[,1]))
         #print(nexp.estimate)
         #nexp.estimate1<-format(nexp.estimate,digits=3)
         #nexp.sd<-my.fitdist.exp.sd(as.numeric(select.mean_week[,1]))
         #print(nexp.sd)
         #nexp.sd<-format(nexp.sd,digits=3)
         #plot(select.mean_week[,1]~select.mean_week[,2],type="p",xlab="week", ylab="concentration",
           #main=paste("concentration vs week_",Source,"\n",method,"\n","lambda=",nexp.estimate1,",","sd=",nexp.sd), sub=season)
         #curve(1/nexp.estimate*exp(-x/nexp.estimate),col = "blue",add = TRUE)
        
         #lambda<- my.glm.exp.lambda(as.numeric(select.mean_week1[,2]),as.numeric(select.mean_week1[,1]))
         #lambda.intercept<- my.glm.exp.lambda.intercept(as.numeric(select.mean_week1[,2]),as.numeric(select.mean_week1[,1]))
        #lambda1<-format(as.numeric(log(2)/-lambda),digits=3)
         #print(lambda1)
         #temp.look <- cbind(select.mean_week1[,1],select.mean_week1[,2])
         #hislook<- rbind(hislook,cbind(as.numeric(lambda1), method))
         #print(paste(dim(temp.look)[[1]],dim(temp.look)[[2]],season,method))
         plot(jitter(as.numeric(select.mean_week_new1[,1]))~select.mean_week_new1[,2],type="p",xlab="week", ylab="concentration",
           main=paste(method,"\n","half-life=",as.numeric(lambda1)*7, "days"))
         
         indxnew3<- which(method==hislook_new$method & hislook_new$Source=="Cattle")
         indxnew4<- which(method==hislook_new$method & hislook_new$Source=="Poultry")
         indxnew5<- which(method==hislook_new$method & hislook_new$Source=="Swine")
              curve(exp(as.numeric(hislook_new[indxnew3,3]))*exp(x*as.numeric(hislook_new[indxnew3,2])),add=TRUE,col="blue",lty = 2)
         if (length(indxnew4) > 0)
         {
         curve(exp(as.numeric(hislook_new[indxnew4,3]))*exp(x*as.numeric(hislook_new[indxnew4,2])),add=TRUE,col="red",lty = 4)
         }
         curve(exp(as.numeric(hislook_new[indxnew5,3]))*exp(x*as.numeric(hislook_new[indxnew5,2])),add=TRUE,col="purple",lty = 8)
         legend(0.75,max(as.numeric(select.mean_week_new1[,1]))-10, c("Cattle", "Poultry", "Swine"), col = c("blue","red","purple"),
       text.col = "green4", lty = c(2, 4, 8),
       merge = TRUE, bg = 'gray90')
         
         #curve(exp(lambda.intercept)*exp(x*lambda1),col = "blue",add = TRUE)

        #lines(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),lty=3)
         
         #select.mean_week_total<-rbind(select.mean_week_total,select.mean_week1) 
 }
       else
       {
         print("There is no data!!!")
       }
     
   }
   
 
}

dev.off()