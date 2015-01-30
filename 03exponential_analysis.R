#library(fitdistrplus)
#my.fitdist.exp.estimate<- function(x,y) {
    #obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
    #if (is(obj, "try-error")) return(NA) else return(obj$estimate)
 #}

#my.fitdist.exp.sd<- function(x,y) {
    #obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
    #if (is(obj, "try-error")) return(NA) else return(obj$sd)
 #}

UnewRunoff5<-UnewRunoff4
nplotnew<-nrow(UnewRunoff5)
for (i in 1:nplotnew)
{
indxnew1<- grep("A", UnewRunoff5[,2], fixed=TRUE)

 UnewRunoff5[indxnew1,2]<-"Event1"
indxnew2<- grep("B", UnewRunoff5[,2], fixed=TRUE)

 UnewRunoff5[indxnew2,2]<-"Event2"
indxnew3<- grep("C", UnewRunoff5[,2], fixed=TRUE)
 UnewRunoff5[indxnew3,2]<-"Event3"
indxnew4<- grep("D", UnewRunoff5[,2], fixed=TRUE)
 UnewRunoff5[indxnew4,2]<-"Event4"

indxnew5<-grep("S", UnewRunoff5[,3], fixed=TRUE)
UnewRunoff5[indxnew5,3]<-"Swine"
indxnew6<-grep("P", UnewRunoff5[,3], fixed=TRUE)
UnewRunoff5[indxnew6,3]<-"Poultry"
indxnew8<-grep("C", UnewRunoff5[,3], fixed=TRUE)
UnewRunoff5[indxnew8,3]<-"Cattle"

indxnew7<-grep("X", UnewRunoff5[,3], fixed=TRUE)
UnewRunoff5[indxnew7,3]<-"Original"


}







my.glm.exp.lamda<-function(x,y){
    obj<-try(glm(y~x, family = Gamma(link = "log")), silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(summary(obj,dispersion=1)$coef[2])
 }
my.glm.exp.lamda.intercept<-function(x,y){
    obj<-try(glm(y~x, family = Gamma(link = "log")), silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(summary(obj,dispersion=1)$coef[1])
 }
#scatter plot for mean of the concentration at each plotID for each week
Sum1<-0
select.mean_week_total<-NULL
 hislook<-NULL
for(Source in c('Cattle','Poultry','Swine','Original'))
{
 for (method in unique(UnewRunoff5$method)) {

   pdf(paste(root.path,"./newmeanofcon_EXDdecaymodelplotID_", Source,"_", method, ".pdf", sep=""))
   par(mfrow=c(2,2))
   for(season in c('Event1','Event2','Event3','Event4'))
   {
     indx <- which(Source==UnewRunoff5$Source & method == UnewRunoff5$method & season == UnewRunoff5$season)
     if(length(indx) > 0)
     {
       select.data <- UnewRunoff5[indx,]
       print(sprintf("%s--%s--%s--%d", Source, method, season, length(unique(select.data$week))))
       select.mean_week <- NULL
       for (week in unique(select.data$week))
       {
         for (PlotID in unique(select.data$PlotID))
         {
           indx3 = which(select.data$week == week & select.data$PlotID == PlotID)
                   Sum1 <- Sum1 + length(indx3)
           if(length(indx3) > 0) {
             detectionfreq1<- paste(sum(as.numeric(select.data[indx3,9])),Sum1, sep="/")
             select.mean_week <- rbind(select.mean_week, cbind(mean(as.numeric(select.data[indx3,]$proxy)), week,PlotID,season,method,Source,detectionfreq1, select.data[indx3,10],select.data[indx3,11],select.data[indx3,13],select.data[indx3,14]))  
              select.mean_week1<-unique(select.mean_week)
             
             }
         }
       }
       if (!is.null(select.mean_week1))
       {
         #nexp.estimate<- my.fitdist.exp.estimate(as.numeric(select.mean_week[,1]))
         #print(nexp.estimate)
         #nexp.estimate1<-format(nexp.estimate,digits=3)
         #nexp.sd<-my.fitdist.exp.sd(as.numeric(select.mean_week[,1]))
         #print(nexp.sd)
         #nexp.sd<-format(nexp.sd,digits=3)
         #plot(select.mean_week[,1]~select.mean_week[,2],type="p",xlab="week", ylab="concentration",
           #main=paste("concentration vs week_",Source,"\n",method,"\n","lamda=",nexp.estimate1,",","sd=",nexp.sd), sub=season)
         #curve(1/nexp.estimate*exp(-x/nexp.estimate),col = "blue",add = TRUE)
        
         lamda<- my.glm.exp.lamda(as.numeric(select.mean_week1[,2]),as.numeric(select.mean_week1[,1]))
         lamda.intercept<- my.glm.exp.lamda.intercept(as.numeric(select.mean_week1[,2]),as.numeric(select.mean_week1[,1]))
        lamda1<-format(as.numeric(log(2)/-lamda),digits=3)
         print(lamda1)
         #temp.look <- cbind(select.mean_week1[,1],select.mean_week1[,2])
         hislook<- rbind(hislook,cbind(as.numeric(lamda1), method,season,Source))
         print(paste(dim(temp.look)[[1]],dim(temp.look)[[2]],season,method))
         plot(jitter(as.numeric(select.mean_week1[,1]))~select.mean_week1[,2],type="p",xlab="week", ylab="concentration",
           main=paste(Source,"\n",method,"\n","half-life=",as.numeric(lamda1)*7, "days"), sub=season)
         
              curve(exp(lamda.intercept)*exp(x*lamda),add=TRUE,col="blue")
         #curve(exp(lamda.intercept)*exp(x*lamda1),col = "blue",add = TRUE)

        #lines(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),lty=3)
         
         select.mean_week_total<-rbind(select.mean_week_total,select.mean_week1) 
 }
       else
       {
         print("There is no data!!!")
       }
     }
   }
   dev.off()
 }
}

#plot histograms(in half-life) to check the distribution of half-life for each method(8 methods)
hislook1<-na.omit(hislook)
write.csv(hislook1,file=paste(root.path,"Half-life_seasons_sources.csv",sep=""))
hislook2<-as.data.frame(hislook1,stringsAsFactors =FALSE)
#nplotnew2<-nrow(hislook)
#for (i in 1:nplotnew2)
#{
 # indxnew20<- grep("Inf", hislook[,1], fixed=TRUE)
#hislook[indxnew20,1]<-"1/0"
#  }
#hislook1<-as.numeric(levels(hislook[,1])[hislook[,1]])
#hislook2<-na.omit(hislook1)
#bins<-c(min(hislook2), 0:3,3:6,6:9,9:12,12:15)

pdf(paste(root.path,"./histofcon_EXDdecaymodelplotID",".pdf", sep=""))
par(mfrow=c(4,2))

for (method in unique(hislook2$method)) {
 indxnew21<-which(method == hislook2$method)
   
   hislook11<-as.numeric(hislook2[indxnew21,1])
   hislook11[which(hislook11>15)]<-15
   hislook11[which(hislook11<0)]= -1

   hist(hislook11,breaks = 12,col="grey",main = paste("Histogram of halflife for","\n",method), xlab="half-life in days",
      ylab="frequency of half-life", xlim=c(-1, 15))
   
}
   
 dev.off()  





### below can be commmented out



#library(fitdistrplus)
library(drc)

#my.fitdist.exp.estimate<- function(x,y) {
    #obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
    #if (is(obj, "try-error")) return(NA) else return(obj$estimate)
 #}

#my.fitdist.exp.sd<- function(x,y) {
    #obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
   # if (is(obj, "try-error")) return(NA) else return(obj$sd)
# }

my.drm.exd.lifetime<-function (x,y){
    obj<-try(drm(y~x,fct=EXD.3()),silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(obj$coef[3])
}

my.drm.exd.lifetime.plot<-function (x,y){
    obj<-try(drm(y~x,fct=EXD.3()),silent=TRUE)
    if (is(obj, "try-error")) return(NA) else try(plot(obj,xlim=c(0, 2),xlab="week", ylab="concentration",main=paste("concentration vs week_",Source,"\n",method), sub=season), silent=TRUE)}

#scatter plot for mean of the concentration at each plotID for each week
Sum1<-0
select.mean_week_total<-NULL
for(Source in c('C','P','S','X'))
{
 for (method in unique(UnewRunoff4$method)) {

   pdf(paste("./newmeanofcon_plotID_", Source,"_", method, ".pdf", sep=""))
   par(mfrow=c(2,2))
   for(season in c('A','B','C','D'))
   {
     indx <- which(Source==UnewRunoff4$Source & method == UnewRunoff4$method & season == UnewRunoff4$season)
     if(length(indx) > 0)
     {
       select.data <- UnewRunoff4[indx,]
       print(sprintf("%s--%s--%s--%d", Source, method, season, length(unique(select.data$week))))
       select.mean_week <- NULL
       for (week in unique(select.data$week))
       {
         for (PlotID in unique(select.data$PlotID))
         {
           indx3 = which(select.data$week == week & select.data$PlotID == PlotID)
                   Sum1 <- Sum1 + length(indx3)
           if(length(indx3) > 0) {
             detectionfreq1<- paste(sum(as.numeric(select.data[indx3,9])),Sum1, sep="/")
             select.mean_week <- rbind(select.mean_week, cbind(mean(as.numeric(select.data[indx3,]$proxy)), week,PlotID,season,method,Source,detectionfreq1, select.data[indx3,10],select.data[indx3,11],select.data[indx3,13],select.data[indx3,14]))  
             }
         }
       }
       if (!is.null(select.mean_week))
       {
         #nexp.estimate<- my.fitdist.exp.estimate(as.numeric(select.mean_week[,1]))
        # print(nexp.estimate)
         #nexp.estimate1<-format(nexp.estimate,digits=3)
         #nexp.sd<-my.fitdist.exp.sd(as.numeric(select.mean_week[,1]))
         #print(nexp.sd)
         #nexp.sd<-format(nexp.sd,digits=3)
         conc.m1<-my.drm.exd.lifetime(as.numeric(select.mean_week[,2]),as.numeric(select.mean_week[,1]))
         print(conc.m1)
       my.drm.exd.lifetime.plot(as.numeric(select.mean_week[,2]),as.numeric(select.mean_week[,1]))
         #plot(select.mean_week[,1]~select.mean_week[,2],type="p",xlab="week", ylab="concentration",
           #main=paste("concentration vs week_",Source,"\n",method,"\n","lamda=",nexp.estimate1,",","sd=",nexp.sd), sub=season)
        #curve(1/nexp.estimate*exp(-x/nexp.estimate),col = "blue",add = TRUE)
         
        #lines(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),lty=3)
         
         select.mean_week_total<-rbind(select.mean_week_total,select.mean_week) 
 }
       else
       {
         print("There is no data!!!")
       }
     }
   }
   dev.off()
 }
}


































library(fitdistrplus)
my.fitdist.exp.estimate<- function(x,y) {
    obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(obj$estimate)
 }

my.fitdist.exp.sd<- function(x,y) {
    obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(obj$sd)
 }

#scatter plot for mean of the concentration at each plotID for each week
Sum1<-0
select.mean_week_total<-NULL
for(Source in c('C','P','S','X'))
{
 for (method in unique(UnewRunoff4$method)) {

   pdf(paste(root.path,"./newmeanofcon_plotID_", Source,"_", method, ".pdf", sep=""))
   par(mfrow=c(2,2))
   for(season in c('A','B','C','D'))
   {
     indx <- which(Source==UnewRunoff4$Source & method == UnewRunoff4$method & season == UnewRunoff4$season)
     if(length(indx) > 0)
     {
       select.data <- UnewRunoff4[indx,]
       print(sprintf("%s--%s--%s--%d", Source, method, season, length(unique(select.data$week))))
       select.mean_week <- NULL
       for (week in unique(select.data$week))
       {
         for (PlotID in unique(select.data$PlotID))
         {
           indx3 = which(select.data$week == week & select.data$PlotID == PlotID)
                   Sum1 <- Sum1 + length(indx3)
           if(length(indx3) > 0) {
             detectionfreq1<- paste(sum(as.numeric(select.data[indx3,9])),Sum1, sep="/")
             select.mean_week <- rbind(select.mean_week, cbind(mean(as.numeric(select.data[indx3,]$proxy)), week,PlotID,season,method,Source,detectionfreq1, select.data[indx3,10],select.data[indx3,11],select.data[indx3,13],select.data[indx3,14]))  
             }
         }
       }
       if (!is.null(select.mean_week))
       {
         nexp.estimate<- my.fitdist.exp.estimate(as.numeric(select.mean_week[,1]))
         print(nexp.estimate)
         nexp.estimate1<-format(nexp.estimate,digits=3)
         nexp.sd<-my.fitdist.exp.sd(as.numeric(select.mean_week[,1]))
         print(nexp.sd)
         nexp.sd<-format(nexp.sd,digits=3)
         plot(select.mean_week[,1]~select.mean_week[,2],type="p",xlab="week", ylab="concentration",
           main=paste("concentration vs week_",Source,"\n",method,"\n","lamda=",nexp.estimate1,",","sd=",nexp.sd), sub=season)
         #lines(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),lty=3)
         
         select.mean_week_total<-rbind(select.mean_week_total,select.mean_week) 
 }
       else
       {
         print("There is no data!!!")
       }
     }
   }
   dev.off()
 }
}

curve(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),col = "blue", add = TRUE)



#library(fitdistrplus)
library(drc)

#my.fitdist.exp.estimate<- function(x,y) {
    #obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
    #if (is(obj, "try-error")) return(NA) else return(obj$estimate)
 #}

#my.fitdist.exp.sd<- function(x,y) {
    #obj<-try(fitdist(x, "exp",method="mle"), silent=TRUE)
   # if (is(obj, "try-error")) return(NA) else return(obj$sd)
# }

my.drm.exd.lifetime<-function (x,y){
    obj<-try(drm(y~x,fct=EXD.3()),silent=TRUE)
    if (is(obj, "try-error")) return(NA) else return(obj$coef[3])
}

my.drm.exd.lifetime.plot<-function (x,y){
    obj<-try(drm(y~x,fct=EXD.3()),silent=TRUE)
    if (is(obj, "try-error")) return(NA) else try(plot(obj,xlim=c(0, 2),xlab="week", ylab="concentration",main=paste("concentration vs week_",Source,"\n",method), sub=season), silent=TRUE)}

#scatter plot for mean of the concentration at each plotID for each week
Sum1<-0
select.mean_week_total<-NULL
for(Source in c('C','P','S','X'))
{
 for (method in unique(UnewRunoff4$method)) {

   pdf(paste("./newmeanofcon_plotID_", Source,"_", method, ".pdf", sep=""))
   par(mfrow=c(2,2))
   for(season in c('A','B','C','D'))
   {
     indx <- which(Source==UnewRunoff4$Source & method == UnewRunoff4$method & season == UnewRunoff4$season)
     if(length(indx) > 0)
     {
       select.data <- UnewRunoff4[indx,]
       print(sprintf("%s--%s--%s--%d", Source, method, season, length(unique(select.data$week))))
       select.mean_week <- NULL
       for (week in unique(select.data$week))
       {
         for (PlotID in unique(select.data$PlotID))
         {
           indx3 = which(select.data$week == week & select.data$PlotID == PlotID)
                   Sum1 <- Sum1 + length(indx3)
           if(length(indx3) > 0) {
             detectionfreq1<- paste(sum(as.numeric(select.data[indx3,9])),Sum1, sep="/")
             select.mean_week <- rbind(select.mean_week, cbind(mean(as.numeric(select.data[indx3,]$proxy)), week,PlotID,season,method,Source,detectionfreq1, select.data[indx3,10],select.data[indx3,11],select.data[indx3,13],select.data[indx3,14]))  
             }
         }
       }
       if (!is.null(select.mean_week))
       {
         #nexp.estimate<- my.fitdist.exp.estimate(as.numeric(select.mean_week[,1]))
        # print(nexp.estimate)
         #nexp.estimate1<-format(nexp.estimate,digits=3)
         #nexp.sd<-my.fitdist.exp.sd(as.numeric(select.mean_week[,1]))
         #print(nexp.sd)
         #nexp.sd<-format(nexp.sd,digits=3)
         conc.m1<-my.drm.exd.lifetime(as.numeric(select.mean_week[,2]),as.numeric(select.mean_week[,1]))
         print(conc.m1)
       my.drm.exd.lifetime.plot(as.numeric(select.mean_week[,2]),as.numeric(select.mean_week[,1]))
         #plot(select.mean_week[,1]~select.mean_week[,2],type="p",xlab="week", ylab="concentration",
           #main=paste("concentration vs week_",Source,"\n",method,"\n","lamda=",nexp.estimate1,",","sd=",nexp.sd), sub=season)
        #curve(1/nexp.estimate*exp(-x/nexp.estimate),col = "blue",add = TRUE)
         
        #lines(predict(loess(as.numeric(select.mean_week[,1])~as.numeric(select.mean_week[,2])))~as.numeric(select.mean_week[,2]),lty=3)
         
         select.mean_week_total<-rbind(select.mean_week_total,select.mean_week) 
 }
       else
       {
         print("There is no data!!!")
       }
     }
   }
   dev.off()
 }
}


