#graphics
str(UnewRunoff4)
newRunoff <- UnewRunoff4

#plot the scatterplot (concentration vesus week) for each analysis method and season
newRunoff[which(is.na(newRunoff$data)),]$data<-NULL
#newRunoff <- na.omit(newRunoff) #delete all NAs in newRunoff

for(method in unique(newRunoff$method)){
  print(method)
  pdf(paste(path.graphics,"./scatterplot_", method, ".pdf", sep=""))
  par(mfrow=c(2,2))
  for(season in c('A','B','C','D')) {
    print(season)
    indx <- which(method == newRunoff$method & season == newRunoff$season)
    if(length(indx) > 0) {
      select_data <- newRunoff[indx,]
      
      #data.plot <-cbind(select_data$week,select_data$data)
      plot(select_data$week,select_data$data,type = "p", main=paste("concentration vs week_", method), 
           sub=season, xlab="week", ylab="concentration")
    }
  }  
  dev.off()
}

#plot the box plot (concentration vesus week) for each analysis method and season
#newRunoff[which(is.na(newRunoff$data)),]$data <- NULL
#newRunoff <- na.omit(newRunoff) #delete all NAs in newRunoff
for(Source in c('C','P','S','X')){
  print(Source)
  for(method in unique(newRunoff$method)){
    print(method)
    pdf(paste(path.graphics,"./boxplot_", Source,"_", method, ".pdf", sep=""))
    par(mfrow=c(2,2))
    for(season in c('A','B','C','D')){
      print(season)
      indx <- which(Source==newRunoff$Source & method == newRunoff$method & season == newRunoff$season)
      if(length(indx) > 0) {
        select_data <- newRunoff[indx,]
        boxplot(as.numeric(select_data$data)~select_data$week,data=select_data,xlab="week", ylab="concentration", col="lightblue", range=0,
                main=paste("concentration vs week_",Source,"\n",method), sub=season)
      }
    }
    dev.off()
  }
}


#plot the box plot (concentration versus week) for each analysis method and season
for(Source in c('C','P','S','X')){
  for (method in unique(UnewRunoff4$method)){
    pdf(paste(path.graphics,"./newboxplot_", Source,"_", method, ".pdf", sep=""))
    par(mfrow=c(2,2))
    for(season in c('A','B','C','D')) {
      print(sprintf("%s--%s--%s", Source, method,season))
      indx <- which(Source==UnewRunoff4$Source & method == UnewRunoff4$method & season == UnewRunoff4$season)
      if(length(indx) > 0) {
        select_data <- UnewRunoff4[indx,]
        boxplot(as.numeric(select_data$data)~select_data$week,data=select_data,xlab="week", ylab="concentration", col="lightblue", range=0,
             main=paste("concentration vs week_",Source,"\n",method), sub=season)
      }
    }
    dev.off()
  }
}
 

my.t.test.p.value1 <- function(x,y) {
  obj<-try(t.test(x,y, var.equal=F), silent=TRUE)
  if (is(obj, "try-error")) return(NA) else return(obj$p.value)
}
my.t.test.p.value2 <- function(x,y) {
  obj<-try(ks.test(x,y), silent=TRUE)
  if (is(obj, "try-error")) return(NA) else return(obj$p.value)
}

Sum <- 0
stat_summary <- NULL
multiplecomparison<-NULL
pvalue1<-NULL
pvalue2<-NULL
for (week in c(0,1,2)){
  for(Source in c('C','P','S','X')){
    for (season in c('A','B','C','D')){
      for (method in c("CampylobacterCulturable_MPNPer100ml","ClostridiumCulturable_TSCF_CFUPer100ml",
            "Crypto_OocystsPer100ml", "EColi0157Culturable_MPNPer100ml", "EColiCulturableColilert_MPNPer100ML",
            "enterococciCulturableMEI_CFUPer100ML","Giardia_CystsPer100ml","SalmonellaCulturableWildType_MPNPer100ml" )){

        indx10<- which(week == UnewRunoff4$week & Source == UnewRunoff4$Source & 
          season == UnewRunoff4$season & method == UnewRunoff4$method & UnewRunoff4[,4] <= 30)
        Sum <- Sum + length(indx10)
        avg = NA
        std = NA
        if(length(indx10> 0)){
          avg = mean(UnewRunoff4[indx10,5])
          std = sd(UnewRunoff4[indx10,5])
          detectionfreq<- paste(sum(as.numeric(UnewRunoff4[indx10,9])),Sum, sep="/")
        }
        stat_summary <- rbind(stat_summary, c(paste(week,Source,season,method,'0_30',sep="_"), avg, std, Sum,detectionfreq))

        indx20<- which(week == UnewRunoff4$week & Source == UnewRunoff4$Source & 
          season == UnewRunoff4$season & method == UnewRunoff4$method & UnewRunoff4[,4] > 30  & UnewRunoff4[,4] <= 60)
        Sum <- Sum + length(indx20)
        avg = NA
        std = NA
        if(length(indx20 > 0)){
          avg = mean(UnewRunoff4[indx20,5])
          std = sd(UnewRunoff4[indx20,5])                                                       
          detectionfreq<- paste(sum(as.numeric(UnewRunoff4[indx20,9])),Sum, sep="/")
        }
        pvalue1<- my.t.test.p.value1(UnewRunoff4[indx10,5],UnewRunoff4[indx20,5])
        print(pvalue1)
        pvalue2<- my.t.test.p.value2(UnewRunoff4[indx10,5],UnewRunoff4[indx20,5])
        print(pvalue2)
        
        stat_summary <- rbind(stat_summary, c(paste(week,Source,season,method,'30_60',sep="_"), avg, std,  Sum,detectionfreq))  
      }
    }
  }
}
# warnings due to ties: In ks.test(x, y) : cannot compute exact p-value with ties
#if (sum != nrow(UnewRunoff4)){print("something wrong")}

colnames(stat_summary)<- c("detail_info","mean", "std","samplesize","detectionfreq")
stat_summary1 <- as.data.frame(stat_summary)

write.csv(stat_summary1,file=paste(path.csv.out,"stat_summary.csv",sep=""))
write.table(stat_summary1,file=paste(path.csv.out,"stat_summary.txt"))

#the rest of this code is fubar - stp - 1/30/15
#scatter plot for mean of the concentration at each plotID for each week
Sum1<-0
select_mean_week_total<-NULL
for(Source in c('C','P','S','X')){
 for (method in unique(UnewRunoff4$method)){
   pdf(paste(path.graphics,"./meanofcon_plotID_", Source,"_", method, ".pdf", sep=""))
   par(mfrow=c(2,2))
   for(season in c('A','B','C','D')){
     indx <- which(Source==UnewRunoff4$Source & method == UnewRunoff4$method & season == UnewRunoff4$season)
     if(length(indx) > 0){
       select_data <- UnewRunoff4[indx,]
       print(sprintf("%s--%s--%s--%d", Source, method, season, length(unique(select_data$week))))
       select_mean_week <- NULL
       for (week in unique(select_data$week)){
         for (PlotID in unique(select_data$PlotID)){
            indx3 = which(select_data$week == week & select_data$PlotID == PlotID)
            Sum1 <- Sum1 + length(indx3)
            if(length(indx3) > 0) {
             detectionfreq1<- paste(sum(as.numeric(select_data[indx3,9])),Sum1, sep="/")
             select_temp <- cbind(mean(as.numeric(select_data[indx3,]$proxy)), 
                                 week,PlotID,season,method,Source,detectionfreq1, as.numeric(select_data[indx3,10]),
                                 as.numeric(select_data[indx3,11]),as.numeric(select_data[indx3,13]),
                                 as.numeric(select_data[indx3,14]))
             dim(select_mean_week)
             dim(select_temp)
             select_mean_week <- rbind(select_mean_week, select_temp)  
            }
            select_mean_week1 <- unique(select_mean_week)
         }
       }
       if (!is.null(select_mean_week1)){
         plot(select_mean_week1[,1]~select_mean_week1[,2],type="p",xlab="week", ylab="concentration",
           main=paste("concentration vs week_",Source,"\n",method), sub=season)
         select_mean_week_total<-rbind(select_mean_week_total,select_mean_week1) 
        } else {
          print("There is no data!!!")
        }
     }
   }
   dev.off()
  }
}

select_mean_week_total<-as.data.frame(select_mean_week_total)
names(select_mean_week_total)[1]<- "proxy"
names(select_mean_week_total)[8]<-"type of mictobes"
names(select_mean_week_total)[9]<-"analysis type"
names(select_mean_week_total)[10]<-"names of microbes"
names(select_mean_week_total)[11]<-"day"
write.csv(select_mean_week_total,file=paste(path.csv_out,"select_mean_week_total.csv",sep=""))




