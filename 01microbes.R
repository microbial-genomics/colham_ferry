
filenames<-list.files(path = path.csv.in, pattern = ".+[^.]Runoff.csv$")
All <- lapply(filenames,function(iname){
    iname <- paste(path.csv.in,iname,sep="")
    test.x <- length(scan(iname,what=character(),nlines=1,skip=1))
    if(test.x>0){
      read.csv(iname, header=T)
    }
})

for(i in 1:length(All)){
  print(i)
  print(filenames[i])
  print(length(unlist(All[i][1])))
  print(length(unlist(All[i][1]))==0)
}

delete.list <- NULL
for(i in 1:length(All)){
  if(length(unlist(All[i][1]))==0){
    print(i)
    delete.list <- c(delete.list,i)
  }
}
All[delete.list] <- NULL
#filenames[delete.list] <- NULL
#filenames <- as.vector(filenames)
#filenames <- gsub("-",".",filenames)
#names(All) <- gsub(".csv","",filenames)

Ntoget <- length(All)

acf<- NULL
acf<-matrix(data=NA,ncol=4)
names(acf) <- c("plot","result","dl","method")
acf <- as.data.frame(acf)

for(i in 1:Ntoget){
  Dat<- NULL
  Dat<-as.data.frame(All[i])
  method<- rep(colnames(Dat[2]), nrow(Dat))
#clean it up- drop the 4th?
  if (ncol(Dat)>3){
    for(j in ncol(Dat):4){
      Dat[,j]<- NULL
    }
    Dat<- as.data.frame(cbind(Dat,method))
  } else {
    Dat<- as.data.frame(cbind(Dat,method))
  }
  #add a field- the second column header
  names(Dat) <- names(acf)
  if(i==1){
    acf <- Dat
  } else {
    acf<-rbind(acf,Dat)
  }
}

acf[,2] <- as.numeric(acf[,2])

#check.plots <- as.character(acf[is.na(acf[,2]),1])
#check.methods <- as.character(acf[is.na(acf[,2]),4])
#check.nas <- cbind(check.plots,check.methods)
#summary(check.plots)
#summary(acf[,1])
  
# remove blank plots
blank.plots <- which(acf[,1]=="")
acf <- acf[-blank.plots,]

names(acf) <- c("plot","result","dl","method")
write.csv(acf,file=paste(path.csv.out,"check_acf.csv",sep=""))

#create a new matrix 
nplot<-nrow(acf)
Runoff<-matrix(data=NA,nrow =nplot,ncol=7, byrow=TRUE, dimnames= list(c(), 
            c("season", "PlotID", "Source", "ElapsedTimeSinceRunoffStarted_Minutes", "data","detection level", "method")))

for (i in 1:nplot){
  Runoff[i,1]<-substr(acf[i,1],1,1)
  Runoff[i,2]<-substr(acf[i,1],3,5)
  if  (grepl("X", substr(acf[i,1],7,7), fixed=FALSE,ignore.case = TRUE)){
    Runoff[i,3]<-substr(sub("*X","X", acf[i,1],fixed=FALSE,ignore.case = TRUE),7,7)
  } else {
    Runoff[i,3]<-substr(acf[i,1],7,7)
  }
  Runoff[i,4]<-substr(acf[i,1],9,15)
  Runoff[i,6]<-as.character(acf[i,3])
  Runoff[i,7]<-as.character(acf[i,4])

  if (grepl("*PerML$", Runoff[i,7], fixed=FALSE,ignore.case = TRUE)){
    Runoff[i,7]<-sub("*PerML", "Per100ml", Runoff[i,7], ignore.case = TRUE,fixed = FALSE)
    Runoff[i,5]<- acf[i,2]*100
  } else if (grepl("*PerLiter$", Runoff[i,7], fixed=FALSE,ignore.case = TRUE)) {
    Runoff[i,7]<-sub("*PerLiter", "Per100ml", Runoff[i,7], ignore.case = TRUE,fixed = FALSE)
    Runoff[i,5]<-acf[i,2]*10
  } else if (grepl("*PerL$", Runoff[i,7], fixed=FALSE,ignore.case = TRUE)) {
    Runoff[i,7]<-sub("*PerL", "Per100ml", Runoff[i,7], ignore.case = TRUE,fixed = FALSE)
    Runoff[i,5]<-acf[i,2]*10
  } else {
    Runoff[i,5]<-acf[i,2]
  }
}

write.csv(Runoff,file=paste(path.csv.out,"Runoff_new.csv",sep=""))

Runoff1<-as.data.frame(Runoff, stringsAsFactors=FALSE)

split_plot<-read.csv(paste(path.csv.in,"split-plot design.csv",sep=""),header=TRUE)
newRunoff0<-merge(Runoff1,split_plot,by.x="PlotID", by.y="plot", all.x = TRUE, all.y = TRUE)
write.csv(newRunoff0,file=paste(path.csv.out,"Runoff_splitplot.csv",sep=""))

Runoff2<- na.omit(Runoff1)
#create a new matrix with updated detection frequencies, analysis type.... ^-^
nplot1<-nrow(Runoff2)
UnewRunoff<-matrix(data=NA,nrow =nplot1,ncol=13, byrow=TRUE, dimnames= list(c(), 
                c("season", "PlotID", "Source", "ElapsedTimeSinceUnewRunoffStarted_Minutes", "data",
                  "proxy","detection level", "method","detection frequency","type of mictobes",
                  "analysis type","amended field","names of microbes")))
for (i in 1:5){UnewRunoff[,i] <- Runoff2[,i]}
for (i in 5:7){UnewRunoff[,i+1] <- Runoff2[,i]}

#define time field
indx <- grep("^30.+", Runoff2[,4], fixed=FALSE)
UnewRunoff[indx,4]<-30

indx <- grep("^T.+", Runoff2[,4], fixed=FALSE)
UnewRunoff[indx,4]<-60

indx <- grep("^40.+", Runoff2[,4], fixed=FALSE) 
UnewRunoff[indx,4]<-30

indx <- grep("drip", Runoff2[,4], fixed=FALSE,ignore.case = TRUE)
UnewRunoff[indx,4]<-0

# UnewRunoff of column from 1 to 8 have been set previously, from 9th column, the data are NA
UnewRunoff[,9]<-1 # detection frequency
UnewRunoff[,12]<-0 #amanded field

# define a column called detection frequency based on detection leves (indicator variable) 
indx <- grep("ANP", UnewRunoff[,7], fixed=FALSE, ignore.case = TRUE)
UnewRunoff[indx,6]<-NA
UnewRunoff[indx,9]<-NA
 
for (DL in c("BDL","BQL","ND")){
  indx <-grep(DL,UnewRunoff[,7], fixed=FALSE,ignore.case = TRUE)
  UnewRunoff[indx,6]<-as.numeric(Runoff2[indx,5])/2 #for now estimate non-detects as 0.5*detection limit,-- 
  #create new field called proxy that is equal to observation when detected, 0.5*detection limit when not
  UnewRunoff[indx,9]<-0
}

# define a column called amended field for for(seasons B,C,D for Salmonella, Ecoli0157,Crypto, Giardia), 
#others not amended (indicator variable)
indx <- grep("A", UnewRunoff[,1], invert=TRUE)
for (amended in c("Salmonella", "Ecoli0157","Crypto","Giardia")){
  indx1<- grep(amended, Runoff2[indx,7],ignore.case = TRUE, fixed = FALSE)
  UnewRunoff[indx1,12]<- 1
}
  
#create factor field for indicators (entero, Ecoli, Clostridium) and pathogens (Salmonella, Camplylobacter, Ecoli0157,Crypt, Giardia)
#create factor field for analysis type: molecular (GU,TSC) or Culturable (MPN,CFU)
for (string1 in c("entero","Ecoli","Clostridium", "TotalColiforms")){
  indx2<-grep(string1, Runoff2[,7],ignore.case = TRUE, fixed = FALSE)
  UnewRunoff[indx2,10]<-"indicator"
}

for (string2 in c("Salmonella", "Campylobacter","EColi0157")){
  indx3<-grep(string2, Runoff2[,7],ignore.case = TRUE, fixed = FALSE)
  UnewRunoff[indx3,10]<-"bacteriapathogens"
}
 
for (string3 in c("Crypto", "Giardia")){
  indx3.5<-grep(string3, Runoff2[,7],ignore.case = TRUE, fixed = FALSE)
  UnewRunoff[indx3.5,10]<-"parasiticpathogens"
}

for (type1 in c("*_GU","*_TSC")){
  indx4<-grep(type1, Runoff2[,7],ignore.case = TRUE, fixed = FALSE)
  UnewRunoff[indx4,11]<-"molecular"
}
  
for (type2 in c("*_MPN","*_CFU","*cysts")){
   indx5<-grep(type2, Runoff2[,7],ignore.case = TRUE, fixed = FALSE)
     UnewRunoff[indx5,11]<-"culturable"
}

# add pathogen field from analysis type: entero, Ecoli, Clostridium, Salmonella, Camplylobacter, Ecoli0157,Crypt, Giardia
for (string3 in c("entero", "EColi", "Clostridium", "Salmonella", "Campylobacter", 
                  "EColi0157", "Crypto","Giardia")){
  indx6<-grep(string3, Runoff2[,7],ignore.case = TRUE, fixed = FALSE)
  UnewRunoff[indx6,13]<- string3
}

UnewRunoff1<-as.data.frame(UnewRunoff,stringsAsFactors = FALSE)
UnewRunoff2<- na.omit(UnewRunoff1)
UnewRunoff3 <- NULL
for(Source in c('C','P','S','X')){
  for (method in unique(UnewRunoff2$method)){
    print(sprintf("%s--%s", Source, method))
    indx <- which(Source==UnewRunoff2$Source & method == UnewRunoff2$method)
    select.season<- UnewRunoff2[indx,]
    if(length(indx) > 0){select.data1 <- UnewRunoff2[indx,]}
    Countseason<-unique(select.data1$season)
    if (length( Countseason)==4){UnewRunoff3<-rbind(UnewRunoff3,UnewRunoff2[indx,])}
  }
}
 
write.csv(UnewRunoff3,file=paste(path.csv.out,"UnewRunoff3_final.csv",sep=""))

split_plot<-read.csv(paste(path.csv.in,"split-plot design.csv",sep=""),header=TRUE)
UnewRunoff4<-merge(UnewRunoff3,split_plot,by.x="PlotID", by.y="plot")
UnewRunoff4[,5] <- as.numeric(UnewRunoff4[,5])
write.csv(UnewRunoff4,file=paste(path.csv.out,"colham_ferry_df.csv",sep=""))
