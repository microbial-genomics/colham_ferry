#code to estimate decay rates for colham ferry data from Marirosa Molina
# code from Yin Gu, Tommy Bohrmann, and Tom Purucker

#source commands
R.version
Sys.info()
sessionInfo()
compname <- Sys.info()[4][[1]]

#import the data from csv files
if((compname == "stp-air-3.local")){ #Tom mac
  path.root <- path.expand("~/git/colham_ferry/")
  path.csv.in <- path.expand("~/git/colham_ferry/csv_in/")
  path.csv.out <- path.expand("~/git/colham_ferry/csv_out/")
  path.graphics <- path.expand("~/git/colham_ferry/graphics/")
} else if(compname == "DC2626UTPURUCKE"){ #Tom cts windows
  path.root <- path.expand("C:\\git\\colham_ferry\\")
  path.csv.in <- path.expand("C:\\git\\colham_ferry\\csv_in\\")
  path.csv.out <- path.expand("C:\\git\\colham_ferry\\csv_out\\")
  path.graphics <- path.expand("C:\\git\\colham_ferry\\graphics\\")
}
#root.path <- "//AA.AD.EPA.GOV/ORD/ATH/USERS/ygu02/Net MyDocuments/Dropbox/colham_ferry/microbes/csv/"

file.exists(path.root)
file.exists(path.csv.in)
file.exists(path.csv.out)
file.exists(path.graphics)

#execute R files
source(paste(path.root,"01microbes.R",sep=""))
