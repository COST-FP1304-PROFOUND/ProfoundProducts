#' This runs the Muso model
#' @author Roland
#' @param calibrationpar vector with line numbers
#' @return No return, outputs are written to file
setup <- function(executable=NULL,
                  parallel = F,
                  calibrationpar =c(1),
                  outputloc=NULL,
                  inputloc=NULL,
                  metinput=NULL,
                  ininput=NULL,
                  epcinput=NULL
                  ){
 
  
  if(is.null(inputloc)){
    # inputloc<- paste(getwd(),"/RMuso/example-data/",sep="")
    inputloc<- "/home/hollorol/Desktop/biomebgc-muso/RMuso/example-data/"
    }
  if(is.null(outputloc)){
    # outputloc<- paste(getwd(),"/RMuso/example-data",sep="")
    outputloc<-inputloc
    }

  #inifilename<-list.files(path=inputloc,pattern='*.ini')[1]
  if(is.null(ininput)){
    ininput[1]<-paste(inputloc,"s.ini",sep="")
    ininput[2]<-paste(inputloc,"n.ini",sep="")
  }
  
  if(is.null(epcinput)){
   epcinput<-paste(inputloc,"c3grass21.epc",sep="")
  }
  
  if(is.null(metinput)){
    metinput[1]<-paste(inputloc,"hhs_1901_2000.mtc43",sep="")
    metinput[2]<-paste(inputloc,"eur_co2_1901-2000.txt",sep="")
    metinput[3]<-paste(inputloc,"HHS_Ndep_1901-2000.txt",sep="")
}
  
  
  if(is.null(executable)){
    executable<-paste(inputloc,"/muso",sep="")} else {
    file.copy(executable,inputloc)}
    
  outputname<-unlist(read.table(ininput[1],skip=96,nrows = 1))
  inputfiles<-c(ininput,epcinput,metinput)
  numdata<-rep(NA,3)
  numyears<-unlist(read.table(ininput[2],skip = 14,nrows = 1)[1])
  numvalues<-unlist(read.table(ininput[2],skip=113,nrows = 1)[1])
  numdata[1]<-numyears*numvalues*365
  numdata[2]<-numyears*numvalues*12
  numdata[3]<-numyears*numvalues
  
  settings = list(executable = executable,
                  calibrationpar = calibrationpar,
                  outputloc=outputloc,
                  outputnames=outputname,
                  inputloc=inputloc,
                  ininput=ininput,
                  metinput=metinput,
                  epcinput=epcinput,
                  inputfiles=inputfiles,
                  numdata=numdata,
                  numyears=numyears
                  )
  
  return(settings)

}


