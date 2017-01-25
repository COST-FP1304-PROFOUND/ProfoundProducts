#' This runs the Muso model
#' @author Roland
#' @param filename Name of the initialisation files
#' @return No return, outputs are written to file 
#' @usage The function works only, if ...
#not solved the path problem yet...

runMuso <- function(settings, parameters=c(" ECOPHYS")){
  #changing section
#   for(i in changeinput){
#     changemulline(settings, parameters[[i]])
#   }
  changemulline(settings,parameters)
  #spinup run
  # changemulline(type=1,setup(), parameters[[2]])
  setwd(settings$inputloc)
  system(paste("./muso",settings$ininput[1],sep=" "))
  #normal run
  setwd(settings$inputloc)
  system(paste("./muso",settings$ininput[2],sep=" "))
}

rungetMuso <- function(settings,parameters=c(" ECOPHYS"),timee="y"){
  #spinup run
  # changemulline(type=1,setup(), parameters[[2]])
  changemulline(settings,parameters)
  setwd(settings$inputloc)
  system(paste("./muso",settings$ininput[1],sep=" "))
  #normal run
  setwd(settings$inputloc)
  system(paste("./muso",settings$ininput[2],sep=" "))
  
  switch(timee,
        "d"=(Reva<-getdailyout(settings)),
        "m"=(Reva<-getmonthlyout(settings)),
        "y"=(Reva<-getyearlyout(settings))
        )
  return(Reva)
}