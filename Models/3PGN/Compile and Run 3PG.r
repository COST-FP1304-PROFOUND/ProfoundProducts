#'---
#'title: "Compiling & running 3PGN model in R"
#'author: Francesco Minunno
#'email: francesco.minunno@helsinki.fi
#'date: 29 April 2015
#'---
#' 


#'Synopsis: This short tutorial discusses how to compile a Fortran version of 3PGN model and run it in R.
#'
#' Creating a library
#' ===============================
#'
#' from terminal navigate to the 3PGN folder
#'
#' Build the library pasting in the terminal the following string

R CMD SHLIB -o 3PG src/declarations.f90 src/routines.f90 src/model.f90 src/init_model.f90 src/initbc.f90 src/changepars.f90
#' 
#' 

#' Load library and first model run
#' ===============================
#'
#' load the model library and site and climatic variables
 dyn.load("3PG")
#'
 .Fortran('init_model')
#' 
#' Define the function to run 3PGN in R
model_3PGN <- function(pValues){
 .Fortran('changepars', pValues)  
 y <- array(1,dim=c(nMonths,8,nSites))
 output <- .Fortran('model',0,y)[[2]]
}

#' Setting model for runs
nMonths <-156 	# number of months for wich running the model
nSites <- 5 	# number of sites
varnames <- c('standAge','NEP','dbh','H','WF','WR','WS','StandVol') # output variables considered
load('pSet.rdata') #load parameters for Eucalypus Globulus in Portugal

#' run model
out_3PGN <- model_3PGN(pSet)

#'
#'Plot model output
lab <- c('standAge','NEP (Mg C ha⁻¹)','dbh (cm)','H (m)','WF (Mg of dry mass ha⁻¹)','WR (Mg of dry mass ha⁻¹)','WS (Mg of dry mass ha⁻¹)','StandVol (m³ ha⁻¹)') #)
for (i in 2:8){
 plot(out_3PGN[,1,1],out_3PGN[,i,5],type='l',col=5, ylab=lab[i],xlab='Stand Age (years)', main=varnames[i])
 lines(out_3PGN[,1,2],out_3PGN[,i,4],col=4)
 lines(out_3PGN[,1,3],out_3PGN[,i,3],col=3)
 lines(out_3PGN[,1,4],out_3PGN[,i,2],col=2)
 lines(out_3PGN[,1,5],out_3PGN[,i,1],col=1)
 legend('topleft',c('site 1','site 2','site 3','site 4','site 5'),col=1:5,lty=1)
 savePlot(file=paste('output/',varnames[i],'.png'),type='png')
}

for (i in 2:8){
 plot(out_3PGN[,1,1],data[,i,5],type='l',col=5, ylab=lab[i],xlab='Stand Age (years)', main=varnames[i])
 lines(out_3PGN[,1,2],data[,i,4],col=4)
 lines(out_3PGN[,1,3],data[,i,3],col=3)
 lines(out_3PGN[,1,4],data[,i,2],col=2)
 lines(out_3PGN[,1,5],data[,i,1],col=1)
 legend('topleft',c('site 1','site 2','site 3','site 4','site 5'),col=1:5,lty=1)
 savePlot(file=paste('output/',varnames[i],'.png'),type='png')
}
plot(data[,1,1])
