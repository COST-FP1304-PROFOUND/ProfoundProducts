#PROFOUND WG3
library(BayesianTools)
?BayesianTools
vignette("AQuickStart")

# to open Sqlite Data basis
library("RSQLite")
library(DBI)

# direkt das working directory im Example:
setwd("C:/vogt/01 TU-Dresden/04 Kiefer DBU/07 Reisen/Springschool Profound/Kursmaterial/Project Iland/iLand_release0.8/example")

rm(list=ls())
setwd("D:/PROJECTS/COST_Action/FP1304/Training School/Bayes2/4c/")

#species_temp.par
#
#template mit kürzel
templateFile<-file("input/species_template.par", open="rt") #open file connection to read     
templateString <- readLines(templateFile)


#########
#einlesen von species_standard_parameter ==>table mit allen Parametern
# Volodymyr is working on that

fourCparameters <- read.csv("4c_parameter.csv", sep=";") #Einlesen von Parameter Werten/Name
# Nur zum testen
#parameterValues = 1:4

#Runs the model with selected Parameters set to parameterValues, the rest to default, and returns output
runModel <- function(parameterSelection, parameterValues){

#durch alle Parameter durchgehen und gucken ob sie geändert werden sollen, wenn nicht standard  
   for (i in 1:nrow(fourCparameters)){
    
     if (fourCparameters$name[i] %in% parameterSelection){
       position = which(fourCparameters$name[i] == parameterSelection)
       value = parameterValues[position]
     } else {
       value = fourCparameters$value[i]
     }
     
     templateString <- gsub(fourCparameters$name[i], value, templateString)
   }
#   
  parDestination <- file("input/species_neu2.par", open="wt") #open file connection to write
   writeLines(templateString, parDestination)
   close(parDestination)
  

  # check that: ilandc.exe -> direct aud der Konsole starten oder xml file

   system("../ilandc.exe project_HJA_WS12.xml 100") # oder direkt hier 4c_disturb.exe < 4c_batch.txt
  
  output <- read.table("output/hyyti/hy_l15_103_man_veg_pi.out1", header=F, quote="\"", skip = 3)
  

  return(output)
}
# als Output sqlite data to open that in R:
con= dbConnect(RSQLite::SQLite(), dbname="output/HJA_WS12.sqlite")
# get list of all tables
alltables <- dbListTables(con)
alltables
carbon <- dbGetQuery(con,"select* from carbon")



#Was sind die daten? ==> Anpassen um Daten richtig einzulesen (welches Jahr...)

dataDBH = read.table("TestDBH.cvs")[,1]

likelihood <- function(par){
  parameterSelection = fourCparameters$name[fourCparameters$species == "pisy" & fourCparameters$process == "allocation"][17]
  out <- runModel(parameterSelection,par[1])
plot(out[,8])  
#write.table(out[,8], "test"i) 

  ll = dnorm(out[,8], mean = dataDBH, sd = par[2], log = T)
  
  return(sum(ll))
}


par = seq(0.05,0.9, len = 10)
ll = par
for (i in 1:10){
  ll[i] = likelihood(c(par[i], 1))
  #write.table(out[,8], "test"i)
}

plot(par, ll)


