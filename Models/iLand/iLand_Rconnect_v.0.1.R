library(XML)
library(dplyr) # to manage big dataframes -> check


# set the working directory
setwd("C:/vogt/01 TU-Dresden/04 Kiefer DBU/07 Reisen/Springschool Profound/Kursmaterial/Project Iland/iLand_release0.8/example")

#--/ read the setting and parameters


#----/ Function to read the xml file and change the particular parrameters
# in the soil data

#--/ Names of the possible parameters for the SITE part
#"availableNitrogen"         "soilDepth"                 "pctSand"                   "pctSilt"                  
#"pctClay"                   "youngLabileC"              "youngLabileN"              "youngLabileDecompRate"    
#"youngRefractoryC"          "youngRefractoryN"          "youngRefractoryDecompRate" "somC"                     
#"somN"                      "somDecompRate"             "soilHumificationRate"  

#other parts are found under model settings see link
#########iland.boku.ac.at/project+file#mModel_settings##########


#---/ Settings for the changing
parameterSetup <- list(site = data.frame(parameter = c("availableNitrogen", "soilDepth"),
                                         value = c(47,150)),
                       climate = data.frame(parameter = c("co2concentration", "temperatureShift"),
                                         value = c(350, 1)))

#----------------------
#   Create a function to replace the settings in the model
#----------------------
setParametersXML <- function(parameterSetup, fileName = "testName"){
  #---/ read the original file and modify the settings
  xmlfile <- xmlRoot(xmlParse("project_HJA_WS12.xml"))
  #--/ change the values of the file
  
  for(GlobSet in names(parameterSetup)){
    for(LocSet in parameterSetup[[GlobSet]]$parameter){
      #--/ Change the value of the file
      xmlValue(xmlfile[["model"]][[GlobSet]][[LocSet]]) <- 
        parameterSetup[[GlobSet]] %>% filter(parameter %in% LocSet) %>% .$value
    }
  }
  
  saveXML(doc = xmlfile, file = paste0(fileName,'.xml'))
}


#---/ Run the model
setParametersXML(parameterSetup, fileName = "testName")

# Bayes exercise
system("../ilandc.exe project_HJA_WS12.xml 500")


#--------------------/
# Some examples of the analysis of the output
#--------------------/
library(RSQLite); library(DBI); library(dplyr); library(ggplot2)

#setwd("~/Documents/2_Work/5_R/6_iLand/R_calibration/iLand_example/output")

# connect to the sqlite file
dbOutput <- dbConnect(RSQLite::SQLite(), dbname="output/HJA_WS12.sqlite")

# get a list of all tables
dbListTables(dbOutput)

# get the populationtable as a data.frame
#carbon <- dbGetQuery( dbOutput,'select * from carbon' )
#carbonflow <- dbGetQuery( dbOutput,'select * from carbonflow' )
stand <- dbGetQuery( dbOutput,'select * from stand' )


stand %>%
  filter(year %in% c(50,100)) %>%
  ggplot(aes(dbh_avg_cm, fill = species)) + 
  geom_histogram(binwidth = 5) +
  theme_classic()+facet_wrap(~year)

