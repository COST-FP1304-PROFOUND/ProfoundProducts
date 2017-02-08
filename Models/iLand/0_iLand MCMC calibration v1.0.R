#---/ Load the libraries
library(RSQLite); library(DBI);library(dplyr);library(ggplot2); library(reshape)
library(BayesianTools)
library(coda)

#--- Set the working directory (where is the program loccated)
#setwd("~/Documents/2_Work/5_R/6_iLand/R_calibration/")
setwd("~/Documents/2_Work/5_R/6_iLand/R_calibration/iLand_example/")
setwd("C:/vogt/01 TU-Dresden/04 Kiefer DBU/07 Reisen/Springschool Profound/Kursmaterial/Project Iland/iLand_release0.8/iland_calibration")

#---------------------
#   1. Create the data with the noise - OBSERVATION DATA
#---------------------
# connect to the sqlite file
dbOutput <- dbConnect(RSQLite::SQLite(), dbname="output/Default20ha.sqlite")
#dbListTables(dbOutput)

# Connect the table of the database
stand <- dbGetQuery( dbOutput,'select * from stand' )

# Modify the data and create the observation data
set.seed(2)

stand <- 
  stand %>%
  group_by(year) %>%
  summarise(count_ha = mean(count_ha, na.rm = T),
            basal_area_m2 = mean(basal_area_m2, na.rm = T),
            LAI = mean(LAI, na.rm = T)) %>%
  data.frame(stringsAsFactors = F) %>%
  melt(id = "year") %>%
  group_by(variable) %>%
  mutate(sdVal = sd(value)*0.3,
         llVals = rnorm(n = length(value), mean = 0, sd = sdVal),
         obsVals = value + llVals,
         obsVals = ifelse(obsVals < 0, 0, obsVals)) 

standOrigin <- stand %>%
  select(year, variable, obsVals) %>%
  cast(year ~ variable, value = "obsVals")

sdOutput <- stand %>%
  group_by(variable) %>%
  summarise(sdVal = mean(sdVal))
  
  #--/ Plot the graph with the data
  stand %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = obsVals), col = "grey50") +
  geom_line(aes(y = value), size = 1.2, col = 'red') +
  theme_classic() +
  facet_wrap(~variable, ncol = 1, scale = "free_y")+
  ylab("Calculated values")



#----------------/
#  2. Create a likelihood function for the program
#----------------/ 

  #!!!! IMPORTANT. before executing, execute functions in the end of the script
  
  #----------------/
  #  9. Additional function
  #----------------/ 
  
  #---/ replace the parameters in the SQL model
  setParametersSQL <- function(parameterSetup){
    
    # connect to the sqlite file
    dbOutput <- dbConnect(RSQLite::SQLite(), dbname="database/PNWparam.sqlite")
    
    for(parSQL in parameterSetup$parameter){
      valSQL <- parameterSetup %>% filter(parameter %in% parSQL) %>% .$best
      #--/Change the parameters
      sql<-paste0("UPDATE species SET ", parSQL, "=", valSQL, " WHERE shortName IN ('Psme')")
      dbGetQuery(dbOutput, sql) 
    }
  }
  
  
  #---/ Parameters set up. Set the minimum and maximum values for the parameters
  parameterSetup <- data.frame(parameter = c("specificLeafArea", "maximumAge", 
                                             "maximumHeight", "fecundity_m2"),
                               best = c(5.8, 1100, 100, 20),
                               lower = c(5, 950, 85, 15),
                               upper = c(6.5, 1250, 115, 25),
                               default = c(5.8, 1100, 100, 20))
  
  #parVals <- c(5,950,85,15)
  
  #---/ Likelihood function for calculations
  likelihood <- function(parVals){
    
    # step 1. replace the values in the sql data
    parameterSetup$best <- parVals #-/change for the new values
    setParametersSQL(parameterSetup)
    
    # step 2. run the iLand
    system("ilandc.exe project_HJA_WS12.xml 300")
    
    # step 3. load the output of the iLand and calculate teh difference with observed data
      # reload the new output of the iLand
    # connect to the sqlite file
    dbTempOut <- dbConnect(RSQLite::SQLite(), dbname="output/HJA_WS12.sqlite")
    #dbListTables(dbOutput)
    standObserv <- dbGetQuery( dbTempOut,'select * from stand' ) %>%
      group_by(year) %>%
      summarise(count_ha = mean(count_ha, na.rm = T),
                basal_area_m2 = mean(basal_area_m2, na.rm = T),
                LAI = mean(LAI, na.rm = T)) 
    
    
    diffOutput <- standOrigin[,2:4] - standObserv[,2:4]
    diffOutput <- diffOutput[51:300,] #take only from 100 years onwards
    
    llValues <- sum(sum(dnorm(diffOutput[,1], sd = max(abs(c(standOrigin[51:300,1])),0.0001) * sdOutput$sdVal[1], log = T))+
          sum(dnorm(diffOutput[,2], sd = max(abs(c(standOrigin[51:300,2])),0.0001) * sdOutput$sdVal[2], log = T))+
          sum(dnorm(diffOutput[,3], sd = max(abs(c(standOrigin[51:300,3])),0.0001) * sdOutput$sdVal[3], log = T))
    )
     
    return(llValues)
  }
  
  #' ## Define the prior
  prior <- createUniformPrior(lower = parameterSetup$lower, upper = parameterSetup$upper)
  
  
  #' ## Create Bayesian Setup
  #' The last step creates the standardized object that the package uses for the 
  #' sampler. 
  
  BSpreles <- createBayesianSetup(likelihood, prior, 
                                  best = parameterSetup$default, names = parameterSetup$parameter,
                                  parallel = F)
  
  #' ## Running the sampler
  #' 
  #' For each sampler three chains are run. This not only gives a better data basis 
  #' but is also necessary for the convergence diagnostic.
  #' 
  
  #+ message=FALSE, echo=TRUE, results='hide'
  # 1000 iterations
  settings <- list(iterations = 1000, optimize=F, nrChains = 2)
  chainDE <- runMCMC(BSpreles, sampler="DEzs", settings = settings)
  
  # only 6 for comparison
  settings6 <- list(iterations = 6, optimize=F, nrChains = 2)
  chainDE6 <- runMCMC(BSpreles, sampler="DEzs", settings = settings6)
  ?runMCMC
  
  
  
  
  # Lets look at the output of the data
  plot(chainDE6)
  #tracePlot(chainDE)
  
  
  marginalPlot(chainDE6[[1]],bounds =parameterSetup[,3:4], true =parameterSetup[,5])# start = 10) 
  #head(parameterSetup)
  correlationPlot(chainDE6[[1]])
  summary(chainDE6)
 
 
  #---/ Ploting the observed values with the error from the model 
  #to be modified
  runModel <- function(parVals){
    parameterSetup$best <- parVals #-/change for the new values
    setParametersSQL(parameterSetup)
    
    # step 2. run the iLand
    system("ilandc.exe project_HJA_WS12.xml 300")
    
    # step 3. load the output of the iLand and calculate teh difference with observed data
    # reload the new output of the iLand
    # connect to the sqlite file
    dbTempOut <- dbConnect(RSQLite::SQLite(), dbname="output/HJA_WS12.sqlite")
    #dbListTables(dbOutput)
    standObserv <- dbGetQuery( dbTempOut,'select * from stand' ) %>%
      group_by(year) %>%
      summarise(count_ha = mean(count_ha, na.rm = T),
                basal_area_m2 = mean(basal_area_m2, na.rm = T),
                LAI = mean(LAI, na.rm = T)) %>%
      data.frame()
    return(standObserv[,2])
  }
  
  
  
  errorFunction <- function(mean, parVals){
      rnorm(length(mean), mean = mean, sd = max(abs(c(standOrigin[51:300,1])),0.0001) * sdOutput$sdVal[1])
  }

  plotTimeSeriesResults(x = chainDE6[[1]], model = runModel, 
                        observed = standOrigin[,2], error = errorFunction)
  
  
  
  #---/ plot the main graphs with the error etc.
  
  parameterSetup$best <- summary(chainDE6)[[1]][,1] #-/change for the new values
  setParametersSQL(parameterSetup)
  
  # step 2. run the iLand
  system("ilandc.exe project_HJA_WS12.xml 300")
  
  # step 3. load the output of the iLand and calculate teh difference with observed data
  # reload the new output of the iLand
  # connect to the sqlite file
  dbTempOut <- dbConnect(RSQLite::SQLite(), dbname="output/HJA_WS12.sqlite")
  #dbListTables(dbOutput)
  standObserv <- dbGetQuery( dbTempOut,'select * from stand' ) %>%
    group_by(year) %>%
    summarise(count_ha = mean(count_ha, na.rm = T),
              basal_area_m2 = mean(basal_area_m2, na.rm = T),
              LAI = mean(LAI, na.rm = T))
  
  
  ggplot()+
    geom_point(data = standOrigin, aes(year, count_ha), col = 'grey50')+
    geom_line(data = standObserv, aes(year, count_ha), col = 'red')+
    theme_classic()
  
  
  
  
  # convergence diagnostics
 
  # chain2 = mcmc(run_metropolis_MCMC(startvalue, 10000)[5000:ncol(chain),], start
 #               = 5000)
 # combinedchains = mcmc.list(chain, chain2)
  plot(combinedchains)
  
  
  gelman.diag(combinedchains)
  cumuplot() # to be modified
  gelman.plot(combinedchains)
  
  #p-values
  

  
  

