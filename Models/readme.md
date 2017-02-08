PROFOUND MODELS
===

Above an overview of models used within Profound, and potentially available for PROFOUND members (availability differs between the models - some can be installed direcly, some need to be requested from the respective modelling group).



## For developers

Currently, the idea is that we will not have one big PROFOUND R package for the models, but that each modelling group should maintain an own R package or code to call their model from R. We think that this should be easier to maintain an coordinate, also easier to give attribution to the people. 

We would strongly encourage to provide an R package, instead of R code, because this is easier to deploy, forces good documentation, and seems overall without disadvantages compared to maintaining code. But it's your choice. How to make your model callable from R, and how to create an R package, is described in Appendix A of the WG3 tutorial https://www.dropbox.com/s/y3d5fqh61eqdzz6/TG13-ModelCalibrationTutorial.pdf?dl=0 


#### Maintaining the same function interface

While not a fundamental requirement, it would be a great advantage to have a similar interface for the functions in the package, meaning that we all have the same structure of how parameters are provided to the model and so on. This will allow easier coupling to the models to existing frameworks such as [PEcAn](http://pecanproject.github.io/index.html), and it will also minize our work when we do model comparisons.

The current suggestion is that each model package should provide the following functions: 

* runModel(par), parameter vector par
* getData(type)
* (optional) also a runModel(par, type) function that combines the run and get in one step
* (optional) accept also a matrix par, where rows are parameter combinations, to be run in parallel

I would suggest to implement this as follows 

```{r}
#' Runs the model with parameter par, potentially returns output
#'
#' @param par A parameter vector
#' @param type A list of output types, such as GPP, Basal Areal, ...
#' @return a list with the data 
runModel(par, type = NULL){
  
  # run the model with par
  
  if !is.null(type){
    out <- getData(type)
    return(out) 
  }
}

#' Gets the data from the model
#'
#' @param type A list of output types, such as GPP, Basal Areal, ...
#' @return a list with the data 
getData(type){
  
  # for loop over types, read in data
}
```

Because we can't easily paralellize the statistical algorithms if models write to file (because then they may write to the same file location), we suggest that modelling groups should provide additionally a parallel implementation of these functions. The suggested way to do this is the following

* Write a setupParallel(n, options) function that creates an R cluster with n cores and suitable directories for writing the output of parallel model runs, as well as copying files for the parallel run if neccessary.

* The setupParallel function should return an object that has the parallel functions as substructures, so that one can do

```{r}
parModel <- setupParallel(5, ...)
parModel$runModel(parList)
```

where parList could be a list or matrix of any length. For an example how to do all this technically, have a look at the LPJ-GUESS package.


