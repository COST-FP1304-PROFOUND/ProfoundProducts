#' This function calls the UNIX(-like) sed program to change specific line to other, using the row numbers.
#' @author Roland
#' @param The name of the file which is needed to be changed in some lines, the numbers of this lines(vector), and
#' the contents(vector).
#' @return void


changspecline <- function(filename, line_number,content){
  #This function calls the UNIX(-like) sed program to change specific line to other, using the row numbers.
  for_command_line <- paste("sed -i '",line_number,"s/.*/",content,"/'"," ",filename, sep="")
  system(for_command_line)
}

changemulline <- function(settings,contents){
  #This is the function which is capable change multiple specific lines to other using their row numbers.
  #The function uses the previous changspecline function to operate.
  varnum <- length(settings$calibrationpar)
  if(length(contents)!=varnum)
  {
    cat("Error: number of the values is not the same as the number of the changed parameters")
  }
  
  for(i in 1:varnum){
    changspecline(settings$epcinput,settings$calibrationpar[i], contents[i] )
  }
}

