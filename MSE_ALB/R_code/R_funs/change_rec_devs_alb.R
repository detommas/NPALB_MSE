#' Replace recruitment deviations
#'
#' This function replaces the recruitment deviations in the
#' ss3.par file with those specified. It then writes a new file 
#'
#' @param recdevs_new A vector of new recruitment deviations.
#' @template par_file_in
#' @template par_file_out
#' @return A modified SS3 .par file.
#' @author Cole Monnahan and D. Tommasi (this is a modified ss3.sim file)


change_rec_devs_alb <- function(recdevs_new, par_file_in,
                            par_file_out){
  
  ## This is the pattern on the line before the vector of current recdevs
  pattern <- "# recdev2"
  
  par <- readLines(par_file_in, warn = FALSE)
  which.line <- grep(pattern=pattern, x=par)+1
  
  ## grab the old ones, note there is a leading space that needs to be
  ## deleted
  recdevs.old <- par[which.line]
  recdevs.old <- gsub("^\\s+|\\s+$", "", recdevs.old) # remove leading blank
  recdevs.old <- gsub("\\s+", " ", recdevs.old)       # remove >1 blanks
  recdevs.old <- as.numeric(unlist(strsplit(recdevs.old, split= " ")))
  
  ##  Add extra recdevs for the next assessement period:
  recdevs_tot <- c(recdevs.old, recdevs_new)
  
  ## replace w/ new recdevs, adding back in that leading space
  par[which.line] <- paste0(" ", recdevs_tot, collapse="")
  
  ## Write it back to file
  writeLines(par, con = par_file_out)
}