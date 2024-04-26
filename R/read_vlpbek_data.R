################################################################################
###
### Reads a vlpbek file
###
################################################################################

# helper function
is_vlpbek_data <- function(chosen_locationname) {
  implied_filename <- stringr::str_sub(chosen_locationname,
    start = -29,
    end = -1
  )
  # should start with "VLPBEK_" and should end with ".csv"
  looks_like_vlpbek_data <- stringr::str_sub(implied_filename, start = 1, end = 7) == "VLPBEK_" &
    stringr::str_sub(implied_filename, start = -4, end = -1) == ".csv"
  looks_like_vlpbek_data
}


#' Read in a vlpbek file
#'
#' `read_vlpbek_data` reads in a vlpbek type of file (bekostigingsbestand), containing interim data about funding.
#'  The function opens a window with which a file can be chosen.
#'
#' @param locationname_vlpbek location of the vlpbek type of file (bekostigingsbestand)
#'
#' @return a dataframe containing the raw vlpbek file
#' @export
#'
read_vlpbek_data <- function(locationname_vlpbek) {

  if (!is_vlpbek_data(locationname_vlpbek)) stop("The name of the chosen file is not compatible with a VLPBEK file. Be sure to select a .csv file that starts with `VLPBEK_`.")

  vlpbek <- utils::read.csv(
    file = locationname_vlpbek,
    sep = "|",
    colClasses = "character",
    header = FALSE
  )
  vlpbek
}
