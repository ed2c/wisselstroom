################################################################################
###
### Reads a bek file
###
################################################################################

# helper function
is_bek_data <- function(chosen_locationname) {
  implied_filename <- stringr::str_sub(chosen_locationname,
                                       start = -29,
                                       end = -1
  )
  # should start with "***BEK_" and should end with ".csv"
  looks_like_bek_data <-
    # filename should start with either "VLP", "DEF" or "HIS"
    stringr::str_sub(implied_filename, start = 1, end = 3) %in% c("VLP", "DEF", "HIS") &
    # then should follow "BEK_"
    stringr::str_sub(implied_filename, start = 4, end = 7) == "BEK_" &
    # end it should end with ".csv"
    stringr::str_sub(implied_filename, start = -4, end = -1) == ".csv"
  looks_like_bek_data
}


#' Read in a bek file
#'
#' `read_bek_data` reads in a bek type of file (bekostigingsbestand), containing data about funding.
#'
#' @param locationname_bek location of the bek type of file (bekostigingsbestand)
#'
#' @return a dataframe containing the raw bek file, with attribute regarding type (vlp, def, his)
#' @export
#'
read_bek_data <- function(locationname_bek) {

  if (!is_bek_data(locationname_bek)) stop("The name of the chosen file is not compatible with a BEK file. Be sure to select a .csv file with a name as `TTTTTT_ 1234_EEJJMMDD_99XX.CSV`.")

  bek <- utils::read.csv(
    file = locationname_bek,
    sep = "|",
    colClasses = "character",
    header = FALSE
  )

  type_of_bek <- stringr::str_sub(locationname_bek,
                                  start = -29,
                                  end = -24)

  comment(bek) <- type_of_bek

  bek
}
