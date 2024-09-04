################################################################################
###
### Makes a vlpbek object
###
################################################################################

# to suppres the "no visible binding for global variable"
utils::globalVariables(c("student_id", "V1", "V12", "V13","V4", "V9","V10",
                         "V11", "V14", "V15", "V16", "V17", "V19",
                         "date_enrolment", "date_graduation"))


# helper functions ------------------------------------------------------------
make_date <- function(date_as_yyyymmdd){
  paste(substr(date_as_yyyymmdd, 1, 4),
        substr(date_as_yyyymmdd, 5, 6),
        substr(date_as_yyyymmdd, 7, 8),
        sep = '-') |>
    as.Date()
}

make_academic_year <- function(date_as_yyyymmdd){
  year <- substr(date_as_yyyymmdd, start = 1, stop = 4)
  month <- substr(date_as_yyyymmdd, start = 6, stop = 7)
  academic_year <- ifelse(month %in% c("09", "10", "11", "12"),
                          paste0(year, "/", as.integer(year)+1),
                          paste0(as.integer(year) -1, "/", year))
  academic_year
}


# makes the actual thing
new_vlpbek <- function(df = data.frame()){

  # validation that df is indeed a vlp type df
  stopifnot(is.data.frame(df))
  stopifnot(df[1,1] == "VLP")
  stopifnot(utils::tail(df, n=1)[1,1] == "SLR")
  stopifnot(names(df)[1] == "V1")
  # polishing student_id
  df_edited <- df |>
    # if  V2 has a value student_id gets that number , prefixed with a b, short for bsn
    # else it becomes V3, with prefix e for educational number
    # so even if columns V2 and V3 have numbers in common there will not be any confusion
    dplyr::mutate(student_id = dplyr::case_when(V1 %in% c("BLB", "BRD", "BRR") & !(V2 == "") ~ paste0("b",V2),
                                                V1 %in% c("BLB", "BRD", "BRR") & V2 == "" ~ paste0("e", V3),
                                                TRUE ~ NA_character_))
   df_brd <- df_edited |>
    dplyr::filter(V1 == "BRD") |>
    dplyr::mutate(V12 = make_date(V12),
                  V13 = make_date(V13)) |>
    # only the columns that are actually used
    dplyr::select(student_id,
                  BRIN = V4,
                  program_code = V9,
                  program_level = V10,
                  program_phase = V11,
                  date_enrolment = V12,
                  date_disenrolment = V13,
                  enrolment_form = V15,
                  program_form = V16,
                  sector = V19) |>
    dplyr::mutate(academic_year = make_academic_year(date_enrolment))

  df_brr <- df_edited |>
    dplyr::filter(V1 == "BRR") |>
    dplyr::mutate(V14 = make_date(V14)) |>
    # only the columns that are actually used
    dplyr::select(student_id,
                  BRIN = V4,
                  program_code = V10,
                  program_level = V11,
                  program_phase = V12,
                  date_graduation = V14,
                  program_form = V15,
                  sector = V17) |>
    dplyr::mutate(academic_year = make_academic_year(date_graduation))

  value <- list("brin_own" = df[1,2],
                "year_funding" = df[1,3],
                "date_retrieval" = make_date(df[1,4]),
                "enrolments" = df_brd,
                "degrees" = df_brr)
  class(value) <- "vlpbek"
  value
}



#' Makes a vlpbek object
#'
#' Takes the data.frame resulting from read_vlpbek_data() and returns a list object
#'
#' @param df a data.frame resulting from using the read_vlpbek_data() on the funding csv file
#'
#' @return a list with 6 objects:
#' \item{brin_own}{text containing the brin of the higher educational institution to which the funding file refers to}
#' \item{year_funding}{text containing the year the funding file refers to}
#' \item{date_retrieval}{date containing the date the funding file was retrieved from DUO}
#' \item{students}{data.frame containing id's from persons with a least one enrolment or degree from the brin concerned }
#' \item{enrolments}{data.frame containing data of enrolments of these students}
#' \item{degrees}{data.frame containing data of acquired degrees of these students }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vlpbek(my_vlpbek_data)
#' }
vlpbek <- function(df = data.frame()){
  # checks for input
  if(!is.data.frame(df)) {stop(paste("The supplied input is of class",class(df),", please input a data.frame."))}
  if(df[1,1] != "VLP") {stop("The supplied data.frame does not contain the right content. Please use the data.frame resulting from applying `read_vlpbek_data()` on an original vlpbek csv.")}
  if(names(df)[1] != "V1") {stop("The supplied data.frame does not contain the right variable names. Please use the data.frame resulting from applying `read_vlpbek_data()` on an original vlpbek csv.")}
  new_vlpbek(df)
}
