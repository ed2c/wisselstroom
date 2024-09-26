################################################################################
###
### Makes a bek object
###
################################################################################

# to suppres the "no visible binding for global variable"
utils::globalVariables(c("student_id", "V1", "V12", "V13","V4", "V9","V10",
                         "V11", "V14", "V15", "V16", "V17", "V19",
                         "date_disenrolment", "date_enrolment",
                         "date_graduation", "year_funding", "enrolment_form",
                         "program_form", "sector"))


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
new_bek <- function(df = data.frame()){

  # validation that df is indeed a bek type df
  stopifnot(attributes(df)$comment %in% c("DEFBEK", "HISBEK", "VLPBEK"))

  type_of_bek <- attributes(df)$comment

  date_retrieval_txt <- dplyr::case_when(type_of_bek %in% c("DEFBEK", "VLPBEK") ~ df[1,4],
                                         type_of_bek == "HISBEK" ~ df[1,3])
  date_retrieval <- make_date(date_retrieval_txt)

  # polishing student_id
  df_edited <- df |>
    # if  V2 has a value student_id gets that number , prefixed with a b, short for bsn
    # else it becomes V3, with prefix e for educational number
    # so even if columns V2 and V3 have numbers in common there will not be any confusion
    dplyr::mutate(student_id = dplyr::case_when(V1 %in% c("BLB", "BRD", "BRR", "HRD", "HRR") & !(V2 == "") ~ paste0("b",V2),
                                                V1 %in% c("BLB", "BRD", "BRR", "HRD", "HRR") & V2 == "" ~ paste0("e", V3),
                                                TRUE ~ NA_character_))
  df_rd <- df_edited |>
    dplyr::filter(V1 %in% c("BRD","HRD")) |>
    dplyr::mutate(year_funding = dplyr::case_when(V1 == "BRD" ~ df[1,3],
                                                  V1 == "HRD" ~ V4)) |>
    dplyr::mutate(BRIN = dplyr::case_when(V1 == "BRD" ~ V4,
                                          V1 == "HRD" ~ V5)) |>
    dplyr::mutate(program_code = dplyr::case_when(V1 == "BRD" ~ V9,
                                                  V1 == "HRD" ~ V10)) |>
    dplyr::mutate(program_level = dplyr::case_when(V1 == "BRD" ~ V10,
                                                   V1 == "HRD" ~ V11)) |>
    dplyr::mutate(program_phase = dplyr::case_when(V1 == "BRD" ~ V11,
                                                   V1 == "HRD" ~ V12)) |>
    dplyr::mutate(date_enrolment = dplyr::case_when(V1 == "BRD" ~ V12,
                                                    V1 == "HRD" ~ V13)) |>
    dplyr::mutate(date_disenrolment = dplyr::case_when(V1 == "BRD" ~ V13,
                                                       V1 == "HRD" ~ V14)) |>
    dplyr::mutate(date_enrolment = make_date(date_enrolment)) |>
    dplyr::mutate(date_disenrolment = make_date(date_disenrolment)) |>
    dplyr::mutate(enrolment_form = dplyr::case_when(V1 == "BRD" ~ V15,
                                                    V1 == "HRD" ~ V16)) |>
    dplyr::mutate(program_form = dplyr::case_when(V1 == "BRD" ~ V16,
                                                  V1 == "HRD" ~ V17)) |>
    dplyr::mutate(sector = dplyr::case_when(V1 == "BRD" ~ V19,
                                            V1 == "HRD" ~ V22)) |>
    # only the columns that are actually used
    dplyr::select(student_id,
                  year_funding,
                  BRIN,
                  program_code,
                  program_level,
                  program_phase,
                  date_enrolment,
                  date_disenrolment,
                  enrolment_form,
                  program_form,
                  sector) |>
    dplyr::mutate(academic_year = make_academic_year(date_enrolment))

  df_rr <- df_edited |>
    dplyr::filter(V1 %in% c("BRR","HRR")) |>
    dplyr::mutate(year_funding = dplyr::case_when(V1 == "BRR" ~ df[1,3],
                                                  V1 == "HRR" ~ V4)) |>
    dplyr::mutate(BRIN = dplyr::case_when(V1 == "BRR" ~ V4,
                                          V1 == "HRR" ~ V5)) |>
    dplyr::mutate(program_code = dplyr::case_when(V1 == "BRR" ~ V10,
                                                  V1 == "HRR" ~ V11)) |>
    dplyr::mutate(program_level = dplyr::case_when(V1 == "BRR" ~ V11,
                                                   V1 == "HRR" ~ V12)) |>
    dplyr::mutate(program_phase = dplyr::case_when(V1 == "BRR" ~ V12,
                                                   V1 == "HRR" ~ V13)) |>
    # dplyr::mutate(date_graduation = dplyr::case_when(V1 == "BRR" ~ make_date(V14),
    #                                                  V1 == "HRR" ~ make_date(V15))) |>
    dplyr::mutate(date_graduation = dplyr::case_when(V1 == "BRR" ~ V14,
                                                     V1 == "HRR" ~ V15)) |>
    dplyr::mutate(date_graduation = make_date(date_graduation))|>
    dplyr::mutate(program_form = dplyr::case_when(V1 == "BRR" ~ V15,
                                                  V1 == "HRR" ~ V16)) |>
    dplyr::mutate(sector = dplyr::case_when(V1 == "BRR" ~ V17,
                                            V1 == "HRR" ~ V18)) |>
    # only the columns that are actually used
    dplyr::select(student_id,
                  year_funding,
                  BRIN,
                  program_code,
                  program_level,
                  program_phase,
                  date_graduation,
                  program_form,
                  sector) |>
    dplyr::mutate(academic_year = make_academic_year(date_graduation))

  value <- list(type = type_of_bek,
                "brin_own" = df[1,2],
                "date_retrieval" = date_retrieval,
                "enrolments" = df_rd,
                "degrees" = df_rr)
  class(value) <- "bek"
  value
}



#' Makes a bek object
#'
#' Takes the data.frame resulting from read_bek_data() and returns a list object
#'
#' @param df a data.frame resulting from using the read_bek_data() on the funding csv file
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
#' bek(my_bek_data)
#' }
bek <- function(df = data.frame()){
  # checks for input
  if(!is.data.frame(df)) {stop(paste("The supplied input is of class",class(df),", please input a data.frame."))}
  if(!attributes(df)$comment %in% c("DEFBEK", "HISBEK", "VLPBEK")){stop(paste("The supplied data.frame is not of the expected type. Please use the data.frame resulting from applying `read_bek_data()` on an original bek csv."))}
  new_bek(df)
}
