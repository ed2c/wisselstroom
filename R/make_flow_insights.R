################################################################################
###
### Makes insights from a flow_basics object
###
################################################################################

# to suppres the "no visible binding for global variable" ----------------------
utils::globalVariables(c("program_level", "program_phase", "n_enrol",
                         "enrolment_type",
                         "date_graduation_D","date_graduation_B",
                         "date_graduation_A","date_graduation_M",
                         "BRIN.x", "program_code.x",
                         "BRIN.y", "program_code.y",
                         "from_BRIN", "to_BRIN",
                         "from_program_code", "to_program_code",
                         "n_students",
                         "academic_year", "BRIN", "program_code",
                         "situation_brin", "situations_brin",
                         "situations_level", "enrolment",
                         "situation_degree", "situations_degree",
                         "all_enrolments", "all_enrolments_maxyear",
                         "final_degree", "n_enrolments",
                         "n_enrolments_maxyear"

))

# helper function that adds a column if it does not yet exists
# thanks to https://www.statology.org/r-add-column-if-does-not-exist/

add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] <- NA
  return(df)
}

# helper functions used in enrolment_degree_compact calculation

all_new_enrolments_nextyear <- function(enrolments_thisyear, enrolments_nextyear){
  old_enrolments <- stringr::str_split_1(enrolments_thisyear,
                                         pattern = " | ")
  old_enrolments <- old_enrolments[nchar(old_enrolments)>1]
  old_enrolments
  # all old enrolments must not be in enrolments_nextyear
  !any(stringr::str_detect(enrolments_nextyear, old_enrolments))
}

any_new_enrolments_nextyear <- function(enrolments_thisyear, enrolments_nextyear){
  if (is.na(enrolments_nextyear)) {
    return(NA)
  }
  new_enrolments <- stringr::str_split_1(enrolments_nextyear,
                                         pattern = " | ")
  new_enrolments <- new_enrolments[nchar(new_enrolments)>1]
  any(stringr::str_detect(enrolments_thisyear, new_enrolments, negate = TRUE))
}





#' Makes insights in flows from a flow_basics oject
#'
#' @param my_flow_basics a flow_basics object
#'
#' @return a flow_insights object as a list with 6 objects
#' \item{type}{text containing the type of bek file the data is from}
#' \item{brin_own}{text containing the brin of the higher educational institution to which the funding file refers to}
#' \item{enrolments_degrees_compact}{data frame with one row per academic year, student, brin and program_code, adorned with date_degree when applicable and a note if the student has at most a single enrolment per year}
#' \item{switches}{dataframe with columns from_brin, from_program_code, to_brin_to_program_code, n_students for all students in df enrolment_degrees_compact, that have at most one enrolment per year, and did not receive a diploma in first year}
#' \item{summary_situations_brin}{situations_brin per academic year summarised over students}
#' \item{summary_situations_level}{situations_level per academic year summarised over students}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_flow_insights(my_flow_basics)
#' }
make_flow_insights <- function(my_flow_basics){

  # validation of input type
  stopifnot(class(my_flow_basics) == "flow_basics")

  # helpers
  min_academic_year <- min(my_flow_basics$enrolments$academic_year)
  max_academic_year <- max(my_flow_basics$enrolments$academic_year)

  type <- my_flow_basics$type
  brin_own <- my_flow_basics$brin_own


  degrees_compact <- suppressMessages(
    my_flow_basics$degrees |>
      dplyr::group_by(academic_year, student_id, BRIN, program_code,
                      program_level, program_phase) |>
      dplyr::summarise(date_graduation = min(date_graduation)) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(names_from = program_phase,
                         values_from = date_graduation,
                         names_prefix = "date_graduation_")
  ) |>
    add_cols( c("date_graduation_A", "date_graduation_B", "date_graduation_M", "date_graduation_D")) |> suppressMessages()

  enrolments_compact <- my_flow_basics$enrolments |>
    # not program phase, this will make distinction between prop and main phase
    dplyr::group_by(academic_year, student_id, BRIN, program_code,program_level) |>
    dplyr::summarise(program_form = paste(unique(program_form), collapse = " | "),
                     date_enrolment = min(date_enrolment),
                     date_disenrolment = max(date_disenrolment)) |>
    dplyr::ungroup() |> suppressMessages()


  # determine whether student has in some year more than 1 enrolment
  enrolment_types <- enrolments_compact |>
    dplyr::count(student_id, academic_year, name = "n_enrol") |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(n_max = max(n_enrol)) |>
    dplyr::ungroup() |>
    dplyr::mutate(enrolment_type = dplyr::case_when(n_max > 1 ~ "multiple",
                                                    TRUE ~ "single")) |>
    dplyr::select(student_id, enrolment_type)

  # in object
  enrolments_degrees_compact <- enrolments_compact |>
    #  matches on academic_year, student_id, BRIN, program_code, program_level
    dplyr::left_join(degrees_compact,
                     by = dplyr::join_by(academic_year, student_id, BRIN, program_code, program_level)) |>
    #  adds enrolmentype of student: single: max 1 per year
    dplyr::left_join(enrolment_types,
                     by = dplyr::join_by(student_id)) |>
    dplyr::mutate(enrolment = paste(program_level, BRIN, program_code, sep = "-")) |>
    # add sit_degree, NA when no degree
    dplyr::mutate(situation_degree = dplyr::case_when(!is.na(date_graduation_M) ~ "M",
                                                      !is.na(date_graduation_A) ~ "A",
                                                      !is.na(date_graduation_D) & !is.na(date_graduation_B) ~ "DB",
                                                      !is.na(date_graduation_D) ~ "D",
                                                      !is.na(date_graduation_B) ~ "B")) |>
    # add sits_brin and sits_level, per student/year, looking at all the enrolments for the student in that year
    dplyr::mutate(situation_brin = ifelse(BRIN == my_flow_basics$brin_own,
                                          "brin_own",
                                          "other HE")) |>
    dplyr::group_by(academic_year, student_id) |>
    dplyr::mutate(situations_brin = paste(sort(unique(situation_brin)),
                                          collapse = " & ")) |>
    dplyr::mutate(situations_level = paste(sort(unique(program_level)),
                                           collapse = " & ")) |>
    dplyr::mutate(situations_degree = paste(sort(unique(situation_degree)),
                                            collapse = " & ")) |>
    dplyr::mutate(n_enrolments = dplyr::n()) |>
    dplyr::mutate(all_enrolments = paste(enrolment, collapse = " | ")) |>
    dplyr::ungroup() |>
    # for maxyear degrees are not complete
    dplyr::mutate(final_degree = situation_degree %in% c("A", "B","M", "DB"),
                  final_degree = ifelse(is.na(final_degree),
                                        FALSE,
                                        final_degree)) |>
    dplyr::group_by(student_id) |>
    # columns specially for minyear enrolments, to determine flow_to
    dplyr::mutate(n_final_degrees_minyear = ifelse(academic_year == min_academic_year,
                                                   sum(final_degree),
                                                   NA)) |>
    dplyr::mutate(all_enrolments_maxyear = ifelse(academic_year == min_academic_year,
                                                  unique(all_enrolments[academic_year == max_academic_year]),
                                                  NA)) |>
    dplyr::mutate(n_enrolments_maxyear = ifelse(academic_year == min_academic_year,
                                                unique(n_enrolments[academic_year == max_academic_year]),
                                                NA)) |>
    dplyr::mutate(n_enrolments_maxyear = ifelse(is.na(n_enrolments_maxyear),
                                                0,
                                                n_enrolments_maxyear)) |>
    dplyr::mutate(situations_brin_maxyear = dplyr::case_when(
      academic_year == min_academic_year & n_enrolments_maxyear == 0 ~ "outside HE",
      academic_year == min_academic_year & n_enrolments_maxyear > 0 ~  situations_brin[academic_year == max_academic_year][1])
    )|>
    dplyr::ungroup() |>
    dplyr::mutate(suffix = ifelse(academic_year == min_academic_year,
                                  paste("(", n_enrolments, "_",n_enrolments_maxyear,")", sep = ""),
                                  NA)) |>
    dplyr::mutate(enrolment_stays = dplyr::case_when(academic_year == min_academic_year & is.na(all_enrolments_maxyear) ~ FALSE,
                                                     academic_year == min_academic_year ~ stringr::str_detect(all_enrolments_maxyear, pattern = enrolment),
                                                     academic_year == max_academic_year ~ NA)) |>
    dplyr::mutate(student_stays = dplyr::case_when(academic_year == min_academic_year & !is.na(all_enrolments_maxyear) ~ TRUE,
                                                   academic_year == min_academic_year & is.na(all_enrolments_maxyear) ~ FALSE,
                                                   academic_year == max_academic_year ~ NA)) |>
    # all enrolments in minyear end in final degree
    dplyr::mutate(all_final_degrees_minyear = dplyr::case_when(academic_year == min_academic_year & n_enrolments == n_final_degrees_minyear ~ TRUE,
                                                               academic_year == min_academic_year & n_enrolments != n_final_degrees_minyear ~ FALSE,
                                                               TRUE ~ NA)) |>
    # any enrolments in minyear end in final degree
    dplyr::mutate(any_final_degrees_minyear = dplyr::case_when(academic_year == min_academic_year & n_final_degrees_minyear >=1 ~ TRUE,
                                                               academic_year == min_academic_year & n_final_degrees_minyear == 0 ~ FALSE,
                                                               TRUE ~ NA)) |>

    # are all enrolments of the student next year new?
    dplyr::rowwise() |>
    dplyr::mutate(all_new_nextyear = all_new_enrolments_nextyear(all_enrolments, all_enrolments_maxyear)) |>
    dplyr::mutate(any_new_nextyear = any_new_enrolments_nextyear(all_enrolments, all_enrolments_maxyear)) |>
    dplyr::ungroup() |>
    # start flow calculation
    dplyr::mutate(flow_to = dplyr::case_when(
      academic_year == min_academic_year & enrolment_stays ~ "stay",
      academic_year == min_academic_year & n_enrolments_maxyear == 0 ~ "stop",
      academic_year == min_academic_year & all_final_degrees_minyear & all_new_nextyear ~ "stack",
      academic_year == min_academic_year & all_final_degrees_minyear & !all_new_nextyear ~ "special",
      academic_year == min_academic_year & any_final_degrees_minyear ~ "special",
      academic_year == min_academic_year & all_new_nextyear ~ "switch",
      academic_year == min_academic_year & !any_new_nextyear ~ "stop",
      academic_year == min_academic_year  ~ "special",
      TRUE ~ NA
    ))


  single_enrolments_without_degree <- enrolments_degrees_compact |>
    dplyr::filter(enrolment_type == "single") |>
    # and no degree after the first year ( last year is oke)
    dplyr::filter(academic_year == max_academic_year |
                    academic_year == min_academic_year &(is.na(date_graduation_D) &
                                                           is.na(date_graduation_B) &
                                                           is.na(date_graduation_A)&
                                                           is.na(date_graduation_M))) |>
    dplyr::select(student_id, academic_year ,BRIN, program_code)


  switches <- dplyr::left_join(
    # min academic year
    single_enrolments_without_degree |>
      dplyr::filter(academic_year == min_academic_year)
    ,
    # max academic year
    single_enrolments_without_degree |>
      dplyr::filter(academic_year == max_academic_year)
    ,
    by = c(student_id = "student_id")
  ) |>
    dplyr::count(from_BRIN = BRIN.x,
                 from_program_code = program_code.x,
                 to_BRIN = BRIN.y,
                 to_program_code = program_code.y,
                 name = "n_students"
    ) |>
    # only interested when the from_brin/from_program
    # is different from the to_Brin / to_program
    dplyr::filter(!(from_BRIN == to_BRIN &
                      from_program_code == to_program_code)) |>
    dplyr::arrange(dplyr::desc(n_students))


  summary_situations_brin <- enrolments_degrees_compact |>
    dplyr::distinct(academic_year, student_id, situations_brin) |>
    tidyr::pivot_wider(names_from = academic_year,
                       names_prefix = "situation_brin_",
                       values_from = situations_brin,
                       values_fill = "outside HE") |>
    dplyr::select(-student_id) |>
    dplyr::count(dplyr::across(.cols = 1:2),
                 name = "n_students")


  summary_situations_level <- enrolments_degrees_compact |>
    dplyr::distinct(academic_year, student_id, situations_level) |>
    tidyr::pivot_wider(names_from = academic_year,
                       names_prefix = "situation_level_",
                       values_from = situations_level,
                       values_fill = "outside HE") |>
    dplyr::select(-student_id) |>
    dplyr::count(dplyr::across(.cols = 1:2),
                 name = "n_students")

  #return
  value <- list(type = type,
                brin_own = brin_own,
                enrolments_degrees_compact = enrolments_degrees_compact,
                switches = switches,
                summary_situations_brin = summary_situations_brin,
                summary_situations_level = summary_situations_level
  )
  class(value) <- "flow_insights"
  value
}
