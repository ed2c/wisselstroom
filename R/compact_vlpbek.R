################################################################################
###
### Makes a summary from a vlpbek object
###
################################################################################

# to suppres the "no visible binding for global variable" ----------------------
utils::globalVariables(c("program_level", "program_phase", "n_enrol",
                         "enrolment_type", "brinsit", "levelsit",
                         "date_graduation_D","date_graduation_B",
                         "date_graduation_A","date_graduation_M",
                         "BRIN.x", "program_code.x",
                         "BRIN.y", "program_code.y",
                         "from_BRIN", "to_BRIN",
                         "from_program_code", "to_program_code",
                         "n_students",
                         "academic_year", "BRIN", "program_code","brin_situation"
                         ))

# helper function that adds a column if it does not yet exists
# thanks to https://www.statology.org/r-add-column-if-does-not-exist/

add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] <- NA
  return(df)
}


#' Makes a compacter version of a vlpbek object, with some summaries
#'
#' @param my_vlpbek a vlpbek object
#'
#' @return a vlpbek_object as a list with 7 objects
#' \item{brin_own}{text containing the brin of the higher educational institution to which the funding file refers to}
#' \item{enrolments_degrees_compact}{data frame with one row per academic year, student, brin and program_code, adorned with date_degree when applicable and a note if the student has at most a single enrolment per year}
#' \item{presences_brin}{dataframe with one row per student per academic year displaying enrolmentinformation regarding brin: either in brin_own and/or other HE, or outside HE }
#' \item{presences_level}{dataframe with one row per student per academic year displaying enrolmentinformation regarding level: HBO-AD, HBO-BA, HBO-MA, WO-BA, WO-MA }
#' \item{switches}{dataframe with columns from_brin, from_program_code, to_brin_to_program_code, n_students for all students in df enrolment_degreees_compact, that have at most one enrolment per year, and did not receive a diploma in first year}
#' \item{summary_presences_brin}{presences_brin per academic year summarised over students}
#' \item{summary_presences_level}{presences_leveln per academic year summarised over students}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' vlpbek_compact(my_vlpbek)
#' }
compact_vlpbek <- function(my_vlpbek){

  # validation of input type
  stopifnot(class(my_vlpbek) == "vlpbek")

  # helpers
  min_academic_year <- min(my_vlpbek$enrolments$academic_year)
  max_academic_year <- max(my_vlpbek$enrolments$academic_year)
  brin_own <- my_vlpbek$brin_own

  degrees_compact <- my_vlpbek$degrees |>
    dplyr::distinct(academic_year, student_id, BRIN, program_code,
                    program_level, program_phase, date_graduation) |>
    tidyr::pivot_wider(names_from = program_phase,
                       values_from = date_graduation,
                       names_prefix = "date_graduation_") |>
    add_cols(c("date_graduation_A",
               "date_graduation_B",
               "date_graduation_M",
               "date_graduation_D"))

  enrolments_compact <- my_vlpbek$enrolments |>
    # not program phase, this will make distinction between prop and main phase
    dplyr::distinct(academic_year, student_id, BRIN, program_code,
                    program_level)

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
                     by = dplyr::join_by(student_id))




  presences_brin <- enrolments_compact |>
    dplyr::mutate(brin_situation = ifelse(BRIN == my_vlpbek$brin_own,
                                          "brin_own",
                                          "other HE")) |>
    dplyr::distinct(academic_year, student_id, brin_situation) |>
    # to have "brin_own" before "other HE"
    dplyr::arrange(brin_situation) |>
    dplyr::group_by(student_id, academic_year) |>
    # alternative calc
    dplyr::summarise(brinsit = paste(brin_situation, collapse = " & "),
                     # to drop the grouping AND prevent messages from showing
                     .groups = "drop") |>

    tidyr::pivot_wider(names_from = academic_year,
                       names_prefix = "brin_situation_",
                       values_from = brinsit,
                       values_fill = "outside HE",
                       # to have the new columns in alphabetical order
                       names_sort = TRUE)

  presences_level <- enrolments_compact |>
    dplyr::distinct(academic_year, student_id, program_level) |>
    dplyr::group_by(student_id, academic_year) |>
    dplyr::arrange(program_level) |>
    # alternative calc
    dplyr::summarise(levelsit = paste(program_level, collapse = " & "),
                     # to drop the grouping AND prevent messages from showing
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = academic_year,
                       names_prefix = "level_situation_",
                       values_from = levelsit,
                       values_fill = "outside HE",
                       # to have the new columns in alphabetical order
                       names_sort = TRUE)

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

  summary_presences_brin <- dplyr::count(presences_brin,
                                         presences_brin[,2:3],
                                         name = "n_students")
  summary_presences_level <- dplyr::count(presences_level,
                                          presences_level[,2:3],
                                          name = "n_students")

  #return
  value <- list(brin_own = brin_own,
                enrolments_degrees_compact = enrolments_degrees_compact,
                presences_brin = presences_brin,
                presences_level = presences_level,
                switches = switches,
                summary_presences_brin = summary_presences_brin,
                summary_presences_level = summary_presences_level
                )
  class(value) <- "vlpbek_compact"
  value
}

