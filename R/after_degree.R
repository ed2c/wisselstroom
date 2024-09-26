################################################################################
###
### Gives insight in enrolments year after degree
###
################################################################################

# to suppres the "no visible binding for global variable" ----------------------
utils::globalVariables(c("brin_from",
                         "brin_to",
                         "date_degree",
                         "program_from",
                         "program_level_from",
                         "program_level_to",
                         "program_to",
                         "student"
))

#' Gives insight in enrolments year after degree
#'
#' @param program_of_interest string containing the 5-character program code of interest
#' @param my_bek_compact a bek_compact object
#' @param prop_exam boolean, default FALSE, set to TRUE for after propedeutic exam
#'
#' @return a list with 10 objects
#' \item{program_of_interest}{the program code that was entered}
#' \item{degree}{D for propedeutic, A for AD, B for Bachelor or M for Master}
#' \item{brin_own}{brin_own from the my_bek_compact object}
#' \item{academic_year_from}{the earliest academic year in the data}
#' \item{academic_year_to}{the latest academic year in the data}
#' \item{n_students}{number of students with a degree from brin_own/program_of_interest in academic_year_from}
#' \item{n_students_enrolment_type_single}{number of above students with at most one enrolment brin/program per academic year}
#' \item{n_students_enrolment_type_multiple}{difference between the two numbers of students}
#' \item{from_to_single}{dataframe with 7 columns with counts per flow, for the students with enrolment_type = single}
#' \item{from_to_multiple}{dataframe with 7 columns with flow information per (anonymized) student, for the students with enrolment_type = single }
#' @export
#'
#' @examples
#' \dontrun{
#' after_degree(program_of_interest = "34507",
#'  my_bek_compact = my_compact_thing,
#'   prop_exam = TRUE)
#' }
after_degree <- function(program_of_interest, my_bek_compact, prop_exam = FALSE){

  # input checks
  stopifnot(class(program_of_interest) == "character")
  stopifnot(nchar(program_of_interest) == 5)
  stopifnot(as.integer(program_of_interest) > 0)
  stopifnot(class(my_bek_compact) == "bek_compact")
  stopifnot(class(prop_exam) == "logical")
  stopifnot((prop_exam & (substr(program_of_interest,1,1) == "3")) | !prop_exam)

  degree <- dplyr::case_when(substr(program_of_interest,1,1) == "3" & prop_exam ~ "D",
                             substr(program_of_interest,1,1) == "3" ~ "B",
                             substr(program_of_interest,1,1) == "5" ~ "B",
                             substr(program_of_interest,1,1) == "4" ~ "M",
                             substr(program_of_interest,1,1) == "6" ~ "M",
                             substr(program_of_interest,1,1) == "8" ~ "A")

  min_academic_year <- min(my_bek_compact$enrolments_degrees_compact$academic_year)
  max_academic_year <- max(my_bek_compact$enrolments_degrees_compact$academic_year)
  brin_own <- my_bek_compact$brin_own

  # find all students with an enrolment on this program at brin_own with a P
  students_of_interest <- my_bek_compact$enrolments_degrees_compact |>
    dplyr::mutate(date_degree = dplyr::case_when(program_level == "HBO-BA" & prop_exam ~ date_graduation_D,
                                                 program_level == "HBO-BA"  ~ date_graduation_B,
                                                 program_level == "WO-BA"  ~ date_graduation_B,
                                                 program_level == "HBO-MA"  ~ date_graduation_M,
                                                 program_level == "WO-MA"  ~ date_graduation_M,
                                                 program_level == "HBO-AD" ~ date_graduation_A
    )) |>
    dplyr::filter(program_code == program_of_interest,
                  BRIN == brin_own,
                  !is.na(date_degree),
                  academic_year == min_academic_year) |>
    dplyr::distinct(student_id, enrolment_type)

  # added to output
  n_students <- students_of_interest |> nrow()
  n_students_enrolment_type_single <- students_of_interest |>
    dplyr::filter(enrolment_type == "single") |> nrow()
  n_students_enrolment_type_multiple <- n_students - n_students_enrolment_type_single

  # find ALL enrolments for these students
  enrolments_of_interest <- students_of_interest |>
    dplyr::left_join(my_bek_compact$enrolments_degrees_compact,
                     by = dplyr::join_by(student_id,  enrolment_type))

  enrol_from <- enrolments_of_interest |>
    dplyr::filter(academic_year == min_academic_year) |>
    dplyr::select(student_id,
                  brin_from = BRIN,
                  program_from = program_code,
                  program_level_from = program_level,
                  enrolment_type)
  enrol_to <- enrolments_of_interest |>
    dplyr::filter(academic_year == max_academic_year) |>
    dplyr::select(student_id,
                  brin_to = BRIN,
                  program_to = program_code,
                  program_level_to = program_level)

  from_to_raw <- dplyr::left_join(enrol_from,
                                  enrol_to,
                                  by = c(student_id = "student_id"),
                                  relationship = "many-to-many")

  from_to_single <- from_to_raw |>
    dplyr::filter(enrolment_type == "single") |>
    dplyr::count(brin_from,
                 program_from,
                 program_level_from,
                 brin_to,
                 program_to,
                 program_level_to,
                 name = "n_students") |>
    dplyr::arrange(dplyr::desc(n_students))

  # to prevent student_id to be in output
  cases <- students_of_interest |>
    dplyr::filter(enrolment_type == "multiple") |>
    dplyr::mutate(student = dplyr::row_number())

  from_to_multiple <- from_to_raw |>
    dplyr::filter(enrolment_type == "multiple") |>
    dplyr::left_join(cases,
                     by = dplyr::join_by(student_id, enrolment_type)) |>
    dplyr::relocate(student, .before = 1) |>
    dplyr::select(-student_id, -enrolment_type)

  degree <- dplyr::case_when(substr(program_of_interest,1,1) == "3" & prop_exam ~ "D",
                             substr(program_of_interest,1,1) == "3" ~ "B",
                             substr(program_of_interest,1,1) == "5" ~ "B",
                             substr(program_of_interest,1,1) == "4" ~ "M",
                             substr(program_of_interest,1,1) == "6" ~ "M",
                             substr(program_of_interest,1,1) == "8" ~ "A")


  value <- list(program_of_interest = program_of_interest,
                degree = degree,
                brin_own = brin_own,
                academic_year_from = min_academic_year,
                academic_year_to = max_academic_year,
                n_students = n_students,
                n_students_enrolment_type_single = n_students_enrolment_type_single,
                n_students_enrolment_type_multiple = n_students_enrolment_type_multiple,
                from_to_single = from_to_single,
                from_to_multiple = from_to_multiple
  )
  value

}


