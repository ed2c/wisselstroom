################################################################################
###
### Calculates key metrics
###
################################################################################

# to suppres the "no visible binding for global variable" ----------------------
utils::globalVariables(c("BRIN", "academic_year", "brin_situation",
                         "year", "academic_year_min", "academic_year_max",
                         "label", "program_code", "N"))



# helper functions -------------------------------------------------------------

calculate_brin_flow <- function(my_vlpbek, label_brin_own = "no", lang = "EN"){

  # calculate min and max year
  year_min <- min(my_vlpbek$enrolments$academic_year)
  year_max <- max(my_vlpbek$enrolments$academic_year)

  # calculate labels
  label_HE_brin_own <- ifelse(label_brin_own == "no",
                              my_vlpbek$brin_own,
                              label_brin_own)
  label_HE_other <- dplyr::case_when(lang == "EN" ~ "other HE",
                                     lang == "NL" ~ "ander HO",
                                     TRUE ~ "other HEI")
  label_HE_zno <- dplyr::case_when(lang == "EN" ~ "not in HE",
                                   lang == "NL" ~ "niet in HO",
                                   TRUE ~ "not in HE")
  label_HE_brin_ownother <- paste0(label_HE_brin_own, " & ", label_HE_other)

  # make translation table
  dTranslate <- data.frame(term = c("HE_brin_own", "HE_brin_ownother",
                                    "HE_other","HE_zno" ),
                           label = c(label_HE_brin_own, label_HE_brin_ownother,
                                     label_HE_other, label_HE_zno))
  # calculation:
  # summarise into brin_own and/or other, per enrolment
  # to be able to handle the years as variable name, change them to min and max
  flows <- my_vlpbek$enrolments |>
    dplyr::mutate(brin_situation = ifelse(BRIN == my_vlpbek$brin_own,
                                          "brin_own",
                                          "other"),
                  year = ifelse(academic_year == year_max,
                                "academic_year_max",
                                "academic_year_min")) |>
    dplyr::distinct(student_id, brin_situation, year) |>
    dplyr::group_by(student_id,year) |>
    # to make sure "brin_own" before "other"
    dplyr::arrange(brin_situation) |>
    # per student year: is student only in brin_own, in other brin or both?
    dplyr::summarise(brin_situation = dplyr::case_when(
      any(brin_situation == "brin_own") & any(brin_situation == "other") ~ "HE_brin_ownother",
      any(brin_situation == "brin_own") ~ "HE_brin_own",
      any(brin_situation == "other") ~ "HE_other"),
      # to drop the grouping AND prevent messages from showing
      .groups = "drop")|>
    # to make sure after pivot "academic_year_min" is the left column
    dplyr::arrange(dplyr::desc(year)) |>
    tidyr::pivot_wider(names_from = year,
                       values_from = brin_situation,
                       values_fill = "HE_zno") |>
    # count different combinations, so we do not need student_id anymore
    dplyr::select(-student_id) |>
    # count over all combinations
    dplyr::group_by(dplyr::across(dplyr::everything())) |>
    dplyr::summarise(N = dplyr::n(),
                     .groups = "drop") |>
    # translate the labels
    dplyr::left_join(dTranslate, by = c(academic_year_min = "term")) |>
    dplyr::select(-academic_year_min) |>
    dplyr::rename(year_min = label) |>
    dplyr::left_join(dTranslate, by = c(academic_year_max = "term")) |>
    dplyr::select(-academic_year_max) |>
    dplyr::rename(year_max = label) |>
    dplyr::select(year_min, year_max, N)

  names(flows)[1] <- as.character(year_min)
  names(flows)[2] <- as.character(year_max)

  as.data.frame(flows)

}

calculate_from_brins <- function(my_vlpbek){
  # per student/BRIN
  year_min <- min(my_vlpbek$enrolments$academic_year)

  my_vlpbek$enrolments |>
    dplyr::filter(academic_year == year_min) |>
    dplyr::distinct(student_id, BRIN) |>
    dplyr::count(BRIN)
}

calculate_to_brins <- function(my_vlpbek){
  # per student/BRIN
  year_max <- max(my_vlpbek$enrolments$academic_year)

  my_vlpbek$enrolments |>
    dplyr::filter(academic_year == year_max) |>
    dplyr::distinct(student_id, BRIN) |>
    dplyr::count(BRIN)
}


# export function --------------------------------------------------------------

#' Calculates summary metrics from vlpbek object
#'
#' @param my_vlpbek a vlpbek object
#' @param label_brin_own an optional text to use instead of the own BRIN,
#' @param language_choice is either "EN" (default) or "NL",
#'
#' @return a list with 12 objects
#' \item{brin_own}{text containing the brin of the higher educational institution to which the funding file refers to}
#' \item{year_funding}{text containing the year the funding file refers to}
#' \item{date_retrieval}{date containing the date the funding file was retrieved from DUO}
#' \item{academic_years}{vector with the two academic years mentioned in enrolments }
#' \item{n_student}{number of different students mentioned in enrolments}
#' \item{from_BRIN}{data.frame containing BRINs where students were enrolled in the older academic year and count of those students}
#' \item{to_BRIN}{data.frame containing BRINs where students were enrolled in the newer academic year and count of those students}
#' \item{n_program_code}{number of different program codes mentioned in enrolments}
#' \item{n_BRIN_program_code}{number of different combination BRIN/ program code mentioned in enrolments}
#' \item{n_student_BRIN_program_code}{number of different combination student / BRIN/ program code mentioned in enrolments}
#' \item{brin_flows}{data.frame containing enrolment flows from older academic year to newer academic year with regard to being enrolled in own brin or other HEI}
#' @export
#'
#' @examples
#' \dontrun{
#' key_metrics(my_vlpbek, label_brin_own = "My institution")
#' }
key_metrics <- function(my_vlpbek, label_brin_own = "no", language_choice = "EN"){

  # validation of input
  stopifnot(class(my_vlpbek) == "vlpbek")
  stopifnot(language_choice %in% c("EN", "NL"))
  stopifnot(is.character(label_brin_own))
  # metrics

  value = list(brin_own = my_vlpbek$brin_own,
               year_funding = my_vlpbek$year_funding,
               date_retrieval = my_vlpbek$date_retrieval,
               academic_years = sort(unique(my_vlpbek$enrolments$academic_year)),
               n_student = length(unique(my_vlpbek$enrolments$student_id)),
               n_BRIN = length(unique(my_vlpbek$enrolments$BRIN)),
               from_BRIN = calculate_from_brins(my_vlpbek),
               to_BRIN = calculate_to_brins(my_vlpbek),
               n_program_code = length(unique(my_vlpbek$enrolments$program_code)),
               n_BRIN_program_code = my_vlpbek$enrolments |> dplyr::distinct(BRIN, program_code) |> nrow(),
               n_student_BRIN_program_code = my_vlpbek$enrolments |> dplyr::distinct(student_id, BRIN, program_code) |> nrow(),
               brin_flows = calculate_brin_flow(my_vlpbek,
                                                label_brin_own = label_brin_own,
                                                lang = language_choice)
  )
  value

}
