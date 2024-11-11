################################################################################
###
### Finds flows from or to own brin, stack or switch
###
################################################################################




#' Extracts switches or stacks between own brin and another brin
#'
#' @param my_flow_insights a flow_insights object
#' @param stack_or_switch either "switch" for flows without final degree or "stack" for flows  with final degree
#' @param direction "to" the other_brin for outgoing flows, or "from" the other brin for incoming flows
#' @param other_brin the brin from another institution of higher education
#'
#' @return a dataframe with a subset of the switches or stacks dataframe from the flow_insights object
#' @export
#'
#' @examples
#' \dontrun{
#' find_flows(my_insights_object, "switch", "to", "98XX")
#' }
find_flows <- function(my_flow_insights,
                       stack_or_switch = c("switch", "stack"),
                       direction= c("to", "from"),
                       other_brin){

  stopifnot(class(my_flow_insights) == "flow_insights")
  if(missing(other_brin)) {
    stop(paste("This function requires 'other_brin' to be specified"))
  }
  if(stack_or_switch == "switch" & direction == "from"){
    return( my_flow_insights$switches |>
              dplyr::filter(from_brin == other_brin,
                            stringr::str_detect(to_enrolments,
                                                my_flow_insights$brin_own)))
  }
  if(stack_or_switch == "switch" & direction == "to"){
    return( my_flow_insights$switches |>
              dplyr::filter(from_brin == my_flow_insights$brin_own,
                            stringr::str_detect(to_enrolments,
                                                other_brin)))
  }
  if(stack_or_switch == "stack" & direction == "from"){
    return( my_flow_insights$stacks |>
              dplyr::filter(from_brin == other_brin,
                            stringr::str_detect(to_enrolments,
                                                my_flow_insights$brin_own)))
  }
  if(stack_or_switch == "stack" & direction == "to"){
    return( my_flow_insights$stacks |>
              dplyr::filter(from_brin == my_flow_insights$brin_own,
                            stringr::str_detect(to_enrolments,
                                                other_brin)))
  }
}
