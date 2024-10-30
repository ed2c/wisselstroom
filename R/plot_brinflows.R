################################################################################
###
### Plots global brinflows from flows_insights list
###
################################################################################


# helper function

prepare_brinflows <- function(my_flow_insights,
                              color_brin_own = "#853887",
                              color_other_he = "#246ABE",
                              color_brin_own_other_he = "#00B17E",
                              color_outside_he = "grey",
                              label_brin_own = "this HEI",
                              label_other_he = "other HEI",
                              label_outside_he = "outside HE"){


  # input validity  checks
  stopifnot(nrow(my_flow_insights$summary_presences_brin) > 0)
  stopifnot(class(my_flow_insights) == "flow_insights")

  # combination label
  label_brin_own_other_he <- paste(label_brin_own, label_other_he, sep = " & ")

  # basis data
  brinflows_basis <- my_flow_insights$summary_situations_brin

  # plot data
  plot_data_basis <- brinflows_basis |>
    # change the column names
    dplyr::rename(column_from = names(brinflows_basis)[1]) |>
    dplyr::rename(column_to = names(brinflows_basis)[2]) |>
    dplyr::rename(values = names(brinflows_basis)[3]) |>
    # making the color_from column
    dplyr::mutate(color_from = dplyr::case_when(column_from == "brin_own" ~ color_brin_own,
                                                column_from == "brin_own & other HE" ~ color_brin_own_other_he,
                                                column_from == "other HE" ~ color_other_he,
                                                column_from == "outside HE" ~ color_outside_he),
                  .after = 2) |>
    # making the color_to column
    dplyr::mutate(color_to = dplyr::case_when(column_to == "brin_own" ~ color_brin_own,
                                              column_to == "brin_own & other HE" ~ color_brin_own_other_he,
                                              column_to == "other HE" ~ color_other_he,
                                              column_to == "outside HE" ~ color_outside_he),
                  .after = 3) |>
    # then  change the labels of the column_from and column_to
    dplyr::mutate(column_from = dplyr::case_when(column_from == "brin_own" ~ label_brin_own,
                                                 column_from == "brin_own & other HE" ~ label_brin_own_other_he,
                                                 column_from == "other HE" ~ label_other_he,
                                                 column_from == "outside HE" ~ label_outside_he)) |>
    dplyr::mutate(column_to = dplyr::case_when(column_to == "brin_own" ~ label_brin_own,
                                               column_to == "brin_own & other HE" ~ label_brin_own_other_he,
                                               column_to == "other HE" ~ label_other_he,
                                               column_to == "outside HE" ~ label_outside_he))
  plot_data_basis
}


#' Plots the global flows between brin_own, other HE and not in HE
#'
#' `plot_brinflows()` takes the summary_presences_brin from a flow_insights list, and turns it into a sankey diagram
#'
#' @param my_flow_insights a flows_insights object
#' @param color_brin_own string containing color code used for brin_own
#' @param color_other_he string containing color code used for other HE
#' @param color_brin_own_other_he string containing color code used for brin_own & other HE
#' @param color_outside_he string containing color code used for outside HE
#' @param label_brin_own string to use instead of "brin_own"
#' @param label_other_he string to use instead of "other_HE"
#' @param label_outside_he string to use instead of "outside_HE"
#' @param padding constant used for displaying space between flows
#' @param alpha numeric, transparancy of colors (0 = complete transparent, 1 = opaque)
#' @param display_labels boolean used for displaying labels or not
#' @param match_label_color boolean used to indicate labels in same color as nodes
#' @param label_fontface string used for styling the font of labels
#' @param label_size number indicating the font size of labels
#'
#' @return a ggplot, displaying the flows between brin_own, other HE and not in HE
#' @export
#'
#' @examples
#' \dontrun{
#' plot_brinflows(my_flow_insights)
#' }
plot_brinflows <-function(my_flow_insights,
                          color_brin_own = "#853887",
                          color_other_he = "#246ABE",
                          color_brin_own_other_he = "#00B17E",
                          color_outside_he = "grey",
                          label_brin_own = "this HEI",
                          label_other_he = "other HEI",
                          label_outside_he = "outside HE",
                          padding = 2,
                          alpha = 0.4,
                          display_labels = TRUE,
                          match_label_color = TRUE,
                          label_fontface = 'bold',
                          label_size = 10){

  # input validity  checks
  stopifnot(nrow(my_flow_insights$summary_presences_brin) > 0)
  stopifnot(class(my_flow_insights) == "flow_insights")

  # prepare data
  df_plot <- prepare_brinflows(my_flow_insights = my_flow_insights,
                               color_brin_own = color_brin_own,
                               color_other_he = color_other_he,
                               color_brin_own_other_he = color_brin_own_other_he,
                               color_outside_he = color_outside_he,
                               label_brin_own = label_brin_own,
                               label_other_he = label_other_he,
                               label_outside_he = label_outside_he)
  # make plot
  plot_sankeygradient(column_from = df_plot$column_from,
                      column_to = df_plot$column_to,
                      color_from = df_plot$color_from,
                      color_to = df_plot$color_to,
                      values = df_plot$values,
                      padding = padding,
                      alpha = alpha,
                      display_labels = display_labels,
                      match_label_color = match_label_color,
                      label_fontface = label_fontface,
                      label_size = label_size
  )
}

