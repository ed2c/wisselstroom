################################################################################
###
### Plots a sankey diagram with gradient colors
###
################################################################################

# Based on https://github.com/ssp3nc3r/ggSankeyGrad as it was beginning of june 2024
# code is changed as suggested in the pull request by MMJansen at beginning of june 2024

# to suppres the "no visible binding for global variable" ----------------------
utils::globalVariables(c("b1", "b2", "bez_b", "bez_t",
                         "csp", "csv", "p", "t1", "t2", "y"
))

#' Plots a sankey diagram with gradient colors, based on geom_ribbon
#'
#' @param column_from vector of character labels, indicating the lefthand side
#' @param column_to vector of character labels, indicating the righthand side
#' @param color_from vector of color values, indicating colors for the lefthand side
#' @param color_to vector of color values, indicating colors for the righthand side
#' @param values vector of values, indicating the widht of the ribbon
#' @param padding constant used for displaying space between ribbons
#' @param alpha constant used for making colors transparent
#' @param display_labels boolean used for displaying labels or not
#' @param match_label_color boolean used to indicate labels in same color as nodes
#' @param label_fontface string used for styling the font of labels
#' @param label_size number indicating hte font size of labels
#'
#' @return a ggplot with geom_ribbons
#' @export
#'
#' @examples
#' \dontrun{
#'plot_sankeygradient(column_from = c("a","b"),
#'                    column_to = c("d","d"),
#'                    values = c(5,10),
#'                    color_from = c("red", "green"),
#'                    color_to = c("blue", "blue"))
#' }
plot_sankeygradient <- function(column_from,
                                column_to,
                                color_from,
                                color_to,
                                values,
                                padding = 2,
                                alpha = 0.4,
                                display_labels = TRUE,
                                match_label_color = TRUE,
                                label_fontface = 'bold',
                                label_size = 10) {

  stopifnot(utils::packageVersion("ggplot2") >= 3.5 )

  # flows will be geom_ribbons from left to right
  # calculate left coordinates bottom and top of the geom_ribbon

  d_from <- data.frame(column_from,
                       values) |>
    dplyr::mutate(p = ifelse(column_from == dplyr::lag(column_from) | is.na(dplyr::lag(column_from)),
                             0,
                             padding)) |>
    dplyr::mutate(csp = cumsum(p)) |>
    dplyr::mutate(csv = cumsum(values)) |>
    dplyr::mutate(t1 = csp + csv) |>
    dplyr::mutate(b1 = t1 - values) |>
    dplyr::select(column_from,b1,t1)

  # calculate right coordinates bottom and top of geom_ribbons

  d_to <- data.frame(column_to,
                     values) |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::arrange(factor(column_to,
                          levels = unique(c(column_from,column_to)))) |>
    dplyr::mutate(p = ifelse(column_to == dplyr::lag(column_to) | is.na(dplyr::lag(column_to)),
                             0,
                             padding)) |>
    dplyr::mutate(csp = cumsum(p)) |>
    dplyr::mutate(csv = cumsum(values)) |>
    dplyr::mutate(t2 = csp + csv) |>
    dplyr::mutate(b2 = t2 - values) |>
    dplyr::arrange(row) |>
    dplyr::select(column_to,b2,t2)

  # combine calculations with beginning and ending colors for each flow
  d <- dplyr::bind_cols(d_from,
                        d_to)
  d$color_from <- color_from
  d$color_to <- color_to

  # create a factor for each flow
  d <- d |> dplyr::mutate(cat = as.integer(factor(paste0(column_from, column_to),
                                                  levels = paste0(column_from, column_to)) ) )

  # x value for each vertical line
  x <- seq(0, 1, length = 101)

  # calculate bottom and top y value along bezier
  bez <- data.frame()
  for(i in seq(nrow(d))) {
    bot <- with(d[i,],
                matrix(c(0,b1, 1,b1, 3,b2, 4,b2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    top <- with(d[i,],
                matrix(c(0,t1, 1,t1, 3,t2, 4,t2),
                       nrow = 4, ncol = 2, byrow = TRUE))
    bez <- dplyr::bind_rows(bez,
                            data.frame(cat = i,
                                       x = x,
                                       bez_b = bezier::bezier(t = x, p = bot)[,2],
                                       bez_t = bezier::bezier(t = x, p = top)[,2],
                                       stringsAsFactors = FALSE)
    )
  }

  bez <- bez |> dplyr::filter(!is.na(bez_b))

  # create base plot with lines
  pl <- ggplot2::ggplot(data = bez) +
    ggplot2::scale_x_continuous(limits = c(-0.2, 1.2))

  # add each flow ribbon to plot
  for(i in seq(length(color_from))) {
    pl <- pl +
      ggplot2::geom_ribbon(
        data = dplyr::filter(bez, cat == i),
        mapping = ggplot2::aes(x = x,
                               ymin = bez_b,
                               ymax = bez_t,
                               group = cat),
        fill = grid::linearGradient(colours = c(d$color_from[i],
                                                d$color_to[i]),
                                    y2 = ggplot2::unit(0, 'npc')),
        alpha = alpha
      )
  }


  # add labels for beginning (left) and ending (right) categories
  if(display_labels == TRUE) {

    loc <- d |>
      dplyr::group_by(column_from) |>
      dplyr:: summarise(y = 0.5*max(t1) + 0.5*min(b1),
                        color_from = color_from[t1 == max(t1)]) |>
      dplyr::ungroup() |>
      dplyr:: filter(!is.na(y))

    for(i in seq(nrow(loc))) {
      pl <- pl + ggplot2::annotate('text', x = -0.01, y = loc$y[i],
                                   label = as.character(loc$column_from[i]),
                                   fontface = label_fontface,
                                   hjust = 1,
                                   size = label_size/ggplot2::.pt,
                                   color = ifelse(match_label_color,
                                                  loc$color_from[i],
                                                  '#000000') )
    }

    loc <- d |>
      dplyr::group_by(column_to) |>
      dplyr::summarise(y = 0.5*max(t2) + 0.5*min(b2),
                       color_to = color_to[t2 == max(t2)]) |>
      dplyr::ungroup() |>
      dplyr:: filter(!is.na(y))


    for(i in seq(nrow(loc))) {
      pl <- pl + ggplot2::annotate('text', x = 1.01, y = loc$y[i],
                                   label = as.character(loc$column_to[i]),
                                   fontface = label_fontface,
                                   hjust = 0, size = label_size/ggplot2::.pt, color = ifelse(match_label_color, loc$color_to[i], '#000000') )
    }


  }

  # remove all theme elements
  pl <- pl + ggplot2::theme_void()

  # return ggplot object
  return(pl)
}
