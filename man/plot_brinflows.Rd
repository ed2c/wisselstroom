% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_brinflows.R
\name{plot_brinflows}
\alias{plot_brinflows}
\title{Plots the global flows between brin_own, other HE and not in HE}
\usage{
plot_brinflows(
  my_bek_compact,
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
  label_fontface = "bold",
  label_size = 10
)
}
\arguments{
\item{my_bek_compact}{bek_compact object}

\item{color_brin_own}{string containing color code used for brin_own}

\item{color_other_he}{string containing color code used for other HE}

\item{color_brin_own_other_he}{string containing color code used for brin_own & other HE}

\item{color_outside_he}{string containing color code used for outside HE}

\item{label_brin_own}{string to use instead of "brin_own"}

\item{label_other_he}{string to use instead of "other_HE"}

\item{label_outside_he}{string to use instead of "outside_HE"}

\item{padding}{constant used for displaying space between flows}

\item{alpha}{numeric, transparancy of colors (0 = complete transparent, 1 = opaque)}

\item{display_labels}{boolean used for displaying labels or not}

\item{match_label_color}{boolean used to indicate labels in same color as nodes}

\item{label_fontface}{string used for styling the font of labels}

\item{label_size}{number indicating the font size of labels}
}
\value{
a ggplot, displaying the flows between brin_own, other HE and not in HE
}
\description{
\code{plot_brinflows()} takes the summary_presences_brin from a bek_compact list, and turns it into a sankey diagram
}
\examples{
\dontrun{
plot_brinflows(my_bek_compact)
}
}
