% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_options.R
\name{opt_footnote_marks}
\alias{opt_footnote_marks}
\title{Modify the set of footnote marks}
\usage{
opt_footnote_marks(data, marks = NULL)
}
\arguments{
\item{data}{A table object that is created using the \code{\link[=gt]{gt()}} function.}

\item{marks}{Either a vector (that will represent the series of marks) or a
keyword that represents a preset sequence of marks. The valid keywords are:
\code{"numbers"} (for numeric marks), \code{"letters"} and \code{"LETTERS"} (for lowercase
and uppercase alphabetic marks), \code{"standard"} (for a traditional set of
four symbol marks), and \code{"extended"} (which adds two more symbols to the
standard set).}
}
\description{
Alter the footnote marks for any footnotes that may be present in the table.
Either a vector of marks can be provided (including Unicode characters), or,
a specific keyword could be used to signify a preset sequence. This function
serves as a shortcut for using \code{tab_options(footnote.marks = {marks})}
}
\details{
We can supply a vector of that will represent the series of marks.
The series of footnote marks is recycled when its usage goes beyond the
length of the set. At each cycle, the marks are simply doubled, tripled, and
so on (e.g., \code{*} -> \code{**} -> \code{***}). The option exists for providing keywords
for certain types of footnote marks. The keywords are:

\itemize{
\item \code{"numbers"}: numeric marks, they begin from 1 and these marks are not
subject to recycling behavior
\item \code{"letters"}: miniscule alphabetic marks, internally uses the \code{letters}
vector
which contains 26 lowercase letters of the Roman alphabet
\item \code{"LETTERS"}: majuscule alphabetic marks, using the \code{LETTERS} vector
which has 26 uppercase letters of the Roman alphabet
\item \code{"standard"}: symbolic marks, four symbols in total
\item \code{"extended"}: symbolic marks, extends the standard set by adding two
more symbols, making six
}
}
\section{Figures}{

\if{html}{\figure{man_opt_footnote_marks_1.svg}{options: width=100\%}}
}

\examples{
# Use `sza` to create a gt table,
# adding three footnotes; call
# `opt_footnote_marks()` to specify
# which footnote marks to use
tab_1 <-
  sza \%>\%
  dplyr::group_by(latitude, tst) \%>\%
  dplyr::summarize(
    SZA.Max = max(sza),
    SZA.Min = min(sza, na.rm = TRUE)
  ) \%>\%
  dplyr::ungroup() \%>\%
  dplyr::filter(latitude == 30, !is.infinite(SZA.Min)) \%>\%
  dplyr::select(-latitude) \%>\%
  gt(rowname_col = "tst") \%>\%
  cols_split_delim(".") \%>\%
  fmt_missing(
    columns = everything(),
    missing_text = "90+"
  ) \%>\%
  tab_stubhead("TST") \%>\%
  tab_footnote(
    footnote = "True solar time.",
    locations = cells_stubhead()
  ) \%>\%
  tab_footnote(
    footnote = "Solar zenith angle.",
    locations = cells_column_labels(groups = "SZA")
  ) \%>\%
  tab_footnote(
    footnote = "The Lowest SZA.",
    locations = cells_stub(rows = "1200")
  ) \%>\%
  opt_footnote_marks(marks = "standard")

}