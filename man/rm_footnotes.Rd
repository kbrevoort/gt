% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tab_remove.R
\name{rm_footnotes}
\alias{rm_footnotes}
\title{Remove table footnotes}
\usage{
rm_footnotes(data, footnotes = everything())
}
\arguments{
\item{data}{\emph{The gt table data object}

\verb{obj:<gt_tbl>} // \strong{required}

This is the \strong{gt} table object that is commonly created through use of the
\code{\link[=gt]{gt()}} function.}

\item{footnotes}{\emph{Footnotes to remove}

\verb{scalar<numeric|integer>|everything()} // \emph{default:} \code{everything()}

A specification of which footnotes should be removed. The footnotes to be
removed can be given as a vector of integer values (they are stored as
integer positions, in order of creation, starting at \code{1}). A select helper
can also be used and, by default, this is \code{everything()} (whereby all
footnotes will be removed).}
}
\value{
An object of class \code{gt_tbl}.
}
\description{
If you have one or more footnotes that ought to be removed, \code{rm_footnotes()}
allows for such a selective removal. The table footer is an optional table
part that is positioned below the table body, containing areas for both the
footnotes and source notes.

This function for removal is useful if you have received a \strong{gt} table
(perhaps through an API that returns \strong{gt} objects) but would prefer that
some or all of the footnotes be removed. This function is safe to use even if
there are no footnotes in the input \code{gt_tbl} object so long as select helpers
(such as the default \code{everything()}) are used instead of explicit integer
values.
}
\section{Examples}{


Use a subset of the \code{\link{sza}} dataset to create a \strong{gt} table. Color the \code{sza}
column using \code{\link[=data_color]{data_color()}}, then, use \code{\link[=tab_footnote]{tab_footnote()}} twice to
add two footnotes (each one targeting a different column label).

\if{html}{\out{<div class="sourceCode r">}}\preformatted{gt_tbl <-
  sza |>
  dplyr::filter(
    latitude == 20 &
      month == "jan" &
      !is.na(sza)
  ) |>
  dplyr::select(-latitude, -month) |>
  gt() |>
  data_color(
    columns = sza,
    palette = c("white", "yellow", "navyblue"),
    domain = c(0, 90)
  ) |>
  tab_footnote(
    footnote = "Color indicates height of sun.",
    locations = cells_column_labels(
      columns = sza
    )
  ) |>
  tab_footnote(
    footnote = "
    The true solar time at the given latitude
    and date (first of month) for which the
    solar zenith angle is calculated.
    ",
    locations = cells_column_labels(
      columns = tst
    )
  ) |>
  cols_width(everything() ~ px(150))

gt_tbl
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_rm_footnotes_1.png" alt="This image of a table was generated from the first code example in the `rm_footnotes()` help file." style="width:100\%;">
}}

If you decide that you don't want the footnotes in the \code{gt_tbl} object,
they can be removed with \code{rm_footnotes()}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{rm_footnotes(data = gt_tbl)
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_rm_footnotes_2.png" alt="This image of a table was generated from the second code example in the `rm_footnotes()` help file." style="width:100\%;">
}}

Individual footnotes can be selectively removed. Footnotes are identified by
their index values. To remove the footnote concerning true solar time
(footnote \code{2}, since it was supplied to \strong{gt} after the other footnote) we
would give the correct index value to \code{footnotes}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{rm_footnotes(data = gt_tbl, footnotes = 2)
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_rm_footnotes_3.png" alt="This image of a table was generated from the third code example in the `rm_footnotes()` help file." style="width:100\%;">
}}
}

\section{Function ID}{

7-4
}

\section{Function Introduced}{

\code{v0.8.0} (November 16, 2022)
}

\seealso{
Other part removal functions: 
\code{\link{rm_caption}()},
\code{\link{rm_header}()},
\code{\link{rm_source_notes}()},
\code{\link{rm_spanners}()},
\code{\link{rm_stubhead}()}
}
\concept{part removal functions}
