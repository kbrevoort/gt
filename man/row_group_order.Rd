% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_rows.R
\name{row_group_order}
\alias{row_group_order}
\title{Modify the ordering of any row groups}
\usage{
row_group_order(data, groups)
}
\arguments{
\item{data}{\emph{The gt table data object}

\verb{obj:<gt_tbl>} // \strong{required}

This is the \strong{gt} table object that is commonly created through use of the
\code{\link[=gt]{gt()}} function.}

\item{groups}{\emph{Specification of row group IDs}

\verb{vector<character>} // \strong{required}

A character vector of row group ID values corresponding to the revised
ordering. While this vector must contain valid group ID values, it is not
required to have all of the row group IDs within it; any omitted values
will be added to the end while preserving the original ordering.}
}
\value{
An object of class \code{gt_tbl}.
}
\description{
We can modify the display order of any row groups in a \strong{gt} object with
\code{row_group_order()}. The \code{groups} argument takes a vector of row group ID
values. After this function is invoked, the row groups will adhere to this
revised ordering. It isn't necessary to provide all row ID values in
\code{groups}, rather, what is provided will assume the specified ordering at the
top of the table and the remaining row groups will follow in their original
ordering.
}
\section{Examples}{


Let's use \code{\link{exibble}} to create a \strong{gt} table with a stub and with row
groups. We can modify the order of the row groups with \code{row_group_order()},
specifying the new ordering in \code{groups}.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{exibble |>
  dplyr::select(char, currency, row, group) |>
  gt(
    rowname_col = "row",
    groupname_col = "group"
  ) |>
  row_group_order(groups = c("grp_b", "grp_a"))
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_row_group_order_1.png" alt="This image of a table was generated from the first code example in the `row_group_order()` help file." style="width:100\%;">
}}
}

\section{Function ID}{

6-3
}

\section{Function Introduced}{

\code{v0.2.0.5} (March 31, 2020)
}

\seealso{
Other row addition/modification functions: 
\code{\link{grand_summary_rows}()},
\code{\link{rows_add}()},
\code{\link{summary_rows}()}
}
\concept{row addition/modification functions}
