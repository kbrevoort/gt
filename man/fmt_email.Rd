% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_data.R
\name{fmt_email}
\alias{fmt_email}
\title{Format email addresses to generate 'mailto:' links}
\usage{
fmt_email(
  data,
  columns = everything(),
  rows = everything(),
  display_name = NULL,
  as_button = FALSE,
  color = "auto",
  show_underline = "auto",
  button_fill = "auto",
  button_width = "auto",
  button_outline = "auto",
  target = NULL
)
}
\arguments{
\item{data}{\emph{The gt table data object}

\verb{obj:<gt_tbl>} // \strong{required}

This is the \strong{gt} table object that is commonly created through use of the
\code{\link[=gt]{gt()}} function.}

\item{columns}{\emph{Columns to target}

\code{\link[=rows-columns]{<column-targeting expression>}} // \emph{default:} \code{everything()}

Can either be a series of column names provided in \code{c()}, a vector of
column indices, or a select helper function (e.g. \code{\link[=starts_with]{starts_with()}},
\code{\link[=ends_with]{ends_with()}}, \code{\link[=contains]{contains()}}, \code{\link[=matches]{matches()}}, \code{\link[=num_range]{num_range()}} and \code{\link[=everything]{everything()}}).}

\item{rows}{\emph{Rows to target}

\code{\link[=rows-columns]{<row-targeting expression>}} // \emph{default:} \code{everything()}

In conjunction with \code{columns}, we can specify which of their rows should
undergo formatting. The default \code{\link[=everything]{everything()}} results in all rows in
\code{columns} being formatted. Alternatively, we can supply a vector of row
captions within \code{c()}, a vector of row indices, or a select helper
function (e.g. \code{\link[=starts_with]{starts_with()}}, \code{\link[=ends_with]{ends_with()}}, \code{\link[=contains]{contains()}}, \code{\link[=matches]{matches()}},
\code{\link[=num_range]{num_range()}}, and \code{\link[=everything]{everything()}}). We can also use expressions to filter
down to the rows we need (e.g., \verb{[colname_1] > 100 & [colname_2] < 50}).}

\item{display_name}{\emph{Display name for the email address}

\verb{scalar<character>} // \emph{default:} \code{NULL} (\code{optional})

The display name is the visible 'label' to use for the email address. If
\code{NULL} (the default) the address itself will serve as the display name.
There are two non-\code{NULL} options: (1) a piece of static text can be used
for the display name by providing a string, and (2) a function can be
provided to fashion a display name from every email address.}

\item{as_button}{\emph{Style email address as a button}

\verb{scalar<logical>} // \emph{default:} \code{FALSE}

An option to style the email address as a button. By default, this is
\code{FALSE}. If this option is chosen then the \code{button_fill} argument becomes
usable.}

\item{color}{\emph{Link color}

\verb{scalar<character>} // \emph{default:} \code{"auto"}

The color used for the resulting email address and its underline. This is
\code{"auto"} by default; this allows \strong{gt} to choose an appropriate color
based on various factors (such as the background \code{button_fill} when
\code{as_button} is \code{TRUE}).}

\item{show_underline}{\emph{Show the link underline}

\verb{scalar<character>|scalar<logical>} // \emph{default:} \code{"auto"}

Should the email address be decorated with an underline? By default this is
\code{"auto"} which means that \strong{gt} will choose \code{TRUE} when
\code{as_button = FALSE} and \code{FALSE} in the other case. The underline will be
the same color as that set in the \code{color} option.}

\item{button_fill, button_width, button_outline}{\emph{Button options}

\verb{scalar<character>} // \emph{default:} \code{"auto"}

Options for styling an email address as a button (and only applies if
\code{as_button = TRUE}). All of these options are by default set to \code{"auto"},
allowing \strong{gt} to choose appropriate fill, width, and outline values.}

\item{target}{\emph{The 'target' anchor element attribute}

\verb{scalar<character>} // \emph{default:} \code{NULL}

The anchor element 'target' attribute value. For a description of the
'target' attribute and its  allowed values, refer to the
\href{https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#attributes}{MDN Web Docs reference on the anchor HTML element}.}
}
\value{
An object of class \code{gt_tbl}.
}
\description{
Should cells contain email addresses, \code{fmt_email()} can be used to make email
addresses work well with email clients on the user system. This should be
expressly used on columns that contain \emph{only} email addresses (i.e., no email
addresses as part of a larger block of text). Should you have such a column
of data, there are options for how the email addresses should be styled. They
can be of the conventional style (with underlines and text coloring that sets
it apart from other text), or, they can appear to be button-like (with a
surrounding box that can be filled with a color of your choosing).

Email addresses in data cells are trusted as email addresses. We can also
provide more readable labels with the \code{display_name} argument. Supplying a
single value there will show the same label for all email addresses but
display names from an adjacent column could be used via a \code{\link[=from_column]{from_column()}}
call within \code{display_name}.
}
\section{Compatibility of formatting function with data values}{


\code{fmt_email()} is compatible with body cells that are of the \code{"character"} or
\code{"factor"} types. Any other types of body cells are ignored during
formatting. This is to say that cells of incompatible data types may be
targeted, but there will be no attempt to format them.
}

\section{Compatibility of arguments with the \code{from_column()} helper function}{


\code{\link[=from_column]{from_column()}} can be used with certain arguments of \code{fmt_email()} to obtain
varying parameter values from a specified column within the table. This means
that each row could be formatted a little bit differently. These arguments
provide support for \code{\link[=from_column]{from_column()}}:
\itemize{
\item \code{display_name}
\item \code{as_button}
\item \code{color}
\item \code{show_underline}
\item \code{button_fill}
\item \code{button_width}
\item \code{button_outline}
}

Please note that for each of the aforementioned arguments, a \code{\link[=from_column]{from_column()}}
call needs to reference a column that has data of the correct type (this is
different for each argument). Additional columns for parameter values can be
generated with \code{\link[=cols_add]{cols_add()}} (if not already present). Columns that contain
parameter data can also be hidden from final display with \code{\link[=cols_hide]{cols_hide()}}.
Finally, there is no limitation to how many arguments the \code{\link[=from_column]{from_column()}}
helper is applied so long as the arguments belong to this closed set.
}

\section{Examples}{


Let's take ten rows from the \code{\link{peeps}} dataset and create a table of contact
information with mailing addresses and email addresses. With the column that
contains email addresses (\code{email_addr}), we can use \code{fmt_email()} to generate
'mailto:' links. Clicking any of these formatted email addresses should
result in new message creation (depending on the OS integration with an email
client).

\if{html}{\out{<div class="sourceCode r">}}\preformatted{peeps |>
  dplyr::filter(country == "AUS") |>
  dplyr::select(
    starts_with("name"),
    address, city, state_prov, postcode, country, email_addr
  ) |>
  dplyr::mutate(city = toupper(city)) |>
  gt(rowname_col = "name_family") |>
  tab_header(title = "Our Contacts in Australia") |>
  tab_stubhead(label = "Name") |>
  fmt_email(columns = email_addr) |>
  fmt_country(columns = country) |>
  cols_merge(
    columns = c(address, city, state_prov, postcode, country),
    pattern = "\{1\}<br>\{2\} \{3\} \{4\}<br>\{5\}"
  ) |>
  cols_merge(
    columns = c(name_family, name_given),
    pattern = "\{1\},<br>\{2\}"
  ) |>
  cols_label(
    address = "Mailing Address",
    email_addr = "Email"
  ) |>
  tab_style(
    style = cell_text(size = "x-small"),
    locations = cells_body(columns = address)
  ) |>
  opt_align_table_header(align = "left")
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_fmt_email_1.png" alt="This image of a table was generated from the first code example in the `fmt_email()` help file." style="width:100\%;">
}}

We can further condense the table by reducing the email link to an icon. The
approach we take here is the use of a \strong{fontawesome} icon within the
\code{display_name} argument. The icon used is \code{"envelope"} and each icon produced
serves as a clickable 'mailto:' link. By adjusting one of the \code{\link[=cols_merge]{cols_merge()}}
calls, we can place the icon/link next to the name of the person.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{peeps |>
  dplyr::filter(country == "AUS") |>
  dplyr::select(
    starts_with("name"),
    address, city, state_prov, postcode, country, email_addr
  ) |>
  dplyr::mutate(city = toupper(city)) |>
  gt(rowname_col = "name_family") |>
  tab_header(title = "Our Contacts in Australia") |>
  fmt_email(
    columns = email_addr,
    display_name = fontawesome::fa(
      name = "envelope",
      height = "0.75em",
      fill = "gray"
    )
  ) |>
  fmt_country(columns = country) |>
  cols_merge(
    columns = c(address, city, state_prov, postcode, country),
    pattern = "\{1\}<br>\{2\} \{3\} \{4\}<br>\{5\}"
  ) |>
  cols_merge(
    columns = c(name_family, name_given, email_addr),
    pattern = "\{1\}, \{2\} \{3\}"
  ) |>
  cols_width(everything() ~ px(200)) |>
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = address)
  ) |>
  tab_options(column_labels.hidden = TRUE) |>
  opt_align_table_header(align = "left")
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_fmt_email_2.png" alt="This image of a table was generated from the second code example in the `fmt_email()` help file." style="width:100\%;">
}}

Another option is to display the names of the email recipients instead of the
email addresses, making the display names serve as 'mailto:' links. We can do
this by using \code{\link[=from_column]{from_column()}} in the \code{display_name} argument. The display
names in this case are the combined given and family names, handled earlier
through a \code{dplyr::mutate()} call. With some space conserved, we take the
opportunity here to add in phone information for each person.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{peeps |>
  dplyr::filter(country == "AUS") |>
  dplyr::mutate(name = paste(name_given, name_family)) |>
  dplyr::mutate(city = toupper(city)) |>
  dplyr::mutate(phone_number = gsub("^\\\\(0|\\\\)", "", phone_number)) |>
  dplyr::select(
    name, address, city, state_prov, postcode, country,
    email_addr, phone_number, country_code
  ) |>
  gt(rowname_col = "email_addr") |>
  tab_header(title = "Our Contacts in Australia") |>
  tab_stubhead(label = "Name") |>
  fmt_email(
    columns = email_addr,
    display_name = from_column("name"),
    color = "gray25"
  ) |>
  cols_hide(columns = name) |>
  fmt_country(columns = country) |>
  cols_merge(
    columns = c(address, city, state_prov, postcode, country),
    pattern = "\{1\}<br>\{2\} \{3\} \{4\}<br>\{5\}"
  ) |>
  cols_merge(
    columns = c(phone_number, country_code),
    pattern = "+\{2\} \{1\}"
  ) |>
  cols_label(
    address = "Mailing Address",
    email_addr = "Email",
    phone_number = "Phone"
  ) |>
  cols_move_to_start(columns = phone_number) |>
  cols_width(everything() ~ px(170)) |>
  tab_style(
    style = cell_text(size = px(11)),
    locations = cells_body(columns = address)
  ) |>
  cols_align(align = "left") |>
  opt_align_table_header(align = "left")
}\if{html}{\out{</div>}}

\if{html}{\out{
<img src="https://raw.githubusercontent.com/rstudio/gt/master/images/man_fmt_email_3.png" alt="This image of a table was generated from the third code example in the `fmt_email()` help file." style="width:100\%;">
}}
}

\section{Function ID}{

3-22
}

\section{Function Introduced}{

\code{v0.11.0} (July 9, 2024)
}

\seealso{
Other data formatting functions: 
\code{\link{data_color}()},
\code{\link{fmt}()},
\code{\link{fmt_auto}()},
\code{\link{fmt_bins}()},
\code{\link{fmt_bytes}()},
\code{\link{fmt_chem}()},
\code{\link{fmt_country}()},
\code{\link{fmt_currency}()},
\code{\link{fmt_date}()},
\code{\link{fmt_datetime}()},
\code{\link{fmt_duration}()},
\code{\link{fmt_engineering}()},
\code{\link{fmt_flag}()},
\code{\link{fmt_fraction}()},
\code{\link{fmt_icon}()},
\code{\link{fmt_image}()},
\code{\link{fmt_index}()},
\code{\link{fmt_integer}()},
\code{\link{fmt_markdown}()},
\code{\link{fmt_number}()},
\code{\link{fmt_partsper}()},
\code{\link{fmt_passthrough}()},
\code{\link{fmt_percent}()},
\code{\link{fmt_roman}()},
\code{\link{fmt_scientific}()},
\code{\link{fmt_spelled_num}()},
\code{\link{fmt_tf}()},
\code{\link{fmt_time}()},
\code{\link{fmt_units}()},
\code{\link{fmt_url}()},
\code{\link{sub_large_vals}()},
\code{\link{sub_missing}()},
\code{\link{sub_small_vals}()},
\code{\link{sub_values}()},
\code{\link{sub_zero}()}
}
\concept{data formatting functions}
