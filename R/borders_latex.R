create_latex_border_statement <- function(data, option, type = 'mid', spacing = NULL) {

  style <- dt_options_get_value(data = data, option = option)

  line_type <- switch(
    style,
    "none" = "",
    "solid" = sprintf("\\%srule", type),
    "double" = "\\midrule \\midrule" #specify_doubleline_l(data)
  )

  if (is.null(spacing)) return(line_type)

  paste0(
    line_type,
    "\\addlinespace[",
    spacing,
    "pt]\n"
  )

}

create_latex_partial_border <- function(data, option, spacing = NULL) {

  style <- dt_options_get_value(data = data, option = option)

  n_cols <- get_effective_number_of_columns(data)

  line_type <- switch(
    style,
    "none" = "",
    "solid" = sprintf("\\cmidrule(lr){2 - %d}\n", n_cols),
    "double" = sprintf("\\cmidrule(lr){2 - %d} \\cmidrule(lr){2 - %d}",
                       n_cols, n_cols)
    # "double" = paste0(
    #   "\\cmidrule(lr){2 - ",
    #   n_cols,
    #   "} \\multicolumn{",
    #   n_cols,
    #   "}{c}{} \\\\[-1.5em] \\cmidrule(lr){2 - ",
    #   n_cols,
    #   "}"
    #)
  )

}

create_vertical_latex_border_symbol <- function(data, option) {

  style <- dt_options_get_value(data = data, option = option)

  switch(
    style,
    'none' = "",
    'solid' = "|",
    'double' = "||"
  )

}

specify_doubleline_l <- function(data) {

  dl_text <- sprintf("\\multicolumn{%d}{c}{} \\\\[-1.1em]",
                       get_effective_number_of_columns(data))

  paste0(
    "\\midrule ",
    dl_text,
    " \\midrule\n",
    collapse = ""
  )

}
