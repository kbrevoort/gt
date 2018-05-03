
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Grammar of Tables

The goal of **gt** is to allow anyone to easily build presentation
tables from tabular data using the Grammar of Tables approach. We
essentially want the activity of building tables to be done with a clear
separation of concerns: you don’t have to decide how the tabular data
gets transformed and structured while also worrying about aesthetics.

While the Grammar of Tables API is still undergoing a first draft, the
model for the constituent parts of a table has been tentatively put
forward. The schematic below provides an outline of how the table
components fit together.

<img src="man/figures/gt_parts_of_a_table.png">

The main structural parts of a table (and their subparts) are:

  - the **heading** (optional; possibly with a **table number**,
    **title**, or **headnote**)
  - the **stub** (contains a **stubhead** and one or more **stub
    blocks**)
  - the **boxhead** (contains **column headings**, optionally within
    **panels** having one or more **spanner headings**)
  - the **field** (contains *cells*, *columns*, and *rows*)
  - the **footnote**
  - the **source note**

The **heading** table part contains the table title and the optional
table number and headnote subparts. This part is often overlooked but as
it relates very closely to the tabular data it is recognized here as
integral to finalized output.

The **stub** is the area to the left in a table that contains **row
captions**, **summary captions**, and **group headings**. Those subparts
can be grouped in a sequence of **stub blocks**. The **stubhead**
provides a location for a caption that describes the **stub**.

The **boxhead** is the part that contains the **column headings**, which
can themselves be supplemented by **spanner headings** (which are placed
above) and contained in one or more **panels**.

The **field** is located below the **boxhead** and right of the
**stub**. Composed of *cells*, where vertical and horizontal collections
are termed *columns* and *rows*.

The **footnote** area contains statements explaining information (or,
lack of information) in a column, row, or one or more cells. This is
done by affixing reference symbols or letters/digits to points of
reference.

The **source note** area is for provision of citation information for
the presented data. As it is commonly seen, the citation is preceeded by
the word `Source`.

## Examples

The `mtcars` dataset can be suitably transformed into a simple HTML
table (for sake of brevity, we’ll use just the first 6 rows). We can use
the `tab_create()` function to initiate the process (providing `mtcars`
to the `tbl` argument), creating an HTML table object. Because `mtcars`
is a data frame with rownames, a stub is automatically created. After
that, a few functions can be used to add and customize the output table:

  - `apply_theme_striped()`: styles the table with a basic striped row
    theme
  - `apply_alignment_center()`: center-aligns cell content in specified
    columns
  - `add_stubhead_caption()`: adds a caption to the stubhead (the box in
    the top-left of the table)
  - `add_heading()`: an opportunity to add a title and, optionally, a
    headnote and a table number
  - `add_source_note()`: not really a footnote, but a specialized note
    concerned with the source of the data presented

<!-- end list -->

``` r

# 1 - we take the first six rows of `datasets::mtcars` data.frame
# 2 - the 'striped' theme is applied
# 3 - we are formatting numbers in 4 columns to have 1 decimal place
# 4 - we are formatting numbers in the `wt` column to have 3 decimal places
# 5 - all `numeric` values are right aligned
# 6 - the dataset has rownames so they've been moved to the stub;
#     here, we supply a stubhead caption
# 7 - a heading is added
# 8,9 - two source notes are added

mtcars_tbl <-
  tab_create(tbl = mtcars[1:6, ]) %>%
  apply_theme_striped() %>%  # 2
  format_as_number(
    columns = c("mpg", "disp", "drat", "qsec"),
    decimals = 1) %>%  # 3
  format_as_number(
    columns = "wt",
    decimals = 3) %>%  # 4
  apply_alignment_right(types = "numeric") %>%  # 5
  add_stubhead_caption(caption = "car model") %>%  # 6
  add_heading(
    title = "Excerpt from the `mtcars` dataset",
    headnote = "[A rather famous Motor Trend table]") %>%  # 7
  add_source_note(
    source_note = "Henderson and Velleman (1981).",
    lead_in = "Main Source of Data: ") %>%  # 8
  add_source_note(
    source_note = "Motor Trend Magazine (1974).",
    lead_in = "Original Data: ")  # 9
```

Invoking the object interactively displays it in the Viewer. In R
Markdown chunks, the object becomes an HTML table.

<img src="man/figures/mtcars.png">

Let’s build another table using another well-known dataset. In this
table, we embrace the naming convention provided in the **iris**
dataset’s column names. The dot-separated column names (such as
`Sepal.Length` and `Petal.Width`) provide a means to specify heading
levels as `<spanner_heading>.<column_heading>`. So long as we follow
this column-name pattern, we can use the `apply_spanner_headings()`
function to parse and expand the given column names into one or more
boxhead panels with spanner headings and column headings.

``` r

# 1 - we take the `datasets::iris` data.frame
# 2 - the `Species` column is moved to the start of the column series
# 3 - the 'striped' theme is applied
# 4 - we are taking the column naming format `[heading].[caption]` and
#     automatically creating spanner headings to add structure
# 5 - all column values are center aligned
# 6 - we are formatting numbers in the named columns to have 1 decimal place
# 7 - a heading is added
# 8 - a source note is added
iris_tbl <-
  tab_create(tbl = iris) %>%  # 1
  move_columns_to_start(columns = "Species") %>%  # 2
  apply_theme_striped() %>%  # 3
  apply_spanner_headings(use_names = TRUE) %>%  # 4
  apply_alignment_center() %>%  # 5
  format_as_number(
    columns = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    decimals = 1) %>%  # 6
  add_heading(
    title = "The `iris` dataset",
    headnote = "[A rather famous dataset about Iris setosa, versicolor, and virginica]",
    table_number = 1) %>%  # 7
  add_source_note(
    source_note = "Anderson, Edgar (1935).",
    lead_in = "The data were collected by ")  # 8
```

This is how the table appears:

<img src="man/figures/iris.png">

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

## License

MIT © RStudio
