## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----results = "asis", echo = FALSE-------------------------------------------
pt <- flattabler::df_ex
rownames(pt) <- sprintf("r%d",1:nrow(pt))
colnames(pt) <- sprintf("c%d",1:ncol(pt))
pander::pandoc.table(pt, split.table = Inf, emphasize.italics.cols = 1:2, emphasize.italics.rows = 1:3)

## -----------------------------------------------------------------------------
library(flattabler)

ft <- pivot_table(df_ex) |>
  set_page(1, 1) |>
  remove_top(1) |>
  define_labels(n_col = 2, n_row = 2) |>
  fill_labels() |>
  remove_agg() |>
  fill_values() |>
  remove_k() |>
  replace_dec() |>
  unpivot()

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(ft)

## -----------------------------------------------------------------------------
f <- function(pt) {
  pt |>
  set_page(1, 1) |>
  remove_top(1) |>
  define_labels(n_col = 2, n_row = 2) |>
  fill_labels() |>
  remove_agg() |>
  fill_values() |>
  remove_k() |>
  replace_dec() |>
  unpivot()
}

## -----------------------------------------------------------------------------
folder <- system.file("extdata", "csvfolder", package = "flattabler")
lpt <- read_text_folder(folder)

class(lpt[[1]])

## -----------------------------------------------------------------------------
ftl <- flatten_table_list(lpt, f)

## ----results = "asis", echo = FALSE-------------------------------------------
ft_sample <- dplyr::slice_sample(ftl, prop = 0.20) |> 
    dplyr::arrange(page, col1, col2, row1, row2)
pander::pandoc.table(ft_sample)

## -----------------------------------------------------------------------------
t <- ftl |>
  tidyr::pivot_wider(names_from = page, values_from = value) |>
  dplyr::rename(B = col1, A = col2, E = row1, D = row2) |> 
  dplyr::select(A, B, D, E, M1, M2, M3, M4) |> 
  dplyr::arrange(A, B, D, E)

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(t)

## -----------------------------------------------------------------------------
pt <- pivot_table(df_ex)

pt <- pivot_table(df_ex, page = "M4")

## -----------------------------------------------------------------------------
file <- system.file("extdata", "csv/set_v_ie.csv", package = "flattabler")
pt <- read_text_file(file, define_page = TRUE)

## -----------------------------------------------------------------------------
file <- system.file("extdata", "excel/set_v.xlsx", package = "flattabler")
pt <- read_excel_sheet(file, define_page = 3)

## -----------------------------------------------------------------------------
pt <- pivot_table(df_set_h_v)
lpt <- pt |> divide()

## -----------------------------------------------------------------------------
folder <- system.file("extdata", "csvfolder", package = "flattabler")
lpt <- read_text_folder(folder)

## -----------------------------------------------------------------------------
folder <- system.file("extdata", "excelfolder", package = "flattabler")
lpt <- read_excel_folder(folder)

## -----------------------------------------------------------------------------
file <- system.file("extdata", "excel/set_sheets.xlsx", package = "flattabler")
lpt <- read_excel_file(file)

## -----------------------------------------------------------------------------
pt <- pt |> set_page(1, 1)

## -----------------------------------------------------------------------------
pt <- pt |> define_labels(n_col = 2, n_row = 2)

## -----------------------------------------------------------------------------
pt <- pt |> remove_top(1)

## -----------------------------------------------------------------------------
pt <- pt |> fill_labels()

## -----------------------------------------------------------------------------
pt <- pt |> remove_agg()

## -----------------------------------------------------------------------------
pt <- pivot_table(df_ex_compact) |>
  extract_labels(col = 1,
                 labels = c("b1", "b2", "b3", "b4", "Total general"))

## -----------------------------------------------------------------------------
file <- system.file("extdata", "csv/set_v_compact.csv", package = "flattabler")
pt <- read_text_file(file)
lpt <- pt |> divide()

df <- get_col_values(lpt, start_row = 4)
labels <- sort(unique(df$label))

## -----------------------------------------------------------------------------
pt <- pt |> fill_values()

## -----------------------------------------------------------------------------
pt <- pt |> remove_k()

## -----------------------------------------------------------------------------
pt <- pt |> replace_dec()

## -----------------------------------------------------------------------------
ft <- pivot_table(df_ex) |>
  set_page(1, 1) |>
  remove_top(1) |>
  define_labels(n_col = 2, n_row = 2) |>
  fill_labels() |>
  remove_agg() |>
  fill_values() |>
  remove_k() |>
  replace_dec() |>
  unpivot()

## -----------------------------------------------------------------------------
f <- function(pt) {
  pt |>
    set_page(1, 1) |>
    define_labels(n_col = 2, n_row = 2) |>
    remove_top(1) |>
    fill_labels() |>
    remove_agg() |>
    fill_values() |>
    remove_k() |>
    replace_dec() |>
    unpivot()
}

ft <- flatten_table_list(lpt, f)

