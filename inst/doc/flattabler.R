## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(flattabler)

## ---- results = "asis", echo = FALSE------------------------------------------
pt <- list_pt_ie[[1]]
rownames(pt) <- sprintf("r%d",1:nrow(pt))
colnames(pt) <- sprintf("c%d",1:ncol(pt))
pander::pandoc.table(pt, split.table = Inf)

## ---- results = "asis", echo = FALSE------------------------------------------
library(tidyr)

ft <- pt %>%
  set_page(1, 1) %>%
  define_labels(n_col = 2, n_row = 2) %>%
  remove_top(1) %>%
  fill_labels() %>%
  remove_agg() %>%
  fill_values() %>%
  remove_k() %>%
  replace_dec() %>%
  unpivot()

pander::pandoc.table(ft)

## ---- eval = FALSE------------------------------------------------------------
#  library(flattabler)
#  library(tidyr)
#  
#  ft <- pt %>%
#    set_page(1, 1) %>%
#    define_labels(n_col = 2, n_row = 2) %>%
#    remove_top(1) %>%
#    fill_labels() %>%
#    remove_agg() %>%
#    fill_values() %>%
#    remove_k() %>%
#    replace_dec() %>%
#    unpivot()

## -----------------------------------------------------------------------------
f <- function(pt) {
  pt %>%
    set_page(1, 1) %>%
    define_labels(n_col = 2, n_row = 2) %>%
    remove_top(1) %>%
    fill_labels() %>%
    remove_agg() %>%
    fill_values() %>%
    remove_k() %>%
    replace_dec() %>%
    unpivot()
}

ft <- flatten_table_list(list_pt_ie, f)

## ---- echo = FALSE------------------------------------------------------------
ft_sample <- dplyr::slice_sample(ft, prop = 0.25) %>% 
    dplyr::arrange(page, col1, col2, row1, row2)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(ft_sample)

## -----------------------------------------------------------------------------
t <- ft %>%
  tidyr::pivot_wider(names_from = page, values_from = value) %>%
  dplyr::rename(A = col1, B = col2, E = row1, D = row2) %>% 
  dplyr::arrange(A, B, E, D)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(t)

## ---- eval = FALSE------------------------------------------------------------
#  df <- data.frame(unclass(pt_m4)[c(1:7)])
#  pt <- pivot_table(df, page = "M4")

## ---- eval = FALSE------------------------------------------------------------
#  file <- system.file("extdata", "csv/set_v_ie.csv", package = "flattabler")
#  pt_set_v_ie <- read_text_file(file)

## ---- eval = FALSE------------------------------------------------------------
#  folder <- system.file("extdata", "csvfolder", package = "flattabler")
#  lpt <- read_text_folder(folder)

## ---- eval = FALSE------------------------------------------------------------
#  file <- system.file("extdata", "csv/set_v_ie.csv", package = "flattabler")
#  pt_set_v_ie <- read_text_file(file)
#  
#  pt_set_v_ie %>% view_table_attr()

## ---- eval = FALSE------------------------------------------------------------
#  list_pt_ie <- pt_set_v_ie %>% divide()

## -----------------------------------------------------------------------------
pt <- list_pt_ie[[1]]

pt <- pt %>% set_page(1, 1)

## -----------------------------------------------------------------------------
pt <- pt %>% define_labels(n_col = 2, n_row = 2)

## -----------------------------------------------------------------------------
pt <- pt %>% remove_top(1)

## -----------------------------------------------------------------------------
pt <- pt %>% fill_labels()

## -----------------------------------------------------------------------------
pt <- pt %>% remove_agg()

## -----------------------------------------------------------------------------
pt2 <- pt_m4_compact %>%
  extract_labels(col = 1, labels = c("b1", "b2", "b3", "b4", "Total general"))

## -----------------------------------------------------------------------------
df <- get_col_values(list_pt_compact, start_row = 4)
labels <- sort(unique(df$label))

## -----------------------------------------------------------------------------
pt <- pt %>% fill_values()

## -----------------------------------------------------------------------------
pt <- pt %>% remove_k()

## -----------------------------------------------------------------------------
pt <- pt %>% replace_dec()

## -----------------------------------------------------------------------------
ft_tmp <- pt %>% unpivot()

## -----------------------------------------------------------------------------
f <- function(pt) {
  pt %>%
    set_page(1, 1) %>%
    define_labels(n_col = 2, n_row = 2) %>%
    remove_top(1) %>%
    fill_labels() %>%
    remove_agg() %>%
    fill_values() %>%
    remove_k() %>%
    replace_dec() %>%
    unpivot()
}

ft <- flatten_table_list(list_pt_ie, f)

## -----------------------------------------------------------------------------
ft <- ft %>% tidyr::pivot_wider(names_from = page, values_from = value) 

## -----------------------------------------------------------------------------
ft <- ft %>% dplyr::rename(A = col1, B = col2, E = row1, D = row2) 

## -----------------------------------------------------------------------------
ft <- ft %>% dplyr::arrange(A, B, E, D) 

## -----------------------------------------------------------------------------
pt <- list_pt_ie[[1]]

ft_at <- pt %>%
  set_page(1, 1) %>%
  define_labels(n_col = 2, n_row = 2) %>%
  remove_top(1) %>%
  unpivot()

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(ft_at)

## -----------------------------------------------------------------------------
ft_at <- ft_at %>% dplyr::mutate_all( ~ dplyr::na_if(., ""))

## -----------------------------------------------------------------------------
ft_at <- ft_at %>% tidyr::fill(col1, row1)

## -----------------------------------------------------------------------------
ft_at <- ft_at %>% dplyr::filter(!is.na(col2) & !is.na(row2) & !is.na(value))

## -----------------------------------------------------------------------------
ft_at <- ft_at %>% dplyr::mutate_at(c("value"),
                                    ~ stringr::str_replace_all(., pattern = "\\.", replacement = "")) %>%
  dplyr::mutate_at(c("value"),
                   ~ stringr::str_replace(., pattern = ",", replacement = "\\."))

## -----------------------------------------------------------------------------
g <- function(pt) {
  pt %>%
    set_page(1, 1) %>%
    define_labels(n_col = 2, n_row = 2) %>%
    remove_top(1) %>%
    unpivot() %>%
    dplyr::mutate_all(~ dplyr::na_if(., "")) %>%
    tidyr::fill(col1, row1) %>%
    dplyr::filter(!is.na(col2) & !is.na(row2) & !is.na(value)) %>%
    dplyr::mutate_at(c("value"),
                     ~ stringr::str_replace_all(., pattern = "\\.", replacement = "")) %>%
    dplyr::mutate_at(c("value"),
                     ~ stringr::str_replace(., pattern = ",", replacement = "\\."))
}

ft2 <- flatten_table_list(list_pt_ie, g)

## ---- echo = FALSE------------------------------------------------------------
ft_sample2 <- dplyr::slice_sample(ft2, prop = 0.25) %>% 
    dplyr::arrange(page, col1, col2, row1, row2)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(ft_sample2)

## -----------------------------------------------------------------------------
t2 <- ft2 %>%
  tidyr::pivot_wider(names_from = page, values_from = value) %>%
  dplyr::rename(A = col1, B = col2, E = row1, D = row2) %>% 
  dplyr::arrange(A, B, E, D)

## ---- results = "asis", echo = FALSE------------------------------------------
pander::pandoc.table(t2)

