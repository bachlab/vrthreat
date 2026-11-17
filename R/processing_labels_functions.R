#' Merge by concatenating labels from different columns 
#' 
#' Merges columns of labels.
#' labels from each column is seperated by `sep` and concatenated.
#' The resulting column of labels is stored under `out_colname`
#'
#' @param df Dataframe with multiple label columns
#' @param col_names A string vector with column names
#' @param new_col A new column name as a string (can be one of the old ones)
#' @param sep 
#'
#' @returns The updated df
#' @export
#'
#' @examples
merge_labels <- function(df, col_names, new_col, sep = '_') {
  if (is.null(df) ||
      !validate_has_columns(df, col_names))
    return(NULL)
  
  inputs <- purrr::map(col_names, function(i) df[[i]])
  inputs$sep = sep
  df %>%
    dplyr::mutate(
      !!new_col := do.call(stringr::str_c, inputs)
    )
}

#' Clean labels
#' 
#' This function removes some special characters (-/=* and space) from labels and replaces
#' them with underscore `_` for better handling with string functions and as 
#' column names in count tibbles.
#' 
#' if `col_names` is left as null then all columns with string data are cleaned.
#'
#' @param df Data frame with label columns
#' @param col_names Column names of labels (string vector, default NULL)
#'
#' @returns The cleaned data frame with unchanged column names
#' @export
#'
#' @examples
clean_labels <- function(df, col_names = NULL) {
  if (is.null(df) ||
      !validate_has_columns(df, col_names))
    return(NULL)
  
  if(anyNA(col_names) || length(col_names) == 0){
    return(NULL)
  }
  
  for (inner_col in col_names) {
   df <- df %>%
    dplyr::mutate(!!inner_col := stringr::str_replace_all(.data[[inner_col]], "[- /=*]", "_"))
  }
  
  return(df)
}

#' Construct a label column from an existing one via a mapping.
#' 
#' A new column is created by mapping each input label to some output label and storing.
#' The mapping argument is of the form `c(input_label = "output_label", ...)`. 
#' These can also be created programmatically with the use of [setNames].
#'
#' @param df An data frame
#' @param mapping A mapping for labels as a named vector.
#' @param inner_col Column that contains labels
#' @param new_col New column
#'
#' @returns the same data frame with new column added
#' @export
#'
#' @examples
#' df <- data.frame(
#'  some_label_col = c("A", "B", "A")
#'  )
#' 
#' mapping <- c(
#' A = "Approve",
#' B = "Reject"
#' )
#' 
#' map_labels(df, mapping = mapping, inner_col = "some_label_col", new_col = "Decision")
map_labels <- function(df, mapping, inner_col, new_col) {
  df %>%
    dplyr::rowwise() %>%
    dplyr::mutate("{ new_col }" := paste0(mapping[.data[[inner_col]]])) %>%
    dplyr::ungroup()
}

#' Add n-grams to a tibble containing label data
#' 
#' This function takes a data frame containing a label column and adds bi-, tri-, and n-grams. 
#' It uses the tidytext library to allow dynamic specification of `n` and returns 
#' the same data frame with added column `ngrams_n` where `n` is defined by the input.
#'
#' @param df An tibble
#' @param inner_col The column name of the label column as a string
#' @param n depth of the n-gram (default: 2, i.e. add bigrams)
#' @param exclude_self exclude self-transitions? (default: FALSE)
#'
#' @returns The input data frame with column `ngrams_n` added
#' @export
#'
#' @examples
add_ngram <- function(df, inner_col, n = 2, exclude_self = FALSE) {
  if (is.null(df) ||
      !validate_has_columns(df, inner_col) ||
      !validate_numeric(n, min = 2))
    return(NULL)
  
  df <- df %>%
    dplyr::ungroup()

  if (exclude_self) {
    df <-
      df %>%
      dplyr::mutate(this_label = .data[[inner_col]],
             next_label = dplyr::lead(this_label),
             exclude = this_label == next_label) %>%
      dplyr::filter(!exclude) %>%
      dplyr::select(!c(this_label, next_label))
  }
  
  if (nrow(df) < n) return(NULL)
  
  ngrams <- df %>%
    dplyr::summarise(label_sequence = stringr::str_c(.data[[inner_col]], collapse = " ")) %>%
    tidytext::unnest_tokens(ngrams, label_sequence, token = "ngrams", n = n) %>%
    dplyr::mutate(ngrams = stringr::str_replace_all(ngrams, " ", ".")) %>%
    dplyr::bind_rows(tibble::tibble(ngrams = rep(NA, n - 1))) %>%
    dplyr::rename(!!stringr::str_c("ngrams_", n) := ngrams)
  
  df %>%
    dplyr::bind_cols(ngrams)
}

#' Reduce a tibble by replacing rows where a label is repeated consecutively with just the first row.
#' 
#' This function also removes rows where the inner_col has NA.
#'
#' @param df An tibble
#' @param inner_col
#' 
#' @returns The filtered df
#' @export
#'
#' @examples
reduce_label_sequence <- function(df, inner_col) {
  df %>%
    filter(.data[[inner_col]] != lag(.data[[inner_col]]) | is.na(lag(.data[[inner_col]])))
}

