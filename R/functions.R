#' Function for descriptice statistics
#'
#' @param data
#'
#' @return A data.frame/Tibble

#' Function for descriptice statistics
#'
#' @param data
#'
#' @return A data.frame/Tibble

descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}




#' Plot distribution function
#'
#' @param data
#'
#' @return Plots/histogram on metabolite distribution
#'
plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scale = "free")
}




#' Function to convert a columns character values to snakecase format
#'
#' @param data The lipidomics dataset
#' @param columns the columns you want to convert to snakecase
#'
#' @return a data frame


column_values_to_snake_case <-
  function(data, columns) {
    data |>
      dplyr::mutate(dplyr::across(
        {{ columns }},
        snakecase::to_snake_case
      ))
  }
