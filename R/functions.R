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
