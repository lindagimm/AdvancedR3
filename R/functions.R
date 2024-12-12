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


#' Function to make metabolits into pivot wider
#'
#' @param data
#'
#' @return a dataframe

metabolites_to_wider <-
    function(data) {
        data |>
            tidyr::pivot_wider(
                names_from = metabolite,
                values_from = value,
                values_fn = mean,
                names_prefix = "metabolite_"
            )
    }




#' Column to metabolite
#'
#' @param data lipidomics
#' @param metabolite_variable column of metabolite variable
#'
#' @return recipe object


create_recipe_spec <-
    function(data, metabolite_variable) {
        recipes::recipe(data) |>
            recipes::update_role(
                {{metabolite_variable}},
                age,
                gender,
                new_role = "predictor" ) |>
        recipes::update_role(
            class, new_role = "outcome"
        ) |>
            recipes::step_normalize(tidyselect::starts_with("metabolite_"))
    }



#' Create workflow object of the model and transformations
#'
#' @param model_specs
#' @param recipe_specs
#'
#' @return a workflow object

create_model_workflow <- function(model_specs, recipe_specs) {
    workflows::workflow() |>
        workflows::add_model(model_specs) |>
        workflows::add_recipe(recipe_specs)
}



#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The model workflow object that has been fitted.
#'
#' @return A data frame
#'

tidy_model_output <- function(workflow_fitted_model) {

    workflow_fitted_model |>
        workflows::extract_fit_parsnip() |>
        broom::tidy(exponentiate = TRUE)
}




