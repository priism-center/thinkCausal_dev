#' Create drag-drop UI
#'
#' @description A fct function
#'
#' @return html
#'
#' @noRd
create_drag_drop_roles <- function(ns, .data, ns_prefix, design, weights, ran_eff){
  if (!inherits(.data, 'data.frame')) stop('.data must be a dataframe')

  # infer which columns are Z, Y, and X columns (i.e. smart defaults)
  auto_columns <- clean_detect_ZYX_columns(.data)


  drag_drop_html <-
    sortable::bucket_list(
      header = "Drag the variables to their respective roles",
      group_name = ns(paste0(ns_prefix, "_dragdrop")),
      orientation = "horizontal",
      class = 'default-sortable sortable-wide',
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_covariates")),
        text = strong("Covariates"),
        labels = auto_columns$X,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_treatment")),
        text = strong("Treatment"),
        labels = auto_columns$Z,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_response")),
        text = strong("Outcome"),
        labels = auto_columns$Y,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      if(design == 'Block randomized treatment'){
        sortable::add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_block")),
          text = strong("Blocking variable(s)"),
          labels = NULL,
          options = sortable::sortable_options(multiDrag = TRUE)
        )
      },
      if(weights == 'Yes'){
        sortable::add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_weight")),
          text = strong("Survey weight"),
          labels = NULL,
          options = sortable::sortable_options(multiDrag = TRUE)
        )
      },
      if(ran_eff == 'Yes'){
        sortable::add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_ran_eff")),
          text = strong("Random Intercept(s)"),
          labels = NULL,
          options = sortable::sortable_options(multiDrag = TRUE)
        )
      },
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_post_treatment")),
        text = create_info_icon(
          label = strong("Post-treatment variables to exclude from analysis"),
          text = "All variables that could potentially be affected by the treatment"
        ),
        labels = NULL,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_delete")),
        text = create_info_icon(
          label = strong("ID or index variables to exclude from analysis"),
          text = "Exclude index or ID variables in addition to extraneous variables"
        ),
        labels = auto_columns$ID,
        options = sortable::sortable_options(multiDrag = TRUE)
      )
    )

  drag_drop_html <- tagList(drag_drop_html)

  return(drag_drop_html)
}

#' Create a UI displaying the role, variable name, variable type, and percent NA for each column in a dataframe
#'
#' @param .data a dataframe
#' @param default_data_types a vector of default data types. Usually from convert_data_type_to_simple()
#' @param ns_prefix a string denoting a prefix to use when creating inputIds
#'
#' @return HTML code
#' @export
#' @noRd
#'
#' @examples
#' # shiny server
#' # renderUI({
#' #   create_data_summary_grid(
#' #     .data = .data,
#' #     default_data_types = default_data_types,
#' #     ns_prefix = 'analysis_data_'
#' #   )
#' # })
create_data_summary_grid <- function(ns, .data, default_data_types, ns_prefix, design, blocking_variables = NULL, survey_weight = NULL, random_effect = NULL){

  # set indices to map over
  all_col_names <- colnames(.data)
  indices <- seq_along(all_col_names)
  n_blocks <- length(blocking_variables)
  n_weight <- length(survey_weight)
  n_random_effects <- length(random_effect)
  n_covariates <- length(all_col_names)-(2 + n_blocks + n_weight + n_random_effects)


  # create vector of column type names and vector variable type choices (dependent on role)
  column_types <- c(
    'Treatment',
    'Outcome',
    rep('Block', n_blocks),
    rep('Random Effect', n_random_effects),
    rep('Survey Weight', n_weight),
    rep('Covariate', n_covariates)
  )
  # order is very important see comment in verify_data_server.R
  variable_type_choices <- c(
    list(c('Categorical', 'Binary')),
    list(c('Continuous', 'Binary')),
    if(n_blocks > 0) lapply(1:n_blocks, function(x) c('Categorical', 'Binary')),
    if(n_random_effects > 0) lapply(1:n_random_effects, function(x) c('Categorical')),
    if(n_weight > 0) lapply(1:n_weight, function(x) c('Continuous')),
    lapply(1:n_covariates, function(x) c('Continuous', 'Categorical', 'Binary'))
  )


  # render the header to the table
  UI_header <- fluidRow(
    column(2, h6(create_info_icon('Role', 'You already specified variable roles on the data upload page.'))),
    column(4, h6(create_info_icon('Rename variable', 'Type in a new variable name, the software will take care of the rest.'))),
    column(4, h6(create_info_icon('Verify variable type', 'Confirm that the assigned variable type matches your data. You can manually override the pre-selected type by clicking the dropdown.'))),
    column(2, h6(create_info_icon('Percent NA', 'The percentage of missing data within each. Some changes you make to your data may change the Percent NA.')))
  )

  # render the rows
  UI_grid <- lapply(indices, function(i){
    fluidRow(
      column(width = 2,
             shinyjs::disabled(
               textInput(
                 inputId = ns(paste0(ns_prefix, "_", i, '_type')),
                 label = NULL,
                 value = column_types[i])
             )
      ),
      column(width = 4,
             textInput(
               inputId = ns(paste0(ns_prefix, "_", i, "_rename")),
               label = NULL,
               value = all_col_names[i])
      ),
      column(width = 4,
             if (i == 1){
               shinyjs::disabled(
                 selectInput(
                   inputId = ns(paste0(ns_prefix, "_", i, "_changeDataType")),
                   label = NULL,
                   choices = variable_type_choices[[i]],
                   selected = default_data_types[i])
               )
             } else {
               selectInput(
                 inputId = ns(paste0(ns_prefix, "_", i, "_changeDataType")),
                 label = NULL,
                 choices = variable_type_choices[[i]],
                 selected = default_data_types[i])
             }
      ),
      column(width = 2,
             shinyjs::disabled(
               textInput(
                 inputId = ns(paste0(ns_prefix, "_", i, "_percentNA")),
                 label = NULL,
                 placeholder = 'TBD')
             )
      )
    )
  })

  # combine the header and the rows
  UI_table <- tagList(UI_header, UI_grid)

  return(UI_table)
}

#' Create a new card on the learning landing page
#'
#' @param page_id the page id of the article
#' @param thumbnail_url the url of the thumbnail image
#' @param title title to display
#' @param description description to display under the title
#' @param width
#'
#' @return html
#' @export
#'
#' @noRd
#'
#' @examples
#' create_learning_card(
#'   page_id = 'concepts_link_causal_estimands',
#'   thumbnail_url = 'estimands.png',
#'   title = "Causal estimands",
#'   description = "BART allows ..."
#' )
create_learning_card <- function(page_id, thumbnail_url, title, description, width = 4){
  shiny::column(
    width = width,
    shiny::wellPanel(
      class = 'learning-card',
      shiny::actionLink(
        page_id,
        shiny::img(src = glue::glue("thumbnails/{thumbnail_url}"))
      ),
      shiny::br(),
      shiny::h3(title),
      shiny::p(description)
    )
  )
}

#' Create hover-able information icon
#'
#' Returns HTML code to create a little (i) icon that shows text when you hover.
#'
#' @param label any text to display next to the icon
#' @param text text to display when hovering over the icon
#'
#' @author Joe Marlo
#'
#' @return html
#' @export
#'
#' @noRd
#'
#' @examples
#' # within shiny UI pages
#' create_info_icon('Average Treatment Effect', 'The ATE is ...')
create_info_icon <- function(label, text){
  html <- paste0(
    '<div class="infoToolTip">',
    label,
    ' <a>&#9432;</a>',
    '<span class="infoToolTipText">',
    text,
    '</span></div>'
  )

  html <- HTML(html)
  return(html)
}

#' Create plain language interpretation of BART model
#'
#' @param .model
#' @param type
#' @param treatment
#' @param units
#' @param participants
#'
#' @author George Perrett
#'
#' @return character
#' @export
#' @noRd
create_interpretation <- function(.model, type, treatment, units, participants){
  if(treatment == '') treatment <- 'treatment condition'
  if(units == '') units <- 'units'
  if(participants == '') participants <- 'participants'
  if(type == 'Causal'){
    if(.model$estimand == 'att') estimand <- paste0('For ', participants, ' in this study that received the ', treatment,  ', receiving the ', treatment)
    if(.model$estimand == 'ate') estimand <- paste0('For ', participants, ' in this study, receiving the ', treatment)
    if(.model$estimand == 'atc') estimand <- paste0('For ', participants, ' in this study that did not receive the ', treatment, ' receiving the ', treatment, ' would have')

    if(as.data.frame(summary(.model)$estimates)[1] > 0) result <- paste0(' led to an increase of ', as.character(round(as.data.frame(summary(.model)$estimates)[1], 2)), ' ', units)
    if(as.data.frame(summary(.model)$estimates)[1] < 0) result <- paste0(' led to a decrease of ', as.character(round(as.data.frame(summary(.model)$estimates)[1], 2)), ' ', units)

    if(.model$estimand == 'att') counterfactual <- paste0(' compared to what would have happened had these ', participants, ' not received the ',treatment, '.')
    if(.model$estimand == 'ate') counterfactual <- paste0(' compared to what would have happened if ', participants, ' did not receive the ', treatment, '.')
    if(.model$estimand == 'atc') counterfactual <- paste0(' compared to the observed state where these ', participants, ' did not receive the ', treatment, '.')

    text <- paste0(estimand, result, counterfactual)
  }

  if(type != 'Causal'){
    if(as.data.frame(summary(.model)$estimates)[1] > 0) point <- 'higher'
    if(as.data.frame(summary(.model)$estimates)[1] < 0) point <- 'lower'
    text <- paste0('When comparing two groups of ', participants, ' who are similar on all covariates included in the analysis except for the ', treatment, ' the group of ', participants, ' that received the ', treatment, ' are expected to have outcomes that are ', as.character(round(as.data.frame(summary(.model)$estimates)[1], 2)),' ', units, ' ', point, ', on average, compared to the group of ', participants, ' that did not receive the ', treatment, '.')
  }

  return(text)

}

