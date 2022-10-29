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
        text = "Covariates",
        labels = auto_columns$X,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_treatment")),
        text = "Treatment",
        labels = auto_columns$Z,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_response")),
        text = "Outcome",
        labels = auto_columns$Y,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      if(design == 'Block randomized treatment'){
        sortable::add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_block")),
          text = "Blocking variable(s)",
          labels = NULL,
          options = sortable::sortable_options(multiDrag = TRUE)
        )
      },
      if(weights == 'Yes'){
        sortable::add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_weight")),
          text = "Survey weight",
          labels = NULL,
          options = sortable::sortable_options(multiDrag = TRUE)
        )
      },
      if(ran_eff == 'Yes'){
        sortable::add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_ran_eff")),
          text = "Random Intercept(s)",
          labels = NULL,
          options = sortable::sortable_options(multiDrag = TRUE)
        )
      },
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_post_treatment")),
        text = create_info_icon(
          label = "Post-treatment variables to exclude from analysis",
          text = "All variables that could potentially be affected by the treatment"
        ),
        labels = NULL,
        options = sortable::sortable_options(multiDrag = TRUE)
      ),
      sortable::add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_delete")),
        text = create_info_icon(
          label = "ID or index variables to exclude from analysis",
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
#' @param .model a bartCause model
#' @param type 'Causal' or NULL
#' @param treatment string to use as the treatment name
#' @param units string to use as the units name
#' @param participants string to useas the participants name
#'
#' @author George Perrett
#'
#' @return character
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



# potential outcomes table ------------------------------------------------

# .data <- tibble(Student = c('Blake', 'Kennedy', 'Jordan', 'Jordan', 'John'),
#                 Z = c(1, 1, 0, 0, 0),
#                 Y0 = c('?', '?', 130, 128, 130),
#                 Y1 = c(115, 130, '?', '?', 120),
#                 Y = c(115, 130, 130, 128, 130),
#                 ITE = c(-6, -5, -2, -8, "?"))
# correct_answers <- c("121", "135", "128", "120", "-10")
# extra_header <- c('', 'Treatment', 'Potential Outcomes', 'Observed Outcomes', 'Treatment Effect')
# extra_header_widths <- c(1, 1, 2, 1, 1)

#' Create a JavaScript table that has editable cells and checks for the correct input
#'
#' Creates a JavaScript table from a dataframe with editable cells where dataframe=='?'. User must input correct answer into those editable cells. Often used to create potential outcomes quizzes.
#'
#' @param .data a dataframe. Any cells that should require user input should be marked with a "?"
#' @param correct_answers a vector of the correct answers that map to the "?" rowwise (left-to-right then top-to-bottom)
#' @param extra_header optional. A vector of column names
#' @param extra_header_widths required only if extra_header is provided. A vector of column widths that sum to length(extra_header). I.e. 1 = width of one column; 2 = width of two columns, etc.
#' @param button logical to inclde clear and submit button only set to false when using for estimands
#' @param table_id optional. A string to use as the html ID for the table. If none is provided, then 'table' + random 15 letters is used
#' @param ns optional. A function defining the name space. To be used with shiny::NS
#'
#' @author Joe Marlo, Junhui Yang
#'
#' @return a string of JS and HTML code
#' @noRd
#'
#' @examples
#' .data <- tibble(
#'   Student = c('Blake', 'Kennedy', 'Jordan', 'Jordan', 'John'),
#'   Z = c(1, 1, 0, 0, 0),
#'   Y1 = c(115, 130, '?', '?', 120),
#'   Y0 = c('?', '?', 130, 128, 130),
#'   Y = c(115, 130, 130, 128, 130),
#'   ITE = c(-6, -5, -2, -8, "?")
#'  )
#' correct_answers <- c("121", "135", "127", "120", "5")
#' extra_header <- c('', 'Treatment', 'Potential Outcomes', 'Observed Outcomes', 'Treatment Effect')
#' extra_header_widths <- c(1, 1, 2, 1, 1)
#' html_string <- create_interactive_table(.data, correct_answers, extra_header, extra_header_widths)
#' # HTML(html_string) within Shiny UI
#' # access the user inputs via get_table_values(<table_id>) within Shiny server
create_interactive_table <- function(.data, correct_answers, extra_header = NULL, extra_header_widths = rep(1, length(extra_header)), button = TRUE, table_id = NULL, ns = NULL){

  # TODO: issue with ? cell border getting cutoff when extra_header = NULL

  total_questions <- length(correct_answers)
  if (sum(.data == '?') != total_questions) stop('Every question mark should have a respective correct_answer')

  # manage namespace
  if (is.null(ns)) ns <- function(x) x
  if (is.null(table_id)) table_id <- paste0('table', paste0(sample(letters, 15, replace = TRUE), collapse = ''))
  table_id <- ns(table_id)

  # create code to download JS scripts and CSS
  # can safely remove this unless using tables in clean HTML environment
  # js_downloads <-
  #   '
  # <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
  # <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
  # <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
  # <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>
  # '

  # convert correct answers to a string
  correct_answers_char <- paste0("'", paste(correct_answers, collapse = "', '"), "'")

  # create the JS code to check if all answers are correct
  js_check_values <- paste0(
    '
  <script type="text/javascript">
    $(document).ready(function() {
      $("#submit_', table_id, '").click(function() {
        var t_vals = [', correct_answers_char, '];
        var q_tot = ', total_questions, ';
        var q = 0;
        var user_inputs = []
        for (i = 1; i <= q_tot; i++) {
          var r = $("#', table_id, ' #add" + i).text();
          r = parseInt(r);
          r = r.toString();
          r = r.replace(NaN, "?");
          user_inputs[i-1] = r
          if (r==t_vals[i-1]) {
            q = q + 1;
            $("#', table_id, ' #add" + i).text(r);
            $("#', table_id, ' #add" + i).append("    " + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\');
          }
          else {
            $("#', table_id, ' #add" + i).text(r);
            $("#', table_id, ' #add" + i).append("    " + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\');
          }
        };
        Shiny.setInputValue("user_input_', table_id, '", user_inputs);
       return false;
      });
    });
  </script>
  '
  )

  # create the JS code that clears the inputs after button click
  js_clear_input <- paste0(
    '
  <script type="text/javascript">
    $(document).ready(function() {
      $("#clear_', table_id, '").click(function() {
        var q_tot = ', total_questions, ';
        for (i = 1; i <= q_tot; i++) {
           $("#', table_id, ' #add" + i).text("?")
        };
       return false;
      });
    });
  </script>
  '
  )

  # create the HTML header
  header <- create_header_html(.data = .data, extra_header = extra_header, extra_header_widths = extra_header_widths)
  html_table_header <- paste0('<table class="table table-bordered table-responsive-md table-striped text-center" id="', table_id, '">',
                              header)

  # create the HTML rows of the table
  rows <- create_row_html(.data = .data)
  html_table_body <- paste0(rows, '</tbody></table>')

  # create the HTML buttons
  html_buttons <- div(
    class = 'backNextContainer',
    actionButton(inputId = paste0('submit_', table_id),
                 label = 'Submit'),
    actionButton(inputId = paste0('clear_', table_id),
                 label = 'Clear'),
    style = 'max-width:40rem'
  )


  # concatenate the code into one string
  html_code <- paste0(
    # js_downloads,
    js_check_values,
    js_clear_input,
    html_table_header,
    html_table_body,
    if(isTRUE(button))html_buttons
  )

  return(html_code)
}

create_header_html <- function(.data, extra_header, extra_header_widths){

  if (!is.null(extra_header) & ncol(.data) != sum(extra_header_widths)) stop("extra_header_widths must sum to the number of columns in .data")

  header_first <- NULL
  if (!is.null(extra_header)){

    # create an extra header (i.e. the very first row)
    header_first <- paste0(
      '<th class="text-center" style="font-weight: 700;" colspan = "',
      extra_header_widths,
      '">',
      extra_header,
      '</th>',
      collapse = ''
    )

    # add head html
    header_first <- paste0(
      '<thead><tr>',
      header_first,
      '</tr></thead>'
    )

    # create the subheader for each column in the data
    header_second <- paste0(
      '<td class="text-center" style="font-weight: 700;">',
      colnames(.data),
      '</td>',
      collapse = ''
    )

    # add head html
    header_second <- paste0(
      '<tbody><tr>',
      header_second,
      '</tr>'
    )

  } else {

    # create a header for each column in the data
    header_second <- paste0(
      '<th class="text-center" style="font-weight: 700;">',
      colnames(.data),
      '</th>',
      collapse = ''
    )

    # add head html
    header_second <- paste0(
      '<thead><tr>',
      header_second,
      '</tr></thead><tbody>'
    )
  }

  # collapse into one string
  html_header <- paste(header_first, header_second, collapse = '')

  return(html_header)
}

create_row_html <- function(.data){

  # transpose the table to make indexing easier
  data_t <- t(.data)

  # surround the content with html denoting a row
  row_html <- paste0(
    '<td contenteditable="false" style="font-weight: 300">',
    as.matrix(data_t),
    '</td>'
  )

  # add in the html ids for the editable cells
  modify_ids <- paste0('add', seq_along(data_t[data_t == '?']))
  row_html[data_t == '?'] <- paste0(
    '<td contenteditable="true" onclick = "this.innerHTML=&#39;&nbsp;&#39;;" style="font-weight: 300; background-color: #e8e8e8; border: 2px dotted #919191;" id = "',
    modify_ids,
    '">?</td>'
  )

  # add in html row dividers
  break_indices <- split(seq_along(as.matrix(data_t)),
                         sort(rep(1:ncol(data_t), nrow(data_t))))
  row_html_collapsed <- sapply(break_indices, function(breaks){
    paste0(
      '<tr>',
      paste0(row_html[breaks], collapse = ''),
      '</tr>'
    )
  })
  row_html_collapsed <- paste0(row_html_collapsed, collapse = '')

  return(row_html_collapsed)
}

#' Retrieve user inputted values from a table
#'
#' Function to use on the server side to retreive the user input from an interactive table.
#'
#' @param input the global `input` object within Shiny server
#' @param table_id string. The id assigned to the table when creating the table
#' @param ns optional. A function defining the name space. To be used with shiny::NS
#' @param convert_to_numeric defaults to TRUE. Automatically coerces values to numeric
#'
#' @author Joe Marlo
#'
#' @return values
#' @noRd
#'
#' @seealso \code{\link{create_interactive_table}}
#'
#' @examples
#' .data <- tibble(
#'   Student = c('Blake', 'Kennedy', 'Jordan', 'Jordan', 'John'),
#'   Z = c(1, 1, 0, 0, 0),
#'   Y1 = c(115, 130, '?', '?', 120),
#'   Y0 = c('?', '?', 130, 128, 130),
#'   Y = c(115, 130, 130, 128, 130),
#'   ITE = c(-6, -5, -2, -8, "?")
#'  )
#' correct_answers <- c("121", "135", "127", "120", "5")
#' html_string <- create_interactive_table(.data, correct_answers, table_id = 'mytable')
#' # HTML(html_string) within Shiny UI
#' # get_table_values(input, 'mytable') within Shiny server
get_table_values <- function(input, table_id, ns = NULL, convert_to_numeric = TRUE){
  if (is.null(ns)) ns <- function(x) x
  table_id <- ns(table_id)
  table_id <- paste0('user_input_', table_id)
  raw_input <- input[[table_id]]
  if (convert_to_numeric) raw_input <- suppressWarnings(as.numeric(raw_input))
  return(raw_input)
}

# goal is to wrap the create_interactive_table so we can either pass it a dataframe
# OR have it generate a table on the fly

#' Automatically create a JavaScript table that has editable cells and checks for the correct input
#'
#' Automatically create a JavaScript table from a dataframe with editable cells where dataframe=='?'. User must input correct answer into those editable cells. Often used to create potential outcomes quizzes.
#'
#' @param .data optional. A dataframe to generate an interactive table. Any cells that should require user input should be marked with a "?"
#' @param correct_answers required only if .data is provided. A vector of the correct answers that map to the "?" rowwise (left-to-right then top-to-bottom)
#' @param n_rows an integer to use as the number of rows for the table
#' @param y_min a numeric value to use as the lower bound of potential outcomes
#' @param y_max a numeric value to use as the upper bound of potential outcomes
#' @param ate a numeric value to use as an indicator of positive/negative and magnitude of ITEs
#' @param po_question logical indicating if the questions (the places of '?') are at the columns of potential outcomes Y0 and Y1
#' @param ite_question logical indicating if the questions (the places of '?') are at the column of ITE
#' @param extra_header a vector of column names. If .data is provided, optional to specify; if .data is not provided, keep it equal NULL.
#' @param extra_header_widths required only if extra_header is provided. A vector of column widths that sum to length(extra_header). I.e. 1 = width of one column; 2 = width of two columns, etc.
#' @param table_id optional. A string to use as the html ID for the table. If none is provided, then 'table' + random 15 letters is used
#' @param button logical to inclde clear and submit button only set to false when using for estimands
#' @param ns optional. A function defining the name space. To be used with shiny::NS
#' @param id_unit optional. A string to set the name of the ID column: Student, Runner, Plant etc
#'
#' @author Joe Marlo, Junhui Yang, George Perrett
#'
#' @return a list with element 1 = a string of JS and HTML code and element 2 = the dataframe used to make the table
#' @noRd
#'
#' @examples
#' html_string <- create_table(n_rows = 10, y_min = 50, y_max = 100, ate = -10, po_question = T, ite_question = T)
#' # HTML(html_string[[1]]) within Shiny UI
#' # access the user inputs via get_table_values(<table_id>) within Shiny server
#' # access data html_string[[2]]
create_table <- function(.data = NULL, correct_answers = NULL, n_rows = 6, y_min = 50, y_max = 100, ate = 10, po_question = TRUE, ite_question = TRUE, extra_header = NULL, extra_header_widths = rep(1, length(extra_header)), table_id = NULL, button = TRUE, ns = NULL, id_unit = NULL){

  # TODO: when po_question = FALSE and ite_question = TRUE, why are only three rows have '?'

  # if .data is not provided, automatically generate a dataframe
  if (is.null(.data)){

    if (n_rows > 20 | n_rows < 4) stop('n_rows must be between [4, 20]')
    if (!is.numeric(ate)) stop('ate must be a numeric')

    # 20 names that a table can randomly draw from
    # names <- c(
    #     'Blake',
    #     'Kennedy',
    #     'Taylor',
    #     'Jordan',
    #     'John',
    #     'Andrew',
    #     'Billie',
    #     'Charlie',
    #     'Casey',
    #     'Alex',
    #     'Parker',
    #     'Andi',
    #     'Anthony',
    #     'Katie',
    #     'Zoe',
    #     'Juan',
    #     'Kevin',
    #     'Pengfei',
    #     'Ruohan',
    #     'Yi'
    #   )
    names <- 1:20


    ID <- names[1:n_rows] #sample(names, size = n_rows, replace = F)

    # randomly assign treatment for the n_row students
    Z <- rbinom(n_rows, size = 1, prob = 0.5)

    # re-assign treatment until each group has at least 2 students
    while (sum(Z == 0) < 2 | sum(Z == 1) < 2) {
      Z <- rbinom(n_rows, size = 1, prob = 0.5)
    }

    # sort treatment by treated group first and then control group
    Z <- sort(Z, decreasing = T)

    if (ate >= 0){ # when ate is a positive value, generate Y1 greater than Y0
      Y0 <- round(runif(n_rows, min = y_min, max = y_max - ate))
      Y1 <- Y0 + round(runif(n_rows, min = 0.7*ate, max = 1.3*ate)) # heterogeneous treatment effect for individuals, varying from 0.7*ate to 1.3*ate
      Y <- ifelse(Z == 1, Y1, Y0)
      ITE <- Y1 - Y0
    } else if (ate < 0){ # when ate is a negative value, generate Y1 less than Y0
      Y0 <- round(runif(n_rows, min = y_min + ate, max = y_max))
      Y1 <- Y0 + round(runif(n_rows, min = 1.3*ate, max = 0.7*ate)) # heterogeneous treatment effect for individuals, varying from 1.3*ate to 0.7*ate
      Y <- ifelse(Z == 1, Y1, Y0)
      ITE <- Y1 - Y0
    }

    # create the dataframe
    df <- data.frame(ID, Z, Y0, Y1, Y, ITE)
    if(!rlang::is_null(id_unit)) names(df)[1] <- id_unit
    df <- dplyr::mutate(df, Y0 = as.character(Y0), Y1 = as.character(Y1), ITE = as.character(ITE))

    if (po_question == T & ite_question == T){ # generate questions ('?') for both potential outcomes (Y1 and Y0) and ITE

      # get the indexes of rows that are of treated group and control group
      idx_Z1 <- which(Z == 1)
      idx_Z0 <- which(Z == 0)

      # the number of questions for potential outcomes are 70% of the number of units in the two groups
      number_questions_Z1 <- round(0.7 * length(idx_Z1))
      number_questions_Z0 <- round(0.7 * length(idx_Z0))

      correct_answers <- c(df$Y0[1:number_questions_Z1], # the first 70% units in the treated group are designed for questions asking for Y0
                           df$ITE[(number_questions_Z1 + 1):length(idx_Z1)], # the rest 30% units in the treated group are designed for questions asking for ITE
                           df$Y1[(length(idx_Z1) + 1):(length(idx_Z1) + number_questions_Z0)], # the first 70% units in the control group are designed for questions asking for Y1
                           df$ITE[(length(idx_Z1) + number_questions_Z0 + 1):n_rows]) # the rest 30% units in the control group are designed for questions asking for ITE
      df$Y0[1:number_questions_Z1] <- '?'
      df$ITE[(number_questions_Z1 + 1):length(idx_Z1)] <- '?'
      df$Y1[(length(idx_Z1) + 1):(length(idx_Z1) + number_questions_Z0)] <- '?'
      df$ITE[(length(idx_Z1) + number_questions_Z0 + 1):n_rows] <- '?'

    } else if (po_question == T & ite_question == F){ # generate questions ('?') for only potential outcomes (Y1 and Y0)

      # get the indexes of rows that are of treated group and control group
      idx_Z0 <- which(Z == 0)
      idx_Z1 <- which(Z == 1)

      # all Y0 for units in the treated group are questions, and all Y1 for units in the control group are questions
      correct_answers <- c(df$Y0[idx_Z1], df$Y1[idx_Z0])
      df$Y0[idx_Z1] <- '?'
      df$Y1[idx_Z0] <- '?'

    } else if (po_question == F & ite_question == T){ # generate questions ('?') for only ITE

      # randomly sample the indexes of half of the sample size
      # TODO: why only half?
      # idx <- sort(sample(1:n_rows, size = round(0.5*n_rows)))
      idx <- 1:n_rows

      # the sampled indexes are the rows for ITE questions
      correct_answers <- df$ITE[idx]
      df$ITE[idx] <- '?'

    } else if(po_question == F & ite_question == F) { # if po_question == F & ite_question == F, this is a complete table used for estimands
      idx <- 1:n_rows

    }

    .data <- df

    # if .data and extra_header are not provided, set default extra_header and extra_header_widths
    if (is.null(extra_header)){
      extra_header <- c('', 'Treatment', 'Potential Outcomes', 'Observed Outcomes', 'Treatment Effect')
      extra_header_widths <- c(1, 1, 2, 1, 1)
    }

  }


  # convert the table into html code
  html_code <- create_interactive_table(
    .data = .data,
    correct_answers = correct_answers,
    extra_header = extra_header,
    extra_header_widths = extra_header_widths,
    table_id = table_id,
    button = button,
    ns = ns
  )

  out <- list(html_code, .data)

  return(out)
}
