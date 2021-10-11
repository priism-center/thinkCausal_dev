

.data <- tibble(Student = c('Blake', 'Kennedy', 'Jordan', 'Jordan', 'John'), 
                Z = c(1, 1, 0, 0, 0),
                Y0 = c('?', '?', 130, 128, 130),
                Y1 = c(115, 130, '?', '?', 120),
                Y = c(115, 130, 130, 128, 130),
                ITE = c(-6, -5, -2, -8, "?"))
correct_answers <- c("121", "135", "128", "120", "-10")
extra_header <- c('', 'Treatment', 'Potential Outcomes', 'Observed Outcomes', 'Treatment Effect')
extra_header_widths <- c(1, 1, 2, 1, 1)

#' Create a JavaScript table that has editable cells and checks for the correct input
#'
#' Creates a JavaScript table from a dataframe with editable cells where dataframe=='?'. User must input correct answer into those editable cells. Often used to create potential outcomes quizzes.
#'
#' @param .data a dataframe. Any cells that should require user input should be marked with a "?"
#' @param correct_answers a vector of the correct answers that map to the "?" rowwise (left-to-right then top-to-bottom)
#' @param extra_header optional. A vector of column names
#' @param extra_header_widths required only if extra_header is provided. A vector of column widths that sum to length(extra_header). I.e. 1 = width of one column; 2 = width of two columns, etc.
#' @param table_id optional. A string to use as the html ID for the table. If none is provided, then 'table' + random 15 letters is used
#' @param ns optional. A function defining the name space. To be used with shiny::NS
#'
#' @author Joe Marlo, Junhui Yang
#'
#' @return a string of JS and HTML code
#' @export
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
create_interactive_table <- function(.data, correct_answers, extra_header = NULL, extra_header_widths = rep(1, length(extra_header)), table_id = NULL, ns = NULL){
  
  # TODO: issue with ? cell border getting cutoff when extra_header = NULL
  
  total_questions <- length(correct_answers)
  if (sum(.data == '?') != total_questions) stop('Every question mark should have a respective correct_answer')
  
  # manage namespace
  if (is.null(ns)) ns <- function(input) input
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
    html_buttons
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
#' @param ns optional. A function defining the name space. To be used with shiny::NS
#'
#' @author Joe Marlo, Junhui Yang
#'
#' @return a string of JS and HTML code
#' @export
#'
#' @examples
#' html_string <- create_table(n_rows = 10, y_min = 50, y_max = 100, ate = -10, po_question = T, ite_question = T)
#' # HTML(html_string) within Shiny UI 
#' # access the user inputs via get_table_values(<table_id>) within Shiny server
create_table <- function(.data = NULL, correct_answers = NULL, n_rows, y_min, y_max, ate, po_question = T, ite_question = T, extra_header = NULL, extra_header_widths = rep(1, length(extra_header)), table_id = NULL, ns = NULL){
  
  # if .data is not provided, automatically generate a dataframe
  if (is.null(.data)){
    
    # 20 names that a table can randomly draw from
    names <- c('Blake', 'Kennedy', 'Taylor', 'Jordan', 'John', 'Andrew', 'Billie', 'Charlie', 'Casey', 'Alex', 'Parker', 'Andi', 'Anthony', 'Katie', 'Zoe','Juan', 'Kevin','Pengfei','Ruohan','Yi')
    
    # the maximum number of rows is 20 and the minimum is 4 
    if(n_rows <= 20 & n_rows >= 4){
      
      # randomly draw n_row students' names  
      Student <- sample(names, size = n_rows, replace = F)
      
      # randomly assign treatment for the n_row students
      Z <- rbinom(n_rows, size = 1, prob = 0.5)
      # re-assign treatment until each group has at least 2 students
      while (sum(Z == 0) < 2 | sum(Z == 1) < 2) {
        Z <- rbinom(n_rows, size = 1, prob = 0.5)
      }
      # sort treatment by treated group first and then control group
      Z <-  sort(Z, decreasing = T)
      
      if(ate >= 0){ # when ate is a positive value, generate Y1 greater than Y0
        Y0 <- round(runif(n_rows, min = y_min, max = y_max - ate))  
        Y1 <- Y0 + round(runif(n_rows, min = 0.7*ate, max = 1.3*ate)) # heterogeneous treatment effect for individuals, varying from 0.7*ate to 1.3*ate
        Y <- ifelse(Z == 1, Y1, Y0)
        ITE <- Y1 - Y0
      }else if(ate < 0){ # when ate is a negative value, generate Y1 less than Y0
        Y0 <- round(runif(n_rows, min = y_min + ate, max = y_max))
        Y1 <- Y0 + round(runif(n_rows, min = 1.3*ate, max = 0.7*ate)) # heterogeneous treatment effect for individuals, varying from 1.3*ate to 0.7*ate
        Y <- ifelse(Z == 1, Y1, Y0)
        ITE <- Y1 - Y0
      }else{ # if the ate entered is not a number
        stop('Please enter a numeric value')
      }
      
    }else if(n_rows > 20){ # if the n_rows entered is greater than 20, stop and prompt an error
      
      stop('Please enter the number of rows less than or equal to 20')
      
    }else{ # if the n_rows entered is less than 4, stop and prompt an error
      
      stop('Please enter the number of rows greater than or equal to 4')
      
    }
    
    df <- data.frame(Student, Z, Y0, Y1, Y, ITE)
    df <- df %>% mutate(Y0 = as.character(Y0), Y1 = as.character(Y1), ITE = as.character(ITE))
    
    if(po_question == T & ite_question == T){ # generate questions ('?') for both potential outcomes (Y1 and Y0) and ITE
      
      # get the indexes of rows that are of treated group and control group
      idx_Z1 <- which(Z == 1)
      idx_Z0 <- which(Z == 0)
      # the number of questions for potential outcomes are 70% of the number of units in the two groups
      number_questions_Z1 <- round(0.7*length(idx_Z1))
      number_questions_Z0 <- round(0.7* length(idx_Z0))
      
      correct_answers <- c(df$Y0[1:number_questions_Z1], # the first 70% units in the treated group are designed for questions asking for Y0
                           df$ITE[(number_questions_Z1 + 1):length(idx_Z1)], # the rest 30% units in the treated group are designed for questions asking for ITE
                           df$Y1[(length(idx_Z1) + 1):(length(idx_Z1) + number_questions_Z0)], # the first 70% units in the control group are designed for questions asking for Y1
                           df$ITE[(length(idx_Z1) + number_questions_Z0 + 1):n_rows]) # the rest 30% units in the control group are designed for questions asking for ITE
      df$Y0[1:number_questions_Z1] <- '?'
      df$ITE[(number_questions_Z1 + 1):length(idx_Z1)] <- '?' 
      df$Y1[(length(idx_Z1) + 1):(length(idx_Z1) + number_questions_Z0)] <- '?'
      df$ITE[(length(idx_Z1) + number_questions_Z0 + 1):n_rows] <- '?'
      
    }else if(po_question == T & ite_question == F){ # generate questions ('?') for only potential outcomes (Y1 and Y0)
      
      # get the indexes of rows that are of treated group and control group
      idx_Z0 <- which(Z == 0)  
      idx_Z1 <- which(Z == 1)
      
      # all Y0 for units in the treated group are questions, and all Y1 for units in the control group are questions
      correct_answers <- c(df$Y0[idx_Z1], df$Y1[idx_Z0])
      df$Y0[idx_Z1] <- '?'
      df$Y1[idx_Z0] <- '?'
      
    }else if(po_question == F & ite_question == T){ # generate questions ('?') for only ITE
      
      # randomly sample the indexes of half of the sample size
      idx <- sort(sample(1:n_rows, size = round(0.5*n_rows)))
      # the sampled indexes are the rows for ITE questions
      correct_answers <- df$ITE[idx]
      df$ITE[idx] <- '?'
      
    }else{ # if po_question == F & ite_question == F, ask for at least one of the argument set to be TRUE
      
      stop('Please select at least one type of questions')
      
    }
    
    .data <- df
    
    # if .data and extra_header are not provided, set default extra_header and extra_header_widths
    if(is.null(extra_header)){
      extra_header <- c('', 'Treatment', 'Potential Outcomes', 'Observed Outcomes', 'Treatment Effect')
      extra_header_widths <- c(1, 1, 2, 1, 1)
    }
    
  }
  
  
  # convert to html code
  html_code <- create_interactive_table(
    .data = .data,
    correct_answers = correct_answers,
    extra_header = extra_header,
    extra_header_widths = extra_header_widths,
    table_id = table_id,
    ns = ns
  )
  
  return(html_code)
}
