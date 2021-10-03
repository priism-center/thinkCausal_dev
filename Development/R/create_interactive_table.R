

.data <- tibble(Student = c('Blake', 'Kennedy', 'Jordan', 'Jordan', 'John'), 
                Z = c(1, 1, 0, 0, 0),
                Y0 = c('?', '?', 130, 128, 130),
                Y1 = c(115, 130, '?', '?', 120),
                Y = c(115, 130, 130, 128, 130),
                ITE = c(-6, -5, -2, -8, "?"))
correct_answers <- c("121", "135", "127", "120", "5")
# correct_answers <- c("121", "135", '120', '120')
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
#'  Y1 = c(115, 130, '?', '?', 120),
#'  Y0 = c('?', '?', 130, 128, 130),
#'  Y = c(115, 130, 130, 128, 130),
#'  ITE = c(-6, -5, -2, -8, "?")
#'  )
#' correct_answers <- c("121", "135", "127", "120", "5")
#' extra_header <- c('', 'Treatment', 'Potential Outcomes', 'Observed Outcomes', 'Treatment Effect')
#' extra_header_widths <- c(1, 1, 2, 1, 1)
#' html_string <- create_interactive_table(.data, correct_answers, extra_header, extra_header_widths)
#' # HTML(html_string) within Shiny UI 
#' # access the user inputs via get_table_values(<table_id>) within Shiny server
create_interactive_table <- function(.data, correct_answers, extra_header = NULL, extra_header_widths = rep(1, length(extra_header)), table_id = NULL, ns = NULL){
  
  # TODO: why is the second row *always* bold? CSS?
  # TODO: clean up buttons with CSS?
  
  total_questions <- length(correct_answers)
  if (sum(.data == '?') != total_questions) stop('Every question mark should have a respective correct_answer')
  
  # manage namespace
  if (is.null(ns)) ns <- function(input) input
  if (is.null(table_id)) table_id <- paste0('table', paste0(sample(letters, 15, replace = TRUE), collapse = ''))
  table_id <- ns(table_id)
  
  # create code to download JS scripts and CSS
  # TODO: is this neccessary? Works without it (unless its being sourced somewhere else in the script and it breaks all shiny outputs across the app)
  # js_downloads <-
  #   '
  # <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
  # <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
  # <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
  # <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>
  # '
  
  # ["121", "135", "127", "120"];
  # convert correct answers to a string
  correct_answers_char <- paste0("'", paste(correct_answers, collapse = "', '"), "'")
  
  # create the JS code to check if all answers are correct
  js_check_values <- paste0(
    '
  <script type="text/javascript">
    $(document).ready(function() {
      $("#submit").click(function() {
        var t_vals = [', correct_answers_char, ']; 
        var q_tot = ', total_questions, ';
        var q = 0;
        var user_inputs = []
        for (i = 1; i <= q_tot; i++) {
          var r = $("#', table_id, ' #add" + i).text();
          r = parseInt(r);
          r = r.toString();
          r = r.replace(NaN, "FILL IN");
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
        if (q == ', total_questions, ') {
          $(".congrats").append("Congratulations!").css("color", "green");
        }
        Shiny.setInputValue("user_input_', table_id, '", user_inputs);
       return false;
      });
    });
  </script>
  ',
    collapse = '')
  
  # create the JS code that clears the inputs after button click
  js_clear_input <- paste0(
    '
  <script type="text/javascript">
    $(document).ready(function() {
      $("#clear").click(function() {
        var q_tot = ', total_questions, ';
        for (i = 1; i <= q_tot; i++) {
           $("#', table_id, ' #add" + i).text("FILL IN")
        };
        $(".congrats").text("");
       return false;
      });
    });
  </script>
  ',
    collapse = '')
  
  # TODO: what does this do? table still works after leaving it out
  # js_add_listener <- 
  #   '
  # <script>
  #   window.onload = function(){
  #   const getCellValue = (tr, idx) => tr.children[idx].innerText || tr.children[idx].textContent;
  #   
  #   const comparer = (idx, asc) => (a, b) => ((v1, v2) => 
  #       v1 !== "" && v2 !== "" && !isNaN(v1) && !isNaN(v2) ? v1 - v2 : v1.toString().localeCompare(v2)
  #       )(getCellValue(asc ? a : b, idx), getCellValue(asc ? b : a, idx));
  #   
  #   document.querySelectorAll("th").forEach(th => th.addEventListener("click", (() => {
  #       const table = th.closest("table");
  #       Array.from(table.querySelectorAll("tr:nth-child(n+2)"))
  #           .sort(comparer(Array.from(th.parentNode.children).indexOf(th), this.asc = !this.asc))
  #           .forEach(tr => table.appendChild(tr) );
  #   })));
  # }
  # </script> 
  # '
  
  # create the HTML header 
  header <- create_header_html(.data, extra_header, extra_header_widths)
  html_table_header <- paste0('<table class="table table-bordered table-responsive-md table-striped text-center" id="', table_id, '">',
                              header)
  
  # create the HTML rows of the table
  rows <- create_row_html(.data)
  html_table_body <- paste0(rows, '</tbody></table>')
  
  # create the HTML buttons
  html_buttons <- 
    '
  <div>
    <input id="submit" type="button" value="Submit" />
  </div>
  <div>
      <input id="clear" type="button" value="Clear" />
  </div>
  <div class = "congrats"></div>
  '
  
  # concatenate the code into one string
  html_code <- paste0(
    # js_downloads,
    js_check_values,
    js_clear_input,
    # js_add_listener,
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
      '<th class="text-center" colspan = "', 
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
      '<td class="text-center">',
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
      '<th class="text-center">',
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
    '<td contenteditable="false">',
    as.matrix(data_t),
    '</td>'
  )
  
  # add in the html ids for the editable cells
  modify_ids <- paste0('add', seq_along(data_t[data_t == '?']))
  row_html[data_t == '?'] <- paste0(
    '<td contenteditable="true" id = "',
    modify_ids,
    '">FILL IN</td>'
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
  if (is.null(ns)) ns <- function(input) input
  table_id <- ns(table_id)
  table_id <- paste0('user_input_', table_id)
  raw_input <- input[[table_id]]
  if (convert_to_numeric) raw_input <- suppressWarnings(as.numeric(raw_input))
  return(raw_input)
}
