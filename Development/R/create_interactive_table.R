

.data <- tibble(Student = c('Blake', 'Kennedy', 'Jordan', 'Jordan'), 
                Z = c(1, 1, 0, 0),
                Y1 = c(115, 130, '?', '?'),
                Y0 = c('?', '?', 130, 128),
                Y = c(115, 130, 130, 128),
                ITE = c(-6, -5, -2, -8))
correct_answers <- c("121", "135", "127", "120")
# correct_answers <- c("121", "135", )

create_interactive_table <- function(.data, correct_answers, extra_header, extra_header_widths, ns){
  
  total_questions <- length(correct_answers)
  if (sum(.data == '?') != total_questions) stop('Every question mark should have a respective correct_answer')
  
  # TODO: theres a disconnect b/t "correct_answers" and I think some math happening in the JS
  # TODO: I think the correct_answers is mapped rowwise to the dataframe "?"
  
  js_downloads <-
  '
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
  <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>
  '
  
  # ["121", "135", "127", "120"];
  correct_answers_char <- paste0("'", paste(correct_answers, collapse = "', '"), "'")
  
  js_check_values <- paste0(
  '
  <script type="text/javascript">
    $(document).ready(function() {
      $("#submit").click(function() {
        var t_vals = [', correct_answers_char, ']; 
        var q_tot = ', total_questions, ';
        var q = 0;
        for (i = 1; i <= q_tot; i++) {
          var r = $("#table1 #add" + i).text();
          r=parseInt(r);
          r = r.toString();
          r = r.replace(NaN, "FILL IN");
          if (r==t_vals[i-1]) {
            q = q + 1;
            $("#table1 #add" + i).text(r);
            $("#table1 #add" + i).append("    " + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\');
          }
          else {
            $("#table1 #add" + i).text(r);
            $("#table1 #add" + i).append("    " + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\');
          }
        };
        if (q == ', total_questions, ') {
          $(".congrats").append("Congratulations!").css("color", "green");
        }
       return false;
      });
    });
  </script>
  ',
  collapse = '')
  
  js_clear_input <- paste0(
  '
  <script type="text/javascript">
    $(document).ready(function() {
      $("#clear").click(function() {
        var q_tot = ', total_questions, ';
        for (i = 1; i <= q_tot; i++) {
           $("#table1 #add" + i).text("FILL IN")
        };
        $(".congrats").text("");
       return false;
      });
    });
  </script>
  ',
  collapse = '')
  
  # TODO: what does this do?
  js_add_listener <- 
  '
  <script>
    window.onload = function(){
    const getCellValue = (tr, idx) => tr.children[idx].innerText || tr.children[idx].textContent;
    
    const comparer = (idx, asc) => (a, b) => ((v1, v2) => 
        v1 !== "" && v2 !== "" && !isNaN(v1) && !isNaN(v2) ? v1 - v2 : v1.toString().localeCompare(v2)
        )(getCellValue(asc ? a : b, idx), getCellValue(asc ? b : a, idx));
    
    document.querySelectorAll("th").forEach(th => th.addEventListener("click", (() => {
        const table = th.closest("table");
        Array.from(table.querySelectorAll("tr:nth-child(n+2)"))
            .sort(comparer(Array.from(th.parentNode.children).indexOf(th), this.asc = !this.asc))
            .forEach(tr => table.appendChild(tr) );
    })));
  }
  </script> 
  '
  
  header <- create_header_html(.data, extra_header, extra_header_widths)
  html_table_header <- paste0('<table class="table table-bordered table-responsive-md table-striped text-center" id="table1">',
                              header)
  
  # TODO why is the second row always bold?
  
  # html_table_header <-
  # '
  # <table class="table table-bordered table-responsive-md table-striped text-center" id="table1">
  #   <thead>
  #     <tr>
  #       <th class="text-center"></th>
  #       <th class="text-center">Treatment</th>
  #       <th class="text-center" colspan = "2">Potential Outcomes</th>
  #       <th class="text-center">Observed Outcomes</th>
  #       <th class="text-center">Treatment Effect</th>
  # 
  #     </tr>
  #   </thead>
  #   <tbody>
  #     <tr>
  #     <td class="text-center">Student</td>
  #     <td class="text-center">Z</td>
  #     <td class="text-center">Y1</td>
  #     <td class="text-center">Y0</td>
  #     <td class="text-center">Y</td>
  #     <td class="text-center">ITE</td>
  #   </tr>
  # '
  
  # create the rows of the table
  rows <- create_row_html(.data)
  html_table_body <- paste0(rows, '</tbody></table>')
  
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
  html_code <-
    paste0(
      js_downloads,
      js_check_values,
      js_clear_input,
      js_add_listener,
      html_table_header,
      html_table_body,
      html_buttons
    )
  
  return(html_code)
}

create_header_html <- function(.data, extra_header, extra_header_widths){
  
  # TODO: make the header reactive to the data
  
  header_first <- NULL
  if (!is.null(extra_header)){
    header_first <- 
      '
          <thead>
      <tr>
        <th class="text-center"></th>
        <th class="text-center">Treatment</th>
        <th class="text-center" colspan = "2">Potential Outcomes</th>
        <th class="text-center">Observed Outcomes</th>
        <th class="text-center">Treatment Effect</th>
  
      </tr>
    </thead>
    '
    
    header_second <- 
      '
      <tbody>
      <tr>
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">Y1</td>
      <td class="text-center">Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
      </tr>
    '
      
  } else {
    header_second <- 
      '
                <thead>
      <tr>
      <th class="text-center">Student</th>
      <th class="text-center">Z</th>
      <th class="text-center">Y1</th>
      <th class="text-center">Y0</th>
      <th class="text-center">Y</th>
      <th class="text-center">ITE</th>
      </tr>
    </thead>
    <tbody>
    '
  }

  # collapse into one string
  html_header <- paste(header_first, header_second, collape = '')
  
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
