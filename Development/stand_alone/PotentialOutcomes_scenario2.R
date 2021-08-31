
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
    )
  ),
  div(
  fluidRow(column(12, h4('Counterfactual Outcome Y1 and Y0', style = "text-align:center"),
                  hr(), 
                  HTML('
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
<link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>

<script type="text/javascript">
$(document).ready(function() {
  $("#submit").click(function() {
    var t_vals = ["121", "135", "127", "120"];
    var q_tot = 4;
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
    if (q == 4) {
      $(".congrats").append("Congratulations! ☺").css("color", "green");
    }
   return false;
  });
});
</script>

<script type="text/javascript">
$(document).ready(function() {
  $("#clear").click(function() {
    var q_tot = 4;
    for (i = 1; i <= q_tot; i++) {
       $("#table1 #add" + i).text("FILL IN")
    };
    $(".congrats").text("");
   return false;
  });
});
</script>

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
<table class="table table-bordered table-responsive-md table-striped text-center" id="table1">
  <thead>
    <tr>
      <th class="text-center"></th>
      <th class="text-center">Treatment</th>
      <th class="text-center" colspan = "2">Potential Outcomes</th>
      <th class="text-center">Observed Outcomes</th>
      <th class="text-center">Treatment Effect</th>

    </tr>
  </thead>
  <tbody>
    <tr>
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">Y1</td>
      <td class="text-center">Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">1</td>
      <td contenteditable="false">115</td>
      <td contenteditable="true" id = "add1">FILL IN</td>
      <td contenteditable="false">115</td>
      <td contenteditable="false">-6</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">1</td>
      <td contenteditable="false">130</td>
      <td contenteditable="true" id = "add2">FILL IN</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">-5</td>

    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">0</td>
      <td contenteditable="true" id = "add3">FILL IN</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">130</td>
       <td contenteditable="false">-3</td>

    </tr>
    <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">0</td>
      <td contenteditable="true" id = "add4">FILL IN</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">-8</td>

    </tr>
  </tbody>
</table>
<div>
    <input id="submit" type="button" value="Submit" />
</div>
<div>
    <input id="clear" type="button" value="Clear" />
</div>
<div class = "congrats"></div>
')
  ))),
  div(
    fluidRow(column(12, h4('Observed Outcome Y', style = "text-align:center"),
                    hr(), 
                    HTML('
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
<link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>

<script type="text/javascript">
$(document).ready(function() {
  $("#submit3").click(function() {
    var t_vals = ["116", "130", "122", "131"];
    var q_tot = 4;
    var q = 0;
    for (i = 1; i <= q_tot; i++) {
      var r = $("#table3 #add" + i).text();
      r=parseInt(r);
      r = r.toString();
      r = r.replace(NaN, "FILL IN");
      if (r==t_vals[i-1]) {
        q = q + 1;
        $("#table3 #add" + i).text(r);
        $("#table3 #add" + i).append("    " + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\');
      }
      else {
        $("#table3 #add" + i).text(r);
        $("#table3 #add" + i).append("    " + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\');
      }
    };
    if (q == 4) {
      $(".congrats").append("Congratulations! ☺").css("color", "green");
    }
   return false;
  });
});
</script>

<script type="text/javascript">
$(document).ready(function() {
  $("#clear3").click(function() {
    var q_tot = 4;
    for (i = 1; i <= q_tot; i++) {
       $("#table3 #add" + i).text("FILL IN")
    };
    $(".congrats").text("");
   return false;
  });
});
</script>

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
<table class="table table-bordered table-responsive-md table-striped text-center" id="table3">
  <thead>
    <tr>
      <th class="text-center"></th>
      <th class="text-center">Treatment</th>
      <th class="text-center" colspan = "2">Potential Outcomes</th>
      <th class="text-center">Observed Outcomes</th>

    </tr>
  </thead>
  <tbody>
    <tr>
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">Y1</td>
      <td class="text-center">Y0</td>
      <td class="text-center">Y</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">1</td>
      <td contenteditable="false">116</td>
      <td contenteditable="false">121</td>
      <td contenteditable="true" id = "add1">FILL IN</td>
    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">1</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">137</td>
      <td contenteditable="true" id = "add2">FILL IN</td>

    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">0</td>
      <td contenteditable="false">118</td>
      <td contenteditable="false">122</td>
      <td contenteditable="true" id = "add3">FILL IN</td>
    </tr>
    <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">0</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">131</td>
      <td contenteditable="true" id = "add4">FILL IN</td>
    </tr>
  </tbody>
</table>
<div>
    <input id="submit3" type="button" value="Submit" />
</div>
<div>
    <input id="clear3" type="button" value="Clear" />
</div>
<div class = "congrats"></div>
')))),
  
  div(
    fluidRow(column(12, h4('Individual Treatment Effect ITE', style = "text-align:center"),
                    hr(), 
                    HTML('
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
<link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>

<script type="text/javascript">
$(document).ready(function() {
  $("#submit2").click(function() {
    var t_vals = ["-6", "-3", "-5", "-9"];
    var q_tot = 4;
    var q = 0;
    for (i = 1; i <= q_tot; i++) {
      var r = $("#table2 #add" + i).text();
      r=parseInt(r);
      r = r.toString();
      r = r.replace(NaN, "FILL IN");
      if (r==t_vals[i-1]) {
        q = q + 1;
        $("#table2 #add" + i).text(r);
        $("#table2 #add" + i).append("    " + \'<span class="glyphicon glyphicon-ok" style="color:green"></span>\');
      }
      else {
        $("#table2 #add" + i).text(r);
        $("#table2 #add" + i).append("    " + \'<span class="glyphicon glyphicon-remove" style="color:red"></span>\');
      }
    };
    if (q == 4) {
      $(".congrats").append("Congratulations! ☺").css("color", "green");
    }
   return false;
  });
});
</script>

<script type="text/javascript">
$(document).ready(function() {
  $("#clear2").click(function() {
    var q_tot = 4;
    for (i = 1; i <= q_tot; i++) {
       $("#table2 #add" + i).text("FILL IN")
    };
    $(".congrats").text("");
   return false;
  });
});
</script>

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
<table class="table table-bordered table-responsive-md table-striped text-center" id="table2">
  <thead>
    <tr>
      <th class="text-center"></th>
      <th class="text-center">Treatment</th>
      <th class="text-center" colspan = "2">Potential Outcomes</th>
      <th class="text-center">Observed Outcomes</th>
      <th class="text-center">Treatment Effect</th>

    </tr>
  </thead>
  <tbody>
    <tr>
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">Y1</td>
      <td class="text-center">Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">1</td>
      <td contenteditable="false">121</td>
      <td contenteditable="false">127</td>
      <td contenteditable="false">121</td>
      <td contenteditable="true" id = "add1">FILL IN</td>
    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">1</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">133</td>
      <td contenteditable="false">130</td>
      <td contenteditable="true" id = "add2">FILL IN</td>

    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">0</td>
      <td contenteditable="false">118</td>
      <td contenteditable="false">123</td>
      <td contenteditable="false">123</td>
      <td contenteditable="true" id = "add3">FILL IN</td>
    </tr>
    <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">0</td>
      <td contenteditable="false">121</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">130</td>
      <td contenteditable="true" id = "add4">FILL IN</td>
    </tr>
  </tbody>
</table>
<div>
    <input id="submit2" type="button" value="Submit" />
</div>
<div>
    <input id="clear2" type="button" value="Clear" />
</div>
<div class = "congrats"></div>
'))))
  
  )
  

server <- function(input, output) {
  
}

shinyApp(ui,server)


