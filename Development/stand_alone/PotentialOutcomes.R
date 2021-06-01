
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
  title = 'Potential Outcomes',
  p("Descriptive Example of treatment"),
  p("ITE = Y1i - Y0i"),
  p("In this example Tau is 5"),
  div(id = "fixedtable",
      fluidRow(column(10, align="center", 
                      h2('Table 1: omniscient scenario'),
                      hr(), 
                      HTML('  
<table class="table table-bordered table-responsive-md table-striped text-center" id="table2">
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
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">95</td>
      <td contenteditable="false">90</td>
      <td contenteditable="false">95</td>
    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">80</td>
    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">70</td>
      <td contenteditable="false">75</td>
    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">77</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">77</td>
    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">87</td>
      <td contenteditable="false">82</td>
      <td contenteditable="false">87</td>
    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">76</td>
      <td contenteditable="false">71</td>
      <td contenteditable="false">71</td>
    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">84</td>
      <td contenteditable="false">79</td>
      <td contenteditable="false">79</td>
    </tr>
    <tr>
      <td contenteditable="false">Alex</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">96</td>
      <td contenteditable="false">91</td>
      <td contenteditable="false">91</td>
    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">77</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">72</td>
    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">79</td>
      <td contenteditable="false">74</td>
      <td contenteditable="false">74</td>
    </tr>
  </tbody>
</table>'))
      ),
    fluidRow(column(10, align="center", 
                    h2('Table 2: reality scenario'),
                    hr(), HTML('  
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
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">95</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">95</td>
    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">80</td>
    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">75</td>
    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">77</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">77</td>
    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">87</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">87</td>
    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">71</td>
      <td contenteditable="false">71</td>
    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">79</td>
      <td contenteditable="false">79</td>
    </tr>
    <tr>
      <td contenteditable="false">Alex</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">91</td>
      <td contenteditable="false">91</td>
    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">72</td>
    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">74</td>
      <td contenteditable="false">74</td>
    </tr>
  </tbody>
</table>'))
    )),
    fluidRow(column(10, h2('Table 3: testing', style = "text-align:center"),
                    hr(), 
                    HTML('
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"></script>
<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
<link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css" rel="stylesheet"/>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js"></script>

<script type="text/javascript">
$(document).ready(function() {
  $("#submit").click(function() {
    var t_vals = ["90", "75", "70", "72", "82", "76", "84", "96", "77", "79"];
    var q_tot = 10;
    var q = 0;
    for (i = 1; i <= q_tot; i++) {
      var r = $("#table1 #add" + i).text();
      r=parseInt(r);
      r = r.toString();
      r = r.replace(NaN, "?");
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
    if (q == 10) {
      $(".congrats").append("Congratulations! ☺").css("color", "green");;
    }
   return false;
  });
});
</script>

<script type="text/javascript">
$(document).ready(function() {
  $("#clear").click(function() {
    var q_tot = 10;
    for (i = 1; i <= q_tot; i++) {
       $("#table1 #add" + i).text("?")
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
    </tr>
  </thead>
  <tbody>
    <tr>
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">95</td>
      <td contenteditable="true" id = "add1">?</td>
      <td contenteditable="false">95</td>
    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">80</td>
      <td contenteditable="true" id = "add2">?</td>
      <td contenteditable="false">80</td>
    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">75</td>
      <td contenteditable="true" id = "add3">?</td>
      <td contenteditable="false">75</td>
    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">77</td>
      <td contenteditable="true" id = "add4">?</td>
      <td contenteditable="false">77</td>
    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">87</td>
      <td contenteditable="true" id = "add5">?</td>
      <td contenteditable="false">87</td>
    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="true" id = "add6">?</td>
      <td contenteditable="false">71</td>
      <td contenteditable="false">71</td>
    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="true" id = "add7">?</td>
      <td contenteditable="false">79</td>
      <td contenteditable="false">79</td>
    </tr>
    <tr>
      <td contenteditable="false">Alex</td>
      <td contenteditable="false">No</td>
      <td contenteditable="true" id = "add8">?</td>
      <td contenteditable="false">91</td>
      <td contenteditable="false">91</td>
    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="true" id = "add9">?</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">72</td>
    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="true" id = "add10">?</td>
      <td contenteditable="false">74</td>
      <td contenteditable="false">74</td>
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
'),
  br(), br())
))
  
  server = function(input, output) {

  }

shinyApp(ui,server)


