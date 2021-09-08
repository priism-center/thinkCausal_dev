
#ns <- NS("potential_outcomes_test")

poUI <- function(id){ 
  ns <- NS(id)
  fluidPage(
    tabsetPanel(
      tabPanel('Learn',
               fluidRow(column(1), column(10,
                                          h2('Potential Outcomes'), 
                                          
                                          h4('Choose an example: '),
                                          wellPanel(
                                            awesomeRadio(inputId = 'example', label = 'Options', 
                                                         choices = c('health', 'education', 'economics', 'exercise science'), 
                                                         selected = 'health')), 
                                          
                                          p(
                                            "Causal inference is about comparing a factual outcome 
          (something that did happen) to a counterfactual outcome 
          (what would have happened) if a treatment or intervention had been 
          different. Comparing factual and counter-factual 
          outcomes is referred to as the potential outcomes framework of causal inference."
                                          ),
                                          #plotOutput(outputId = 'dag.1'), 
                                          
                                          p("As a concrete example, imagine that you are interested in the causal 
        effet of omega-3 fish oil supplements on blood pressure. To determine the causal effect, 
        you set up a research study where participants either take a daily fish oil supplement 
        for 6 months(treatment condition) or do not take a fish oil supplement for 6 monts(control condition)."),
                                          
                                          
                                          p("Aki and Audrey are two participants in the study. 
        Under the potential outcomes framework, Aki and Audrey can recive one of the 
        two conditions. Different possible conditions are represented by the letter Z. 
        Z = 1 represents reciving the treatment condition (taking the fish oil) 
        and Z = 0 represents reciving the control condition (not taking the fish oil)."),
                                          
                                          
                                          #plotOutput(outputId = 'dag.2'),
                                          
                                          plotOutput(outputId = 'dag.3'),
                                          
                                          
                                          p("Under the potential outcomes framework we concider what would have happened to both 
      Aki and Auderys blood pressure had they taken the fish oil supplements or
      had they not taken the fish oil supplements. Y1 is used to represent what Aki 
      and Audreys blood pressure would have been after taking fish oil supplements
      (the treatment condition) and Y0 is used to represent what Aki and Audreys blood
      pressure would have been had they not taken the fish oil suplements (the control condition)."),
                                          
                                          
                                          
                                          plotOutput(outputId = 'dag.4'),
                                          
                                          p("The individual treatment effect(ITE) is the causal effect of the fish oil supplements for each individaul.
    Aki and Audreys' individual treatment effects can be determined by taking 
      the difference between their two potential outcomes. If Aki had taken the fish oil supplements, Aki's 
      blood pressure would have decreased by 5 points and if Audrey had taken the 
      Audrey's blood pressure would have decreased by 7 points."),
                                          
                                          plotOutput(outputId = 'dag.6'),
                                          
                                          p("The fundemental problem of causal inference is that both potential outcomes Y1 and Y0
      can never be observed at the same time. Seeing both Y1 and Y0 at the same time would requior an
      'All Knowing' view that theoretically exists but is never avalible to the researcher. 
      Under the researcher view, each participant has a single value of Z and Y(either Y1 or Y0). 
      
      In practice, participants 
      only recive the treatment or control conditions and a single value of Z and 
      only one value of Y (either Y1 or Y0). 
      The 'All Knowing' view where both potential outcomes are observed is theoretical 
      and differs from the view researchers have in practice."),
                                          
                                          wellPanel(
                                            awesomeRadio(inputId = 'world.1', 
                                                         label = "View:", 
                                                         choices = c('All Knowing', 
                                                                     'Researcher'), 
                                                         inline = T)),
                                          
                                          plotOutput(outputId = 'dag.5'),
                                          
                                          
                                          p("Looking between the All Knowing and Researcher views 
      reveals the importance of the potential outcomes franework. 
      Access to the true All Knowing view shows us that the fish oil supplemts 
      cause a 5 point drop in blood pressure. If you limited causal 
      inference to comparing the differences between Aki and Audrey's observed 
      blood pressure (the Researcher view), you would reach the incorrect 
      conclussion that the fish oil supplements have no causal effect on blood pressure.
      When you have access to the All Knowing view of both potential outcomes, 
      you can see that the fish oil supplements would have reduced both Aki and Audreys' blood pressure by 5 points."),
                                          
                                          br(), 
                                          
                                          p("While the All Knowing view is never avalible to reseachers in pracitice, 
      researchers use experemental designs and modeling methods to predict 
      counterfactuals and estimate the differences in potential outcomes"),
                                          
                                          p("To learn more about how Z, Y1, Y0, Y and ITE are related you can use the buttons below flip between 
      the All Knowing and Researcher view for 10 other participants in the the study on 
      the causal effect of fish oil supplements on blood pressure:"),
                                          
                                          wellPanel(
                                            awesomeRadio(inputId = 'world2', 
                                                         label = "View:", 
                                                         choices = c('All Knowing', 
                                                                     'Researcher'), 
                                                         selected = 'Researcher',
                                                         inline = T)))),
               
               
               
               conditionalPanel(condition = "input.world2 == 'All Knowing'",
                                tags$head(
                                  tags$style(
                                    "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
                                  )
                                ),
                                fluidRow(column(1),column(10, align="center",
                                                          h4('All Knowing View'),
                                                          hr(), HTML('
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
      <td class="text-center">Patient</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">120</td>
      <td contenteditable="false">125</td>
      <td contenteditable="false">120</td>
      <td contenteditable="false">-5</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">125</td>
      <td contenteditable="false">132</td>
      <td contenteditable="false">125</td>
      <td contenteditable="false">-7</td>

    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">140</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">-10</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">124</td>
      <td contenteditable="false">126</td>
      <td contenteditable="false">124</td>
      <td contenteditable="false">-2</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">132</td>
      <td contenteditable="false">135</td>
      <td contenteditable="false">132</td>
      <td contenteditable="false">-3</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">120</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">-8</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">132</td>
      <td contenteditable="false">137</td>
      <td contenteditable="false">137</td>
      <td contenteditable="false">-5</td>

    </tr>
    <tr>
      <td contenteditable="false">Alex</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">115</td>
      <td contenteditable="false">121</td>
      <td contenteditable="false">121</td>
      <td contenteditable="false">-6</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">126</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">-4</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">135</td>
      <td contenteditable="false">140</td>
      <td contenteditable="false">140</td>
      <td contenteditable="false">-5</td>

    </tr>
  </tbody>
</table>'))
                                )),
               
               
               conditionalPanel(condition = "input.world2 == 'Researcher'",
                                tags$head(
                                  tags$style(
                                    "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
                                  )
                                ),
                                
                                fluidRow(column(1),column(10, align="center",
                                                          h4('Researcher View'),
                                                          hr(), HTML('
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
      <td class="text-center">Patient</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">120</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">120</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">125</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">125</td>
      <td contenteditable="false">?</td>

    </tr>
     <tr>
      <td contenteditable="false">Jordan</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">124</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">124</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">132</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">132</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">128</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">137</td>
      <td contenteditable="false">137</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Alex</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">121</td>
      <td contenteditable="false">121</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">140</td>
      <td contenteditable="false">140</td>
      <td contenteditable="false">?</td>

    </tr>
  </tbody>
</table>'))
                                )), 
               
               tags$head(
                 tags$style(
                   "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
                 )
               )),
      tabPanel('Practice',
               div(
                 fluidRow(column(12, h3('Practice'),
                                 p("Double click on missing cells and fill in the tables below to check your understanding of potential outcomes and the relationships between Y, Y1, Y0, Z and ITE.
                       After you've entered your answers click submit to check your work."),
                                 
                                 h4('Counterfactual Outcome Y1 and Y0', style = "text-align:center"),
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
    Shiny.onInputChange("', ns('correct_number'), '", q);
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
      )))
  
  }

poServer <- function(id) {
  ns <- NS(id)
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$correct_number, {
        if(input$correct_number == 4){
          show_alert(
            title = "All Correct!!",
            text = "You can move on to next part!",
            type = "success"
          )
        }else{
          show_alert(
            title = "Wrong",
            text = "Please check which one(s) you answered incorrect and try again!",
            type = "error"
          )
        }
        
      })
      
      observeEvent(input$correct_number2, {
        if(input$correct_number2 == 10){
          show_alert(
            title = "All Correct!!",
            text = "You can move on to next part!",
            type = "success"
          )
        }else{
          show_alert(
            title = "Wrong",
            text = "Please check which one(s) you answered incorrect and try again!",
            type = "error"
          )
        }
        
      })
      
      observeEvent(input$correct_number3, {
        if(input$correct_number3 == 10){
          show_alert(
            title = "All Correct!!",
            text = "You can move on to learning next concept!",
            type = "success"
          )
        }else{
          show_alert(
            title = "Wrong",
            text = "Please check which one(s) you answered incorrect and try again!",
            type = "error"
          )
        }
        
      })
      
    }
  )
}