library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)

ui <- fluidPage(
    tabsetPanel(
      tabPanel('Learn',
    fluidRow(column(1), column(10,
        h2('Potential Outcomes'), 
       
        h4('Choose an example: '),
        wellPanel(
        radioButtons(inputId = 'example', label = 'Options', 
                     choices = c('health', 'education', 'economics', 'exercise science'), 
                     selected = 'health')), 
        
        p(
          "Causal inference requires comparing a factual outcome (what did happen) to a counterfactual outcome (what would have happened if a treatment or intervention had not been received). 
          Defining causal effects as comparisons between factual and counterfactual outcomes is referred to as the potential outcomes framework of causal inference."
        ),
        
        # health case
        
        conditionalPanel("input.example === 'health'",
        
        p("As a concrete example, imagine that you are interested in the causal effect of omega-3 fish oil supplements on blood pressure. 
          To determine the causal effect, you set up a research study where participants either take a daily fish oil supplement for 6 months(treatment condition) or do not take a fish oil supplement for 6 months(control condition)."),
        
        
        p("Aki and Audrey are two participants in the study. Aki and Audrey can each receive one of the two conditions. 
          Treatment condition (received or didn't) is represented by the letter Z. 
          Z = 1 represents receiving the treatment (taking the fish oil) and Z = 0 represents not receiving the treatment (not taking the fish oil)."),
        

    plotOutput(outputId = 'dag.3'),
    
    
    p("Under the potential outcomes framework we consider what would have happened to both Aki and Audrey's blood pressure had they taken the fish oil supplements or had they not taken the fish oil supplements. 
      Y(1) represents what Aki or Audrey's blood pressure would have been after taking fish oil supplements (the treatment condition) and Y(0) represents what Aki or Audrey's blood pressure would have been had they not taken the fish oil supplements (the control condition)."),
    

    plotOutput(outputId = 'dag.4'),
    
    p("The individual treatment effect (ITE) is the causal effect of the fish oil supplements for an individual. For any given individual, their ITE is defined as the difference between their two potential outcomes. 
      If Aki had taken the fish oil supplements, Aki's blood pressure would have decreased by 5 points compared to what would have happend if he didn't take the supplements. 
      If Audrey had taken the Audrey's blood pressure would have decreased by 7 points."),
    
    plotOutput(outputId = 'dag.6')),
    
    # education case
    
    conditionalPanel("input.example === 'education'",
                     
                     p("
                     As a concrete example, imagine that you are interested in the causal effect of after-school math program on math test score for upper elementary students with math difficulties.
                     To determine the causal effect, you set up a research study where participants are randomly assigned to either receive a supplementing after-school math intervention for one school year (treatment condition) 
                     or not receive the after-school math intervention for one school year (control condition)."),
                     
                     
                     p("
                     Aki and Audrey are two participants in the study. Aki and Audrey can each receive one of the two conditions. Treatment condition (received or didn't) is represented by the letter Z. 
                     Z = 1 represents receiving the treatment (in the after-school math program) and Z = 0 represents not receiving the treatment (not in the after-school math program)."),
                     
                     
                     plotOutput(outputId = 'dag.3.edu'),
                     
                     
                     p("
                     Under the potential outcomes framework we consider what would have happened to both Aki and Audrey's math scores had they participated in the after-school math program or had they not participated in the after-school math program.
                     Y(1) represents what Aki or Audrey's math scores would have been after participating the after-school math program (the treatment condition) and 
                     Y(0) represents what Aki or Audrey's math score would have been had they not participated in the after-school math program (the control condition)."),
                     
                     
                     plotOutput(outputId = 'dag.4.edu'),
                     
                     p("
                     The individual treatment effect (ITE) is the causal effect of the after-school math program for an individual. 
                     For any given individual, their ITE is defined as the difference between their two potential outcomes. 
                     If Aki had participated in the after-school math program, Aki's math score would have incresased by 5 points compared to what would have happend if he didn't participat in the after-school math program. 
                     If Audrey had participated the Audrey's math score would have incresased by 7 points."),
                     
                     plotOutput(outputId = 'dag.6.edu')),
    
    # Economics case
    
    conditionalPanel("input.example === 'economics'",
                     
                     p("As a concrete example, imagine that you are interested in the causal effect of job training program on college student salary.
                     To determine the causal effect, you set up a research study where participants are randomly assigned to either receive a job training for one school year (treatment condition) 
        or not receive the job training for one school year (control condition)."),
                     
                     
                     p("Aki and Audrey are two participants in the study. Aki and Audrey can each receive one of the two conditions. Treatment condition (received or didn't) is represented by the letter Z. 
                     Z = 1 represents receiving the treatment (in the job training program) and Z = 0 represents not receiving the treatment (not in the job training program)."),
                     
                     
                     plotOutput(outputId = 'dag.3.econ'),
                     
                     
                     p("Under the potential outcomes framework we consider what would have happened to both Aki and Audrey's salary had they received the job training or had they not received the job training. 
                     Y(1) represents what Aki or Audrey's salary would have been after receiving the job training (the treatment condition) and Y(0) represents what Aki or Audrey's salary 
                       would have been had they not received the job training (the control condition)."),
                     
                     
                     plotOutput(outputId = 'dag.4.econ'),
                     
                     p("The individual treatment effect (ITE) is the causal effect of the job training program for an individual. 
                     For any given individual, their ITE is defined as the difference between their two potential outcomes. 
                     If Aki had received the job training, Aki's salary would have increased by 5000 dollars compared to what would have happend if he didn't receive the job training. 
                     If Audrey had received the Audrey's salary would have incresased by 7000 dollars."),
                     
                     plotOutput(outputId = 'dag.6.econ')),
    
    # exercise science case
    
    conditionalPanel("input.example === 'exercise science'",
                     
                     p("As a concrete example, imagine that you are interested in the causal effect of aerobic exercise program on VO2 max. 
                     To determine the causal effect, you set up a research study where participants are randomly assigned to either take part in an aerobic exercise program for 6 months (treatment condition) 
                       or not take part in the aerobic exercise program for 6 months (control condition)."),
                     
                     
                     p("Aki and Audrey are two participants in the study. Aki and Audrey can each receive one of the two conditions. Treatment condition (received or didn't) is represented by the letter Z. 
                     Z = 1 represents receiving the treatment (in the aerobic exercise program) and Z = 0 represents not receiving the treatment (not in the aerobic exercise program)."),
                     
                     
                     plotOutput(outputId = 'dag.3.exercise'),
                     
                     
                     p("Under the potential outcomes framework we consider what would have happened to both Aki and Audrey's VO2 max had they taken part in the aerobic exercise program or
      had they not taken part in the aerobic exercise program.
      Y(1) represents what Aki or Audrey's VO2 max would have been after taking part in the aerobic exercise program
      (the treatment condition) and Y(0) represents what Aki or Audrey's VO2 max would have been had they not taken part in the aerobic exercise program (the control condition)."),
                     
                     
                     plotOutput(outputId = 'dag.4.exercise'),
                     
                     p("The individual treatment effect (ITE) is the causal effect of the aerobic exercise program for an individual. 
                     For any given individual, their ITE is defined as the difference between their two potential outcomes. 
                     If Aki had taken part in the aerobic exercise program, Aki's VO2 max would have increased by 5 ml/kg/min compared to what would have happend if he didn't take part in the aerobic exercise program. 
                     If Audrey had taken part in the Audrey's VO2 max would have increased by 7 ml/kg/min."),
                     
                     plotOutput(outputId = 'dag.6.exercise')),
    
    p("The fundamental problem of causal inference is that both potential outcomes, Y(1) and Y(0), can never be observed at the same time. 
      Seeing both Y(1) and Y(0) at the same time would require an All Knowing view of the world that theoretically exists but is never available to the researcher. 
      In contrast, as a researcher, one can only observe one potential outcome for each participant (either Y(1) or Y(0)). 
      In future exercises we will distinguish between the 'All Knowing View' or 'Researcher View' of the world."),
    
    wellPanel(
    radioButtons(inputId = 'world.1', 
                 label = "View:", 
                 choices = c('All Knowing', 
                             'Researcher'), 
                 inline = T)),
    
    plotOutput(outputId = 'dag.5'),
    
    conditionalPanel("input.example === 'health'",
                     p("Comparing the All Knowing and Researcher views of the world reveals the importance of the potential outcomes framework. 
      We illustrate using hypothetical data from a small study. Access to the true All Knowing view reveals the true effects for the participants. 
      However when you are in researcher view you can only see one outcome per person highlighting the difficulty of discovering these effects. 
      We can use this to think about what we might do with these data and the disconnect between what we can estimate and the truth. 
      For instance consider the difference between the sample average treatment effect, SATE (the mean of all the ITEs in the sample) 
      and the difference in mean outcomes between those who receive the supplements and those who do not."),
                     
                     br(), 
                     
    p("To learn more about how Z, Y(1), Y(0), Y and ITEs are related you can use the buttons below flip between the All Knowing and Researcher view for the 10 participants in a study on the causal effect of fish oil supplements on blood pressure:")),
    
    conditionalPanel("input.example === 'education'",
                     p("Comparing the All Knowing and Researcher views of the world reveals the importance of the potential outcomes framework. 
      We illustrate using hypothetical data from a small study. Access to the true All Knowing view reveals the true effects for the participants. 
      However when you are in researcher view you can only see one outcome per person highlighting the difficulty of discovering these effects. 
      We can use this to think about what we might do with these data and the disconnect between what we can estimate and the truth. 
      For instance consider the difference between the sample average treatment effect, SATE (the mean of all the ITEs in the sample) 
      and the difference in mean outcomes between those who receive the after-school math intervention and those who do not."),
                     
                     br(), 
                     
                     p("To learn more about how Z, Y(1), Y(0), Y and ITEs are related you can use the buttons below flip between the All Knowing and Researcher view for the 10 participants in a study on the causal effect of after-school math program on math score:")),
    
    conditionalPanel("input.example === 'economics'",
                     p("Comparing the All Knowing and Researcher views of the world reveals the importance of the potential outcomes framework. 
      We illustrate using hypothetical data from a small study. Access to the true All Knowing view reveals the true effects for the participants. 
      However when you are in researcher view you can only see one outcome per person highlighting the difficulty of discovering these effects. 
      We can use this to think about what we might do with these data and the disconnect between what we can estimate and the truth. 
      For instance consider the difference between the sample average treatment effect, SATE (the mean of all the ITEs in the sample) 
      and the difference in mean outcomes between those who receive the job training and those who do not."),
                     
                     br(), 
                     
                     p("To learn more about how Z, Y(1), Y(0), Y and ITEs are related you can use the buttons below flip between the All Knowing and Researcher view for the 10 participants in a study on the causal effect of job tranining on salary:")),
    
    conditionalPanel("input.example === 'exercise science'",
                     
                     p("Comparing the All Knowing and Researcher views of the world reveals the importance of the potential outcomes framework. 
      We illustrate using hypothetical data from a small study. Access to the true All Knowing view reveals the true effects for the participants. 
      However when you are in researcher view you can only see one outcome per person highlighting the difficulty of discovering these effects. 
      We can use this to think about what we might do with these data and the disconnect between what we can estimate and the truth. 
      For instance consider the difference between the sample average treatment effect, SATE (the mean of all the ITEs in the sample) 
      and the difference in mean outcomes between those who take part in the aerobic exercise program and those who do not."),
                     
                     br(), 
                     
                     p("To learn more about how Z, Y(1), Y(0), Y and ITEs are related you can use the buttons below flip between the All Knowing and Researcher view for the 10 participants in a study on the causal effect of aerobic exercise program on VO2 max:")),
    
    wellPanel(
        radioButtons(inputId = 'world2', 
                     label = "View:", 
                     choices = c('All Knowing', 
                                 'Researcher'), 
                     selected = 'Researcher',
                     inline = T)))),
    
    
    
    conditionalPanel(condition = "input.example === 'health' && input.world2 == 'All Knowing'",
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
      <td contenteditable="false">Pengfei</td>
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
      <td contenteditable="false">Xinyu</td>
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
    
    
   conditionalPanel(condition = "input.example === 'health' && input.world2 == 'Researcher'",
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
      <td contenteditable="false">Pengfei</td>
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
      <td contenteditable="false">Xinyu</td>
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
   
   conditionalPanel(condition = "input.example === 'education' && input.world2 == 'All Knowing'",
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
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">5</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">92</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">92</td>
      <td contenteditable="false">7</td>

    </tr>
     <tr>
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">90</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">90</td>
      <td contenteditable="false">10</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">76</td>
      <td contenteditable="false">74</td>
      <td contenteditable="false">76</td>
      <td contenteditable="false">2</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">82</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">3</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">88</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">8</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">77</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">5</td>

    </tr>
    <tr>
      <td contenteditable="false">Xinyu</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">81</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">6</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">90</td>
      <td contenteditable="false">86</td>
      <td contenteditable="false">86</td>
      <td contenteditable="false">4</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">5</td>

    </tr>
  </tbody>
</table>'))
                    )),
   
   
   conditionalPanel(condition = "input.example === 'education' && input.world2 == 'Researcher'",
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
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">92</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">92</td>
      <td contenteditable="false">?</td>

    </tr>
     <tr>
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">90</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">90</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">76</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">76</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">85</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">80</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">72</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Xinyu</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">86</td>
      <td contenteditable="false">86</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">75</td>
      <td contenteditable="false">?</td>

    </tr>
  </tbody>
</table>'))
                    )), 
   
   conditionalPanel(condition = "input.example === 'economics' && input.world2 == 'All Knowing'",
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
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">5,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">92,000</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">92,000</td>
      <td contenteditable="false">7,000</td>

    </tr>
     <tr>
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">90,000</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">90,000</td>
      <td contenteditable="false">10,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">76,000</td>
      <td contenteditable="false">74,000</td>
      <td contenteditable="false">76,000</td>
      <td contenteditable="false">2,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">82,000</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">3,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">88,000</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">8,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">77,000</td>
      <td contenteditable="false">72,000</td>
      <td contenteditable="false">72,000</td>
      <td contenteditable="false">5,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Xinyu</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">81,000</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">6,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">90,000</td>
      <td contenteditable="false">86,000</td>
      <td contenteditable="false">86,000</td>
      <td contenteditable="false">4,000</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">5,000</td>

    </tr>
  </tbody>
</table>'))
                    )),
   
   
   conditionalPanel(condition = "input.example === 'economics' && input.world2 == 'Researcher'",
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
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">92,000</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">92,000</td>
      <td contenteditable="false">?</td>

    </tr>
     <tr>
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">90,000</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">90,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">76,000</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">76,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">85,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">80,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">72,000</td>
      <td contenteditable="false">72,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Xinyu</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">86,000</td>
      <td contenteditable="false">86,000</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">75,000</td>
      <td contenteditable="false">?</td>

    </tr>
  </tbody>
</table>'))
                    )), 
   
   conditionalPanel(condition = "input.example === 'exercise science' && input.world2 == 'All Knowing'",
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
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">5</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">42</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">42</td>
      <td contenteditable="false">7</td>

    </tr>
     <tr>
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">50</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">50</td>
      <td contenteditable="false">10</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">36</td>
      <td contenteditable="false">34</td>
      <td contenteditable="false">36</td>
      <td contenteditable="false">2</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">42</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">3</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">48</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">8</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">37</td>
      <td contenteditable="false">32</td>
      <td contenteditable="false">32</td>
      <td contenteditable="false">5</td>

    </tr>
    <tr>
      <td contenteditable="false">Xinyu</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">41</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">6</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">50</td>
      <td contenteditable="false">46</td>
      <td contenteditable="false">46</td>
      <td contenteditable="false">4</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">5</td>

    </tr>
  </tbody>
</table>'))
                    )),
   
   
   conditionalPanel(condition = "input.example === 'exercise science' && input.world2 == 'Researcher'",
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
      <td class="text-center">Student</td>
      <td class="text-center">Z</td>
      <td class="text-center">If Z =1, Y1</td>
      <td class="text-center">If Z = 0, Y0</td>
      <td class="text-center">Y</td>
      <td class="text-center">ITE</td>
    </tr>
    <tr>
      <td contenteditable="false">Blake</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Kennedy</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">42</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">42</td>
      <td contenteditable="false">?</td>

    </tr>
     <tr>
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">50</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">50</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Taylor</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">36</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">36</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Billie</td>
      <td contenteditable="false">Yes</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">45</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Charlie</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">40</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Casey</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">32</td>
      <td contenteditable="false">32</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Xinyu</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Parker</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">46</td>
      <td contenteditable="false">46</td>
      <td contenteditable="false">?</td>

    </tr>
    <tr>
      <td contenteditable="false">Andi</td>
      <td contenteditable="false">No</td>
      <td contenteditable="false">?</td>
      <td contenteditable="false">35</td>
      <td contenteditable="false">35</td>
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
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">0</td>
      <td contenteditable="true" id = "add3">FILL IN</td>
      <td contenteditable="false">130</td>
      <td contenteditable="false">130</td>
       <td contenteditable="false">-3</td>

    </tr>
    <tr>
      <td contenteditable="false">Pengfei</td>
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
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">0</td>
      <td contenteditable="false">118</td>
      <td contenteditable="false">122</td>
      <td contenteditable="true" id = "add3">FILL IN</td>
    </tr>
    <tr>
      <td contenteditable="false">Pengfei</td>
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
      <td contenteditable="false">Pengfei</td>
      <td contenteditable="false">0</td>
      <td contenteditable="false">118</td>
      <td contenteditable="false">123</td>
      <td contenteditable="false">123</td>
      <td contenteditable="true" id = "add3">FILL IN</td>
    </tr>
    <tr>
      <td contenteditable="false">Pengfei</td>
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






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$dag.3 <- renderPlot({
                dat <- tibble(
                    label = c('Aki', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                              'Audrey', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0'),
                    x = c(1,.5, 1.5, 3, 2.5, 3.5),
                    y = c(3.5,2.75, 2.75, 3.5,2.75, 2.75),
                    z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control'),
                    state = c('', 'Observed (factual)', 'Not Observed (counterfactual)',
                              '', 'Not Observed (counterfactual)', 'Observed (factual)')
                )
                p <- ggplot(dat, aes(x = x,y = y, fill = z)) +
                    geom_point(shape = 21, size = 25, alpha = .7)  +
                    geom_text(aes(label = label)) +
                    coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
                    scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
                    geom_segment(data = tibble(x = c(NA,.83,1.17, NA,2.83,3.17), xend = c(NA,.6,1.4, NA,2.6,3.4),
                                               y = c(NA,3.3, 3.3, NA,3.3, 3.3), yend = c(NA,3, 3,NA,3, 3),
                                               z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control')),
                                 aes(x = x, xend = xend, y = y, yend = yend),
                                 alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                                 size = 1.2,
                                 arrow = arrow(length = unit(0.02, "npc"))) +
                    guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',legend.title = element_blank())
                   return(p)
    })
    
    output$dag.3.edu <- renderPlot({
      dat <- tibble(
        label = c('Aki', 'Math \nProgram \nZ =1', 'No Math \nProgram \nZ=0',
                  'Audrey', 'Math \nProgram \nZ =1', 'No Math \nProgram \nZ=0'),
        x = c(1,.5, 1.5, 3, 2.5, 3.5),
        y = c(3.5,2.75, 2.75, 3.5,2.75, 2.75),
        z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control'),
        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)',
                  '', 'Not Observed (counterfactual)', 'Observed (factual)')
      )
      p <- ggplot(dat, aes(x = x,y = y, fill = z)) +
        geom_point(shape = 21, size = 25, alpha = .7)  +
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
        geom_segment(data = tibble(x = c(NA,.83,1.17, NA,2.83,3.17), xend = c(NA,.6,1.4, NA,2.6,3.4),
                                   y = c(NA,3.3, 3.3, NA,3.3, 3.3), yend = c(NA,3, 3,NA,3, 3),
                                   z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control')),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                     size = 1.2,
                     arrow = arrow(length = unit(0.02, "npc"))) +
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',legend.title = element_blank())
      return(p)
    })
    
    output$dag.3.econ <- renderPlot({
      dat <- tibble(
        label = c('Aki', 'Job \nTraining \nZ =1', 'No Job \nTraining \nZ=0',
                  'Audrey', 'Job \nTraining \nZ =1', 'No Job \nTraining \nZ=0'),
        x = c(1,.5, 1.5, 3, 2.5, 3.5),
        y = c(3.5,2.75, 2.75, 3.5,2.75, 2.75),
        z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control'),
        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)',
                  '', 'Not Observed (counterfactual)', 'Observed (factual)')
      )
      p <- ggplot(dat, aes(x = x,y = y, fill = z)) +
        geom_point(shape = 21, size = 25, alpha = .7)  +
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
        geom_segment(data = tibble(x = c(NA,.83,1.17, NA,2.83,3.17), xend = c(NA,.6,1.4, NA,2.6,3.4),
                                   y = c(NA,3.3, 3.3, NA,3.3, 3.3), yend = c(NA,3, 3,NA,3, 3),
                                   z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control')),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                     size = 1.2,
                     arrow = arrow(length = unit(0.02, "npc"))) +
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',legend.title = element_blank())
      return(p)
    })
      
    
    output$dag.3.exercise <- renderPlot({
      dat <- tibble(
        label = c('Aki', 'Aerobic \nExercise \nZ =1', 'No Aerobic \nExercise \nZ=0',
                  'Audrey', 'Aerobic \nExercise \nZ =1', 'No Aerobic \nExercise \nZ=0'),
        x = c(1,.5, 1.5, 3, 2.5, 3.5),
        y = c(3.5,2.75, 2.75, 3.5,2.75, 2.75),
        z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control'),
        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)',
                  '', 'Not Observed (counterfactual)', 'Observed (factual)')
      )
      p <- ggplot(dat, aes(x = x,y = y, fill = z)) +
        geom_point(shape = 21, size = 25, alpha = .7)  +
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
        geom_segment(data = tibble(x = c(NA,.83,1.17, NA,2.83,3.17), xend = c(NA,.6,1.4, NA,2.6,3.4),
                                   y = c(NA,3.3, 3.3, NA,3.3, 3.3), yend = c(NA,3, 3,NA,3, 3),
                                   z = c('', 'Treatment', 'Control', '', 'Treatment', 'Control')),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                     size = 1.2,
                     arrow = arrow(length = unit(0.02, "npc"))) +
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',legend.title = element_blank())
      return(p)
    })
    
    output$dag.4 <- renderPlot({
        dat <- tibble(
                        label = c('Aki', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                                  'Blood \nPresure \nY1 = 125', 'Blood \nPresure \nY0 = 130',
                                  'Audrey', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                                  'Blood \nPresure \nY1 = 118', 'Blood \nPresure \nY0 = 125'),
                        x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
                        y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
                        z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
                        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                                  '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
                    )
                    p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
                        geom_point(shape = 21, size = 25, alpha = .7)  +
                        geom_text(aes(label = label)) +
                        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
                        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
                        geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                                   y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                                   z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                                     aes(x = x, xend = xend, y = y, yend = yend),
                                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                                     size = 1.2,
                                     arrow = arrow(length = unit(0.02, "npc"))) +
                        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                                          legend.title = element_blank())
                    return(p)

    })
    
    output$dag.4.edu <- renderPlot({
      dat <- tibble(
        label = c('Aki', 'Math \nProgram \nZ =1', 'No Math \nProgram \nZ=0',
                  'Math \nScore \nY1 = 70', 'Math \nScore \nY0 = 65',
                  'Audrey', 'Math \nProgram \nZ =1', 'No Math \nProgram \nZ=0',
                  'Math \nScore \nY1 = 77', 'Math \nScore \nY0 = 70'),
        x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
        y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
        z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                  '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
      )
      p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
        geom_point(shape = 21, size = 25, alpha = .7)  +
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
        geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                   y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                   z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                     size = 1.2,
                     arrow = arrow(length = unit(0.02, "npc"))) +
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                          legend.title = element_blank())
      return(p)
      
    })
    
    
    output$dag.4.econ <- renderPlot({
      dat <- tibble(
        label = c('Aki', 'Job \nTraining \nZ =1', 'No Job \nTraining \nZ=0',
                  'Salary \nY1 = 90k', 'Salary \nY0 = 85k',
                  'Audrey', 'Job \nTraining \nZ =1', 'No Job \nTraining \nZ=0',
                  'Salary \nY1 = 97k', 'Salary \nY0 = 90k'),
        x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
        y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
        z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                  '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
      )
      p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
        geom_point(shape = 21, size = 25, alpha = .7)  +
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
        geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                   y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                   z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                     size = 1.2,
                     arrow = arrow(length = unit(0.02, "npc"))) +
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                          legend.title = element_blank())
      return(p)
      
    })
    
    output$dag.4.exercise <- renderPlot({
      dat <- tibble(
        label = c('Aki', 'Aerobic \nExercise \nZ =1', 'No Aerobic \nExercise \nZ=0',
                  'VO2 \nMax \nY1 = 40', 'VO2 \nMax \nY0 = 35',
                  'Audrey', 'Aerobic \nExercise \nZ =1', 'No Aerobic \nExercise \nZ=0',
                  'VO2 \nMax \nY1 = 47', 'VO2 \nMax \nY0 = 40'),
        x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
        y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
        z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
        state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                  '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
      )
      p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
        geom_point(shape = 21, size = 25, alpha = .7)  +
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
        scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue', 'coral3')) +
        geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                   y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                   z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                     aes(x = x, xend = xend, y = y, yend = yend),
                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                     size = 1.2,
                     arrow = arrow(length = unit(0.02, "npc"))) +
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                          legend.title = element_blank())
      return(p)
      
    })
    
    output$dag.6 <- renderPlot({
      dat <- tibble(x = c(1, 2, 3, 1, 2, 3),
                    y = c(1.5, 1.5, 1.5, 1, 1, 1), 
                    Z = c('Fish Oil Supplement (Z =1)','No Fish Oil Supplement (Z =0)','','Fish Oil Supplement (Z =1)','No Fish Oil Supplement (Z =0)',''), 
                    label = c(
                      'Aki \nY1 = 125',
                      'Aki \nY0 = 130',
                      'Aki \nITE = 5',
                      'Audrey \nY1 = 118',
                      'Audrey \nY0 = 125',
                      'Audrey \nITE = 7'
                    )) 
      ggplot(dat, aes(x,y, fill = Z)) +
        geom_point(size = 25, shape = 21, alpha = .7) + 
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(.5, 3.5), ylim = c(.5,2)) +
        scale_fill_manual(
          breaks = c('Fish Oil Supplement (Z =1)', 'No Fish Oil Supplement (Z =0)'),
          values = c(NA, 'coral3', 'steelblue')
        ) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1), size = 10) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1.5), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1.5), size = 10) + 
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
    })
    
    output$dag.6.edu <- renderPlot({
      dat <- tibble(x = c(1, 2, 3, 1, 2, 3),
                    y = c(1.5, 1.5, 1.5, 1, 1, 1), 
                    Z = c('After-school Math Program (Z =1)','No After-school Math Program (Z =0)','','After-school Math Program (Z =1)','No After-school Math Program (Z =0)',''), 
                    label = c(
                      'Aki \nY1 = 70',
                      'Aki \nY0 = 65',
                      'Aki \nITE = 5',
                      'Audrey \nY1 = 77',
                      'Audrey \nY0 = 70',
                      'Audrey \nITE = 7'
                    )) 
      ggplot(dat, aes(x,y, fill = Z)) +
        geom_point(size = 25, shape = 21, alpha = .7) + 
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(.5, 3.5), ylim = c(.5,2)) +
        scale_fill_manual(
          breaks = c('After-school Math Program (Z =1)', 'No After-school Math Program (Z =0)'),
          values = c(NA, 'coral3', 'steelblue')
        ) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1), size = 10) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1.5), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1.5), size = 10) + 
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
    })
    
    
    output$dag.6.econ <- renderPlot({
      dat <- tibble(x = c(1, 2, 3, 1, 2, 3),
                    y = c(1.5, 1.5, 1.5, 1, 1, 1), 
                    Z = c('Job Training (Z =1)','No Job Training (Z =0)','','Job Training (Z =1)','No Job Training (Z =0)',''), 
                    label = c(
                      'Aki \nY1 = 90k',
                      'Aki \nY0 = 85k',
                      'Aki \nITE = 5k',
                      'Audrey \nY1 = 97k',
                      'Audrey \nY0 = 90k',
                      'Audrey \nITE = 7k'
                    )) 
      ggplot(dat, aes(x,y, fill = Z)) +
        geom_point(size = 25, shape = 21, alpha = .7) + 
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(.5, 3.5), ylim = c(.5,2)) +
        scale_fill_manual(
          breaks = c('Job Training (Z =1)', 'No Job Training (Z =0)'),
          values = c(NA, 'coral3', 'steelblue')
        ) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1), size = 10) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1.5), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1.5), size = 10) + 
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
    })
    
    output$dag.6.exercise <- renderPlot({
      dat <- tibble(x = c(1, 2, 3, 1, 2, 3),
                    y = c(1.5, 1.5, 1.5, 1, 1, 1), 
                    Z = c('Aerobic Exercise Program (Z =1)','No Aerobic Exercise Program (Z =0)','','Aerobic Exercise Program (Z =1)','No Aerobic Exercise Program (Z =0)',''), 
                    label = c(
                      'Aki \nY1 = 40',
                      'Aki \nY0 = 35',
                      'Aki \nITE = 5',
                      'Audrey \nY1 = 47',
                      'Audrey \nY0 = 40',
                      'Audrey \nITE = 7'
                    )) 
      ggplot(dat, aes(x,y, fill = Z)) +
        geom_point(size = 25, shape = 21, alpha = .7) + 
        geom_text(aes(label = label)) +
        coord_cartesian(xlim = c(.5, 3.5), ylim = c(.5,2)) +
        scale_fill_manual(
          breaks = c('Aerobic Exercise Program (Z =1)', 'No Aerobic Exercise Program (Z =0)'),
          values = c(NA, 'coral3', 'steelblue')
        ) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1), size = 10) + 
        annotate('text', label = c('-'), x = c(1.5), y = c(1.5), size = 10) + 
        annotate('text', label = c('='), x = c(2.5), y = c(1.5), size = 10) + 
        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
    })
    
    output$dag.5 <- renderPlot({
        
        if(input$example == 'health' & input$world.1 == 'All Knowing'){
            
            dat <- tibble(
                            label = c('Aki', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                                      'Blood \nPresure \nY1 = 125', 'Blood \nPresure \nY0 = 130',
                                      'Audrey', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                                      'Blood \nPresure \nY1 = 118', 'Blood \nPresure \nY0 = 125'),
                            x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
                            y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
                            z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
                            state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                                      '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
                        )
                        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
                            geom_point(shape = 21, size = 25, alpha = .7)  +
                            geom_text(aes(label = label)) +
                            coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
                            scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue','coral3')) +
                            geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                                       y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                                       z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                                         aes(x = x, xend = xend, y = y, yend = yend),
                                         alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                                         size = 1.2,
                                         arrow = arrow(length = unit(0.02, "npc"))) +
                            guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                                              legend.title = element_blank())
                        
        }
        
        if(input$example == 'health' & input$world.1 == 'Researcher'){
                    dat <- tibble(
                        label = c('Aki', 'Fish Oil \nZ =1',
                                  'Blood \nPresure \nY1 = 125',
                                  'Audrey', 'No \nFish Oil \nZ=0',
                                  'Blood \nPresure \nY0 = 125'),
                        x = c(1,.5, .5, 3, 3.5, 3.5),
                        y = c(3.5, 2.75, 1.8, 3.5, 2.75, 1.8),
                        state = c('', 'Observed (factual)', 'Observed (factual)',
                                  '','Observed (factual)', 'Observed (factual)'),
                        z =  c('Treatment','Treatment', 'Treatment', 'Control', 'Control', 'Control')
                    )
                    p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
                        geom_point(shape = 21, size = 25, alpha = .7)  +
                        geom_text(aes(label = label)) +
                        coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
                        scale_fill_manual(breaks = c('Treatment', 'Control'),values = c('coral3', 'steelblue')) +
                        geom_segment(data = tibble(x = c(NA,.83,.5, NA,3.17, 3.5), xend = c(NA,.6,.5, NA,3.4, 3.5),
                                                   y = c(NA,3.3, 2.5, NA,3.3,2.5), yend = c(NA,3, 2.08,NA,3, 2.08),
                                                   z = c('Treatment', 'Treatment', 'Treatment',
                                                         'Control','Control', 'Control')),
                                     aes(x = x, xend = xend, y = y, yend = yend),
                                     alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                                     size = 1.2,
                                     arrow = arrow(length = unit(0.02, "npc"))) +
                        guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
        }
      
      
      if(input$example == 'education' & input$world.1 == 'All Knowing'){
        
        dat <- tibble(
          label = c('Aki', 'Math \nProgram \nZ =1', 'No Math \nProgram \nZ=0',
                    'Math \nScore \nY1 = 70', 'Math \nScore \nY0 = 65',
                    'Audrey', 'Math \nProgram \nZ =1', 'No Math \nProgram \nZ=0',
                    'Math \nScore \nY1 = 77', 'Math \nScore \nY0 = 70'),
          x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
          y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
          z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
          state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                    '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
        )
        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue','coral3')) +
          geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                     y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                     z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                            legend.title = element_blank())
        
      }
      
      if(input$example == 'education' & input$world.1 == 'Researcher'){
        dat <- tibble(
          label = c('Aki', 'Math \nProgram \nZ =1',
                    'Math \nScore \nY1 = 70',
                    'Audrey', 'No Math \nProgram \nZ=0',
                    'Math \nScore \nY0 = 70'),
          x = c(1,.5, .5, 3, 3.5, 3.5),
          y = c(3.5, 2.75, 1.8, 3.5, 2.75, 1.8),
          state = c('', 'Observed (factual)', 'Observed (factual)',
                    '','Observed (factual)', 'Observed (factual)'),
          z =  c('Treatment','Treatment', 'Treatment', 'Control', 'Control', 'Control')
        )
        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          scale_fill_manual(breaks = c('Treatment', 'Control'),values = c('coral3', 'steelblue')) +
          geom_segment(data = tibble(x = c(NA,.83,.5, NA,3.17, 3.5), xend = c(NA,.6,.5, NA,3.4, 3.5),
                                     y = c(NA,3.3, 2.5, NA,3.3,2.5), yend = c(NA,3, 2.08,NA,3, 2.08),
                                     z = c('Treatment', 'Treatment', 'Treatment',
                                           'Control','Control', 'Control')),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
      }
      
      
      
      if(input$example == 'economics' & input$world.1 == 'All Knowing'){
        
        dat <- tibble(
          label = c('Aki', 'Job \nTraning \nZ =1', 'No Job \nTraning \nZ=0',
                    'Salary \nY1 = 90k', 'Salary \nY0 = 85k',
                    'Audrey', 'Job \nTraning \nZ =1', 'No Job \nTraning \nZ=0',
                    'Salary \nY1 = 97k', 'Salary \nY0 = 90k'),
          x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
          y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
          z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
          state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                    '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
        )
        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue','coral3')) +
          geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                     y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                     z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                            legend.title = element_blank())
        
      }
      
      if(input$example == 'economics' & input$world.1 == 'Researcher'){
        dat <- tibble(
          label = c('Aki', 'Job \nTraning \nZ =1',
                    'Salary \nY1 = 90k',
                    'Audrey', 'No Job \nTraning \nZ=0',
                    'Salary \nY0 = 90k'),
          x = c(1,.5, .5, 3, 3.5, 3.5),
          y = c(3.5, 2.75, 1.8, 3.5, 2.75, 1.8),
          state = c('', 'Observed (factual)', 'Observed (factual)',
                    '','Observed (factual)', 'Observed (factual)'),
          z =  c('Treatment','Treatment', 'Treatment', 'Control', 'Control', 'Control')
        )
        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          scale_fill_manual(breaks = c('Treatment', 'Control'),values = c('coral3', 'steelblue')) +
          geom_segment(data = tibble(x = c(NA,.83,.5, NA,3.17, 3.5), xend = c(NA,.6,.5, NA,3.4, 3.5),
                                     y = c(NA,3.3, 2.5, NA,3.3,2.5), yend = c(NA,3, 2.08,NA,3, 2.08),
                                     z = c('Treatment', 'Treatment', 'Treatment',
                                           'Control','Control', 'Control')),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
      }
      
      if(input$example == 'exercise science' & input$world.1 == 'All Knowing'){
        
        dat <- tibble(
          label = c('Aki', 'Aerobic \nExercise \nZ =1', 'No Aerobic \nExercise \nZ=0',
                    'VO2 \nMax \nY1 = 40', 'VO2 \nMax \nY0 = 35',
                    'Audrey', 'Aerobic \nExercise \nZ =1', 'No Aerobic \nExercise \nZ=0',
                    'VO2 \nMax \nY1 = 47', 'VO2 \nMax \nY0 = 40'),
          x = c(1,.5, 1.5, .5, 1.5, 3, 2.5, 3.5,  2.5, 3.5),
          y = c(3.5,2.75, 2.75, 1.8, 1.8, 3.5,2.75, 2.75, 1.8, 1.8),
          z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control'),
          state = c('', 'Observed (factual)', 'Not Observed (counterfactual)', 'Observed (factual)', 'Not Observed (counterfactual)',
                    '', 'Not Observed (counterfactual)', 'Observed (factual)',  'Not Observed (counterfactual)', 'Observed (factual)')
        )
        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          scale_fill_manual(breaks = c('Treatment','Control'),values = c(NA, 'steelblue','coral3')) +
          geom_segment(data = tibble(x = c(NA,.83,1.17,.5, 1.5, NA,2.83,3.17, 2.5, 3.5), xend = c(NA,.6,1.4,.5, 1.5, NA,2.6,3.4,2.5, 3.5),
                                     y = c(NA,3.3, 3.3, 2.5,2.5, NA,3.3, 3.3,2.5,2.5), yend = c(NA,3, 3, 2.08, 2.08,NA,3, 3,2.08, 2.08),
                                     z = c('', 'Treatment', 'Control','Treatment', 'Control', '', 'Treatment', 'Control', 'Treatment', 'Control')),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top',
                                                                                            legend.title = element_blank())
        
      }
      
      if(input$example == 'exercise science' & input$world.1 == 'Researcher'){
        dat <- tibble(
          label = c('Aki', 'Aerobic \nExercise \nZ =1',
                    'VO2 \nMax \nY1 = 40',
                    'Audrey', 'No Aerobic \nExercise \nZ=0',
                    'VO2 \nMax \nY0 = 40'),
          x = c(1,.5, .5, 3, 3.5, 3.5),
          y = c(3.5, 2.75, 1.8, 3.5, 2.75, 1.8),
          state = c('', 'Observed (factual)', 'Observed (factual)',
                    '','Observed (factual)', 'Observed (factual)'),
          z =  c('Treatment','Treatment', 'Treatment', 'Control', 'Control', 'Control')
        )
        p <- ggplot(dat,aes(x = x,y = y, fill = z)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          scale_fill_manual(breaks = c('Treatment', 'Control'),values = c('coral3', 'steelblue')) +
          geom_segment(data = tibble(x = c(NA,.83,.5, NA,3.17, 3.5), xend = c(NA,.6,.5, NA,3.4, 3.5),
                                     y = c(NA,3.3, 2.5, NA,3.3,2.5), yend = c(NA,3, 2.08,NA,3, 2.08),
                                     z = c('Treatment', 'Treatment', 'Treatment',
                                           'Control','Control', 'Control')),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void() + theme(legend.position = 'top', legend.title = element_blank())
      }
      
        
        return(p)
    })
    
    
    output$practice.1 <- renderTable({
      dat <- tibble(z = c(1,1,1,0,0,0), y1 = c(5,4,3,5,1,2))
      tab.1 <- data.table(dat)
      return(tab.1)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
