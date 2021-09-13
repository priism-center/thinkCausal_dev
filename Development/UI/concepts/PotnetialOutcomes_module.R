
ns <- NS("concepts_potentialoutcomes")

PotentialOutcomesUI <- function(id){ 
  
  fluidPage(  
    tags$head(
    tags$style(
      "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
    )
  ),
  includeMarkdown("UI/markdowns/PotentialOutcomes_scenario1.md"),
  
  fluidRow(column(10, align="center", 
                  h2('Table 1: all-knowing perspective'),
                  hr(), 
                  HTML(PotentialOutcomes_table1))),
  fluidRow(column(10, align="center", 
                  h2('Table 2: reality perspective'),
                  hr(), 
                  HTML(PotentialOutcomes_table2))),
  fluidRow(column(10, 
                  h2('Table 3: testing', style = "text-align:center"),
                  hr(), 
                  HTML(PotentialOutcomes_table3))),
  
  fluidRow(column(12, 
                  h2('Table 3: testing', style = "text-align:center"),
                  hr(), 
                  HTML(PotentialOutcomes_table6))),
  
  includeMarkdown("UI/markdowns/PotentialOutcomes_scenario3.md"),
  fluidRow(column(12, 
                  h2('Table 3: testing', style = "text-align:center"),
                  hr(), 
                  HTML(PotentialOutcomes_table9)))
  
  )}

PotentialOutcomesServer <- function(id) {
  
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
        if(input$correct_number2 == 4){
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
        if(input$correct_number3 == 4){
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



