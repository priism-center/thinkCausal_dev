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
    tabsetPanel(id = ns('tabs'),
      tabPanel('Learn', 
                fluidRow(
                  column(1), 
                  column(10,
                         h2('Potential Outcomes'), 
                         
                         h4('Choose an example: '),
                         wellPanel(
                           awesomeRadio(inputId = 'example', label = 'Options', 
                                        choices = c('health', 'education', 'economics', 'exercise science'), 
                                        selected = 'health')), 
                         
                         p(
                           "Causal inference requires comparing a factual outcome 
                                          (what did happen) to a counterfactual outcome 
                                          (what would have happened if a treatment or intervention 
                                          had not been received). Defining causal effects as comparisons 
                                          between factual and counterfactual outcomes is referred to as 
                                          the potential outcomes framework of causal inference."
                         ),
                         #plotOutput(outputId = 'dag.1'), 
                         
                         p("As a concrete example, imagine that you are interested in the causal 
        effect of omega-3 fish oil supplements on blood pressure. To determine the causal effect, 
        you set up a research study where participants either take a daily fish oil supplement 
        for 6 months(treatment condition) or do not take a fish oil supplement for 6 months(control condition)."),
                         
                         
                         p("Aki and Audrey are two participants in the study. 
        Aki and Audrey can each receive one of the two conditions. Treatment condition (received or didn't) is represented by the letter Z. 
        Z = 1 represents receiving the treatment (taking the fish oil) 
        and Z = 0 represents not receiving the treatment (not taking the fish oil)."),
                         
                         
                         #plotOutput(outputId = 'dag.2'),
                         
                         plotOutput(ns('dag.3')),
                         
                         
                         p("Under the potential outcomes framework we consider what would have happened to both 
      Aki and Audrey's blood pressure had they taken the fish oil supplements or
      had they not taken the fish oil supplements. Y(1) represents what Aki 
      or Audrey's blood pressure would have been after taking fish oil supplements
      (the treatment condition) and Y(0) represents what Aki or Audrey's blood
      pressure would have been had they not taken the fish oil supplements (the control condition)."),
                         
                         
                         plotOutput(ns('dag.4')),
                         
                         p("The individual treatment effect (ITE) is the causal effect of the fish oil supplements for an individual.
    For any given individual, their ITE is defined as 
      the difference between their two potential outcomes. If Aki had taken the fish oil supplements, Aki's 
      blood pressure would have decreased by 5 points compared to what would have happend if he didn't take the supplements. If Audrey had taken the 
      Audrey's blood pressure would have decreased by 7 points."),
                         
                         plotOutput(ns('dag.6')),
                         
                         p("The fundamental problem of causal inference is that 
                                          both potential outcomes, Y(1) and Y(0), can never be 
                                          observed at the same time. Seeing both Y(1) and Y(0) at 
                                          the same time would require an All Knowing view of the 
                                          world that theoretically exists but is never available 
                                          to the researcher. In contrast, as a researcher, one can
                                          only observe one potential outcome for each participant 
                                          (either Y(1) or Y(0)). In future exercises we will 
                                          distinguish between the 'All Knowing View' or 
                                          'Researcher View' of the world."),
                         tagList(
                           wellPanel(
                             awesomeRadio(inputId = NS(id, 'world1'), 
                                          label = "View:", 
                                          choices = c('All Knowing', 
                                                      'Researcher'), 
                                          inline = T))),
                         
                         # plotOutput('dag.5'),
                         
                         conditionalPanel(condition = "input.world1 == 'All Knowing'",ns = ns,
                                          plotOutput(ns('dag.5'))),
                         
                         conditionalPanel(condition = "input.world1 == 'Researcher'",ns = ns, 
                                          plotOutput(ns('dag.7'))),
                         
                         
                         p("Comparing the All Knowing and Researcher views of the world
      reveals the importance of the potential outcomes framework. We illustrate using 
                                          hypothetical data from a small study. 
                                          Access to the true All Knowing view reveals
                                          the true effects for the participants. 
                                          However when you are in researcher view you 
                                          can only see one outcome per person highlighting 
                                          the difficulty of discovering these effects. 
                                          We can use this to think about what we might 
                                          o with these data and the disconnect between 
                                          what we can estimate and the truth. For 
                                          instance consider the difference between 
                                          the sample average treatment effect,
                                          SATE (the mean of all the ITEs in the sample) 
                                          and the difference in mean outcomes between 
                                          those who receive the supplements and those who do not."),
                         
                         br(), 
                         
                         p("To learn more about how Z, Y(1), Y(0), Y and ITEs are related you can use the buttons below flip between 
      the All Knowing and Researcher view for the 10 participants in a study on
      the causal effect of fish oil supplements on blood pressure:"),
                         
                         wellPanel(
                           awesomeRadio(inputId = NS(id, 'world2'), 
                                        label = "View:", 
                                        choices = c('All Knowing', 
                                                    'Researcher'), 
                                        selected = 'Researcher',
                                        inline = T))
                         )
                ), # end of fluid row
                
                conditionalPanel(condition = 'input.world2 == "All Knowing"',ns = ns,
                                 tags$head(
                                   tags$style(
                                     "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
                                   )),
                                 fluidRow(column(1), column(10, 
                                                            align = 'center', 
                                                            h4('All Knowing View'),
                                                            hr(),
                                                            HTML(PotentialOutcomes_table1)
                                                            ))
                              
                                 ),
                
                conditionalPanel(condition = 'input.world2 == "Researcher"', ns = ns,
                                 tags$head(
                                   tags$style(
                                     "tr:nth-child(1) {font-weight: bold;}
         tr:nth-child(even) {background-color: #f2f2f2;}
         tr:nth-child(odd) {background: #fafafa;}
         th {border:1px solid #e0e0e0; cursor:pointer;}
         td {border:1px solid #e0e0e0;height: 35px;}"
                                   )),
                                 fluidRow(column(1), column(10,
                                                            align="center",
                                                            h4('Researcher View'),
                                                            hr(), 
                                                            HTML(PotentialOutcomes_table2)
                                                            ))
                                 
                                 ),
                
                fluidRow(column(1), column(4,actionButton(ns('to_practice'), 'Continue to Practice Section')))
    ), # end of learning tab 
    tabPanel('Practice',
            
               fluidRow(column(12, h3('Practice'),
                               p("Double click on missing cells and fill in the tables below to check your understanding of potential outcomes and the relationships between Y, Y1, Y0, Z and ITE.
                       After you've entered your answers click submit to check your work."))),
                               
             tabsetPanel(id = ns('Practice_Problems'), type = 'hidden',
                         #tabPanel('Blank'), 
                         tabPanel('Problem1', 
                                  fluidRow(column(12, h4('Observed Outcome Y', style = "text-align:center"),
                                                  hr(), 
                                                  HTML(PotentialOutcomes_table6)))), 
                         tabPanel('Problem2', 
                                  fluidRow(column(12,  h4('Individual Treatment Effect ITE', style = "text-align:center"), 
                                                  hr(), 
                                                  HTML(PotentialOutcomes_table9)))), 
                         tabPanel('Problem3', 
                                  fluidRow(column(12, h4('Counterfactual Outcome Y1 and Y0', style = "text-align:center"), 
                                                  hr(), 
                                                  HTML(PotentialOutcomes_table3)))), 
                         
                         tabPanel('All', 
                                  fluidRow(column(12, h4('Observed Outcome Y', style = "text-align:center"),
                                                  hr(), 
                                                  HTML(PotentialOutcomes_table6))), 
                                  fluidRow(column(12,  h4('Individual Treatment Effect ITE', style = "text-align:center"), 
                                                  hr(), 
                                                  HTML(PotentialOutcomes_table9))), 
                                  fluidRow(column(12, h4('Counterfactual Outcome Y1 and Y0', style = "text-align:center"), 
                                                  hr(), 
                                                  HTML(PotentialOutcomes_table3)))
                                  )
                         
             ),
        
             fluidRow(column(4, actionButton(ns('to_learning'), 'Back to Learning Section')))
             

             ) # end of practice tab 
    ) # end of tabset 

  )# end of page 
}



PotentialOutcomesServer <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      
      observeEvent(input$to_practice,{
        updateTabsetPanel(session = session, inputId = 'tabs', selected = 'Practice')
      })
      
      observeEvent(input$to_learning,{
        updateTabsetPanel(session = session, inputId = 'tabs', selected = 'Learn')
      })
      
      # code to manage practice progression
      practice_page <- reactiveVal(value = 0)
      
      practice_next <- reactive({
        practice_page(practice_page() + 1)
      })
      
      
      output$dag.1 <- renderPlot({
        
        dat <- tibble(
          label = c('Aki', 'Audrey'),
          x = c(1, 3),
          y = c(3.5, 3.5)
        )
        p <- ggplot(dat,aes(x = x,y = y)) +
          geom_point(shape = 21, size = 25)  +
          geom_text(aes(label = label)) + 
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          theme_void()
        
        return(p)
      })
      
      
      output$dag.2 <- renderPlot({
        dat <- tibble(
          label = c('Aki', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                    'Audrey', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0'),
          x = c(1,.5, 1.5, 3, 2.5, 3.5),
          y = c(3.5,2.75, 2.75, 3.5,2.75, 2.75)
        )
        p <- ggplot(dat,aes(x = x,y = y)) +
          geom_point(shape = 21, size = 25, alpha = .7)  +
          geom_text(aes(label = label)) +
          coord_cartesian(xlim = c(-0.10, 4), ylim = c(-.1, 4)) +
          geom_segment(data = tibble(x = c(NA,.83,1.17, NA,2.83,3.17), xend = c(NA,.6,1.4, NA,2.6,3.4),
                                     y = c(NA,3.3, 3.3, NA,3.3, 3.3), yend = c(NA,3, 3,NA,3, 3)),
                       aes(x = x, xend = xend, y = y, yend = yend),
                       alpha = 0.5, lineend = 'round', linejoin = 'mitre',
                       size = 1.2,
                       arrow = arrow(length = unit(0.02, "npc"))) +
          guides(fill = guide_legend(override.aes = list(size = 3))) + theme_void()
        return(p)
      })
      
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
      
      
      output$dag.4 <- renderPlot({
        dat <- tibble(
          label = c('Aki', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                    'Blood \nPresure \nY1 = 125', 'Blood \nPresure \nY0 = 130',
                    'Audrey', 'Fish Oil \nZ =1', 'No \nFish Oil \nZ=0',
                    'Blood \nPresure \nY1 = 115', 'Blood \nPresure \nY0 = 120'),
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
      
      
      output$dag.5 <- renderPlot({
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
      
      
      
      
      output$dag.7 <- renderPlot({
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
        
        return(p)
      })
      
      
      observeEvent(input$correct_number2, {
        if(input$correct_number2 == 4){
          sendSweetAlert(session,
                         title = "All Correct!!", 
                         type = 'success', 
                         showCloseButton = FALSE, 
                         btn_labels = NA,
                         closeOnClickOutside = TRUE,
                         text = tags$div(div(class = 'backNextContainer', 
                                             style= "width:60%;display:inline-block;horizontal-align:center;",
                                             actionButton(inputId = ns('to_practice2'), 
                                                          label = 'Continue to Next Question'))))
        }else{
          show_alert(
            title = "Wrong",
            text = "Please check which one(s) you answered incorrect and try again!",
            type = "error"
          )
        }
        
      })
      
      
      observeEvent(input$to_practice2, {
        updateTabsetPanel(session = session,inputId = 'Practice_Problems', selected = 'Problem2')
        shinyWidgets::closeSweetAlert()
      })
      
      
      
      
      
      observeEvent(input$correct_number3, {
        if(input$correct_number3 == 4){
          sendSweetAlert(session,
                         title = "All Correct!!", 
                         type = 'success', 
                         showCloseButton = FALSE, 
                         btn_labels = NA,
                         closeOnClickOutside = TRUE,
                         text = tags$div(div(class = 'backNextContainer', 
                                             style= "width:60%;display:inline-block;horizontal-align:center;",
                                             actionButton(inputId = ns('to_practice3'), 
                                                          label = 'Continue to Next Question'))))
        }else{
          show_alert(
            title = "Wrong",
            text = "Please check which one(s) you answered incorrect and try again!",
            type = "error"
          )
        }
        
      })
      
      
      observeEvent(input$to_practice3, {
        updateTabsetPanel(session = session,inputId = 'Practice_Problems', selected = 'Problem3')
        shinyWidgets::closeSweetAlert()
      })
      
      
      
      
      observeEvent(input$correct_number, {
        if(input$correct_number == 4){
          sendSweetAlert(session,
                         title = "All Correct!!", 
                         type = 'success', 
                         showCloseButton = FALSE, 
                         btn_labels = NA,
                         closeOnClickOutside = TRUE,
                         text = tags$div(div(class = 'backNextContainer', 
                                             style= "width:60%;display:inline-block;horizontal-align:center;",
                                             actionButton(inputId = ns('review'), 
                                                          label = 'Review Practice Questions'), 
                                             br(), 
                                             br(),
                                             actionButton(inputId = ns('to_learning'), 
                                                          label = 'Back to Learning Section'), 
                                             br(), 
                                             br(),
                                             actionButton(inputId = 'new_topic', 
                                                          label = 'Move on to Next Topic'))))
        }else{
          show_alert(
            title = "Wrong",
            text = "Please check which one(s) you answered incorrect and try again!",
            type = "error"
          )
        }
        
      })
      
      
      observeEvent(input$review, {
        updateTabsetPanel(session = session,inputId = 'Practice_Problems', selected = 'All')
        shinyWidgets::closeSweetAlert()
      })
      
      # 
      # observeEvent(input$correct_number3, {
      #   if(input$correct_number3 == 4){
      #     show_alert(
      #       title = "All Correct!!",
      #       text = "You can move on to learning next concept!",
      #       type = "success"
      #     )
      #   }else{
      #     show_alert(
      #       title = "Wrong",
      #       text = "Please check which one(s) you answered incorrect and try again!",
      #       type = "error"
      #     )
      #   }
      #   
      # })
      
      
      
    }
  )
}



