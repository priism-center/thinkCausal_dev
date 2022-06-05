
server_design <- function(store, id, global_session){
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # render example language
      output$analysis_design_text <- renderText({
        
        # TODO: somehow clean these inputs
        name <- input$treatment_name
        units <- input$treatment_units
        participants <- input$treatment_participants
        
        # set defaults
        if (name == '') name <- 'treatment condition'
        if (units == '') units <- 'units'
        if (participants == '') participants <- 'participants'
        
        # create the text
        text_out <- paste0(
          'The <b>',
          name,
          '</b> led to an ',
          '<i>increase/decrease</i>',
          ' of X <b>',
          units,
          '</b> for <b>',
          participants,
          '</b> in this study'
        )
        text_out <- HTML(text_out)
        
        return(text_out)
      })
      
      # save input and remove downstream dataframes if study design changes
      observeEvent(input$analysis_design_button_next, {
        
        # browser()
        
        # make sure required inputs have values 
        local({
          req_inputs <- c(
            'analysis_design_estimand',
            'analysis_design',
            'analysis_weights',
            'analysis_random_intercept'
          )
          req_values <- reactiveValuesToList(input)[req_inputs]
          
          # trigger animation if any inputs is unsure or blank
          inputs_to_animate <- req_inputs[which(req_values == 'Unsure' | req_values == '')]
          inputs_to_animate_selectors <- paste0("#", ns(inputs_to_animate), "+ div", collapse = ', ')
          runjs(glue::glue('$("<<inputs_to_animate_selectors>>").effect("shake", {times: 4, distance: 3})',
                           .open = "<<", .close = ">>"))
          
          # stop here if any unsures or blank inputs
          req(!isTRUE(length(inputs_to_animate) > 0))
        })

        # save input to store
        store$analysis$design$design <- input$analysis_design
        store$analysis$design$weights <- input$analysis_weights
        store$analysis$design$random_effects <- input$analysis_random_intercept
        store$analysis$design$treatment_name <- input$treatment_name
        store$analysis$design$treatment_units <- input$treatment_units
        store$analysis$design$treatment_participants <- input$treatment_participants
        store$analysis$design$estimand <- input$analysis_design_estimand
        
        # remove saved dataframes if they exist
        # TODO: error here if user goes back and changes the estimand then saves the design
        store <- remove_downstream_data(store, page = 'design')
        
        # update page
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$data)
      })
      
      # open slide over if answer is unsure
      dropdown_inputs <- c("analysis_design_estimand", "analysis_design", "analysis_weights", "analysis_random_intercept")
      purrr::map(dropdown_inputs, function(input_id){
        observeEvent(input[[input_id]], {
          if (input[[input_id]] == "Unsure") shinyjs::runjs('openHelpSection("help-studydesign")')
        })
      })


      return(store)
    }
  )
}
