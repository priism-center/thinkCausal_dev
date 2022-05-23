
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
        
        # save input to store
        store$analysis_design <- input$analysis_design
        store$analysis$design$treatment_name <- input$treatment_name
        store$analysis$design$treatment_units <- input$treatment_units
        store$analysis$design$treatment_participants <- input$treatment_participants
        
        # remove saved dataframes if they exist
        store <- remove_downstream_data(store, page = 'design')
      })
      
      observeEvent(input$analysis_design_button_next, {
        updateNavbarPage(global_session, inputId = "nav", selected = store$module_ids$analysis$data)
      })

      return(store)
    }
  )
}
