# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # initialize list to store variables  
  store <- reactiveValues(uploaded_df = data.frame())

  # back next buttons -------------------------------------------------------

  # data page
  observeEvent(input$analysis_data_load_button_next, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected ="Select Data")
  })
  observeEvent(input$analysis_data_select_button_back, {
    updateTabsetPanel(session, inputId ="analysis_data_tabs", selected = "Load")
  })
  observeEvent(input$analysis_data_select_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  
  # plotting page
  observeEvent(input[[NS('analysis_plots_descriptive')('analysis_plots_descriptive_button_back')]], {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Select Data")
  })
  observeEvent(input[[NS('analysis_plots_descriptive')('analysis_plots_descriptive_button_next')]], {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
  })
  observeEvent(input$analysis_plots_support_button_back, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Descriptive Plots")
  })
  observeEvent(input$analysis_plots_support_button_next, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
  })
  observeEvent(input$analysis_plots_balance_button_back, {
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Common Support Plots")
  })
  observeEvent(input$analysis_plots_balance_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Model")
  })
  
  # model page
  observeEvent(input$analysis_model_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Exploratory Plots")
    updateTabsetPanel(session, inputId = "analysis_plot_tabs", selected = "Balance Plots")
  })
  observeEvent(input$analysis_model_button_popup, {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load")
    shinyWidgets::closeSweetAlert()
  })
  
  # diagnostics page
  observeEvent(input$analysis_diagnostics_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Model")
  })
  observeEvent(input$analysis_diagnostics_button_next, {
    updateNavbarPage(session, inputId = "nav", selected = "Results")
  })
  
  # results page
  observeEvent(input$analysis_results_button_back, {
    updateNavbarPage(session, inputId = "nav", selected = "Model diagnostics")
  })
  
  
  # upload data -------------------------------------------------------------
  
  # read in the uploaded file
  uploaded_df <- reactive({
    # TODO: test all the file types (e.g. stata is only stata 15 or older)
    # TODO: this behavior is really unintuitive; need to rethink it
    # TODO: add parsing failures to log
    
    #req(input$analysis_data_upload)
    
    # extract the filepath and the filetype
    filepath <- input$analysis_data_upload$datapath
    filetype <- tools::file_ext(filepath)
    practice <- input$create_practice
    
    if(length(filepath != 0) & practice%%2 == 0){
        tryCatch({
          
          # if it's a txt file then ask the user what the delimiter is  
          if (filetype == 'txt'){
            output$analysis_data_delim <- renderUI({ 
              textInput(inputId = 'analysis_data_delim_value',
                        label = "Column delimiter",
                        # value = ',',
                        placeholder = ", | - :")
            })
            req(input$analysis_data_delim_value)
          }
          
          # upload the file based on its filetype
          if (filetype == "csv"){
            uploaded_file <- readr::read_csv(
              file = filepath,
              col_names = input$analysis_data_header) 
          } else if (filetype == 'dta'){
            uploaded_file <- readstata13::read.dta13(file = filepath)
          } else if (filetype == 'xlsx'){
            uploaded_file <- xlsx::read.xlsx(file = filepath)
          } else if (filetype == 'txt'){
            delim <- input$analysis_data_delim_value
            if (delim == "") delim <- ","
            uploaded_file <- readr::read_delim(
              file = filepath,
              delim = delim,
              col_names = input$analysis_data_header
            )
          } else if (filetype == 'spss'){
            uploaded_file <- Hmisc::spss.get(file = filepath)
          } else stop("File type is invalid")
        },
        error = function(e) {
          # return a safeError if a parsing error occurs or if dataset isn't yet uploaded
          stop(safeError(e))
        })
    }
    
    else if(practice%%2 != 0){
      uploaded_file <- make_dat_demo()
    }
    
    else{
      uploaded_file <- ''
    }
    
    return(uploaded_file)
  })
  

  # # Practice df
  # uploaded_df <-  reactive({
  #   req(input$create_practice)
  #   sim <- make_dat_demo()
  #   return(sim)
  # })

  
  #TODO: need to clean column names during upload; bad csvs will crash the server
  
  # add dataframe to store object
  observeEvent(uploaded_df(), {
    store$uploaded_df <- uploaded_df()
  })
  
  # render UI for renaming the columns
  output$analysis_data_rename <- renderUI({
    tagList(
      lapply(X = seq_along(colnames(store$uploaded_df)), FUN = function(i) {
        textInput(
          inputId = paste0("analysis_data_rename_", i),
          label = colnames(store$uploaded_df)[i],
          value = colnames(store$uploaded_df)[i]
        )
      }
      ))
  })
  
  # overwrite column names when user saves new names
  observeEvent(input$analysis_data_rename_save, {
    validate(need(length(colnames(store$uploaded_df)) > 0, "No dataframe uploaded"))
    input_ids <- paste0("analysis_data_rename_", seq_along(colnames(store$uploaded_df)))
    inputted_name_values <- reactiveValuesToList(input)[input_ids]
    colnames(store$uploaded_df) <- inputted_name_values
  })
  
  # table of selected dataset
  output$analysis_data_table <- DT::renderDataTable({
    pretty_datatable(
      store$uploaded_df,
      selection = "none"
    )
  })
  

  # select data -------------------------------------------------------------

  # vector of selector ids
  analysis_data_select_selector_ids <-
    c(
      "analysis_data_select_select_zcol",
      "analysis_data_select_select_ycol",
      "analysis_data_select_select_xcol"
    )
  
  # update select inputs when the input data changes
  observeEvent(store$uploaded_df, {

    # update the first three dropdown options with the column names from the uploaded dataset
    lapply(analysis_data_select_selector_ids[1:2], function(id){
      # other_select_values <- reactiveValuesToList(input)[setdiff(selector_ids, id)]
      updateSelectInput(session = session, 
                        inputId = id,
                        choices = colnames(store$uploaded_df),
                        selected = colnames(store$uploaded_df)[which(analysis_data_select_selector_ids == id)]
                        )
    })
    
    # update X drop down with the remaining columns
    updateSelectInput(session = session, 
                      inputId = analysis_data_select_selector_ids[3],
                      choices = colnames(store$uploaded_df),
                      selected = setdiff(colnames(store$uploaded_df), colnames(store$uploaded_df)[1:2])
    )
  })

  # when user hits 'save column assignments', create a new dataframe from store$uploaded_df
  # with the new columns
  observeEvent(input$analysis_data_select_column_save, {
    
    # get the current values of the select inputs
    all_selected_vars <- reactiveValuesToList(input)[analysis_data_select_selector_ids]
    has_values <- all(!sapply(all_selected_vars, is.null))
    
    # stop here if all dropdowns are somehow not selected
    validate(need(has_values, "Need all dropdowns to be selected"))
    
    # if all the values have been selected then update the new dataframe
    Z <- all_selected_vars[1]
    Y <- all_selected_vars[2]
    X <- unlist(all_selected_vars[3])
    
    # stop here if there are overlapping column assignments
    inputs_are_all_unique <- length(unique(unlist(all_selected_vars))) == length(unlist(all_selected_vars))
    if (isFALSE(inputs_are_all_unique)){
      shinyWidgets::show_alert(
        title = 'Duplicative column assignment',
        text = "At least one column has been selected for two different assignments. Please correct before saving.",
        type = 'error'
      )
    }
    validate(need(inputs_are_all_unique, "There are duplicative input columns"))
    
    # new column names
    # TODO: update these with better labels
    new_col_names <- paste0(c('Z', 'Y', paste0('X', 1:length(X))),
                            "_",
                            unlist(all_selected_vars))
      
    # create new dataframe of just the selected vars and rename them
    store$selected_df <- store$uploaded_df[, unlist(all_selected_vars)]
    colnames(store$selected_df) <- new_col_names
      
    # save original column names
    store$selected_df_original_names <- all_selected_vars
    
    # save the column names by their respective class
    # TODO: UNIT TEST THIS!!!
    classes_categorical <- c('logical', 'character', 'factor')
    classes_continuous <- c('numeric', 'double', 'integer')
    cols_by_class <- split(names(store$selected_df), sapply(store$selected_df, function(x) paste(class(x), collapse = " ")))
    store$selected_df_categorical_vars <- as.vector(unlist(cols_by_class[classes_categorical]))
    store$selected_df_numeric_vars <- as.vector(unlist(cols_by_class[classes_continuous]))
    
    # render the UI eda with this data
    output$analysis_plots_descriptive_eda_module <- renderUI({
      edaUI(id = "analysis_plots_descriptive", 
            col_names = colnames(store$selected_df), 
            categorical_names = store$selected_df_categorical_vars)
    })
    
    # run the eda module server
    # TODO: remove or integrate random sampling of data; including for now for dev speed
    edaServer(id = 'analysis_plots_descriptive', input_data = slice_sample(store$selected_df, n = 100))
    
    # update selects on balance plots
    # TODO: exclude categorical vars here???
    cols <- store$selected_df_numeric_vars
    X_cols <- cols[stringr::str_starts(cols, "X")]
    updateSelectInput(session = session,
                      inputId = 'analysis_plot_balance_select_var',
                      choices = X_cols,
                      selected = X_cols
    )
    updateSelectInput(session = session,
                      inputId = 'analysis_plot_overlap_select_var',
                      choices = X_cols,
                      selected = X_cols
    )
  })
  
  # table of selected dataset
  output$analysis_data_select_table <- DT::renderDataTable({
    
    # render the table
    pretty_datatable(
      store$selected_df,
      selection = "none"
    )
  })
  

  # EDA ---------------------------------------------------------------------
  
  # create the overlap plot
  output$analysis_plot_overlap_plot <- renderPlot({
    
    # stop here if data hasn't been uploaded and selected
    validate(need(is.data.frame(store$selected_df), 
                  "Data must be first uploaded and selected. Please see 'Data' tab."))
    
    # columns to plot
    selected_cols <- input$analysis_plot_overlap_select_var
    
    # pivot the data
    dat_pivoted <- store$selected_df %>% 
      rename(Z = starts_with("Z")) %>% 
      dplyr::select(all_of(c(selected_cols, "Z"))) %>% 
      pivot_longer(cols = -Z) %>% 
      mutate(Z = as.logical(Z))
    
    # histograms showing overlaps
    ggplot() + 
      geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_histogram(data = dat_pivoted %>% filter(Z == 1), 
                     aes(x = value, y = ..density.., fill = Z), 
                     alpha = 0.8)+ 
      geom_histogram(data = dat_pivoted %>% filter(Z == 0), 
                     aes(x = value, y = -..density.., fill = Z), 
                     alpha = 0.8) +
      scale_y_continuous(labels = function(brk) abs(brk)) +
      scale_fill_manual(values = c('#bd332a', '#262991')) +
      facet_wrap(~name, scales = 'free', ncol = 3) +
      labs(title = "Overlap by treatment status",
           subtitle = 'Informative subtitle to go here',
           x = NULL,
           y = 'Density',
           fill = "Treatment")
  })
  
  # create the balance plot
  output$analysis_plot_balance_plot <- renderPlot({
    
    # stop here if data hasn't been uploaded and selected
    validate(need(is.data.frame(store$selected_df), 
                  "Data must be first uploaded and selected. Please see 'Data' tab."))
    
    # stop here if there are no numeric columns selected
    selected_cols <- input$analysis_plot_balance_select_var
    validate(need(length(selected_cols) > 0,
                  "No numeric columns selected"))
    
    # plot it
    store$selected_df %>% 
      rename(Z = starts_with("Z")) %>% 
      dplyr::select(all_of(c(selected_cols, "Z"))) %>% 
      pivot_longer(cols = -Z) %>% 
      group_by(name) %>% 
      mutate(value = scale(value)[,1]) %>%
      group_by(name, Z) %>% 
      summarize(mean = mean(value),
                .groups = 'drop') %>% 
      group_by(name) %>% 
      summarize(diff = mean - lag(mean),
                .groups = 'drop') %>% 
      na.omit() %>% 
      ggplot(aes(x=diff, y=name, color=abs(diff))) +
      geom_vline(xintercept = 0, linetype = 'dashed', color = 'gray60') +
      geom_point(size=4) +
      scale_colour_gradient(low = 'gray30', high = 'red3') + #or should color be scaled to finite values?
      labs(title = 'Treatment and control balance',
           subtitle = 'Informative subtitle to go here',
           x = 'Scaled mean difference',
           y = NULL) +
      theme(legend.position = 'none')
  })
  
  # run the eda module server. the UI is rendered server side within an observeEvent function
  # edaServer(id = 'analysis_plots_descriptive', input_data = store$selected_df) #user_data) #
  

  # diagnostics -------------------------------------------------------------
  
  # render either both the back and next buttons or just the back if its a bad
    # model fit
  output$analysis_diagnosis_buttons_ui <- renderUI({
    if (isTRUE(store$good_model_fit)){
      tagList(
        div(
          class = 'backNextContainer',
          actionButton(inputId = "analysis_diagnostics_button_back",
                       label = "Back to specify model"),
          actionButton(inputId = "analysis_diagnostics_button_next",
                       label = "Model results")
        )
      )
    } else {
      actionButton(inputId = "analysis_diagnostics_button_back",
                   label = "Back to specify model")
    }
  })
  
  # trace plot
  output$analysis_diagnostics_plot_trace <- renderPlot({
    
    # stop here if model is not run yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fit on 'Model' tab"))
    
    # extract model from store
    mod <- store$model_results
    
    # plot it
    mod %>% 
      bartCause::extract() %>% 
      as_tibble() %>% 
      mutate(index = row_number()) %>% 
      ggplot(aes(x = index, y = value)) + 
      geom_line() + 
      labs(title = 'Diagnostics: Trace Plot', 
           subtitle = 'Informative subtitle to go here',
           x = 'Iteration', 
           y = base::toupper(mod$estimand))
  })
  
  # common support plot
  output$analysis_diagnostics_plot_support_sd <- renderPlot({
    
    # stop here if model is not run yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be run on 'Model' tab"))
    
    # retrieve model from the store
    BART_model <- store$model_results
    
    # calculate summary stats
    total <- sum(BART_model$sd.cf > max(BART_model$sd.obs) + sd(BART_model$sd.obs))
    prop <- total / length(BART_model$sd.cf)
    sd_test <- paste0(prop, "% of cases would have been removed by the standard deviation common support check")
    
    # plot it
    BART_model$sd.cf %>% 
      as_tibble() %>% 
      mutate(rownumber = row_number()) %>% 
      ggplot(aes(rownumber, value)) + 
      geom_point(alpha = 0.8)+
      geom_hline(aes(yintercept = max(BART_model$sd.obs) + sd(BART_model$sd.obs)),
                 color = 'coral3', linetype = 'dashed') + 
      labs(title ="Diagnostics: Common Support Checks", 
           subtitle = paste0("Standard Deviation method: ", sd_test),
           x = NULL, #"Row index",
           y = 'Counterfactual Uncertanty') + 
      theme(legend.title = element_blank(), 
            legend.position = 'bottom')
  })
  
  # common support plot
  output$analysis_diagnostics_plot_support_chi <- renderPlot({
    
    # stop here if model is not run yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be run on 'Model' tab"))
    
    # retrieve model from the store
    BART_model <- store$model_results
    
    # calculate summary stats
    total <- sum((BART_model$sd.cf / BART_model$sd.obs) ** 2 > 3.841)
    prop <- total / length(BART_model$sd.cf)
    chi_test <- paste0(prop, "% of cases would have been removed by the chi squred common support check")
    
    # plot it
    (BART_model$sd.cf / BART_model$sd.obs)**2  %>% 
      as_tibble() %>% 
      mutate(rownumber = row_number()) %>% 
      ggplot(aes(rownumber, value)) + 
      geom_point(alpha = 0.8) + 
      geom_hline(aes(color = 'Removal threshold', yintercept = 3.841), linetype = 'dashed') + 
      scale_color_manual(values = 'coral3') +
      labs(title = NULL, 
           subtitle = paste0("Chi Squared method: ", chi_test),
           x = "Row index",
           y = 'Counterfactual Uncertanty') + 
      theme(legend.title = element_blank(), 
            legend.position = 'bottom')
  })
  

  # specify model -----------------------------------------------------------

  # render text output to summarize the users inputs
  output$analysis_model_summary <- renderText({
    
    # extract inputs
    design <- input$analysis_model_radio_design
    estimand <- input$analysis_model_radio_estimand
    support <- input$analysis_model_radio_support
    if (is.null(design)) design <- "None"
    if (is.null(estimand)) estimand <- "None"
    if (is.null(support)) support <- "None"
    
    # grab text from object
    design_text <- analysis_model_text$design[[design]]
    estimand_text <- analysis_model_text$estimand[[estimand]]
    support_text <- analysis_model_text$support[[support]]

    # paste together all the text
    custom_text <- paste0(
      "<h3>Design</h3>",
      design_text,
      "<br><br>",
      "<h3>Estimand</h3>",
      estimand_text,
      "<br><br>",
      "<h3>Common Support</h3>",
      support_text
    )
    
    return(custom_text)
  })
  
  # render text below the radio buttons
  output$analysis_model_text_design <- renderUI({

    if (isTRUE(input$analysis_model_radio_design == 'quasi')){
      html_out <- tags$span(
        style = 'color: red;', 
        "Natural experiment design is not currently supported",
        br(), br()
      )
      return(html_out)
    }
    
    html_out <- ''
    return(html_out)
  })
  
  # remove no text UI spawns if user makes a selection
  observeEvent(input$analysis_model_radio_design, {
    removeUI('#analysis_model_text_design_noinput')
  })
  observeEvent(input$analysis_model_radio_estimand, {
    removeUI('#analysis_model_text_estimand_noinput')
  })
  observeEvent(input$analysis_model_radio_support, {
    removeUI('#analysis_model_text_support_noinput')
  })
  
  # when user runs the model, take a number of actions
  observeEvent(input$analysis_model_button_next, {
    
    # launch popup if data is not yet selected
    if (!is.data.frame(store$selected_df)) {
      shinyWidgets::show_alert(
        title = 'Data must be first uploaded and columns selected',
        text = tags$div(
          actionButton(
            inputId = 'analysis_model_button_popup',
            label = 'Take me to the Data tab')
        ),
        type = 'error',
        btn_labels = NA
      ) 
    }
    
    # spawn red text if selection isn't made
    if (isTRUE(is.null(input$analysis_model_radio_design))) {
      output$analysis_model_text_design_noinput <- renderUI({
        html_out <- tags$span(style = 'color: red;',
                              "Please make a selection",
                              br(), br())
        return(html_out)
      })
    }
    if (isTRUE(is.null(input$analysis_model_radio_estimand))) {
      output$analysis_model_text_estimand_noinput <- renderUI({
        html_out <- tags$span(style = 'color: red;',
                              "Please make a selection",
                              br(), br())
        return(html_out)
      })
    }
    if (isTRUE(is.null(input$analysis_model_radio_support))) {
      output$analysis_model_text_support_noinput <- renderUI({
        html_out <- tags$span(style = 'color: red;',
                              "Please make a selection",
                              br(), br())
        return(html_out)
      })
    }
    
    # stop here if inputs aren't found
    validate(
      need(
        is.data.frame(store$selected_df),
        "Data must be first uploaded and selected. Please see 'Data' tab."
      ),
      need(
        isFALSE(input$analysis_model_radio_design == 'quasi'),
        'Natural experiment design is not currently supported'
      ),
      need(
        isFALSE(is.null(input$analysis_model_radio_design)),
        'Please select an assignment mechanism'
      ),
      need(
        isFALSE(is.null(input$analysis_model_radio_estimand)),
        'Please select an estimand and common support rule'
      ),
      need(
        isFALSE(is.null(input$analysis_model_radio_support)),
        'Please select a common support rule'
      )
    )
    
    # insert popup to notify user of model fit process
    # TODO estimate the time remaining empirically?
    shinyWidgets::show_alert(
      title = 'Model fitting...',
      text = "Please wait",
      type = 'info',
      # text = tags$div(
      #   class = 'spinner-grow',
      #   role = 'status',
      #   tags$span(class = 'sr-only', "Loading...")
      # ),
      # html = TRUE,
      btn_labels = NA
    )
    
    # pull the response, treatment, and confounders variables out of the df
    response_v <- store$selected_df[, 1]
    treatment_v <- store$selected_df[, 2]
    confounders_mat <- as.matrix(store$selected_df[, 3:ncol(store$selected_df)])
    
    # run model    
    store$model_results <- bartCause::bartc(
      response = response_v,
      treatment = treatment_v,
      confounders = confounders_mat,
      estimand = base::tolower(input$analysis_model_radio_estimand),
      commonSup.rule = input$analysis_model_radio_support
    )
    
    # store the results
    store$good_model_fit <- TRUE
    
    # close the alert
    shinyWidgets::closeSweetAlert()
    
    # move to next page based on model fit
    if (isTRUE(store$good_model_fit)){
      updateNavbarPage(session, inputId = "nav", selected = "Results")
    } else {
      updateNavbarPage(session, inputId = "nav", selected = "Model diagnostics")
    }
  })
  
  
  # results -----------------------------------------------------------------

  # render the summary table
  output$analysis_results_table_summary <- renderText({
    
    # stop here if model isn't fit yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fit on 'Model' tab"))
    
    # extract estimates and format
    summary(store$model_results)$estimates %>% 
      t() %>% 
      knitr::kable(digits = 3, format = 'html') %>% 
      kableExtra::kable_styling(bootstrap_options = c("hover", "condensed"))
  })
  
  # render the interpretation text
  
  # ITE plot
  output$anlaysis_results_plot_ITE <- renderPlot({
    
    # stop here if model isn't fit yet
    validate(need(is(store$model_results, "bartcFit"), 
                  "Model must first be fit on 'Model' tab"))
    
    # retrieve model from the store
    BART_model <- store$model_results

    # calculate stats
    ites <- bartCause::extract(BART_model, 'icate')
    ite.m <- apply(ites, 2, mean)
    sd.ite <- apply(ites, 2, sd)
    ite.ub <- ite.m + 1.96 * sd.ite
    ite.lb <-  ite.m - 1.96 * sd.ite
    
    # plot it
    tibble(ite.m, ite.ub, ite.lb) %>% 
      # arrange(ite.m) %>% 
      # mutate(rank = row_number()) %>% 
      ggplot(aes(x = ite.m)) + 
      geom_vline(xintercept = 0, linetype = 'dashed', color = 'grey60') +
      geom_histogram(alpha = 0.8) + 
      labs(title = 'Individual Treatment Effects',
           x = base::toupper(BART_model$estimand), 
           y = 'Frequency')
  })
  
  
  # concepts ----------------------------------------------------------------
  
  # add listeners that link the concepts title image to its article
  tab_titles <- c("Randomization", 'Fundamental problem', 'Assumptions', 'Regression methods')
  lapply(tab_titles, function(page_to_go_to) {
    page_id <- paste0("concepts_link_", page_to_go_to)
    observeEvent(input[[page_id]], {
      updateNavbarPage(session, "nav", page_to_go_to)
    })
  })
  
  # run the randomization module
  randomizationServer(id = 'concepts_randomization')
  
  
  # welcome page ------------------------------------------------------------

  # add listeners that link the front page images to their respective pages
  observeEvent(input$welcome_link_concepts, {
    updateNavbarPage(session, inputId = "nav", selected = "All concepts")
  })
  observeEvent(input$welcome_link_Analysis, {
    updateNavbarPage(session, inputId = "nav", selected = "Data")
    updateTabsetPanel(session, inputId = "analysis_data_tabs", selected = "Load Data")
  })
  
  
})
