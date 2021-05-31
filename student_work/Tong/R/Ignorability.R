# Server -----------------------------------------------------------------------
Ignorability <- function (input, output, session) {
  
  resample <- reactiveValues(data = FALSE)
  
  plot_reactive <- reactive({
    
    if(resample$data == FALSE){
      set.seed(1234)
      data <- dgp(500, 5)
    }else{
      data <- df_resample()
    }
    
    data0 <- data[,c("Y","Z")]
    if (input$var1) {
      data0 <- cbind(data0, data$var1)
    }
    if (input$var2) {
      data0 <- cbind(data0, data$var2)
    }
    if (input$var3) {
      data0 <- cbind(data0, data$var3)
    }
    if (input$var4) {
      data0 <- cbind(data0, data$var4)
    }
    if (input$var5) {
      data0 <- cbind(data0, data$var5)
    }
    if (input$var6) {
      data0 <- cbind(data0, data$var6)
    }
    if (input$var7) {
      data0 <- cbind(data0, data$var7)
    }
    if (input$var8) {
      data0 <- cbind(data0, data$var8)
    }
    if (input$var9) {
      data0 <- cbind(data0, data$var9)
    }
    if (input$var_bi) {
      data0 <- cbind(data0, data$var_bi)
    }
    
    mod_lm <- lm(Y ~ ., data = data0)
    mod_lm_full <- lm(Y ~ var2 + var4 + var6 + var7 + var8 + var9 + var_bi + Z, data = data)
    
    if(sum(c("data$var2", "data$var4", "data$var6", "data$var7", "data$var8", "data$var9", "data$var_bi") %in% colnames(data0)) == 7){
      dat_out <- create_summary_table(estimated = mod_lm, full = mod_lm_full, tau = isolate(input$tau), unbiased = TRUE)
      output$res0 <- renderTable({ xtable(dat_out) })
    }else{
      dat_out <- create_summary_table(estimated = mod_lm, full = mod_lm_full, tau = isolate(input$tau))
      output$res0 <- renderTable({ xtable(dat_out) })
    }
    plot_effects(dat_out)
    
  })
  
  output$plot0 <- renderPlot({
    plot_reactive()
  })
  
  # changed sampsize and tau will be effctive only when resample is clicked
  df_resample <- eventReactive( input$gen, {
    dgp(input$sampsize, input$tau)
  })
  
  observeEvent(input$gen, {
    resample$data <- TRUE
  })
}