
server <- function(input, output){
  callModule(Ignorability, "AllConfounders")
}