simple_data_types <- function(input_data){
  # simplifies the data types to either categorical, logical, or numeric
  # returns NA if it can't simplify
  
  raw_data_types <- apply(input_data, 2, class)
  
  # create mapping between complex and simple data types
  data_type_mapping <- data.frame(
    complex = c("character", "factor", "logical", "numeric", "integer"),
    simple = c("Categorical", "Categorical", 'Logical', "Continuous", "Continuous"),
    stringsAsFactors = FALSE
  )
  
  # get simple data
  simple_data_types <- left_join(
    x = data.frame(complex = as.vector(raw_data_types)),
    y = data_type_mapping,
    by = 'complex')
  
  return(simple_data_types$simple)
}