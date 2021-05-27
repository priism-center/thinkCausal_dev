
create_summary_table <- function(estimated, full, tau, unbiased = FALSE){
  
  unbiased_Z <- summary(full)$coefficients['Z', 'Estimate']
  sd_unbiased <- summary(full)$coefficients['Z', 'Std. Error']
  estimated_Z <- summary(estimated)$coefficients['Z', 'Estimate']
  sd_estimated <- summary(estimated)$coefficients['Z', 'Std. Error']
  
  if(unbiased == FALSE){
    
    dat_out <- data.frame(
      Category = c("Actual", "Unbiased", "Problematic"),
      Estimate = c(tau, unbiased_Z, estimated_Z),
      Std.Dev = c(0, sd_unbiased, sd_estimated))
  }else{
    dat_out <- data.frame(
      # when all confounders are included, the estimate used to be problematic becomes unbiased
      Category = c("Actual", "Unbiased", "Unbiased"), 
      Estimate = c(tau, unbiased_Z, estimated_Z),
      Std.Dev = c(0, sd_unbiased, sd_estimated))
  }
 
  return(dat_out)
}