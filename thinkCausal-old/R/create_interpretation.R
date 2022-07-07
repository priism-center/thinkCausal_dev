create_interpretation <- function(.model, type, treatment, units, participants){
  if(treatment == '') treatment <- 'treatment condition'
  if(units == '') units <- 'units'
  if(participants == '') participants <- 'participants'
  if(type == 'Causal'){
    if(.model$estimand == 'att') estimand <- paste0('For ', participants, ' in this study that received the ', treatment,  ', receiving the ', treatment)
    if(.model$estimand == 'ate') estimand <- paste0('For ', participants, ' in this study, receiving the ', treatment)
    if(.model$estimand == 'atc') estimand <- paste0('For ', participants, ' in this study that did not receive the ', treatment, ' receiving the ', treatment, ' would have')

    if(as.data.frame(summary(.model)$estimates)[1] > 0) result <- paste0(' led to an increase of ', as.character(round(as.data.frame(summary(.model)$estimates)[1], 2)), ' ', units)
    if(as.data.frame(summary(.model)$estimates)[1] < 0) result <- paste0(' led to a decrease of ', as.character(round(as.data.frame(summary(.model)$estimates)[1], 2)), ' ', units)

    if(.model$estimand == 'att') counterfactual <- paste0(' compared to what would have happened had these ', participants, ' not received the ',treatment, '.')
    if(.model$estimand == 'ate') counterfactual <- paste0(' compared to what would have happened if ', participants, ' did not receive the ', treatment, '.')
    if(.model$estimand == 'atc') counterfactual <- paste0(' compared to the observed state where these ', participants, ' did not receive the ', treatment, '.')

    text <- paste0(estimand, result, counterfactual)
  }
  
  if(type != 'Causal'){
    if(as.data.frame(summary(.model)$estimates)[1] > 0) point <- 'higher'
    if(as.data.frame(summary(.model)$estimates)[1] < 0) point <- 'lower'
    text <- paste0('When comparing two groups of ', participants, ' who are similar on all covariates included in the analysis except for the ', treatment, ' the group of ', participants, ' that received the ', treatment, ' are expected to have outcomes that are ', as.character(round(as.data.frame(summary(.model)$estimates)[1], 2)),' ', units, ' ', point, ', on average, compared to the group of ', participants, ' that did not receive the ', treatment, '.')
  }

  return(text)

}
