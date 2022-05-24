create_drag_drop_roles <- function(ns, .data, ns_prefix, design, weights, ran_eff){
  if (!inherits(.data, 'data.frame')) stop('.data must be a dataframe')
  
  # infer which columns are Z, Y, and X columns (i.e. smart defaults)
  auto_columns <- clean_detect_ZYX_columns(.data)
  
  
  drag_drop_html <- 
    bucket_list(
      header = "Drag the variables to their respective roles",
      group_name = ns(paste0(ns_prefix, "_dragdrop")),
      orientation = "horizontal",
      class = 'default-sortable sortable-wide',
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_covariates")),
        text = strong("Covariates"),
        labels = auto_columns$X,
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_treatment")),
        text = strong("Treatment"),
        labels = auto_columns$Z,
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_response")),
        text = strong("Outcome"),
        labels = auto_columns$Y,
        options = sortable_options(multiDrag = TRUE)
      ),
      if(design == 'Block randomized treatment'){
        add_rank_list(
          input_id = ns(paste0(ns_prefix, "_dragdrop_block")),
          text = strong("Blocking variable(s)"),
          labels = NULL,
          options = sortable_options(multiDrag = TRUE)
        )
      }, 
      if(weights == 'Yes'){
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_weight")),
        text = strong("Survey weight"),
        labels = NULL,
        options = max_1_item_opts
      )
      }, 
      if(ran_eff == 'Yes'){
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_ran_eff")),
        text = strong("Random Intercept(s)"),
        labels = NULL,
        options = sortable_options(multiDrag = TRUE)
      )
      },
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_post_treatment")),
        text = create_info_icon(
          label = strong("Post-treatment variables to exclude from analysis"),
          text = "All variables that could potentially be affected by the treatment"
        ),
        labels = NULL,
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = ns(paste0(ns_prefix, "_dragdrop_delete")),
        text = create_info_icon(
          label = strong("ID or index variables to exclude from analysis"),
          text = "Exclude index or ID variables in addition to extraneous variables"
        ),
        labels = auto_columns$ID,
        options = sortable_options(multiDrag = TRUE)
      )
    )
  
  drag_drop_html <- tagList(drag_drop_html)
  
  return(drag_drop_html)
}

