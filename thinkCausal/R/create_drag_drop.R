create_drag_drop_roles <- function(.data, ns_prefix){
  
  if (!inherits(.data, 'data.frame')) stop('.data must be a dataframe')
  
  # infer which columns are Z, Y, and X columns (i.e. smart defaults)
  auto_columns <- clean_detect_ZYX_columns(.data)
  
  # render the UI
  drag_drop_html <- tagList(
    bucket_list(
      header = "Drag the variables to their respective roles",
      group_name = paste0(ns_prefix, "_dragdrop"),
      orientation = "horizontal",
      class = 'default-sortable sortable-wide',
      add_rank_list(
        input_id = paste0(ns_prefix, "_dragdrop_covariates"),
        text = strong("Covariates"),
        labels = auto_columns$X,
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = paste0(ns_prefix, "_dragdrop_treatment"),
        text = strong("Treatment"),
        labels = auto_columns$Z,
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = paste0(ns_prefix, "_dragdrop_response"),
        text = strong("Response"),
        labels = auto_columns$Y,
        options = sortable_options(multiDrag = TRUE)
      ),
      add_rank_list(
        input_id = paste0(ns_prefix, "_dragdrop_delete"),
        text = strong("Exclude these variables"),
        labels = auto_columns$ID,
        options = sortable_options(multiDrag = TRUE)
      )
    )
  )
  
  return(drag_drop_html)
}


create_drag_drop_groups <- function(.data, ns_prefix, n_dummy_groups, grouped_varibles = NULL){
  
  if (!inherits(.data, 'data.frame')) stop('.data must be a dataframe')
  
  # infer variables that are of logical other than the treatment and response
  df <- .data[, -c(1:2)]
  cat_var_names <- colnames(df)[sapply(df, clean_detect_logical)]
  
  if(is.null(grouped_varibles)){
    # infer which columns are grouped (i.e. smart defaults)
    auto_groups <- clean_detect_dummy_cols_unique(df)
    n_dummy_groups <- max(n_dummy_groups, length(auto_groups))
    ungrouped_vars <- setdiff(cat_var_names, unlist(auto_groups))
  }else{
    auto_groups <- lapply(grouped_varibles, function(x) x[-1])
    user_group_name <- lapply(grouped_varibles, function(x) x[1])
    n_dummy_groups <- max(n_dummy_groups, length(auto_groups))
    ungrouped_vars <- setdiff(cat_var_names, as.character(unlist(auto_groups)))
  }
  

  ## TODO: 'add group' will overwrite the names
  
  # create the grouping UI
  drag_drop_html <- tagList(
    bucket_list(
      header = "Drag the variables to their respective groups",
      group_name = paste0(ns_prefix, "_dragdrop_grouping"),
      orientation = "horizontal",
      class = 'default-sortable sortable-wide',
        
      add_rank_list(
        input_id = paste0(ns_prefix, "_dragdrop_grouping_variables"),
        text = strong("Ungrouped variables"),
        labels = ungrouped_vars,
        options = sortable_options(multiDrag = TRUE)
      )
    ),
    # allow user to add groups
    lapply(c(1:n_dummy_groups), function(i){
      bucket_list(
        header = " ",
        group_name = "analysis_data_dragdrop_grouping",
        orientation = "horizontal",
        class = 'default-sortable sortable-wide',
        
        add_rank_list(
          input_id = paste0(ns_prefix, '_categorical_group_', i),
          text = textInput(inputId = paste0("rename_group_", i), 
                           label = NULL, 
                           value = ifelse(i <= length(grouped_varibles), user_group_name[i], paste0("Group ", i))),
          labels = tryCatch(auto_groups[[i]], error = function(e) NULL),
          options = sortable_options(multiDrag = TRUE)
        )
      )
    })
  )
  
  out <- list(
    html = drag_drop_html,
    n_dummy_groups = n_dummy_groups
  )
  
  return(out)
}
