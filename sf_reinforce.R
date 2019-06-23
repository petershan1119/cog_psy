social_feedback <- hBayesDM_model(
  task_name       = "sf",
  model_name      = "reinforce",
  data_columns    = c("subjID", "Decision", "OC", "reFeedback"),
  parameters      = list("alpha"    = c(0, 0.5, 1),
                         "beta_vsp" = c(-Inf, 0, Inf),
                         "beta_oc"  = c(-Inf, 0, Inf),
                         "beta_0"   = c(-Inf, 0, Inf)),
  preprocess_func = function(raw_data, general_info) {
    # Currently class(raw_data) == "data.table"
    
    # Use general_info of raw_data
    subjs   <- general_info$subjs
    n_subj  <- general_info$n_subj
    t_subjs <- general_info$t_subjs
    t_max   <- general_info$t_max
    
    # Initialize (model-specific) data arrays
    Decision     <- array( 0, c(n_subj, t_max))
    OC           <- array( 0, c(n_subj, t_max))
    reFeedback   <- array( 0, c(n_subj, t_max))
    
    # Write from raw_data to the data arrays
    for (i in 1:n_subj) {
      subj <- subjs[i]
      t <- t_subjs[i]
      DT_subj <- raw_data[subjid == subj]
      
      Decision[i, 1:t]     <- DT_subj$decision
      OC[i, 1:t]           <- DT_subj$oc
      reFeedback[i, 1:t]   <- DT_subj$refeedback
    }
    
    # Wrap into a list for Stan
    data_list <- list(
      N          = n_subj,
      T          = t_max,
      Tsubj      = t_subjs,
      Decision   = Decision,
      OC         = OC,
      reFeedback = reFeedback
    )
    
    # Returned data_list will directly be passed to Stan
    return(data_list)
  }
)