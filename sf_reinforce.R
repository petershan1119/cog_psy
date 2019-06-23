#' @templateVar MODEL_FUNCTION sf_reinforce
#' @templateVar CONTRIBUTOR {Sang Won Han}
#' @templateVar TASK_NAME Social Feedback Task
#' @templateVar TASK_CITE (Yoon et al., 2018)
#' @templateVar MODEL_NAME Social Feedback Reinforcement Model
#' @templateVar MODEL_TYPE Hierarchical
#' @templateVar DATA_COLUMNS "subjID", "Decision", "OC", "reFeedback"
#' @templateVar PARAMETERS "alpha", "beta_vsp", "beta_oc", "beta_0"
#' @templateVar LENGTH_DATA_COLUMNS 4
#' @templateVar DETAILS_DATA_1 \item{"subjID"}{A unique identifier for each subject in the data-set.}
#' @templateVar DETAILS_DATA_2 \item{"Decision"}{Which option was chosen?}
#' @templateVar DETAILS_DATA_3 \item{"OC"}{Outcome.}
#' @templateVar DETAILS_DATA_4 \item{"reFeedback"}{Reverse-coded Feedback.}
#'
#' @template model-documentation
#'
#' @export
#' @include hBayesDM_model.R
#'
#' @references
#' Yoon, L., Somerville, L. H., & Kim H. (2018). Development of MPFC function
#'   mediates shifts in self-protective behavior provoked by social feedback.
#'   Nature Neuroscience, 9(1), 1-10.

sf_reinforce <- hBayesDM_model(
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