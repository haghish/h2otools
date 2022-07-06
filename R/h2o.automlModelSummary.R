# R define h2o.automlModelSummary function
# ============================================================
h2o.automlModelSummary <- function(model) {
  algorithm <- model@algorithm
  
  # Deeplearning model summary
  # ============================================================
  if (algorithm == "deeplearning") {
    
    # Model ID
    # --------------------
    model_id <- unlist(model@model_id)
    
    # Model Parameters
    # --------------------
    activation <- unlist(model@parameters$activation)
    if (is.null(activation)) activation <- NA
    hidden <- unlist(model@parameters$hidden)
    if (is.null(hidden)) hidden <- NA
    epochs <- unlist(model@parameters$epochs)
    if (is.null(epochs)) epochs <- NA
    seed <- unlist(model@parameters$seed)
    if (is.null(seed)) seed <- NA
    distribution <- unlist(model@parameters$distribution)
    if (is.null(distribution)) distribution <- NA
    epsilon <- unlist(model@parameters$epsilon)
    if (is.null(epsilon)) epsilon <- NA
    input_dropout_ratio <- unlist(model@parameters$input_dropout_ratio)
    if (is.null(input_dropout_ratio)) input_dropout_ratio <- NA
    hidden_dropout_ratios <- unlist(model@parameters$hidden_dropout_ratios)
    if (is.null(hidden_dropout_ratios)) hidden_dropout_ratios <- NA
    categorical_encoding <- unlist(model@parameters$categorical_encoding)
    if (is.null(categorical_encoding)) categorical_encoding <- NA
    l1 <- unlist(model@parameters$l1)
    if (is.null(l1)) l1 <- NA
    l2 <- unlist(model@parameters$l2)
    if (is.null(l2)) l2 <- NA
    categorical_encoding <- unlist(model@parameters$categorical_encoding)
    if (is.null(categorical_encoding)) categorical_encoding <- NA
    target_ratio_comm_to_comp <- unlist(model@parameters$target_ratio_comm_to_comp)
    if (is.null(target_ratio_comm_to_comp)) target_ratio_comm_to_comp <- NA
    adaptive_rate <- unlist(model@parameters$adaptive_rate)
    if (is.null(adaptive_rate)) adaptive_rate <- NA
    rho <- unlist(model@parameters$rho)
    if (is.null(rho)) rho <- NA
    rate <- unlist(model@parameters$rate)
    if (is.null(rate)) rate <- NA
    rate_annealing <- unlist(model@parameters$rate_annealing)
    if (is.null(rate_annealing)) rate_annealing <- NA
    rate_decay <- unlist(model@parameters$rate_decay)
    if (is.null(rate_decay)) rate_decay <- NA
    
    
    
    
    
    # Stopping Rules
    # --------------------
    stopping_metric <- unlist(model@parameters$stopping_metric)
    if (is.null(stopping_metric)) stopping_metric <- NA
    stopping_tolerance <- unlist(model@parameters$stopping_tolerance)
    if (is.null(stopping_tolerance)) stopping_tolerance <- NA
    stopping_rounds <- unlist(model@parameters$stopping_rounds)
    if (is.null(stopping_rounds)) stopping_rounds <- NA
    
    res <- c(model_id, activation, hidden, epochs,
             epsilon, input_dropout_ratio, hidden_dropout_ratios,
             hidden_dropout_ratios,
             categorical_encoding, l1, l2,
             seed, distribution, 
             stopping_metric, stopping_tolerance, stopping_rounds)
    
    names(res) <- c("model_id", "activation", "hidden", "epochs",
                    "epsilon", "input_dropout_ratio", "hidden_dropout_ratios",
                    "hidden_dropout_ratios",
                    "categorical_encoding", "l1","l2",
                    "seed", "distribution", 
                    "stopping_metric", "stopping_tolerance", "stopping_rounds")
    
    return(as.data.frame(t(res)))
  }
  
  
  
  # GBM model summary
  # ============================================================
  if (algorithm == "gbm") {
    
    # Model ID
    # --------------------
    model_id <- unlist(model@model_id)
    
    # Model Parameters
    # --------------------
    ntrees <- unlist(model@parameters$ntree)
    if (is.null(ntrees)) ntrees <- NA
    max_depth <- unlist(model@parameters$max_depth)
    if (is.null(max_depth)) max_depth <- NA
    min_rows <- unlist(model@parameters$min_rows)
    if (is.null(min_rows)) min_rows <- NA
    seed <- unlist(model@parameters$seed)
    if (is.null(seed)) seed <- NA
    distribution <- unlist(model@parameters$distribution)
    if (is.null(distribution)) distribution <- NA
    sample_rate <- unlist(model@parameters$sample_rate)
    if (is.null(sample_rate)) sample_rate <- NA
    col_sample_rate_per_tree <- unlist(model@parameters$col_sample_rate_per_tree)
    if (is.null(col_sample_rate_per_tree)) col_sample_rate_per_tree <- NA
    min_split_improvement <- unlist(model@parameters$min_split_improvement)
    if (is.null(min_split_improvement)) min_split_improvement <- NA
    
    # Stopping Rules
    # --------------------
    stopping_metric <- unlist(model@parameters$stopping_metric)
    if (is.null(stopping_metric)) stopping_metric <- NA
    stopping_tolerance <- unlist(model@parameters$stopping_tolerance)
    if (is.null(stopping_tolerance)) stopping_tolerance <- NA
    
    res <- c(model_id, ntrees, max_depth, min_rows,
             sample_rate, col_sample_rate_per_tree, min_split_improvement,
             seed, distribution, 
             stopping_metric, stopping_tolerance)
    
    names(res) <- c("model_id", "ntrees", "max_depth", "min_rows",
                    "sample_rate", "col_sample_rate_per_tree", "min_split_improvement",
                    "seed", "distribution", 
                    "stopping_metric", "stopping_tolerance")
    
    return(as.data.frame(t(res)))
  }
}
