#' @title AutoML Models' Parameters Summary
#' @description Extracts models' parameters from AutoML grid
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom h2o h2o.precision h2o.recall
#' @importFrom curl curl
#' @param model a h2o AutoML object
#' @return a dataframe of models' parameters
#' @author E. F. Haghish
#'
#' @examples
#' \dontrun{
#' if(requireNamespace("h2o")) {
#'   library(h2o)
#'   h2o.init(ignore_config = TRUE, nthreads = 2, bind_to_localhost = FALSE, insecure = TRUE)
#'   prostate_path <- system.file("extdata", "prostate.csv", package = "h2o")
#'   prostate <- h2o.importFile(path = prostate_path, header = TRUE)
#'   y <- "CAPSULE"
#'   prostate[,y] <- as.factor(prostate[,y])  #convert to factor for classification
#'   aml <- h2o.automl(y = y,
#'                     training_frame = prostate,
#'                     include_algos = "GLM",
#'                     max_models = 1,
#'                     max_runtime_secs = 60)
#'
#'   # extract the model parameters
#'   model.param <- automlModelParam(aml@leader)
#' }
#' }
#' @export
automlModelParam <- function(model) {
  algorithm <- model@algorithm

  # Deeplearning model summary
  # ============================================================
  if (algorithm == "deeplearning") {

    # Model ID
    # --------------------
    model_id <- unlist(model@model_id)

    # Model Parameters
    # --------------------
    activation <- unlist(model@allparameters$activation)
    if (is.null(activation)) activation <- NA
    # avoid multiple values and convert them to string
    hidden <- unlist(model@parameters$hidden)
    if (!is.null(hidden)) hidden <- paste(hidden, collapse = ",", sep="")
    else hidden <- NA
    epochs <- unlist(model@allparameters$epochs)
    if (is.null(epochs)) epochs <- NA
    seed <- unlist(model@allparameters$seed)
    if (is.null(seed)) seed <- NA
    distribution <- unlist(model@allparameters$distribution)
    if (is.null(distribution)) distribution <- NA
    epsilon <- unlist(model@allparameters$epsilon)
    if (is.null(epsilon)) epsilon <- NA
    input_dropout_ratio <- unlist(model@allparameters$input_dropout_ratio)
    if (is.null(input_dropout_ratio)) input_dropout_ratio <- NA

    hidden_dropout_ratios <- unlist(model@allparameters$hidden_dropout_ratios)
    # avoid multiple values and convert them to string
    if (!is.null(hidden_dropout_ratios)) hidden_dropout_ratios <- paste(hidden_dropout_ratios, collapse = ",", sep="")
    else hidden_dropout_ratios <- NA

    categorical_encoding <- unlist(model@allparameters$categorical_encoding)
    if (is.null(categorical_encoding)) categorical_encoding <- NA
    l1 <- unlist(model@allparameters$l1)
    if (is.null(l1)) l1 <- NA
    l2 <- unlist(model@allparameters$l2)
    if (is.null(l2)) l2 <- NA
    target_ratio_comm_to_comp <- unlist(model@allparameters$target_ratio_comm_to_comp)
    if (is.null(target_ratio_comm_to_comp)) target_ratio_comm_to_comp <- NA
    adaptive_rate <- unlist(model@allparameters$adaptive_rate)
    if (is.null(adaptive_rate)) adaptive_rate <- NA
    rho <- unlist(model@allparameters$rho)
    if (is.null(rho)) rho <- NA
    rate <- unlist(model@allparameters$rate)
    if (is.null(rate)) rate <- NA
    rate_annealing <- unlist(model@allparameters$rate_annealing)
    if (is.null(rate_annealing)) rate_annealing <- NA
    rate_decay <- unlist(model@allparameters$rate_decay)
    if (is.null(rate_decay)) rate_decay <- NA





    # Stopping Rules
    # --------------------
    stopping_metric <- unlist(model@allparameters$stopping_metric)
    if (is.null(stopping_metric)) stopping_metric <- NA
    stopping_tolerance <- unlist(model@allparameters$stopping_tolerance)
    if (is.null(stopping_tolerance)) stopping_tolerance <- NA
    stopping_rounds <- unlist(model@allparameters$stopping_rounds)
    if (is.null(stopping_rounds)) stopping_rounds <- NA

    res <- c(model_id, activation, hidden, epochs,
             epsilon, input_dropout_ratio,
             hidden_dropout_ratios,
             categorical_encoding, l1, l2,
             seed, distribution,
             stopping_metric, stopping_tolerance, stopping_rounds)

    names(res) <- c("model_id", "activation", "hidden", "epochs",
                    "epsilon", "input_dropout_ratio",
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

  # GBM model summary
  # ============================================================
  if (algorithm == "xgboost") {

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
    min_child_weight <- unlist(model@parameters$min_child_weight)
    if (is.null(min_child_weight)) min_child_weight <- NA
    seed <- unlist(model@parameters$seed)
    if (is.null(seed)) seed <- NA
    distribution <- unlist(model@parameters$distribution)
    if (is.null(distribution)) distribution <- NA
    sample_rate <- unlist(model@parameters$sample_rate)
    if (is.null(sample_rate)) sample_rate <- NA
    subsample <- unlist(model@parameters$subsample)
    if (is.null(subsample)) subsample <- NA
    col_sample_rate <- unlist(model@parameters$col_sample_rate)
    if (is.null(col_sample_rate)) col_sample_rate <- NA
    col_sample_rate_per_tree <- unlist(model@parameters$col_sample_rate_per_tree)
    if (is.null(col_sample_rate_per_tree)) col_sample_rate_per_tree <- NA
    colsample_bylevel <- unlist(model@parameters$colsample_bylevel)
    if (is.null(colsample_bylevel)) colsample_bylevel <- NA
    colsample_bytree <- unlist(model@parameters$colsample_bytree)
    if (is.null(colsample_bytree)) colsample_bytree <- NA
    score_tree_interval <- unlist(model@parameters$score_tree_interval)
    if (is.null(score_tree_interval)) score_tree_interval <- NA
    tree_method <- unlist(model@parameters$tree_method)
    if (is.null(tree_method)) tree_method <- NA
    dmatrix_type <- unlist(model@parameters$dmatrix_type)
    if (is.null(dmatrix_type)) dmatrix_type <- NA



    # Stopping Rules
    # --------------------
    stopping_metric <- unlist(model@parameters$stopping_metric)
    if (is.null(stopping_metric)) stopping_metric <- NA
    stopping_tolerance <- unlist(model@parameters$stopping_tolerance)
    if (is.null(stopping_tolerance)) stopping_tolerance <- NA
    else stopping_tolerance <- round(stopping_tolerance, 4)

    res <- c(model_id, ntrees, max_depth, min_rows,
             min_child_weight,
             sample_rate, subsample, col_sample_rate,
             col_sample_rate_per_tree, colsample_bylevel,
             colsample_bytree,score_tree_interval,
             tree_method, dmatrix_type,
             seed, distribution,
             stopping_metric, stopping_tolerance)

    names(res) <- c("model_id", "ntrees", "max_depth", "min_rows",
                    "min_child_weight",
                    "sample_rate", "subsample","col_sample_rate",
                    "col_sample_rate_per_tree", "colsample_bylevel",
                    "colsample_bytree", "score_tree_interval",
                    "tree_method", "dmatrix_type",
                    "seed", "distribution",
                    "stopping_metric", "stopping_tolerance")

    return(as.data.frame(t(res)))
  }

  # DRF model summary
  # ============================================================
  if (algorithm == "drf") {
    # Model ID
    # --------------------
    model_id <- unlist(model@model_id)

    # Model Parameters
    # --------------------
    ntrees <- unlist(model@allparameters$ntree)
    if (is.null(ntrees)) ntrees <- NA
    max_depth <- unlist(model@allparameters$max_depth)
    if (is.null(max_depth)) max_depth <- NA
    min_rows <- unlist(model@allparameters$min_rows)
    if (is.null(min_rows)) min_rows <- NA
    mtries <- unlist(model@allparameters$mtries)
    if (is.null(mtries)) mtries <- NA
    min_split_improvement <- unlist(model@allparameters$min_split_improvement)
    if (is.null(min_split_improvement)) min_split_improvement <- NA
    calibrate_model <- unlist(model@allparameters$calibrate_model)
    if (is.null(calibrate_model)) calibrate_model <- NA
    seed <- unlist(model@parameters$seed)
    if (is.null(seed)) seed <- NA
    distribution <- unlist(model@parameters$distribution)
    if (is.null(distribution)) distribution <- NA
    sample_rate <- unlist(model@allparameters$sample_rate)
    if (is.null(sample_rate)) sample_rate <- NA
    categorical_encoding <- unlist(model@parameters$categorical_encoding)
    if (is.null(categorical_encoding)) categorical_encoding <- NA
    col_sample_rate_change_per_level <- unlist(model@allparameters$col_sample_rate_change_per_level)
    if (is.null(col_sample_rate_change_per_level)) col_sample_rate_change_per_level <- NA
    col_sample_rate_per_tree <- unlist(model@allparameters$col_sample_rate_per_tree)
    if (is.null(col_sample_rate_per_tree)) col_sample_rate_per_tree <- NA
    binomial_double_trees <- unlist(model@allparameters$binomial_double_trees)
    if (is.null(binomial_double_trees)) binomial_double_trees <- NA
    colsample_bytree <- unlist(model@allparameters$colsample_bytree)
    score_tree_interval <- unlist(model@parameters$score_tree_interval)
    if (is.null(score_tree_interval)) score_tree_interval <- NA
    nbins <- unlist(model@allparameters$nbins)
    if (is.null(nbins)) nbins <- NA
    nbins_top_level <- unlist(model@allparameters$nbins_top_level)
    if (is.null(nbins_top_level)) nbins_top_level <- NA
    nbins_cats <- unlist(model@allparameters$nbins_cats)
    if (is.null(nbins_cats)) nbins_cats <- NA
    histogram_type <- unlist(model@parameters$histogram_type)
    if (is.null(histogram_type)) histogram_type <- NA




    # Stopping Rules
    # --------------------
    stopping_metric <- unlist(model@parameters$stopping_metric)
    if (is.null(stopping_metric)) stopping_metric <- NA
    stopping_tolerance <- unlist(model@parameters$stopping_tolerance)
    if (is.null(stopping_tolerance)) stopping_tolerance <- NA
    else stopping_tolerance <- round(stopping_tolerance, 4)

    res <- c(model_id, ntrees, max_depth, min_rows,
             mtries, min_split_improvement, calibrate_model,
             sample_rate, categorical_encoding, col_sample_rate_change_per_level,
             col_sample_rate_per_tree, binomial_double_trees,
             score_tree_interval,
             nbins, nbins_top_level, nbins_cats, histogram_type,
             seed, distribution,
             stopping_metric, stopping_tolerance)

    names(res) <- c("model_id", "ntrees", "max_depth", "min_rows",
                    "mtries", "min_split_improvement","calibrate_model",
                    "sample_rate", "categorical_encoding","col_sample_rate_change_per_level",
                    "col_sample_rate_per_tree", "binomial_double_trees",
                    "score_tree_interval",
                    "nbins", "nbins_top_level", "nbins_cats","histogram_type",
                    "seed", "distribution",
                    "stopping_metric", "stopping_tolerance")

    return(as.data.frame(t(res)))
  }

  # GLM model summary
  # ============================================================
  if (algorithm == "glm") {
    # Model ID
    # --------------------
    model_id <- unlist(model@model_id)

    # Model Parameters
    # --------------------
    family <- unlist(model@parameters$family)
    if (is.null(family)) family <- NA
    solver <- unlist(model@allparameters$solver)
    if (is.null(solver)) solver <- NA

    # these values are preserved as string to be evaluated as 'eval(parse(text=VAR))'
    alpha <- paste0("c(",paste0(model@allparameters$alpha, sep="", collapse = ","),")")
    if (is.null(alpha)) alpha <- NA
    lambda <- paste0("c(",paste0(model@allparameters$lambda, sep="", collapse = ","),")")
    if (is.null(lambda)) lambda <- NA

    lambda_search <- unlist(model@allparameters$lambda_search)
    if (is.null(lambda_search)) lambda_search <- NA
    nlambdas <- unlist(model@allparameters$nlambdas)
    if (is.null(nlambdas)) nlambdas <- NA
    seed <- unlist(model@parameters$seed)
    if (is.null(seed)) seed <- NA
    max_iterations <- unlist(model@allparameters$max_iterations)
    if (is.null(max_iterations)) max_iterations <- NA
    objective_epsilon <- unlist(model@parameters$objective_epsilon)
    if (is.null(objective_epsilon)) objective_epsilon <- NA
    gradient_epsilon <- unlist(model@allparameters$gradient_epsilon)
    if (is.null(gradient_epsilon)) gradient_epsilon <- NA
    link <- unlist(model@allparameters$link)
    if (is.null(link)) link <- NA
    lambda_min_ratio <- unlist(model@allparameters$lambda_min_ratio)
    if (is.null(lambda_min_ratio)) lambda_min_ratio <- NA
    max_active_predictors <- unlist(model@parameters$max_active_predictors)
    if (is.null(max_active_predictors)) max_active_predictors <- NA
    obj_reg <- unlist(model@allparameters$obj_reg)
    if (is.null(obj_reg)) obj_reg <- NA


    # Stopping Rules
    # --------------------
    stopping_metric <- unlist(model@allparameters$stopping_metric)
    if (is.null(stopping_metric)) stopping_metric <- NA
    stopping_tolerance <- unlist(model@allparameters$stopping_tolerance)
    if (is.null(stopping_tolerance)) stopping_tolerance <- NA
    else stopping_tolerance <- round(stopping_tolerance, 4)

    res <- c(model_id, family, solver,
             alpha, lambda, # THIS HAS A PROBLEM BECAUSE IT HAS TO BE A MATRIX
             lambda_search, nlambdas,
             max_iterations, objective_epsilon, gradient_epsilon,
             link, lambda_min_ratio,
             max_active_predictors,
             obj_reg,
             seed,
             stopping_metric, stopping_tolerance)

    names(res) <- c("model_id", "family", "solver",
                    "alpha","lambda",
                    "lambda_search","nlambdas",
                    "max_iterations", "objective_epsilon","gradient_epsilon",
                    "link", "lambda_min_ratio",
                    "max_active_predictors",
                    "obj_reg",
                    "seed",
                    "stopping_metric", "stopping_tolerance")

    return(as.data.frame(t(res)))
  }

  # StackEnsemble model summary
  # ============================================================
  if (algorithm == "stackedensemble") {
    # Model ID
    # --------------------
    model_id <- unlist(model@model_id)

    # Model Parameters
    # --------------------
    metalearner_algorithm <- unlist(model@parameters$metalearner_algorithm)
    if (is.null(metalearner_algorithm)) metalearner_algorithm <- NA
    metalearner_transform <- unlist(model@allparameters$metalearner_transform)
    if (is.null(metalearner_transform)) metalearner_transform <- NA
    seed <- unlist(model@parameters$seed)
    if (is.null(seed)) seed <- NA

    res <- c(model_id, metalearner_algorithm, metalearner_transform,
             seed)

    names(res) <- c("model_id", "metalearner_algorithm", "metalearner_transform",
                    "seed")

    return(as.data.frame(t(res)))
  }
}

