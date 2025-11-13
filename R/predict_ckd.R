#' Predict Probabilities for Models
#'
#' The function eturns a two-column matrix/data.frame of class probabilities.
#' Used for evaluation and curves.
#'
#' @param df A data frame. Cleaned testing data returned by
#'   `data_process_ckd()`.
#' @param model A list. Contain model returned by `model_train_ckd()`.
#'
#' @return A data.frame. With columns `notckd`, `ckd`.
#'
#' @seealso predict_class_ckd(), model_evaluation_ckd(), model_train_ckd()
#'
#' @examples
#' set.seed(123)
#' test_df <- readRDS(
#'   system.file("extdata", "test_df.rds", package="ckdPrediction")
#'   )
#' svm_fit <- readRDS(
#'   system.file("extdata", "svm_fit.rds", package="ckdPrediction")
#'   )
#' predict_prob <- predict_ckd(test_df, svm_fit)
#' head(predict_prob)
#'
#' @seealso model_train_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom stats model.matrix predict
#'
#' @export
predict_ckd <- function(df, model) {
  if (model$model_type == "rf_fit") {
    prob <- predict(model$model, df, type = "prob")
    prob <- prob[, c("notckd", "ckd")]
  } else if (model$model_type == "glm_fit") {
    X <- stats::model.matrix(classification ~ . - id, data = df)[, -1]
    p <- as.numeric(stats::predict(model$model, newx = X,
                                   s = "lambda.min", type = "response"))
    prob <- data.frame(notckd = 1 - p, ckd = p, row.names = rownames(df))
  } else if (model$model_type == "svm_fit") {
    X <- subset(df, select = -c(id, classification))
    pp <- attr(stats::predict(model$model, X, probability = TRUE),
               "probabilities")
    prob <- pp[, c("notckd", "ckd")]
  } else {
    stop("Invalid Model Input!")
  }

  return(prob)
}


#' Predict Class Labels for Models
#'
#' The function returns a two-column matrix/data.frame of class labels.
#' Used for evaluation and curves.
#'
#' @param df A data frame. Cleaned testing data returned by
#'   `data_process_ckd()`.
#' @param model A list. Contain model returned by `model_train_ckd()`.
#' @param threshold A nemeric number in (0,1), default is 0.5.
#'   The "ckd" probability over this threshold lead to a "ckd" prediction.
#'
#' @return A factor. Contain all predicted labels (`notckd` / `ckd`).
#'
#' @examples
#' set.seed(123)
#' test_df <- readRDS(
#'   system.file("extdata", "test_df.rds", package="ckdPrediction")
#'   )
#' svm_fit <- readRDS(
#'   system.file("extdata", "svm_fit.rds", package="ckdPrediction")
#'   )
#' predict_class <- predict_class_ckd(test_df, svm_fit, threshold = 0.5)
#' head(predict_class)
#'
#' @seealso predict_ckd(), model_evaluation_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom stats model.matrix predict
#'
#' @export
predict_class_ckd <- function(df, model, threshold = 0.5) {
  pr <- predict_ckd(df, model)
  predict_class <- factor(
    ifelse(pr[, "ckd"] <= threshold, "notckd", "ckd"),
    levels = c("notckd", "ckd")
  )

  return(predict_class)
}
