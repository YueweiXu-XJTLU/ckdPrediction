#' Train Machine Learning Models (rf/glm/svm)
#'
#' This function uses cleaned ckd data to train one of the three ML-models:
#'   1. random forest (rf);
#'   2. (ridge) logistic regression (glm);
#'   3. support vector machine (svm).
#'
#' @param df A data.frame. Cleaned training data returned by
#'   `data_process_ckd()`.
#' @param type A numeric number. Representing the model type:
#'   1 = random forest (rf);
#'   2 = (ridge) logistic regression (glm);
#'   3 = support vector machine (svm).
#' @param ... Extra arguments passed to the underlying learner
#'   (e.g., super-parameters for certain model supported by their packages).
#'
#' @return A list. With following elements:
#' \itemize{
#'   \item model       A model object. Fitted model.
#'   \item model_type  A character string. Name labels for the models,
#'     one of "rf_fit","glm_fit","svm_fit"
#' }
#'
#' @examples
#' set.seed(123)
#' train_df <- readRDS(
#'   system.file("extdata", "train_df.rds", package="ckdPrediction")
#'   )
#' rf_fit <- model_train_ckd(train_df, type = 1)
#' rf_fit
#
#' glm_fit <- model_train_ckd(train_df, type = 2)
#' glm_fit
#
#' svm_fit <- model_train_ckd(train_df, type = 3)
#' svm_fit
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom randomForest randomForest importance
#' @importFrom glmnet cv.glmnet
#' @importFrom e1071 svm
#'
#' @export
model_train_ckd <- function(df, type = 1, ...) {
  x <- subset(df, select = -c(id, classification))
  y <- factor(df$classification, levels = c("notckd", "ckd"))

  if (type == 1) {
    model_fit  <- randomForest::randomForest(x = x, y = y, ...)
    model_type <- "rf_fit"
  } else if (type == 2) {
    yy <- ifelse(df$classification == "ckd", 1, 0)
    model_fit  <- glmnet::cv.glmnet(x = as.matrix(x), y = yy,
                                    family = "binomial", alpha = 0,
                                    standardize = TRUE, ...)
    model_type <- "glm_fit"
  } else if (type == 3) {
    p <- ncol(df)
    model_fit  <- e1071::svm(x = x, y = y, kernel = "radial",
                             probability = TRUE, cost = 1,
                             gamma = 1 / p, ...)
    model_type <- "svm_fit"
  } else {
    stop("Invalid Model Index!")
  }

  return(
    list(model = model_fit, model_type = model_type)
  )
}
