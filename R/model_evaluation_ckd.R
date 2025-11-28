#' Full Evaluation for Single Model on Given Dataset
#'
#' This function predicts the data using a given model,
#' and evaluating the model at a certain threshold for classification.
#' Metrics including:
#'   1. accuracy;
#'   2. recall;
#'   3. false positive rate (FPR);
#'   4. confusion matrix;
#'   5. PRC curve with its metrics, such as area under curve (AUC);
#'   6. ROC curve with its metrics, such as area under curve (AUC);
#'   7. a recommended threshold, decided by highest F1-Score.
#'
#' @param df A data.frame. Cleaned testing data returned by
#'   `data_process_ckd()`.
#' @param model A list containing model returned by `model_train_ckd()`.
#' @param threshold Numeric threshold used for model evaluation, (0,1).
#'
#' @return A list. With following elements:
#' \itemize{
#'   \item metrics         A list. Contain accuracy; precision; recall; fpr;
#'   and a confusion-matrix
#'   \item curves          A list. Contain a curve's plot and metrics
#'   \item best_threshold  A list. Contain info and value of
#'   a recommended threshold
#' }
#'
#' @examples
#' set.seed(123)
#' test_df <- readRDS(
#'   system.file("extdata", "test_df.rds", package="ckdPrediction")
#'   )
#' svm_fit <- readRDS(
#'   system.file("extdata", "svm_fit.rds", package="ckdPrediction")
#'   )
#' ev <- model_evaluation_ckd(test_df, svm_fit, threshold = 0.5)
#' ev$metrics$accuracy
#' ev$metrics$cm
#' ev$curves$plot
#' ev$curves$auc
#' ev$best_threshold$info
#' ev$best_threshold$value
#'
#' @seealso predict_ckd(), model_train_ckd(), metrics_ckd(), curves_ckd(),
#'   threshold_optimization_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom precrec evalmod auc
#' @importFrom ggplot2 ggplot aes
#'
#' @export
model_evaluation_ckd <- function(df, model, threshold = 0.5) {
  prob_test <- predict_ckd(df, model)
  metrics   <- metrics_ckd(df, prob_test, threshold)

  curves_raw <- curves_ckd(df, prob_test)
  df_curves <- as.data.frame(curves_raw)
  df_curves$type_label <- factor(
    df_curves$type,
    levels = c("ROC", "PRC"),
    labels = c("ROC (Recall vs. FPR)", "PRC (Precision vs. Recall)")
  )
  plot <- ggplot2::ggplot(df_curves, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(size = 1, colour = "#71b7ed") +
    ggplot2::geom_abline(
      data        = subset(df_curves, type == "ROC")[1, , drop = FALSE],
      ggplot2::aes(slope = 1, intercept = 0),
      linetype    = "dashed",
      colour      = "#d3d3d3",
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~ type_label, nrow = 1, scales = "free") +
    ggplot2::labs(
      title    = "Receiver Operating Characteristic curve & Precision-Recall curve",
      x        = NULL,
      y        = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text             = ggplot2::element_text(face = "bold"),
      plot.title       = ggplot2::element_text(hjust = 0.5, size = 13),
      strip.background = ggplot2::element_rect(fill = "white", colour = NA),
      strip.text       = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_line(size = 0.2, colour = "#e5e7eb"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "none"
    )
  auc  <- precrec::auc(curves_raw)
  curves <- list(plot = plot, auc = auc)

  best_threshold <- threshold_optimization_ckd(df, prob_test, curves_raw)

  return(
    list(metrics = metrics, curves = curves, best_threshold = best_threshold)
  )
}


#' Compute Confusion-Matrix Metrics at a Given Threshold
#'
#' This function computes accuracy, precision, recall,
#' false positive rate (FPR), and return a confusion matrix given
#' class probabilities and a decision threshold.
#'
#' @param df A data.frame. Provide cleaned testing data with column
#'   `classification`.
#' @param prob_test A data.frame. Provide two probability columns
#'   `notckd` and `ckd`.
#' @param threshold A numeric number. Specify decision threshold for model
#'   in (0, 1).
#'
#' @return A list. With following elements:
#' \itemize{
#'   \item accuracy   A numeric number. Compute overall accuracy.
#'   \item precision  A numeric number. Compute positive predictive value.
#'   \item recall     A numeric number. Compute sensitivity (TPR).
#'   \item fpr        A numeric number. Compute false positive rate.
#'   \item cm         A table. Provide confusion matrix with
#'     levels `notckd`/`ckd`.
#' }
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' pr <- predict_ckd(test_df, rf_fit)
#' out <- metrics_ckd(test_df, pr, threshold = 0.5)
#' out$cm; out$accuracy
#' }
#'
#' @seealso `model_evaluation_ckd()`
#'
#' @family ckd-evaluation-helpers-internal
#'
#' @keywords internal
metrics_ckd <- function(df, prob_test, threshold = 0.5) {

  pred_bin <- ifelse(prob_test[, 2] <= threshold, "notckd", "ckd")

  cm <- table(
    Predicted = factor(pred_bin, levels = c("notckd", "ckd")),
    Actual    = factor(df$classification, levels = c("notckd", "ckd"))
  )

  TP <- cm["ckd", "ckd"]
  TN <- cm["notckd", "notckd"]
  FP <- cm["ckd", "notckd"]
  FN <- cm["notckd", "ckd"]

  accuracy  <- (TP + TN) / (TP + TN + FP + FN)
  precision <-  TP / (TP + FP)
  recall    <-  TP / (TP + FN)
  fpr       <-  FP / (FP + TN)

  return(
    list(accuracy = accuracy,
         precision = precision,
         recall = recall,
         fpr = fpr,
         cm = cm)
  )
}


#' Build ROC/PR Curves from Probabilities
#'
#' This function constructs ROC and PRC data for binary classification
#' using `precrec`, given true labels and predicted probabilities.
#'
#' @param df A data.frame. Provide cleaned testing data with column
#'   `classification`.
#' @param prob_test A data.frame. Provide two probability columns
#'   `notckd` and `ckd`.
#'
#' @return A `precrec` object. Provide curve data usable by `ggplot2::autoplot`
#'   and metrics obtainable via `precrec::auc`.
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' pr  <- predict_ckd(test_df, glm_fit)
#' evc <- curves_ckd(test_df, pr)
#' ggplot2::autoplot(evc)
#' precrec::auc(evc)
#' }
#'
#' @seealso `model_evaluation_ckd()`
#'
#' @family ckd-evaluation-helpers-internal
#'
#' @importFrom precrec evalmod
#'
#' @keywords internal
curves_ckd <- function(df, prob_test) {
  return(
    precrec::evalmod(scores = prob_test[, "ckd"],
                     labels = df$classification,
                     posclass = "ckd")
  )
}


#' Optimize Classification Threshold via F1 on PR Curve
#'
#' This function scans candidate thresholds, compute precision/recall/FPR,
#' identify the PR point with maximum F1-score, and return the recommended
#' cutoff together with its metric summary.
#'
#' @param df A data.frame. Provide cleaned testing data with column
#'   `classification`.
#' @param prob_test A data.frame. Provide two probability columns
#'   `notckd` and `ckd`.
#' @param curves A `precrec` object. Provide PR curve used to locate
#'   the max-F1 point.
#'
#' @return A list. With following elements:
#' \itemize{
#'   \item info  A data.frame. Provide the selected cutoff and its
#'     precision/recall/FPR.
#'   \item value A numeric number. Provide the recommended threshold in (0, 1).
#' }
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' pr  <- predict_ckd(test_df, svm_fit)
#' cv  <- curves_ckd(test_df, pr)
#' th  <- threshold_optimization_ckd(test_df, pr, cv)
#' th$value; th$info
#' }
#'
#' @seealso `model_evaluation_ckd()`
#'
#' @family ckd-evaluation-helpers-internal
#'
#' @keywords internal
threshold_optimization_ckd <- function(df, prob_test, curves) {
  n <- 1000
  th <- seq(0.005, 0.995, length.out = n)

  mat <- data.frame(precision = rep(NA_real_, n),
                    recall    = rep(NA_real_, n),
                    FPR       = rep(NA_real_, n))

  for (i in 1:n) {
    m <- metrics_ckd(df, prob_test, threshold = th[i])
    mat[i, ] <- unlist(m[-c(1, 5)])
  }

  pr <- data.frame(x = curves$prcs[[1]]$x,
                   y = curves$prcs[[1]]$y)
  pr$F1 <- (2 * pr$x * pr$y) / (pr$x + pr$y)

  tgt <- pr[which.max(pr$F1), ]
  idx <- which.min(abs(mat$recall - tgt$x) + abs(mat$precision - tgt$y))
  best_cutoff <- th[idx]

  return(
    list(info = cbind(cutoff = best_cutoff, mat[idx, ]),
         value = best_cutoff)
  )
}
