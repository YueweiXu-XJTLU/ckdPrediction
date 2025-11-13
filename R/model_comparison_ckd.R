#' Compare Performance Metrics of Multiple Models on Same Testing Set
#'
#' This function generates a bar plot comparing metrics among models including:
#'   1. Accuracy;
#'   2. Precision;
#'   3. Recall;
#'   4. Specificity;
#'   5. F1-score;
#'   6. AUC ROC;
#'   7. AUC PRC;
#'  under a same threshold for prediction classification.
#'  In order to compare model performance under a desired threshold.
#'
#' @param model_list A list. Contain trained models from `model_train_ckd()`.
#' @param df A data.frame. Cleaned testing data returned by
#'   `data_process_ckd()`.
#' @param threshold A numeric number. Specify decision threshold for model
#'   in (0, 1).
#'
#' @return A ggplot object. A bar chart to comparison performances among models.
#'
#' @examples
#' set.seed(123)
#' test_df <- readRDS(
#'   system.file("extdata", "test_df.rds", package="ckdPrediction")
#'   )
#' rf_fit <- readRDS(
#'   system.file("extdata", "rf_fit.rds", package="ckdPrediction")
#'   )
#' glm_fit <- readRDS(
#'   system.file("extdata", "glm_fit.rds", package="ckdPrediction")
#' )
#' svm_fit <- readRDS(
#'   system.file("extdata", "svm_fit.rds", package="ckdPrediction")
#' )
#' model_list <- list(rf_fit, glm_fit, svm_fit)
#' plot <- model_comparison_ckd(model_list, test_df)
#' plot
#'
#' @seealso model_evaluation_ckd(), predict_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @import ggplot2
#' @importFrom precrec evalmod auc
#'
#' @export
model_comparison_ckd <- function(model_list, df, threshold = 0.5) {

  perf_list <- list()

  for (md in model_list) {
    name <- md$model_type
    ev   <- model_evaluation_ckd(df, md, threshold = threshold)
    m    <- ev$metrics
    auc_tab <- ev$curves$auc

    auc_roc <- as.numeric(
      auc_tab[auc_tab$curvetypes == "ROC", "aucs"])
    auc_prc <- as.numeric(
      auc_tab[auc_tab$curvetypes == "PRC", "aucs"])

    f1 <- 2 * m$precision * m$recall / (m$precision + m$recall)

    perf_list[[name]] <- c(
      Recall      = m$recall,
      Precision   = m$precision,
      FPR         = m$fpr,
      Accuracy    = m$accuracy,
      Specificity = 1 - m$fpr,
      `F1-score`  = f1,
      `AUC ROC`   = auc_roc,
      `AUC PRC`   = auc_prc
    )
  }

  perf_mat <- do.call(rbind, perf_list)
  df_plot  <- data.frame(Model = rownames(perf_mat),
                         perf_mat, row.names = NULL,
                         check.names = FALSE)

  metrics_order <- c("Accuracy","Precision","Recall","Specificity",
                     "F1-score","AUC ROC","AUC PRC")

  df_long <- stats::reshape(df_plot,
                            direction = "long",
                            varying   = metrics_order,
                            v.names   = "Value",
                            timevar   = "Metric",
                            times     = metrics_order)

  df_long$Metric <- factor(df_long$Metric, levels = metrics_order)
  model_levels   <- names(perf_list)
  df_long$Model  <- factor(df_long$Model, levels = model_levels)

  legend_labels <- c(
    rf_fit  = "Random Forest",
    glm_fit = "(Ridge) Logistic Regression",
    svm_fit = "Support Vector Machine"
  )

  ggplot(df_long, aes(x = Metric, y = Value,
                                        fill = Model)) +
    geom_col(position = position_dodge(width = 0.8),
                      width = 0.7) +
    theme_bw() +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_manual(
      values = stats::setNames(c("#0096C7", "#023E8A", "#5D3FD3")
                               [seq_along(model_levels)], model_levels),
      breaks = model_levels,
      labels = legend_labels[model_levels]
    ) +
    labs(title = paste0(
      "Model Performance Comparison (threshold = ",
      threshold,
      ")"
      ),
                  x = NULL, y = NULL) +
    theme(axis.text.x =
                     element_text(angle = 30, hjust = 1),
                   plot.title =
                     element_text(hjust = 0.5, face = "bold"))
}
