#' Plot Feature Importance for rf / glm / svm Models
#'
#' This function computes and visualizes feature importance for models.
#' Each model uses specific interpretation method for feature importance:
#'   1. Random Forest: MeanDecreaseGini;
#'   2. (Ridge) Logistic Regression: |beta_j| * sd(x_j)
#'   3. Support Vector Machine: permutation-based delta-Accuracy.
#'
#' @param df A data.frame. A dataset returned by `data_process_ckd()`,
#'   (training/testing/full data are all available).
#' @param model A list. Contain model returned by `model_train_ckd()`.
#'
#' @return A ggplot2 object. A feature importance plot.
#'
#' @examples
#' set.seed(123)
#' train_df <- readRDS(
#'   system.file("extdata", "train_df.rds", package="ckdPrediction")
#'   )
#' svm_fit <- readRDS(
#'   system.file("extdata", "svm_fit.rds", package="ckdPrediction")
#'   )
#' rf_importance_plot <- model_interpretation_importance_ckd(train_df, svm_fit)
#' rf_importance_plot
#'
#' @seealso model_train_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom stats reorder sd
#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_line
#' @importFrom ggplot2 scale_color_manual theme element_text labs
#'
#' @export
model_interpretation_importance_ckd <- function(df, model) {

  feature_labels <- c(
    age="Age", bp="Blood Pressure", sg="Specific Gravity", al="Albumin",
    su="Sugar", rbc="Red Blood Cells", rbc_missing="Red Blood Cells (Missing)",
    pc="Pus Cell", pcc="Pus Cell Clumps", ba="Bacteria",
    bgr="Blood Glucose Random", bu="Blood Urea", sc="Serum Creatinine",
    sod="Sodium", pot="Potassium", hemo="Hemoglobin", pcv="Packed Cell Volume",
    wc="White Blood Cell Count", wc_missing="White Blood Cell Count (Missing)",
    rc="Red Blood Cell Count", rc_missing="Red Blood Cell Count (Missing)",
    htn="Hypertension", dm="Diabetes Mellitus", cad="Coronary Artery Disease",
    appet="Appetite", pe="Pedal Edema", ane="Anemia"
  )

  if (model$model_type == "rf_fit") {
    imp <- randomForest::importance(model$model)
    df <- data.frame(Feature = rownames(imp), Importance = imp[, 1])
    title <- "Feature Importance: Random Forest"
  } else if (model$model_type == "glm_fit") {
    X <- stats::model.matrix(classification ~ . - id, data = df)[, -1]
    sds <- apply(X, 2, sd)
    beta <- as.numeric(stats::coef(model$model, s = "lambda.min"))[-1]
    df <- data.frame(Feature = colnames(X), Importance = abs(beta * sds))
    title <- "Feature Importance: Logistic Regression"
  } else if (model$model_type == "svm_fit") {
    X <- subset(df, select = -c(id, classification))
    y <- factor(df$classification, levels = c("notckd","ckd"))
    pred_class <- function(object, newdata, thr = 0.5) {
      p <- attr(
        predict(
          object,
          newdata,
          probability = TRUE),
        "probabilities"
        )[, "ckd"]
      factor(ifelse(p >= thr, "ckd", "notckd"), levels = c("notckd","ckd"))
    }
    base_acc <- mean(pred_class(model$model, X) == y)
    set.seed(123)
    imp <- sapply(seq_len(ncol(X)), function(i) {
      acc <- replicate(30, {
        Xp <- X; Xp[[i]] <- sample(Xp[[i]])
        mean(pred_class(model$model, Xp) == y)
      })
      base_acc - mean(acc)
    })
    df <- data.frame(Feature = colnames(X), Importance = imp)
    title <-
      "Feature Importance: Support Vector Machine"
  } else {
    stop("Invalid Model Input!")
  }

  df$Feature_Full <- ifelse(!is.na(feature_labels[df$Feature]),
                            feature_labels[df$Feature], df$Feature)

  plot_obj <- ggplot(df, aes(x = reorder(Feature_Full, Importance),
                 y = Importance, fill = Importance)) +
    geom_col() +
    coord_flip() +
    theme_bw() +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
    labs(title = title, x = "Feature", y = "Importance") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold", size = 12),
      axis.text  = element_text(face = "bold", size = 9),
      legend.position = "none"
    )

  return(plot_obj)
}


#' Plot SHAP Explanations for Models
#'
#' This function calculates SHAP values, and return a plot
#' in one of the three types:
#'   type = 1: SHAP plot for all features and all samples;
#'   type = 2: SHAP plot for one specific features and all samples
#'     (need input of `visualization_feature`);
#'   type = 3: SHAP plot for all features and one specific sample
#'     (need input of `visualization_row`).
#'
#' @param train_df A data frame. Cleaned training data returned by
#'   `data_process_ckd()`.
#' @param test_df  A data frame. Cleaned testing data returned by
#'   `data_process_ckd()`.
#' @param model A list. Contain model returned by `model_train_ckd()`.
#' @param visualization_type A numeric number. Represent the plot type:
#'   type = 1: SHAP plot (beeswarm) for all features and all samples;
#'   type = 2: SHAP plot (dependence) for one specific features and all samples
#'     (need input of `visualization_feature`);
#'   type = 3: SHAP plot (waterfall) for all features and one specific sample
#'     (need input of `visualization_row`).
#' @param visualization_feature A numeric number. Represent a single column
#'   drawn in the plot.
#' @param visualization_row A numeric number. Represent a single row id drawn
#'   in the plot.
#'
#' @return A ggplot2 object. A SHAP plot.
#'
#' @examples
#' # This example is time & resource consuming, please wait for a short while.
#' set.seed(123)
#' train_df <- readRDS(
#'   system.file("extdata", "train_df.rds", package="ckdPrediction")
#'   )
#' test_df <- readRDS(
#'   system.file("extdata", "test_df.rds", package="ckdPrediction")
#'   )
#' svm_fit <- readRDS(
#'   system.file("extdata", "svm_fit.rds", package="ckdPrediction")
#'   )
#' plot_beeswarm <- model_interpretation_shap_ckd(
#'   train_df, test_df, svm_fit, visualization_type = 1
#' )
#' plot_beeswarm
#'
#' plot_dependence <- model_interpretation_shap_ckd(
#'   train_df, test_df, svm_fit, visualization_type = 2,
#'   visualization_feature = "sc"
#' )
#' plot_dependence
#'
#' plot_waterfall <- model_interpretation_shap_ckd(
#'   train_df, test_df, svm_fit, visualization_type = 3,
#'   visualization_row = 1
#' )
#' plot_waterfall
#'
#' @seealso calculate_shap_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom shapviz sv_importance sv_dependence sv_waterfall
#' @importFrom stats predict model.matrix
#'
#' @export
model_interpretation_shap_ckd <- function(train_df,
                                          test_df,
                                          model,
                                          visualization_type = 1,
                                          visualization_feature = "sc",
                                          visualization_row = 1
                                          ) {
  feature_labels <- c(
    age="Age", bp="Blood Pressure", sg="Specific Gravity", al="Albumin",
    su="Sugar", rbc="Red Blood Cells", rbc_missing="Red Blood Cells (Missing)",
    pc="Pus Cell", pcc="Pus Cell Clumps", ba="Bacteria",
    bgr="Blood Glucose Random", bu="Blood Urea", sc="Serum Creatinine",
    sod="Sodium", pot="Potassium", hemo="Hemoglobin", pcv="Packed Cell Volume",
    wc="White Blood Cell Count", wc_missing="White Blood Cell Count (Missing)",
    rc="Red Blood Cell Count", rc_missing="Red Blood Cell Count (Missing)",
    htn="Hypertension", dm="Diabetes Mellitus", cad="Coronary Artery Disease",
    appet="Appetite", pe="Pedal Edema", ane="Anemia"
  )

  sv <- calculate_shap_ckd(
    train_df = train_df,
    test_df  = test_df,
    model    = model
  )

  if (visualization_type == 1) {
    plot <- sv_importance(
      sv,
      kind = "beeswarm",
      show_numbers = TRUE
      )
  } else if (visualization_type == 2) {
    plot <- sv_dependence(
      sv,
      feature_labels[visualization_feature],
      color_var = feature_labels[visualization_feature]
      )
  } else if (visualization_type == 3) {
    plot <- sv_waterfall(
      sv,
      row_id = visualization_row
      )
  } else {
    stop("Invalid Visualiation Type!")
  }

  return(plot)
}


#' Calculate SHAP Contributions for Models
#'
#' This function computes SHAP-like explanations for CKD classifiers.
#' Map raw feature names to pretty clinical labels and return a `shapviz`
#' object for downstream plotting.
#'
#' @param train_df A data.frame. Provide cleaned training data returned by
#'   `data_process_ckd()`.
#' @param test_df A data.frame. Provide cleaned testing data for which to
#'   compute SHAP values.
#' @param model A list. Contain a fitted model from `model_train_ckd()`
#'   with fields "model" and "model_type"
#'   in "rf_fit"/"glm_fit"/"svm_fit".
#'
#' @return A `shapviz` object. Represent per-feature contributions (S) and the
#'   corresponding feature matrix (X) with human-readable labels applied.
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' sv <- calculate_shap_ckd(train_df = train_df,
#'                            test_df  = test_df,
#'                            model    = rf_fit)
#' shapviz::sv_importance(sv, kind = "beeswarm", show_numbers = TRUE)
#' }
#'
#' @seealso `model_interpretation_shap_ckd()`
#'
#' @keywords internal
#'
#' @family ckd-prediction-helpers-internal
#'
#' @importFrom stats predict model.matrix
#' @importFrom fastshap explain
#' @importFrom shapviz shapviz
#' @importFrom utils capture.output
calculate_shap_ckd <- function(train_df, test_df, model) {
  feature_labels <- c(
    age="Age", bp="Blood Pressure", sg="Specific Gravity", al="Albumin",
    su="Sugar", rbc="Red Blood Cells", rbc_missing="Red Blood Cells (Missing)",
    pc="Pus Cell", pcc="Pus Cell Clumps", ba="Bacteria",
    bgr="Blood Glucose Random", bu="Blood Urea", sc="Serum Creatinine",
    sod="Sodium", pot="Potassium", hemo="Hemoglobin", pcv="Packed Cell Volume",
    wc="White Blood Cell Count", wc_missing="White Blood Cell Count (Missing)",
    rc="Red Blood Cell Count", rc_missing="Red Blood Cell Count (Missing)",
    htn="Hypertension", dm="Diabetes Mellitus", cad="Coronary Artery Disease",
    appet="Appetite", pe="Pedal Edema", ane="Anemia"
  )

  if (model$model_type == "rf_fit") {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("Package 'randomForest' is required but is not installed.")
    }

    xnames <- setdiff(names(train_df), c("classification", "id"))
    X <- test_df[, xnames, drop = FALSE]
    for (nm in xnames) {
      if (is.factor(train_df[[nm]])) {
        X[[nm]] <- factor(X[[nm]], levels = levels(train_df[[nm]]))
      } else if (is.character(train_df[[nm]])) {
        X[[nm]] <- as.character(X[[nm]])
      } else {
        X[[nm]] <- as.numeric(as.character(X[[nm]]))
      }
    }

    pred_fun <- function(object, newdata) {
      newdata <- as.data.frame(newdata)
      newdata <- newdata[, xnames, drop = FALSE]
      for (nm in xnames) {
        if (is.factor(train_df[[nm]])) {
          newdata[[nm]] <- factor(
            newdata[[nm]],
            levels = levels(train_df[[nm]])
          )
        } else if (is.character(train_df[[nm]])) {
          newdata[[nm]] <- as.character(newdata[[nm]])
        } else {
          newdata[[nm]] <- as.numeric(as.character(newdata[[nm]]))
        }
      }
      p <- stats::predict(
        object,
        newdata = newdata,
        type = "prob"
      )[ , "ckd"]
      return(as.numeric(p))
    }

    sh <- fastshap::explain(
      object       = model$model,
      X            = X,
      pred_wrapper = pred_fun,
      nsim         = 50
    )
    sv <- shapviz::shapviz(sh, X = X)
  } else if (model$model_type == "glm_fit") {
    pred_fun <- function(object, newdata) {
      as.numeric(stats::predict(object,
                                newx = as.matrix(newdata),
                                s = "lambda.min",
                                type = "response"))
    }
    X <- stats::model.matrix(
      classification ~ . - id, data = test_df
    )[, -1, drop = FALSE]
    sh <- fastshap::explain(
      object = model$model,
      X = as.data.frame(X),
      pred_wrapper = pred_fun,
      nsim = 100
    )
    sv <- shapviz::shapviz(sh, X = as.data.frame(X))
  } else if (model$model_type == "svm_fit") {
    xnames <- setdiff(names(train_df), c("classification", "id"))
    xnames <- intersect(xnames, names(test_df))
    pred_fun <- function(object, newdata) {
      attr(stats::predict(object, newdata, probability = TRUE),
           "probabilities")[, "ckd"]
    }
    X <- test_df[, xnames, drop = FALSE]
    sh <- fastshap::explain(
      object = model$model,
      X = X,
      pred_wrapper = pred_fun,
      nsim = 100
    )
    sv <- shapviz::shapviz(sh, X = X)
  } else {
    stop("Invalid Model Type!")
  }

  existing <- colnames(sv$S)
  mapped   <- feature_labels[existing]
  colnames(sv$S) <- ifelse(!is.na(mapped), mapped, existing)
  colnames(sv$X) <- ifelse(!is.na(mapped), mapped, existing)

  return(sv)
}
