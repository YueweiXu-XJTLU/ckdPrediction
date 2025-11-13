#' Data Clean and Process for Chronic Kidney Disease (ckd) Dataset
#'
#' This function conducts manipulations to a raw dataset, including:
#'   1. Filling in empty and mess cells with NA;
#'   2. Dropping rows with too much missing values;
#'   3. Splitting dataset into train-dataset and test-dataset;
#'   4. Handling with NA with different strategies;
#' Return tidy objects for downstream modeling.
#'
#' @param file_name A character string. Path to raw CSV file.
#' @param row_threshold A numeric number. Row drop threshold for missing rate
#'   (0-1), rows with missing rate over threshold are dropped.
#' @param train_percentage A numeric number. Train fraction in (0,1),
#'   this percentage of random rows are extracted as train-dataset.
#'
#' @return A list. With following elements:
#' \itemize{
#'   \item train_df            A data.frame. Cleaned training data.
#'   \item test_df             A data.frame. Cleaned test data.
#'   \item row_missing_table   A table. Per-row missing level table.
#'   \item col_missing_table   A table. Per-col missing level table.
#' }
#'
#' @examples
#' set.seed(123)
#' res <- data_process_ckd(
#'   system.file("extdata", "kidney_disease.csv", package="ckdPrediction"),
#'   row_threshold = 0.3,
#'   train_percentage = 0.7
#'   )
#' head(res$train_df, 3)
#' head(res$test_df, 3)
#' head(res$row_missing_table, 3)
#' head(res$col_missing_table, 3)
#'
#' @seealso
#' data_split_ckd(), row_process_ckd(), col_process_ckd(),
#'   common_process_ckd()
#'
#' @family ckd-prediction-helpers
#'
#' @importFrom utils read.csv
#' @importFrom stats median
#'
#' @export
data_process_ckd <- function(file_name,
                                row_threshold = 0.3,
                                train_percentage = 0.7) {

  df <- utils::read.csv(file_name)

  df <- common_process_ckd(df)

  row_list <- row_process_ckd(df, threshold = row_threshold)
  row_missing_table <- row_list$table
  df <- row_list$df

  split_list <- data_split_ckd(df, train_percentage = train_percentage)
  train_df <- split_list$train_df
  test_df  <- split_list$test_df

  col_list <- col_process_ckd(df, train_df, test_df)
  train_df <- col_list$train_df
  test_df  <- col_list$test_df
  col_missing_table <- col_list$table

  return(
    list(
      train_df = train_df,
      test_df = test_df,
      row_missing_table = row_missing_table,
      col_missing_table = col_missing_table
      )
  )
}


#' Clean and Common Pre-process CKD Fields
#'
#' This function standardizes raw CKD data fields: replace empty/messy tokens
#' with `NA`, trim whitespace, remove tab characters, coerce numeric columns,
#' recode binary clinical variables to 0/1, and set `classification` as a
#' factor with `notckd` as reference.
#'
#' @param df A data.frame. Provide the raw input dataset read from CSV and
#'   contain columns such as `age`, `bp`, `sg`, `al`, `su`, and clinical flags.
#'
#' @return A data.frame. Return the cleaned dataset with harmonized types and
#'   encodings ready for downstream processing.
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' clean <- common_process_ckd(df)
#' str(clean)
#' }
#'
#' @seealso `data_process_ckd()`
#'
#' @family ckd-prediction-helpers-internal
#'
#' @keywords internal
common_process_ckd <- function(df) {
  df[df == ""] <- NA

  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x <- trimws(x)
      x <- gsub("\\t", "", x)
      x[x %in% c("", "?", "NA")] <- NA
    }
    x
  })

  num_cols <- c("age","bp","sg","al","su","bgr","bu","sc",
                "sod","pot","hemo","pcv","wc","rc")
  for (col in num_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  df$htn   <- ifelse(df$htn   == "no", 0L, ifelse(df$htn   == "yes", 1L, NA))
  df$dm    <- ifelse(df$dm    == "no", 0L, ifelse(df$dm    == "yes", 1L, NA))
  df$cad   <- ifelse(df$cad   == "no", 0L, ifelse(df$cad   == "yes", 1L, NA))
  df$appet <- ifelse(df$appet == "poor", 0L,
                     ifelse(df$appet == "good", 1L, NA))
  df$pe    <- ifelse(df$pe    == "no", 0L, ifelse(df$pe    == "yes", 1L, NA))
  df$ane   <- ifelse(df$ane   == "no", 0L, ifelse(df$ane   == "yes", 1L, NA))
  df$pcc   <- ifelse(df$pcc   == "notpresent", 0L,
                     ifelse(df$pcc == "present", 1L, NA))
  df$ba    <- ifelse(df$ba    == "notpresent", 0L,
                     ifelse(df$ba == "present", 1L, NA))
  df$rbc   <- ifelse(df$rbc   == "abnormal", 0L,
                     ifelse(df$rbc == "normal", 1L, NA))
  df$pc    <- ifelse(df$pc    == "abnormal", 0L,
                     ifelse(df$pc == "normal", 1L, NA))

  df$classification <- factor(df$classification,
                              levels = c("notckd", "ckd"))
  return(df)
}


#' Screen and Drop Rows by Missing Values (row-level QC)
#'
#' This function computes per-row missing values, sort rows by missing rate,
#' and drop rows whose missing rate exceeds the specified threshold.
#'
#' @param df A data.frame. Provide the cleaned dataset from the common step.
#' @param threshold A numeric number. Specify the row-level missing value cutoff
#'   in (0, 1) above which rows are removed.
#'
#' @return A list. Return row-filtered data and the per-row missingness table.
#' \itemize{
#'   \item df         A data.frame. Cleaned data after row filtering.
#'   \item table      A data.frame. Per-row missing summary (id, counts, rates).
#' }
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' out <- row_process_ckd(df = clean, threshold = 0.3)
#' head(out$table)
#' nrow(out$df)
#' }
#'
#' @seealso `data_process_ckd()`
#'
#' @keywords internal
#'
#' @family ckd-prediction-helpers-internal
row_process_ckd <- function(df, threshold = 0.3) {
  row_missing_table <- data.frame(
    row_id        = 1:nrow(df),
    missing_count = rowSums(is.na(df)),
    missing_rate  = rowSums(is.na(df)) / ncol(df)
  )
  row_missing_table <- row_missing_table[
    order(-row_missing_table$missing_rate), ]

  dirty_rows <- row_missing_table[
    row_missing_table$missing_count > (threshold * ncol(df)), ]
  df <- df[-as.vector(dirty_rows$row_id), ]

  return(list(df = df, table = row_missing_table))
}


#' Split Dataset into Training and Test Partitions
#'
#' This function samples row indices to create training and test splits with a
#' user-defined training fraction.
#'
#' @param df A data.frame. Provide the row-filtered dataset to be split.
#' @param train_percentage A numeric number. Specify the fraction in (0, 1) for
#'   the training set; the remainder is assigned to the test set.
#'
#' @return A list. Return two data partitions for model development.
#' \itemize{
#'   \item train_df          A data.frame. Training split of the input data.
#'   \item test_df           A data.frame. Test split of the input data.
#' }
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' sp <- data_split_ckd(df = filtered, train_percentage = 0.7)
#' str(sp$train_df)
#' str(sp$test_df)
#' }
#'
#' @seealso `data_process_ckd()`
#'
#' @keywords internal
#'
#' @family ckd-prediction-helpers-internal
data_split_ckd <- function(df, train_percentage = 0.7) {
  n  <- nrow(df)
  id <- sample.int(n, size = round(n * train_percentage), replace = FALSE)
  return(list(train_df = df[id, ], test_df = df[-id, ]))
}


#' Clean by Column Missing Value Levels (column-level QC)
#'
#' This function computes per-column missing values, categorize columns into
#' low/moderate/high levels, impute low-level with median/mode, impute
#' moderate-level by class-specific medians/modes using `classification`,
#' add `_missing` indicators for high-level columns and impute by class mode,
#' and apply train-derived statistics to the test set to avoid leakage.
#'
#' @param df A data.frame. Provide the row-filtered full dataset for reference.
#' @param train_df A data.frame. Provide the current training split before
#'   column-level imputation.
#' @param test_df A data.frame. Provide the current test split to receive
#'   train-derived imputations and missingness indicators.
#'
#' @return A list. Return train/test data and the per-column summary table.
#' \itemize{
#'   \item train_df   A data.frame. Training data after column-level imputation
#'         and high-missingness indicators.
#'   \item test_df    A data.frame. Test data imputed using train-derived
#'         statistics with aligned indicators.
#'   \item table      A data.frame. Per-column missing counts, rates, and level.
#' }
#'
#' @examples
#' \dontrun{
#' # just an example, this function is not exported by design.
#' out <- col_process_ckd(df = clean, train_df = tr, test_df = te)
#' head(out$table, 5)
#' str(out$train_df); str(out$test_df)
#' }
#'
#' @seealso `data_process_ckd()`
#'
#' @keywords internal
#'
#' @family ckd-prediction-helpers-internal
#'
#' @importFrom stats median
col_process_ckd <- function(df, train_df, test_df) {

  col_missing_table <- data.frame(
    col_id        = 1:ncol(df),
    missing_count = colSums(is.na(df)),
    missing_rate  = colSums(is.na(df)) / nrow(df),
    missing_level = NA_character_
  )

  for (i in 1:ncol(df)) {
    rate <- col_missing_table$missing_rate[i]
    col_missing_table$missing_level[i] <-
      if (rate < 0.05) "low" else if (rate < 0.2) "moderate" else "high"
  }
  col_missing_table <- col_missing_table[
    order(-col_missing_table$missing_rate), ]

  median_low  <- c("bu","bp","sc","age")
  mode_low    <- c("pcc","ba","htn","dm","cad","appet","pe","ane")
  median_mid  <- c("pot","sod","pcv","bgr","hemo","su","sg","al")
  mode_mid    <- c("pc")
  mode_high   <- c("rbc","rc","wc")

  calculate_mode <- function(x) {
    ux <- unique(x[!is.na(x)])
    return(ux[which.max(tabulate(match(x, ux)))])
  }

  for (nm in median_low) {
    train_df[[nm]][is.na(train_df[[nm]])] <-
      stats::median(train_df[[nm]], na.rm = TRUE)
  }
  for (nm in mode_low) {
    train_df[[nm]][is.na(train_df[[nm]])] <- calculate_mode(train_df[[nm]])
  }

  for (nm in median_mid) {
    m_ckd    <- stats::median(train_df[train_df$classification == "ckd", nm],
                              na.rm = TRUE)
    m_notckd <- stats::median(train_df[train_df$classification == "notckd", nm],
                              na.rm = TRUE)
    train_df[train_df$classification == "ckd" &
               is.na(train_df[, nm]), nm] <- m_ckd
    train_df[train_df$classification == "notckd" &
               is.na(train_df[, nm]), nm] <- m_notckd
  }

  for (nm in mode_mid) {
    mo_ckd    <- calculate_mode(
      train_df[train_df$classification == "ckd", nm]
      )
    mo_notckd <- calculate_mode(
      train_df[train_df$classification == "notckd", nm]
      )
    train_df[train_df$classification == "ckd" &
               is.na(train_df[, nm]), nm] <- mo_ckd
    train_df[train_df$classification == "notckd" &
               is.na(train_df[, nm]), nm] <- mo_notckd
  }

  for (nm in mode_high) {
    train_df[[paste0(nm, "_missing")]] <- ifelse(is.na(train_df[[nm]]), 1L, 0L)
    mo_ckd    <- calculate_mode(
      train_df[train_df$classification == "ckd", nm]
      )
    mo_notckd <- calculate_mode(
      train_df[train_df$classification == "notckd", nm]
      )
    train_df[train_df$classification == "ckd" &
               is.na(train_df[, nm]), nm] <- mo_ckd
    train_df[train_df$classification == "notckd" &
               is.na(train_df[, nm]), nm] <- mo_notckd
  }

  for (nm in mode_high) {
    test_df[[paste0(nm, "_missing")]] <- ifelse(is.na(test_df[[nm]]), 1L, 0L)
  }
  for (nm in c(median_low, median_mid)) {
    test_df[[nm]][is.na(test_df[[nm]])] <-
      stats::median(train_df[[nm]], na.rm = TRUE)
  }
  for (nm in c(mode_low, mode_mid, mode_high)) {
    test_df[[nm]][is.na(test_df[[nm]])] <- calculate_mode(train_df[[nm]])
  }

  return(
    list(train_df = train_df, test_df = test_df, table = col_missing_table)
  )
}
