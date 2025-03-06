
internal_consistency <- function(survey_responses, columns) {
    #'Calculates the internal consistency of a set of columns in a dataframe
    #' @param columns A vector of column names to calculate the internal consistency of
    return(cronbach.alpha(survey_responses[, columns], na.rm = TRUE)$alpha)
}

quantify_distribution <- function(survey_responses, column) {
    library(e1071)
    library(nortest)

    #'Calculates and returns the skewness, kurtosis, and normality of the distribution of a column in a dataframe
    #' @param column The name of the column to analyze
    data <- survey_responses[, column]
    skewness_value <- skewness(data, na.rm = TRUE)
    kurtosis_value <- kurtosis(data, na.rm = TRUE)
    normality_test <- ad.test(data) #TODO: Check out which test to use here.
    return(list(
        skewness = skewness_value,
        kurtosis = kurtosis_value,
        normality_p_value = normality_test$p.value
    ))
}


spearman_correlation <-function(columns1, columns2 = NULL) {
    #'Calculates the Spearman correlation between two sets of columns
    #' @param columns1 A vector of column names to calculate the correlation of
    #' @param columns2 A vector of column names to calculate the correlation of. If this is not provided, correlations will be calculated just for columns1
    #' @return A matrix of the correlation values
    if (is.null(columns2)) {
        columns2 = columns1
    }
    return(matrix(cor(survey_response_data[, columns1], survey_response_data[, columns2], use="complete.obs", method="spearman"))) #Currently not printing out the right thing
}

visualise_correlation <- function(correlation_matrix) {
    library(corrplot)
    #'Visualises a correlation matrix
    #' @param correlation_matrix A matrix of correlation values
    corrplot(correlation_matrix, method="color")
}