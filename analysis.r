
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

visualise_distribution <- function(survey_responses, column, title) {
    #'Visualises the distribution of a column in a dataframe
    #' @param column The name of the column to visualise
    if (length(column) > 1) {
        stop("The column parameter should be a single column name.")
    }
    if (missing(title)) {
        hist(survey_responses[, column], main=paste("Histogram of", column), xlab=column) #TODO: Should this be a bar chart? Also get names for questions here
    }
    else {
        hist(survey_responses[, column], main=title, xlab=column) #TODO: Should this be a bar chart? Also get names for questions here
    }
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


"Cluster analysis of participants"

"Work out summary stats for data, including:
 -Median/modal response for each question
 -Visualisation of distributions
 -Skewness and kurtosis of each question (or closeness to normal distribution)
 -Segregate for different demographic groups (school, year group, gender)"


"Functions to write

-remove_null_data([column]): Removes null data for a particular set of columns (should this be on a cell level or overall? And in place or not?)
-get_distribution(column): Gets distribution of column and visualises result of each one, with modal response and proximity to normal distribution written (could maybe be split up into two functions)
-get_individual_student_data(student): Gets all of the data of a particular student
-get_factor_loadings(): gets factor loadings, illustrating the values of the loadings, the visualisation of them, and the constituent variables that make them up.
-Have separate functions for visualising"