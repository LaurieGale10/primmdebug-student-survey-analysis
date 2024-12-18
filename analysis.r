library(ltm)

internal_consistency <- function(columns) {
    #'Calculates the internal consistency of a set of columns in a dataframe
    #' @param columns A vector of column names to calculate the internal consistency of
    return(cronbach.alpha(survey_response_data[, columns], na.rm = TRUE))
}

quantify_distribution <- function(column) {
    library(e1071)
    library(nortest)

    quantify_distribution <- function(column) {
        #'Calculates and returns the skewness, kurtosis, and normality of the distribution of a column in a dataframe
        #' @param column The name of the column to analyze
        data <- survey_response_data[, column]
        skewness_value <- skewness(data, na.rm = TRUE)
        kurtosis_value <- kurtosis(data, na.rm = TRUE)
        normality_test <- ad.test(data) #TODO: Check out which test to use here.
        return(list(
            skewness = skewness_value,
            kurtosis = kurtosis_value,
            normality_p_value = normality_test$p.value
        ))
    }
}

visualise_distribution <- function(column) {
    #'Visualises the distribution of a column in a dataframe
    #' @param column The name of the column to visualise
    if (length(column) > 1) {
        stop("The column parameter should be a single column name.")
    }
    hist(survey_response_data[, column], main = column) #TODO: Should this be a bar chart? In order to get other distribution stats I'll need to read up on them (skewness/kurtoisis/normality)
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

#Load data into dataframe
survey_response_data <- read.table("pilot_survey_responses.csv", header=TRUE, sep=",")

#Creates vectors containing columns names for the whole dataset and each variable
all_quant_responses <- names(survey_response_data)[sapply(survey_response_data, is.numeric)]
primmdebug_usability_questions <- c("Q1_1","Q1_2","Q1_3","Q1_4","Q1_5")
primmdebug_utility_questions <- c("Q2_1","Q2_2","Q2_3","Q2_4","Q2_5","Q2_6")
sifft_utility_questions <- c("Pilot3","Q4","Q5")

"Check internal consistency of data"
internal_consistency(all_quant_responses)
internal_consistency(primmdebug_usability_questions)
internal_consistency(primmdebug_utility_questions)
internal_consistency(sifft_utility_questions)

visualise_correlation(spearman_correlation(all_quant_responses))

"Functions to write

-remove_null_data([column]): Removes null data for a particular set of columns (should this be on a cell level or overall? And in place or not?)
-get_distribution(column): Gets distribution of column and visualises result of each one, with modal response and proximity to normal distribution written (could maybe be split up into two functions)
-get_individual_student_data(student): Gets all of the data of a particular student
-get_factor_loadings(): gets factor loadings, illustrating the values of the loadings, the visualisation of them, and the constituent variables that make them up.
-Have separate functions for visualising"