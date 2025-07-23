
cronbachs_alpha <- function(survey_responses, columns = NULL) {
    #'Calculates the internal consistency of a set of columns in a dataframe
    #' @param columns A vector of column names to calculate the internal consistency of
    if (is.null(columns)) {
        return(cronbach.alpha(survey_responses, na.rm = TRUE)$alpha)
    }
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

correlation_matrix <- function(responses) {
    #'Calculates the correlation matrix of a dataframe
    #' @param responses A dataframe of survey responses
    return(cor(responses, use="complete.obs", method="spearman"))
}

polychoric_correlation_matrix <- function(responses) {
    library(EFA.dimensions)
    #'Calculates the polychoric correlation matrix of a dataframe
    #' @param responses A dataframe of survey responses
    return(POLYCHORIC_R(responses))
}

visualise_correlation <- function(correlation_matrix) {
    library(corrplot)
    #'Visualises a correlation matrix
    #' @param correlation_matrix A matrix of correlation values
    corrplot(correlation_matrix, method="color")
}

calculate_sus_score_for_row <- function(survey_response) {
    #'Calculates the System Usability Scale (SUS) score for a single response
    #' @param survey_response A dataframe for a single survey response
    #' @return The SUS score for a single response
    odd_items <- survey_response[c("Q1_1", "Q1_3", "Q1_5")]
    even_items <- survey_response[c("Q1_2", "Q1_4")]
    sum_odd_items <- sum(odd_items, na.rm = TRUE) - 3
    sum_even_items <- 10 - sum(even_items, na.rm = TRUE)
    sus_score <- (sum_odd_items + sum_even_items) * 5
    return(round(sus_score, 2))
}

calculate_mean_sus_score <- function(survey_responses) {
    #'Calculates the mean System Usability Scale (SUS) score from a set of survey responses
    #' @param survey_responses A dataframe of survey responses
    #' @return The mean SUS score
    sus_scores <- c()
    for (i in 1:nrow(survey_responses)) {
        sus_scores[i] <- calculate_sus_score_for_row(survey_responses[i, ])
    }
    return(mean(sus_scores, na.rm = TRUE))
}

calculate_median_sus_score <- function(survey_responses) {
    #'Calculates the median System Usability Scale (SUS) score from a set of survey responses
    #' @param survey_responses A dataframe of survey responses
    #' @return The median SUS score
    sus_scores <- c()
    for (i in 1:nrow(survey_responses)) {
        sus_scores[i] <- calculate_sus_score_for_row(survey_responses[i, ])
    }
    return(median(sus_scores, na.rm = TRUE))
}

filter_by_school <- function(data, school) {
    source("filter_by_school.r")
    student_ids <- get_student_ids_from_school(school)
    return (data[data$student_id %in% student_ids, ])
}

get_school_breakdown <- function(data) {
    for (i in 1:5) {
        n_respondents_from_school = filter_by_school(survey_response_data, paste("School", i)) %>%
            nrow()
        school_counts[i] <- n_respondents_from_school
    }
    school_counts[6] <- nrow(survey_response_data[survey_response_data$student_id == "", ])
}