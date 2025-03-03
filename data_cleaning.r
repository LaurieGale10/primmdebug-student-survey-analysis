get_survey_data_labelled <- function() {
    survey_data_labelled <- read.csv("survey_responses/survey_responses_labelled.csv")
    return(survey_data_labelled)
}

# Creates and returns a dataframe represent the survey data with numerical values.
# This had to be created due to a glitch in the way Qualtrics saved Likert scale values.
get_survey_data_numerical <- function() {
    survey_data_labelled <- get_survey_data_labelled()

    agreement_likert <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
    helpfulness_likert <- c("Very unhelpful", "Slightly unhelpful", "Slightly helpful", "Very helpful")
    difficulty_likert <- c("Far too challenging", "A little too challenging",  "A good level of difficulty for me", "A little too easy", "Far too easy")
    regularity_likert <- c("Not at all", "I barely use it", "I sometimes use it", "I use it every time I get an error")

    # Convert the labelled data to numerical data using the likert scales defined above
    survey_data_numeric <- survey_data_labelled %>%
        mutate(across(starts_with("Q1_") | starts_with("Q3_"), ~as.numeric(factor(., levels = agreement_likert, ordered = TRUE))),
        across(starts_with("Q2_") | starts_with("Q5"), ~as.numeric(factor(., levels = helpfulness_likert, ordered = TRUE))),
        across(Q4, ~as.numeric(factor(., levels = difficulty_likert, ordered = TRUE))),
        across(Q6, ~as.numeric(factor(., levels = regularity_likert, ordered = TRUE))))
        
    write.csv(survey_data_numeric, "survey_responses/survey_responses_numeric.csv", row.names = FALSE)
    return(survey_data_numeric)
}

get_survey_data_numerical()