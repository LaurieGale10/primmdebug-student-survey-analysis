gender_counts <- data.frame(
    gender = c("Female", "Male", "Non-binary", "Other", "Prefer not to say"),
    count = c(0, 0, 0, 0, 0)
)

get_gender_counts <- function() {
    return(gender_counts)
}

get_total_student_count <- function() {
    total_students <- sum(gender_counts$count)
    return(total_students)
}

year_group_split <- data.frame(
    year_group = c("Year 7", "Year 8", "Year 9", "Year 10", "Year 11"),
    count = c(0, 0, 0, 0, 0)
)

get_year_group_split <- function() {
    return(year_group_split)
}

school_split <- data.frame(
    school = c("School A", "School B", "School C", "School D", "School E"),
    count = c(0, 0, 0, 0, 0)
)

get_school_split <- function() {
    return(school_split)
}

question_labels <- c(
    Q1 = "How much do you agree or disagree with these sentences about the PRIMMDebug website?",
    Q1_1 = "I thought the website was easy to use.", 
    Q1_2 = "I found the website too complicated.", 
    Q1_3 = "I think most people would learn to use the website very quickly.", 
    Q1_4 = "I needed to learn a lot of things before I could get going  with the website.", 
    Q1_5 = "I felt very confident using the website.",
    Q2 = "How much do you agree or disagree with these sentences about the restrictive factors of PRIMMDebug?",
    Q2_1 = "Having to write out my thoughts before moving on to some of the stages.", 
    Q2_2 = "Only being able to run the program at certain stages.", 
    Q2_3 = "Having to select the line number of the error in the 'Find the Error' stage before moving on.", 
    Q2_4 = "Only being able to edit the program at certain stages.", 
    Q2_5 = "Being given test cases for some of the challenges.",
    Q3 = "How much do you agree or disagree with these sentences about the PRIMMDebug challenges?",
    Q3_1 = "The computer programs used in the challenges were at a good level for me.", 
    Q3_2 = "The errors in the challenges gave me practice with errors I get in my own programs.",
    Q4 = "What did you think about the difficulty of the PRIMMDebug challenges you tried?",
    Q5 = "How helpful was the SIFFT process for teaching you to find and fix errors in computer programs?", 
    Q6 = "Since being taught it, how much have you used SIFFT when solving errors in your own programs?"
)

get_question_labels <- function() {
    return(question_labels)
}