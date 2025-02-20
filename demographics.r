gender_counts <- list(
    "female" = 0,
    "male" = 0,
    "non-binary" = 0,
    "other" = 0,
    "prefer not to say" = 0
)

get_gender_counts <- function() {
    return(gender_counts)
}

get_total_student_count <- function() {
    total_students = 0
    for (count in gender_counts) {
        total_students = total_students + count
    }
    return(total_students)
}

year_group_split <- list(
    "Year 7" = 0,
    "Year 8" = 0,
    "Year 9" = 0,
    "Year 10" = 0,
    "Year 11" = 0
)

get_year_group_split <- function() {
    return(year_group_split)
}

school_split <- list(
    "School A" = 0,
    "School B" = 0,
    "School C" = 0,
    "School D" = 0,
    "School E" = 0
)

get_school_split <- function() {
    return(school_split)
}