# Test file for SUS score calculations
# Install testthat if you haven't already: install.packages("testthat")
library(testthat)

# Source the analysis functions
source("analysis_functions.r")

# Test cases for calculate_sus_score_for_row function
test_that("calculate_sus_score_for_row calculates correct SUS scores", {
  
  # Test case 1: Perfect score (all positive items = 5, all negative items = 1)
  # Q1_1, Q1_3, Q1_5 are positive items (should be 5 for best score)
  # Q1_2, Q1_4 are negative items (should be 1 for best score)
  perfect_response <- data.frame(
    Q1_1 = 5, Q1_2 = 1, Q1_3 = 5, Q1_4 = 1, Q1_5 = 5
  )
  expect_equal(calculate_sus_score_for_row(perfect_response), 100)
  
  # Test case 2: Worst score (all positive items = 1, all negative items = 5)
  worst_response <- data.frame(
    Q1_1 = 1, Q1_2 = 5, Q1_3 = 1, Q1_4 = 5, Q1_5 = 1
  )
  expect_equal(calculate_sus_score_for_row(worst_response), 0)
  
  # Test case 3: Neutral score (all items = 3)
  neutral_response <- data.frame(
    Q1_1 = 3, Q1_2 = 3, Q1_3 = 3, Q1_4 = 3, Q1_5 = 3
  )
  expect_equal(calculate_sus_score_for_row(neutral_response), 50)
  
  # Test case 4: Mixed responses example 1
  # Q1_1=4, Q1_2=2, Q1_3=4, Q1_4=2, Q1_5=4
  # Odd items: 4+4+4 = 12, minus 3 = 9
  # Even items: 10 - (2+2) = 6
  # Total: (9+6) * 5 = 75
  mixed_response_1 <- data.frame(
    Q1_1 = 4, Q1_2 = 2, Q1_3 = 4, Q1_4 = 2, Q1_5 = 4
  )
  expect_equal(calculate_sus_score_for_row(mixed_response_1), 75)
  
  # Test case 5: Mixed responses example 2
  # Q1_1=2, Q1_2=4, Q1_3=3, Q1_4=3, Q1_5=2
  # Odd items: 2+3+2 = 7, minus 3 = 4
  # Even items: 10 - (4+3) = 3
  # Total: (4+3) * 5 = 35
  mixed_response_2 <- data.frame(
    Q1_1 = 2, Q1_2 = 4, Q1_3 = 3, Q1_4 = 3, Q1_5 = 2
  )
  expect_equal(calculate_sus_score_for_row(mixed_response_2), 35)
  
  # Test case 6: From your actual data (first row)
  actual_response_1 <- data.frame(
    Q1_1 = 1, Q1_2 = 3, Q1_3 = 2, Q1_4 = 3, Q1_5 = 3
  )
  # Odd items: 1+2+3 = 6, minus 3 = 3
  # Even items: 10 - (3+3) = 4
  # Total: (3+4) * 5 = 35
  expect_equal(calculate_sus_score_for_row(actual_response_1), 35)
  
  # Test case 7: From your actual data (second row)
  actual_response_2 <- data.frame(
    Q1_1 = 1, Q1_2 = 1, Q1_3 = 2, Q1_4 = 3, Q1_5 = 3
  )
  # Odd items: 1+2+3 = 6, minus 3 = 3
  # Even items: 10 - (1+3) = 6
  # Total: (3+6) * 5 = 45
  expect_equal(calculate_sus_score_for_row(actual_response_2), 45)
})

test_that("calculate_sus_score_for_row handles edge cases", {
  
  # Test with data frame that has extra columns (should ignore them)
  response_with_extra_cols <- data.frame(
    student_id = "test-id",
    Q1_1 = 4, Q1_2 = 2, Q1_3 = 4, Q1_4 = 2, Q1_5 = 4,
    Q2_1 = 3, Q2_2 = 3  # Extra columns
  )
  expect_equal(calculate_sus_score_for_row(response_with_extra_cols), 75)
  
  # Test with minimum possible values (all 1s)
  min_response <- data.frame(
    Q1_1 = 1, Q1_2 = 1, Q1_3 = 1, Q1_4 = 1, Q1_5 = 1
  )
  # Odd items: 1+1+1 = 3, minus 3 = 0
  # Even items: 10 - (1+1) = 8
  # Total: (0+8) * 5 = 40
  expect_equal(calculate_sus_score_for_row(min_response), 40)
  
  # Test with maximum possible values (all 5s)
  max_response <- data.frame(
    Q1_1 = 5, Q1_2 = 5, Q1_3 = 5, Q1_4 = 5, Q1_5 = 5
  )
  # Odd items: 5+5+5 = 15, minus 3 = 12
  # Even items: 10 - (5+5) = 0
  # Total: (12+0) * 5 = 60
  expect_equal(calculate_sus_score_for_row(max_response), 60)
})

# Test for handling NA values (if this is expected behavior)
test_that("calculate_sus_score_for_row handles NA values", {
  
  # Test with NA in odd items
  response_with_na_odd <- data.frame(
    Q1_1 = NA, Q1_2 = 2, Q1_3 = 4, Q1_4 = 2, Q1_5 = 4
  )
  # This should work because sum(..., na.rm = TRUE) handles NAs
  # Odd items: NA+4+4 with na.rm=TRUE = 8, minus 3 = 5
  # Even items: 10 - (2+2) = 6
  # Total: (5+6) * 5 = 55
  expect_equal(calculate_sus_score_for_row(response_with_na_odd), 55)
  
  # Test with NA in even items
  response_with_na_even <- data.frame(
    Q1_1 = 4, Q1_2 = NA, Q1_3 = 4, Q1_4 = 2, Q1_5 = 4
  )
  # Odd items: 4+4+4 = 12, minus 3 = 9
  # Even items: 10 - (NA+2) with na.rm=TRUE = 10 - 2 = 8
  # Total: (9+8) * 5 = 85
  expect_equal(calculate_sus_score_for_row(response_with_na_even), 85)
})

# Tests for dataset-level calculations (mean and median)
test_that("calculate_mean_sus_score calculates correct mean SUS scores", {
  
  # Test case 1: Dataset with known scores
  # Scores: 100, 0, 50 -> Mean = 50
  test_dataset_1 <- data.frame(
    Q1_1 = c(5, 1, 3), Q1_2 = c(1, 5, 3), Q1_3 = c(5, 1, 3), 
    Q1_4 = c(1, 5, 3), Q1_5 = c(5, 1, 3)
  )
  expect_equal(calculate_mean_sus_score(test_dataset_1), 50)
  
  # Test case 2: Dataset with identical scores
  # All scores = 75 -> Mean = 75
  test_dataset_2 <- data.frame(
    Q1_1 = c(4, 4, 4), Q1_2 = c(2, 2, 2), Q1_3 = c(4, 4, 4), 
    Q1_4 = c(2, 2, 2), Q1_5 = c(4, 4, 4)
  )
  expect_equal(calculate_mean_sus_score(test_dataset_2), 75)
  
  # Test case 3: Single row dataset
  single_row_dataset <- data.frame(
    Q1_1 = 4, Q1_2 = 2, Q1_3 = 4, Q1_4 = 2, Q1_5 = 4
  )
  expect_equal(calculate_mean_sus_score(single_row_dataset), 75)
  
  # Test case 4: Dataset with varied scores
  # Scores: 35, 45, 75 -> Mean = 51.67 (rounded)
  varied_dataset <- data.frame(
    Q1_1 = c(1, 1, 4), Q1_2 = c(3, 1, 2), Q1_3 = c(2, 2, 4), 
    Q1_4 = c(3, 3, 2), Q1_5 = c(3, 3, 4)
  )
  expected_mean <- (35 + 45 + 75) / 3
  expect_equal(calculate_mean_sus_score(varied_dataset), expected_mean)
})

test_that("calculate_median_sus_score calculates correct median SUS scores", {
  
  # Test case 1: Dataset with odd number of scores
  # Scores: 0, 50, 100 -> Median = 50
  test_dataset_odd <- data.frame(
    Q1_1 = c(1, 3, 5), Q1_2 = c(5, 3, 1), Q1_3 = c(1, 3, 5), 
    Q1_4 = c(5, 3, 1), Q1_5 = c(1, 3, 5)
  )
  expect_equal(calculate_median_sus_score(test_dataset_odd), 50)
  
  # Test case 2: Dataset with even number of scores
  # Scores: 0, 50, 75, 100 -> Median = (50 + 75) / 2 = 62.5
  test_dataset_even <- data.frame(
    Q1_1 = c(1, 3, 4, 5), Q1_2 = c(5, 3, 2, 1), Q1_3 = c(1, 3, 4, 5), 
    Q1_4 = c(5, 3, 2, 1), Q1_5 = c(1, 3, 4, 5)
  )
  expect_equal(calculate_median_sus_score(test_dataset_even), 62.5)
  
  # Test case 3: Dataset with identical scores
  # All scores = 40 -> Median = 40
  identical_dataset <- data.frame(
    Q1_1 = c(1, 1, 1), Q1_2 = c(1, 1, 1), Q1_3 = c(1, 1, 1), 
    Q1_4 = c(1, 1, 1), Q1_5 = c(1, 1, 1)
  )
  expect_equal(calculate_median_sus_score(identical_dataset), 40)
  
  # Test case 4: Single row dataset
  single_row_dataset <- data.frame(
    Q1_1 = 2, Q1_2 = 4, Q1_3 = 3, Q1_4 = 3, Q1_5 = 2
  )
  expect_equal(calculate_median_sus_score(single_row_dataset), 35)
  
  # Test case 5: Dataset with repeated values
  # Scores: 35, 35, 75 -> Median = 35
  repeated_values_dataset <- data.frame(
    Q1_1 = c(1, 1, 4), Q1_2 = c(3, 3, 2), Q1_3 = c(2, 2, 4), 
    Q1_4 = c(3, 3, 2), Q1_5 = c(3, 3, 4)
  )
  expect_equal(calculate_median_sus_score(repeated_values_dataset), 35)
})

test_that("mean and median functions handle edge cases", {
  
  # Test with real data structure (extra columns should be ignored)
  real_data_structure <- data.frame(
    student_id = c("test1", "test2", "test3"),
    Q1_1 = c(4, 2, 3), Q1_2 = c(2, 4, 3), Q1_3 = c(4, 3, 3), 
    Q1_4 = c(2, 3, 3), Q1_5 = c(4, 2, 3),
    Q2_1 = c(1, 2, 3), Q2_2 = c(2, 3, 4)  # Extra columns
  )
  
  # Should calculate: 75, 35, 50 -> Mean = 53.33, Median = 50
  expect_equal(round(calculate_mean_sus_score(real_data_structure), 2), 53.33)
  expect_equal(calculate_median_sus_score(real_data_structure), 50)
  
  # Test with dataset containing some NA values
  data_with_nas <- data.frame(
    Q1_1 = c(4, NA, 3), Q1_2 = c(2, 4, 3), Q1_3 = c(4, 3, 3), 
    Q1_4 = c(2, 3, 3), Q1_5 = c(4, 2, 3)
  )
  
  # Should handle NAs appropriately in the sum calculations
  # Row 1: 75, Row 2: 35 (NA handled by na.rm=TRUE), Row 3: 50
  mean_result <- calculate_mean_sus_score(data_with_nas)
  median_result <- calculate_median_sus_score(data_with_nas)
  
  expect_false(is.na(mean_result))
  expect_false(is.na(median_result))
  expect_true(mean_result > 0)
  expect_true(median_result > 0)
})

# Function to run all tests
run_sus_tests <- function() {
  cat("Running SUS calculation tests...\n")
  test_file("test_sus_calculations.r")
  cat("Tests completed!\n")
}

# Uncomment the line below to run tests when sourcing this file
# run_sus_tests()
