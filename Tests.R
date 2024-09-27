# This is a script to save your own tests for the function
source("FunctionsLR.R")

# Test 1: Check if objective function is calculated correctly
test_objective <- function() {
  set.seed(123)
  X <- cbind(1, matrix(rnorm(500), 100, 5))
  y <- sample(0:4, 100, replace = TRUE)
  Xt <- cbind(1, matrix(rnorm(100), 20, 5))
  yt <- sample(0:4, 20, replace = TRUE)
  
  result <- LRMultiClass(X, y, Xt, yt, numIter = 50)
  
  # Check if all objective values are positive
  is_positive <- all(result$objective > 0)
  cat("Objective values are all positive :", is_positive, "\n")
  
  # The objective should be positive
  stopifnot(is_positive)
}

# Test 2: Check behavior with two normal populations
test_two_populations <- function() {
  set.seed(456)
  n <- 100
  X1 <- cbind(1, matrix(rnorm(n*2, mean = 0, sd = 1), n, 2))
  X2 <- cbind(1, matrix(rnorm(n*2, mean = 2, sd = 1), n, 2))
  X <- rbind(X1, X2)
  y <- c(rep(0, n), rep(1, n))
  
  Xt <- rbind(
    cbind(1, matrix(rnorm(20*2, mean = 0, sd = 1), 20, 2)),
    cbind(1, matrix(rnorm(20*2, mean = 2, sd = 1), 20, 2))
  )
  yt <- c(rep(0, 20), rep(1, 20))
  
  result <- LRMultiClass(X, y, Xt, yt, numIter = 50)
  
  cat("Final training error:", tail(result$error_train, 1), "%\n")
  cat("Final testing error:", tail(result$error_test, 1), "%\n")
  
  # Errors should be reasonably low for well-separated populations
  stopifnot(tail(result$error_train, 1) < 10)
  stopifnot(tail(result$error_test, 1) < 15)
}

# Test 3: Check if objective value improves across iterations
test_objective_improvement <- function() {
  set.seed(789)
  X <- cbind(1, matrix(rnorm(500), 100, 5))
  y <- sample(0:4, 100, replace = TRUE)
  Xt <- cbind(1, matrix(rnorm(100), 20, 5))
  yt <- sample(0:4, 20, replace = TRUE)
  
  result <- LRMultiClass(X, y, Xt, yt, numIter = 50)
  
  # Check if objective values are decreasing
  is_decreasing <- all(diff(result$objective) <= 0)
  cat("Objective function is monotonically decreasing:", is_decreasing, "\n")
  
  stopifnot(is_decreasing)
}

# Test 4: Check how classification error changes across iterations
test_error_change <- function() {
  set.seed(101112)
  X <- cbind(1, matrix(rnorm(1000), 200, 5))
  y <- sample(0:2, 200, replace = TRUE)
  Xt <- cbind(1, matrix(rnorm(200), 40, 5))
  yt <- sample(0:2, 40, replace = TRUE)
  
  result <- LRMultiClass(X, y, Xt, yt, numIter = 50)
  
  cat("Initial training error:", result$error_train[1], "%\n")
  cat("Final training error:", tail(result$error_train, 1), "%\n")
  cat("Initial testing error:", result$error_test[1], "%\n")
  cat("Final testing error:", tail(result$error_test, 1), "%\n")
  
  # Check if final errors are lower than initial errors
  stopifnot(tail(result$error_train, 1) < result$error_train[1])
  stopifnot(tail(result$error_test, 1) < result$error_test[1])
}

# Run all tests
cat("\nRunning Test 1: Objective Function Calculation\n")
test_objective()

cat("\nRunning Test 2: Two Normal Populations\n")
test_two_populations()

cat("\nRunning Test 3: Objective Value Improvement\n")
test_objective_improvement()

cat("\nRunning Test 4: Classification Error Change\n")
test_error_change()

cat("\nAll tests completed.\n")
