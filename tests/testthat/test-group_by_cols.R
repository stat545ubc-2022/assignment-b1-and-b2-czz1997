#First, sample a small dataset for testing:
set.seed(123)
apt_buildings_small <- apt_buildings %>%
  filter(year_built >= 1955, year_built < 1960) %>%
  sample_n(50)
head(apt_buildings_small)

# Test with no NA’s:
result <- apt_buildings_small %>%
  percentage_by_group('year_built', sprinkler_system == "YES")
expected <- as_tibble(data.frame(
  year_built = c(1955, 1956, 1957, 1958, 1959),
  percentage = c(0.43, 0.25, 0.62, 0.64, 0.6))
)
test_that("Test with no NA’s: percentage of buildings that has sprinkler system by year built", {
  expect_identical(result, expected)
})

result <- apt_buildings_small %>%
  percentage_by_group('property_type', year_built < 1957)
expected <- as_tibble(data.frame(
  property_type = c('PRIVATE', 'SOCIAL HOUSING', 'TCHC'),
  percentage = c(0.37, 0, 0.33))
)
test_that("Test with no NA’s: percentage of buildings that was built before 1957 by property type", {
  expect_identical(result, expected)
})

# Test with NA’s:
apt_buildings_small[nrow(apt_buildings_small) + 1, ] <- NA # insert a row of NA
result <- apt_buildings_small %>%
  percentage_by_group('year_built', sprinkler_system == "YES")
expected <- as_tibble(data.frame(
  year_built = c(1955, 1956, 1957, 1958, 1959),
  percentage = c(0.43, 0.25, 0.62, 0.64, 0.6))
)
test_that("Test with NA’s", {
  expect_identical(result, expected)
})

# Test with different type:
result <- apt_buildings_small %>%
  percentage_by_group(28, sprinkler_system == "YES")
expected <- as_tibble(data.frame(
  year_built = c(1955, 1956, 1957, 1958, 1959),
  percentage = c(0.43, 0.25, 0.62, 0.64, 0.6))
)
test_that("Test with different type: numeric factor for group_by_cols", {
  expect_identical(result, expected)
})

test_that("Test with different type: invalid numeric subscript vector for group_by_cols", {
  expect_error(
    apt_buildings_small %>%
      percentage_by_group(c(1.1, 2, 3), sprinkler_system == "YES")
  )
})

rm('apt_buildings_small')
rm('result')
rm('expected')
