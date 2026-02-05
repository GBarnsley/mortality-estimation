test_that("categorize_age works correctly", {
  # Test standard breaks
  expect_equal(categorize_age(0), "0-4")
  expect_equal(categorize_age(4), "0-4")
  expect_equal(categorize_age(5), "5-9")
  expect_equal(categorize_age(94), "90-94")
  expect_equal(categorize_age(95), "95+")
  expect_equal(categorize_age(120), "95+")

  # Test vectorization
  expect_equal(categorize_age(c(2, 7, 96)), c("0-4", "5-9", "95+"))
})

test_that("calculate_age handles intervals correctly", {
  dob <- as.Date("1990-01-01")

  # Exactly 30 years
  expect_equal(calculate_age(dob, as.Date("2020-01-01")), 30)

  # One day before 30th birthday
  expect_equal(calculate_age(dob, as.Date("2019-12-31")), 29)

  # Handling leap years implicitly via lubridate
  dob_leap <- as.Date("2000-02-29")
  # 2001-02-28 is 1 day before the full period completion if we consider Feb 29 -> Feb 28
  expect_equal(calculate_age(dob_leap, as.Date("2001-02-28")), 0)
  expect_equal(calculate_age(dob_leap, as.Date("2001-03-01")), 1)
})

test_that("is_trauma identifies V-Y codes correctly", {
  expect_true(is_trauma("V01"))
  expect_true(is_trauma("X99"))
  expect_true(is_trauma("Y34"))

  expect_false(is_trauma("A00")) # Infectious
  expect_false(is_trauma("C00")) # Neoplasm
  expect_false(is_trauma("I10")) # Circulatory
  expect_false(is_trauma("R99")) # Unknown

  # Test vectorization
  expect_equal(is_trauma(c("V01", "I10")), c(TRUE, FALSE))
})
