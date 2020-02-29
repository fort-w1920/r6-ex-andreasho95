context("R6 Account")

test_that("Withdraw and deposit methods work correctly", {
  my_account <- Account$new()
  expect_equal(my_account$balance, 0)
  my_account$deposit(1000)
  expect_equal(my_account$balance, 1000)
  my_account$deposit(2000)
  my_account$withdraw(1500)
  expect_equal(my_account$balance, 1500)
  expect_error(my_account$deposit(-1000), "is not >= 0")
  expect_error(my_account$deposit(10.5), "not close to an integer")
  expect_error(my_account$withdraw(-1000), "is not >= 0")
  expect_error(my_account$withdraw(10.5), "not close to an integer")
})

context("R6 GiroAccount - Subclass of Account")

test_that("Withdraw and deposit methods work correctly", {

  my_giro <- GiroAccount$new(limit = 1000)
  expect_equal(my_giro$balance, 0)
  expect_equal(my_giro$overdraft_fee, 0.08)
  expect_equal(my_giro$limit, 1000)
  my_giro$withdraw(500)
  expect_equal(my_giro$balance, -540)
  expect_error(my_giro$withdraw(500), "would exceed the overdraft limit")
  expect_error(my_giro$deposit(-1000), "is not >= 0")
  expect_error(my_giro$deposit(10.5), "not close to an integer")
  expect_error(my_giro$withdraw(-1000), "is not >= 0")
  expect_error(my_giro$withdraw(10.5), "not close to an integer")
})


context("R6 SafeAccount")

test_that("Withdraw and deposit methods work correctly", {

  my_safeaccount <- SafeAccount$new()
  expect_equal(my_safeaccount$withdraw, 0)
  my_safeaccount$withdraw <- 500
  expect_equal(my_safeaccount$withdraw, -500)
  my_safeaccount$deposit <- 1500
  expect_equal(my_safeaccount$withdraw, 1000)

  expect_error(my_safeaccount$deposit <- -1000, "is not >= 0")
  expect_error(my_safeaccount$deposit <- 10.5 , "not close to an integer")
  expect_error(my_safeaccount$withdraw <- -1000, "is not >= 0")
  expect_error(my_safeaccount$withdraw <- 10.5, "not close to an integer")
})
