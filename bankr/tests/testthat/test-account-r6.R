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
  my_giro$deposit(1040)
  my_giro$withdraw(700)
  expect_equal(my_giro$balance, -216)
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


context("R6 TransactionLog")

test_that("Transactions work correctly", {

  my_translog <- TransactionLog$new()
  tlog <- my_translog$trans_log
  expect_equal(nrow(tlog), 0)
  expect_true(checkmate::check_data_frame(tlog, types = c("date", "character", "integerish", "integerish")))

  my_translog$make_trans("deposit", 1000)
  tlog <- my_translog$trans_log
  expect_true(tlog[1, "Transaction"] == "deposit", tlog[1, "Amount"] == 1000, tlog[1, "Balance"] == 1000)

  my_translog$make_trans("withdraw", 500)
  tlog <- my_translog$trans_log
  expect_true(tlog[1, "Transaction"] == "deposit", tlog[1, "Amount"] == 1000, tlog[1, "Balance"] == 1000)
  expect_true(tlog[2, "Transaction"] == "withdraw", tlog[1, "Amount"] == -500, tlog[1, "Balance"] == 500)

  expect_error(my_translog$make_trans("no idea", 1000), "Must be element of set \\{'deposit','withdraw'}")
  expect_error(my_translog$make_trans("deposit", "viel"), "Must be of type 'integerish'")
})


context("R6 AccountWithLog")

test_that("Transactions work correctly", {

  acc_with_log <- AccountWithLog$new()
  expect_equal(acc_with_log$balance, 0)
  tlog <- acc_with_log$trans_log
  expect_equal(nrow(tlog), 0)
  expect_true(checkmate::check_data_frame(tlog, types = c("date", "character", "integerish", "integerish")))

  acc_with_log$withdraw(1000)
  expect_equal(acc_with_log$balance, -1000)
  tlog <- acc_with_log$trans_log
  expect_true(tlog[1, "Transaction"] == "withdraw", tlog[1, "Amount"] == 1000, tlog[1, "Balance"] == 1000)

  acc_with_log$deposit(2500)
  expect_equal(acc_with_log$balance, 1500)
  expect_equal(tlog[1, "Transaction"] == "deposit", tlog[1, "Amount"] == 2500, tlog[1, "Balance"] == 1500)

  expect_error(acc_with_log$deposit(-1000), "is not >= 0")
  expect_error(acc_with_log$deposit(10.5), "not close to an integer")
  expect_error(acc_with_log$withdraw(-1000), "is not >= 0")
  expect_error(acc_with_log$withdraw(10.5), "not close to an integer")
})

test_that("Cloning works correctly", {
  acc_with_log <- AccountWithLog$new()
  acc_with_log$deposit(2500)
  tlog <- acc_with_log$trans_log
  expect_true(tlog[1, "Transaction"] == "deposit", tlog[1, "Amount"] == 2500, tlog[1, "Balance"] == 2500)
  new_acc <- acc_with_log$clone()
  new_acc$deposit(5000)
  tlog_clone <- new_acc$trans_log
  expect_true(tlog_clone[2, "Transaction"] == "deposit", tlog_clone[2, "Amount"] == 5000,
              tlog_clone[1, "Balance"] == 7500)
  expect_true(nrow(acc_with_log$trans_log) == 1)
})



