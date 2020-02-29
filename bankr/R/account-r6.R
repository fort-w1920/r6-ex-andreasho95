#' @title
#' R6 Object representing a bank account
#'
#' @description
#' Implements an R6 structure for bank accounts and corresponding actions
#'
#' @export
Account <- R6::R6Class("Account",
  public = list(
    #' @field balance Current account balance
    balance = NULL,

    #' @description
    #' Create a new account object.
    #' @return A new `Account` object with `balance` of 0.
    initialize = function() {
      self$balance <- 0
    },

    #' @description
    #' Withdraw money from bank account.
    #' @param amount Amount to be withdrawn. Must be a whole number.
    #' @examples
    #' A <- Account$new()
    #' A$deposit(100)
    #' A$withdraw(50)
    withdraw = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      self$balance <- self$balance - amount
    },

    #' @description
    #' Deposit money into bank account.
    #' @param amount Amount to be deposited. Must be a whole number.
    #' @examples
    #' A <- Account$new()
    #' A$deposit(100)
    deposit = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      self$balance <- self$balance + amount
    }
  )
)

GiroAccount <- R6::R6Class("GiroAccount",
  inherit = Account,
  public = list(
    limit = NULL,
    overdraft_fee = 0.08,
    initialize = function(limit = 0) {
      self$limit <- limit
      self$balance <- 0
    },
    withdraw = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      if (amount > self$balance) {
        amount <- amount * (1 + self$overdraft_fee)
        if (abs(self$balance - amount) > self$limit) {
          stop("The withdrawal would exceed the overdraft limit")
        }
      }
      self$balance <- self$balance - amount
    }
  )
)




