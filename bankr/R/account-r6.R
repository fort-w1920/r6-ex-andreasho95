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


