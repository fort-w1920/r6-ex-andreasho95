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

#' @title
#' R6 Object representing a giro bank account
#'
#' @description
#' Implements an R6 structure for giro bank accounts and corresponding actions
#'
#' @export
GiroAccount <- R6::R6Class("GiroAccount",

  inherit = Account,
  public = list(
    #' @field limit Overdraft limit.
    limit = NULL,

    #' @field overdraft_fee Overdraft fee.
    overdraft_fee = 0.08,

    #' @description
    #' Create a new GiroAccount object.
    #' @param limit Overdraft limit.
    #' @return A new `GiroAccount` object.
    initialize = function(limit = 0) {
      self$limit <- limit
      self$balance <- 0
    },

    #' @description
    #' Withdraw money from bank account.
    #' @details
    #' If more money is withdrawn than client has the exceeding amount will be charged with
    #' overdraft fees. If the amount withdrawn would exceed the overdraft limit, no withdrawal will be made.
    #' is possible and an error message is returned.
    #' @param amount Amount to be withdrawn. Must be a whole number.
    withdraw = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      if (amount > self$balance) {
        amount <- amount + (amount - self$balance) * self$overdraft_fee
        if (abs(self$balance - amount) > self$limit) {
          stop("The withdrawal would exceed the overdraft limit")
        }
      }
      self$balance <- self$balance - amount
    }
  )
)

#' @title
#' R6 Object representing a safe bank account
#'
#' @description
#' Implements an R6 structure for safe bank accounts and corresponding actions
#'
#' @export
SafeAccount <- R6::R6Class("SafeAccount",
  private = list(
    .balance = NULL
  ),
  public = list(
    #' @description
    #' Create a new SafeAccount object.
    #' @return A new `SafeAccount` object.
    initialize = function() {
      private$.balance <- 0
    }
  ),
  active = list(
    #' @field withdraw Withdraw money from bank account or providing access current balance.
    #' If missing argument then the currenct balance will be returned
    withdraw = function(amount) {
      if (missing(amount)) {
        private$.balance
      } else {
        checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
        private$.balance <- private$.balance - amount
      }
    },

    #' @field deposit Deposit money into bank account or providing access current balance.
    #' If missing argument then the currenct balance will be returned
    deposit = function(amount) {
      if (missing(amount)) {
        private$.balance
      } else {
        checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
        private$.balance <- private$.balance + amount
      }
    }
  )
)

#' @title
#' R6 Object representing a transaction log
#'
#' @description
#' Implements an R6 structure for a transaction log and corresponding actions
#'
#' @export
TransactionLog <- R6::R6Class("TransactionLog",
  private = list(
    .trans_log = data.frame(
      "Date" = as.Date(character(0)),
      "Transaction" = character(0),
      "Amount" = numeric(0),
      "Balance" = numeric(0),
      stringsAsFactors = FALSE
    )
  ),
  active = list(
    #' @field trans_log Provides access to current transition log.
    trans_log = function(value) {
      if (missing(value)) {
        private$.trans_log
      } else {
        stop("`$trans_log is read only`")
      }
    }
  ),
  public = list(
    #' @description
    #' Make a transaction and add to the transaction log.
    #' @param trans_type The transaction type. Must be either 'deposit' or 'withdraw'.
    #' @param amount Amount to be deposited. Must be a whole number.
    #' @return
    #' A data.frame with the columns 'Date', 'Transaction', 'Amount', 'Balance'.
    #' Each row describes a transaction
    make_trans = function(trans_type, amount) {
      checkmate::assert_choice(trans_type, c("deposit", "withdraw"))
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      if (trans_type == "withdraw") {
        amount <- -amount
      }
      if (nrow(private$.trans_log) == 0) {
        balance <- amount
      } else {
        old_trans_log <- private$.trans_log
        balance <- old_trans_log[nrow(old_trans_log), "Balance"] + amount
      }
      new_trans <- data.frame(date(), trans_type, amount, balance, stringsAsFactors = FALSE)
      names(new_trans) <- c("Date", "Transaction", "Amount", "Balance")
      new_translog <- rbind(private$.trans_log, new_trans)
      private$.trans_log <- new_translog
      cat("Transaction completed!")
      invisible(self)
    }
  )
)


#' @title
#' R6 Object representing a bank account with transaction log
#'
#' @description
#' Implements an R6 structure for a bank account with transaction log and corresponding actions.
#'
#' @export
AccountWithLog <- R6::R6Class("AccountWithLog",
  private = list(
    .balance = NULL,
    .trans_log = NULL
  ),
  active = list(
    #' @field balance Provides access to account balance.
    balance = function(value){
      if (missing(value)) {
        private$.balance
      } else {
        stop("`$balance is read only`")
      }
    },

    #' @field trans_log Provides access to current transition log.
    trans_log = function(value){
      if (missing(value)) {
        private$.trans_log$trans_log
      } else {
        stop("`$trans_log is read only`")
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new AccountWithLog object.
    #' @param balance Initial balance of account.
    #' @param trans_log Transition log for the account. Requires a 'TransactionLog' object.
    #' On default it will create a fresh log for the new account.
    #' @return A new `AccountWithLog`.
    initialize = function(balance = 0, trans_log = TransactionLog$new()) {
      checkmate::assert_integerish(balance, lower = 0, any.missing = FALSE, len = 1)
      checkmate::assert_class(trans_log, "R6")
      private$.balance <- balance
      private$.trans_log <- trans_log
    },

    #' @description
    #' Withdraw money from bank account.
    #' @param amount Amount to be withdrawn. Must be a whole number.
    withdraw = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      private$.balance <- private$.balance - amount
      private$.trans_log$make_trans("withdraw", amount)
    },

    #' @description
    #' Deposit money into bank account.
    #' @param amount Amount to be deposited. Must be a whole number.
    deposit = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      private$.balance <- private$.balance + amount
      private$.trans_log$make_trans("deposit", amount)
    },

    #' @description Printes the current bank statement in a nice format including a header.
    print_bank_statement = function(){
      cat(paste0(rep("=", 60)), "\n   Bank Statement   \n", paste0(rep("=", 60)), "\n", sep = "")
      private$.trans_log$trans_log
    }
  ),
  cloneable = FALSE
)

# Is there a better solution?
# https://stackoverflow.com/questions/53935619/changing-cloning-behavior-in-r6-classes
AccountWithLog$set("public", "clone", function() {
  cloned_acc <- AccountWithLog$new(private$.balance, private$.trans_log$clone())
  cloned_acc
})

