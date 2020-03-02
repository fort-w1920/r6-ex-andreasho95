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

SafeAccount <- R6::R6Class("SafeAccount",
  private = list(
    .balance = NULL
  ),
  public = list(
    initialize = function() {
      private$.balance <- 0
    }
  ),
  active = list(
    withdraw = function(amount) {
      if (missing(amount)) {
        private$.balance
      } else {
        checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
        private$.balance <- private$.balance - amount
      }
    },
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
    trans_log = function(value) {
      if (missing(value)) {
        private$.trans_log
      } else {
        stop("`$trans_log is read only`")
      }
    }
  ),
  public = list(
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



AccountWithLog <- R6::R6Class("AccountWithLog",
  private = list(
    #' @field balance Current account balance
    .balance = NULL,
    .trans_log = NULL
  ),
  active = list(
    balance = function(value){
      if (missing(value)) {
        private$.balance
      } else {
        stop("`$balance is read only`")
      }
    },
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
    #' Create a new account object.
    #' @return A new `Account` object with `balance` of 0.
    initialize = function(balance = 0, trans_log = TransactionLog$new()) {
      checkmate::assert_integerish(balance, lower = 0, any.missing = FALSE, len = 1)
      checkmate::assert_class(trans_log, "R6")
      private$.balance <- balance
      private$.trans_log <- trans_log
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
      private$.balance <- private$.balance - amount
      private$.trans_log$make_trans("withdraw", amount)
    },

    #' @description
    #' Deposit money into bank account.
    #' @param amount Amount to be deposited. Must be a whole number.
    #' @examples
    #' A <- Account$new()
    #' A$deposit(100)
    deposit = function(amount) {
      checkmate::assert_integerish(amount, lower = 0, any.missing = FALSE, len = 1)
      private$.balance <- private$.balance + amount
      private$.trans_log$make_trans("deposit", amount)
    },
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

# acc_with_log <- AccountWithLog$new()
# acc_with_log
# acc_with_log$withdraw(1000)
# acc_with_log$balance
# acc_with_log$trans_log
# acc_with_log$print_bank_statement()
# acc_with_log$deposit(2500)
# acc_with_log$balance
# acc_with_log$trans_log
# acc_with_log$print_bank_statement()
# new_acc <- acc_with_log$clone()
# new_acc$deposit(5000)
# new_acc$print_bank_statement()
# acc_with_log$print_bank_statement()

