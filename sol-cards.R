library(R6)


deck <- R6Class("bavarian_deck", list(
  cards = NULL,
  initialize = function(seed = NULL) {
    checkmate::assert_number(seed, null.ok = TRUE)
    farbe <- c("G", "H", "E", "S")
    wert <- c(6:10, "U", "O", "K", "A")
    cards <- paste0(rep(farbe, each = 9), rep(wert, times = 4))
    self$cards <- sample(cards)
  },
  draw = function(number) {
    checkmate::assert_count(number)
    if (number > length(self$cards)) {
      stop("Not enough cards in deck. First restock the deck by calling `$restock()`")
    }
    drawn <- sample(self$cards, size = number)
    self$cards <- self$cards[!self$cards %in% drawn]
    drawn
  }, 
  restock = function() {
    self$initialize()
  },
  cut = function(index) {
    checkmate::assert_count(index, positive = TRUE)
    if (index > length(self$cards)) {
      stop("Not enough cards in deck. First restock the deck by calling `$restock()`")
    }
    # Chosen card will be removed from deck - I guess?
    drawn_card <- self$cards[-index]
    first_part <- self$cards[seq_len(index - 1)]
    second_part <- self$cards[(index + 1):length(self$cards)]
    self$cards <- c(second_part, first_part)
    self$cards[index]
  }
))


