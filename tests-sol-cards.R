library(testthat)
context("R6 bavarian_deck")

source("sol-cards.R")

test_that("Some basic tests on functionality", {
  our_deck <- deck$new()
  expect_true(length(unique(our_deck$cards)) == 36)
  drawn <- our_deck$draw(15)
  expect_true(length(unique(our_deck$cards)) == 21)
  expect_true(length(drawn) == 15)
  expect_error(our_deck$draw(32), "Not enough cards")
  our_deck$restock()
  expect_true(length(unique(our_deck$cards)) == 36)
  expect_error(our_deck$draw(5.5), "type 'count'")
  index <- 8
  first_part <- our_deck$cards[seq_len(index - 1)]
  second_part <- our_deck$cards[seq(index + 1, length(our_deck$cards))]
  cut_card <- our_deck$cards[index]
  expect_identical(our_deck$cut(8), our_deck$cards[8])
  expect_identical(c(second_part, first_part), our_deck$cards)
  expect_true(length(unique(our_deck$cards)) == 35)
  expect_identical(c(second_part, first_part), our_deck$cards)
})