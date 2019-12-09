# tests for lexer


cat("\ntest_lexer.R ")
source("helpers.R")




###############################################################################
# lexing small components


test_that("Lex on missing file", {
  fmissing = file.path("data", "missing.Rcss")
  expect_error(RcssFileCheckRead(fmissing))
})

  
test_that("initialize Lexer", {
  f5  = file.path("data","style.5.Rcss")
  lexed = RcssLexer(f5)
  expect_is(lexed, "data.frame")
})


test_that("parse a comment", {
  fdata = tochardata("/** comment **/")
  result = RcssParseComment(fdata, 1)
  expect_equal(result, nrow(fdata)+1)
})


test_that("parse a string", {
  fdata = tochardata("comment")
  expect_equal(RcssParseString(fdata, 1), nrow(fdata)+1)
})


test_that("parse a string with escaped", {
  fdata = tochardata("comment\'with\'")
  expect_equal(RcssParseString(fdata, 1), nrow(fdata)+1)
})


test_that("parse a number", {
  fdata = tochardata("34")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("parse a positive number", {
  fdata = tochardata("+34")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("parse a negative number", {
  fdata = tochardata("-34")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("parse a real number", {
  fdata = tochardata("-3.4")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("parse a real number with positive exponent", {
  fdata = tochardata("1.4e3")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("parse a real number with positive exponent 2", {
  fdata = tochardata("1.4e+3")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("parse a real number with negative exponent", {
  fdata = tochardata("1.4e-3")
  expect_equal(RcssParseNumber(fdata, 1), nrow(fdata)+1)
})


test_that("cannot parse a misformed negative", {
  fdata = tochardata("-e3")
  expect_error(RcssParseNumber(fdata, 1))
})


test_that("cannot parse with multiple dots", {
  fdata = tochardata("3.4.5.6")
  expect_error(RcssParseNumber(fdata, 1))
})


test_that("cannot parse with multiple exponents", {
  fdata = tochardata("3e4e5e6")
  expect_error(RcssParseNumber(fdata, 1))
})


test_that("cannot parse without leading digit", {
  fdata = tochardata("+.34")
  expect_error(RcssParseNumber(fdata, 1))
})


###############################################################################
# lexing colors


test_that("parse hex tokens", {
  fdata = tochardata("#ff0033 #ffdd4488")
  ## good input will point to a hash sign
  expect_equal(RcssParseHexToken(fdata, 1), 8)
  expect_equal(RcssParseHexToken(fdata, 9), 18)
})

test_that("cannot parse hex with non-6 or non-8", {
  fdata = tochardata("#ff00")
  expect_error(RcssParseHexToken(fdata, 1))
  fdata = tochardata("#ff0044112233")
  expect_error(RcssParseHexToken(fdata, 1))
})

test_that("parse hex tokens in uppercase", {
  fdata = tochardata("#FF0033 #AACC4488")
  ## good input will point to a hash sign
  expect_equal(RcssParseHexToken(fdata, 1), 8)
  expect_equal(RcssParseHexToken(fdata, 9), 18)
})




###############################################################################
# lexing from text

test_that("Lex from a string", {
  expect_silent(RcssLexer(text=c("text { cex: 2; }", "points { pch: 19; }")))
})


test_that("Lex number with a positive sign", {
  # parse text with and without plus sign
  raw.with = RcssLexer(text="abc { value: +2; }")
  raw.without = RcssLexer(text="abc { value: 2; }")
  result.with = raw.with[raw.with$type=="NUMBER", "token"]
  result.without = raw.without[raw.without$type=="NUMBER", "token"]  
  expect_equal(as.integer(result.with), as.integer(result.without))
})


test_that("Lex incomplete string", {
  # parse text with and without plus sign
  expect_silent(RcssLexer(text="abc { value: 123"))
})


test_that("Lex comment without ending", {
  # parse text with and without plus sign
  expect_silent(RcssLexer(text="abc { x: 1; } /** comment "))
})

