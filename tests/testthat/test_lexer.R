## tests for lexer


cat("\ntest_lexer.R ")
source("helpers.R")


###############################################################################
## test lexing small components


test_that("Lex on missing file", {
  fmissing = file.path("data", "missing.Rcss")
  expect_error(RcssFileCheckRead(fmissing))
})

  
test_that("initialize Lexer", {
  f5  = file.path("data","style.5.Rcss")
  lexed = RcssLexer(f5)
  expect_equal(class(lexed), "data.frame")
})


test_that("parse a comment", {
  fdata = tochars("/** comment **/")
  result = RcssParseComment(fdata, 1)
  expect_equal(result, length(fdata)+1)
})

test_that("parse a string", {
  fdata = tochars("comment")
  expect_equal(RcssParseString(fdata, 1), length(fdata)+1)
})

test_that("parse a string with escaped", {
  fdata = tochars("comment\'with\'")
  expect_equal(RcssParseString(fdata, 1), length(fdata)+1)
})


test_that("parse a number", {
  fdata = tochars("34")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("parse a positive number", {
  fdata = tochars("+34")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("parse a negative number", {
  fdata = tochars("-34")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("parse a real number", {
  fdata = tochars("-3.4")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("parse a real number with positive exponent", {
  fdata = tochars("1.4e3")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("parse a real number with positive exponent 2", {
  fdata = tochars("1.4e+3")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("parse a real number with negative exponent", {
  fdata = tochars("1.4e-3")
  expect_equal(RcssParseNumber(fdata, 1), length(fdata)+1)
})

test_that("cannot parse a misformed negative", {
  fdata = tochars("-e3")
  expect_error(RcssParseNumber(fdata, 1))
})

test_that("cannot parse with multiple dots", {
  fdata = tochars("3.4.5.6")
  expect_error(RcssParseNumber(fdata, 1))
})

test_that("cannot parse with multiple exponents", {
  fdata = tochars("3e4e5e6")
  expect_error(RcssParseNumber(fdata, 1))
})

test_that("cannot parse without leading digit", {
  fdata = tochars("+.34")
  expect_error(RcssParseNumber(fdata, 1))
})







test_that("parse hex tokens", {
  fdata = tochars("#ff0033 #ffdd4488")
  ## good input will point to a hash sign
  expect_equal(RcssParseHexToken(fdata, 1), 8)
  expect_equal(RcssParseHexToken(fdata, 9), 18)
})

test_that("cannot parse hex with non-6 or non-8", {
  fdata = tochars("#ff00")
  expect_error(RcssParseHexToken(fdata, 1))
  fdata = tochars("#ff0044112233")
  expect_error(RcssParseHexToken(fdata, 1))
})


