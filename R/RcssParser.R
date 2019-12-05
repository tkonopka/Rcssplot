# Function part of Rcssplot package
#
# Parser or Rcss grammar
# Call RcssParser() - the others are helper functions)
#
# Author: Tomasz Konopka
# 


#' parse a text file assuming it is in css format.
#'
#' @param file character, files on disk
#'
#' @return Rcss object augmented by settings from the file(s)
RcssParser <- function(file) {
  lex <- RcssLexer(file)
  parsetree <- RcssMakeParseTree(lex)
  parsetree
}



#' wrapper to produce generic errors
#'
#' @param lextab data frame with lexed information
#' @param n integer, row number in lextab
#' @param parsefunction character, used in error message
#' @param expecting character, used to hint what the expected token should have been
RcssParseError <- function(lextab, n, parsefunction, expecting) {
  stopCF(paste0(parsefunction,": \n",
             "   expecting: ", expecting,"\n",
             "   got: ", lextab[n,"token"],"\n"))
}


# ################################################################
# Top-level manipulation of parsed tokens, conversion into Rcss object


#' convert lexed tokens into a tree with selectors and declarations 
#' 
#' @param lextab data.frame with lexed tokens
#' @return tree 
RcssMakeParseTree <- function(lextab) {
  
  parsetree <- list()
  n <- 1
  
  ## Start Parsing until "n" is within the lex table
  while (n <= nrow(lextab)) {
    ruleset <- RcssParseRuleSet(lextab, n)
    parsetree[[length(parsetree)+1]] <- ruleset$RuleSet
    n <- ruleset$n
  }
  
  parsetree
}


# ################################################################
# Parsing of individual states


#' parse one rule set (Selectors { Declarations })
#'
#' @param lextab data frame with tokens
#' @param n inteer, current row in the data frame
RcssParseRuleSet <- function(lextab, n) {
  selectors = RcssParseSelectorSet(lextab, n)
  declarations = RcssParseDeclarationSet(lextab, selectors$n)
  list(n = declarations$n,
       RuleSet = list(SelectorSet = selectors$SelectorSet,
                      DeclarationSet = declarations$DeclarationSet))
}


#' parse one selector (IDENT [ class]* | class)
#' 
#' @param lextab integer, data frame with tokens
#' @param n ineger, current row in the data frame
#'
#' @return integer, new n after parsed selectors, list of selectors
RcssParseSelector <- function(lextab, n) {

  # the output here will be an object name followed by classes
  ans <- c("")

  # parse the initial IDENT object
  if (lextab[n, "token"] == ".") {
    # this is ok too  
  } else if (lextab[n, "type"] == "IDENT") {
    ans[1] <- lextab[n, "token"]
    n <- n + 1
  } else {
    RcssParseError(lextab, n, "RcssParseSelector", ".")
  }
  
  # after the first ident, can have classes
  while (n <= nrow(lextab) & lextab[n,"token"] == ".") {
    n <- n + 1
    if (lextab[n,"type"] == "IDENT") {
      ans[length(ans)+1] = lextab[n, "token"]
      n <- n +1
    } else {
      RcssParseError(lextab, n, "RcssParseSelector", "IDENT")
    }
  }

  # at this point, collected all IDENT and classes
  list(n = n, Selector = ans)
}


#' parse a set of selectors (i.e. selector [ ',' selector ]*)
#'
#' @param lextab data frame with tokens
#' @param n integer, current row in the data frame 
RcssParseSelectorSet <- function(lextab, n) {

  ans <- list()
  
  # get the first selector and advance counter
  sel <- RcssParseSelector(lextab, n)
  ans[[1]] <- sel$Selector;
  n <- as.integer(sel$n);
  
  # now parse other selectors as long as they are separated by ','
  while (n <= nrow(lextab) & lextab[n,"token"] == ",") {
    sel <- RcssParseSelector(lextab, n + 1)
    ans[[length(ans) + 1]] <- sel$Selector
    n <- sel$n
  }

  list(n = n, SelectorSet = ans)
}


#' parse a set of declarations
#' (expects to start with a '{' and end with a '}')
#'
#' @param lextab data frame with tokens
#' @param n integer, current row in the data.frame to process
RcssParseDeclarationSet <- function(lextab, n) {

  ## check the parsing starts with an open brace
  if (lextab[n,"token"] != "{") {
    RcssParseError(lextab, n, "RcssParseDeclarationSet", "{");
  }
  
  n <- n + 1;
  ans <- list();

  # keep reading declarations until hit a }
  while(n <= nrow(lextab) & lextab[n, "token"] != "}") {
    # parse a property/expr pair
    exprprop = RcssParseDeclaration(lextab, n)
    if (is.null(exprprop$Expr)) {
      ans[length(ans)+1] = list(NULL)
    } else {
      ans[[length(ans) + 1]] <- exprprop$Expr    
    }
    names(ans)[length(ans)] = exprprop$Property
    
    # advance the n counter over this property/expr pair
    n <- exprprop$n
  }

  # when the loop ends, the current state is a "}". Move over, finish.
  n <- n + 1
  list(n = n, DeclarationSet = ans)
}


#' parse special strings into R primitives
#'
#' @param tok one string
#' @return R primitive for that string, "NULL" gives an empty list
parseIDENT <- function(tok) {
  if (tok == "NULL") {
    return(list())
  } else if (tok %in% c("TRUE", "FALSE", "NA")) {
    return(as.logical(tok))
  } 
  tok
}


#' Parse one declaration (expects an IDENT)
#'
#' @param lexta data frame
RcssParseDeclaration <- function(lextab, n) {
  
  # get name of the property, then move over the property
  property <- RcssParseProperty(lextab, n)
  n <- property$n
  property <- property$Property
  
  # move over the ':'
  if (lextab[n,"token"] != ":") {
    RcssParseError(lextab, n, "RcssParseDeclaration", ":");
  }
  n <- n + 1

  # keep reading the declarations until hit a ';' or a '}'
  expr <- c()
  exprlen <- 0
  while (n <= nrow(lextab) & !lextab[n,"token"] %in% c(";", "}")) {
    
    # allowed tokens are IDENT, NUMBER, HEXCOLOR, STRING
    if (lextab[n, "type"] %in% c("IDENT", "STRING", "HEXCOLOR")) {
      expr[exprlen + 1] <- parseIDENT(lextab[n, "token"])
    } else if (lextab[n, "type"]=="NUMBER") {
      expr[exprlen+1] <- as.numeric(lextab[n,"token"])
    } else {
      RcssParseError(lextab, n, "RcssParseDeclaration",
                     "IDENT|NUMBER|HEXCOLOR|STRING")
    }
    
    n <- n + 1
    exprlen <- exprlen + 1
  }

  # if current token is an end of declaration, skip over it
  if (lextab[n, "token"] == ";") {
    n <- n + 1
  }

  # when the user leaves the expression blank, assume that means ""
  # e.g. this can be used to set xlab=""
  if (exprlen==0) {
    expr <- ""
  }

  # return an object with the property/expr pair, but also
  # the number of the next non-trivial token
  list(n = n, Property = property, Expr = expr)
}


#' parse the name of a property
#'
#' This is usually one token,
#' but if a property has dots (e.g. cex.axis) this function
#' will concatenate the text together
#'
#' @param lextab data frame
#' @param n integer, row number in lextab
RcssParseProperty <- function(lextab, n) {

  if (lextab[n,"type"] != "IDENT") {
    RcssParseError(lextab, n, "RcssParseProperty", "IDENT");
  }

  # deal with the expected case (one token)
  property <- lextab[n, "token"]
  n <- n + 1

  # deal with extension of property when the next tokens are ".IDENT"
  while (n < nrow(lextab) & lextab[n,"type"] == "TERMINAL" & lextab[n, "token"] == ".") {  
    if (lextab[n + 1, "type"] == "IDENT") {
      property <- c(property, ".", lextab[n + 1, "token"])
    }
    n <- n + 2;
  }
  
  # build a unified property
  property <- paste(property, collapse = "")
  
  list(n = n, Property = property)
}

