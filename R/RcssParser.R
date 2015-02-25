##
## Function part of Rcssplot package
##
## Parser or Rcss grammar
## Call RcssParser() - the others are helper functions)
##
## Author: Tomasz Konopka
## 

##
## Function parses a text file assuming it is in css format.
## Function transfers css information into css object 
##
## file - file on disk
##
## returns - a rcss object augmented by setting within the file
##
RcssParser <- function(file) {

  if (!file.exists(file)) {
    stop("Rcssplot: error reading css file (file does not exist)\n")
  }
  
  ## Load the Rcss file, parse it into a tree
  lex <- RcssLexer(file)
  parsetree <- RcssMakeParseTree(lex)
  
  ## That's it, return the parsetree
  return(parsetree)  
}




## ################################################################
## Helper functions

## Wrapper to produce generic errors
RcssParseError <- function(lextab, n, parsefunction, expecting) {
  stopCF(PSZ(parsefunction,": \n",
             "   expecting: ", expecting,"\n",
             "   got: ", lextab[n,"token"],"\n"))
}




## ################################################################
## Functions for top-level manipulations of parsed tokens
## and their conversion into an rcss object

## Converts a table of lexed tokens into a tree with
## slectors and declarations 
## 
## lextab - a data.frame with lexed tokens
##
RcssMakeParseTree <- function(lextab) {
  
  parsetree <- list()
  n <- 1
  
  ## Start Parsing until "n" is within the lex table
  while (n <= nrow(lextab)) {
    ruleset <- RcssParseRuleSet(lextab, n)
    parsetree[[length(parsetree)+1]] = ruleset$RuleSet
    n <- ruleset$n
  }
  
  return(parsetree)
}







## ################################################################
## Functions that parse at individual states


##
## Parse one rule set (Selectors { Declarations })
##
## lextab - data frame with tokens
## n - current row in the data frame
##
##
RcssParseRuleSet <- function(lextab, n) {
  ## This is straightforward as it requires a SelectorSet and a DeclarationSet
  selectors = RcssParseSelectorSet(lextab, n)
  declarations = RcssParseDeclarationSet(lextab, selectors$n)
  return(list(n = declarations$n,
              RuleSet = list(SelectorSet = selectors$SelectorSet,
                DeclarationSet = declarations$DeclarationSet)))
}



##
## Parse one selector (IDENT [ class]* | class)
## 
## lextab - data frame with tokens
## n - the current row in the data frame
##
RcssParseSelector <- function(lextab, n) {

  ## the output here will be an object name followed by classes
  ans <- c("")

  ## parse the initial IDENT object
  if (lextab[n, "token"] == ".") {
    ## this is ok too  
  } else if (lextab[n, "type"] == "IDENT") {
    ans[1] <- lextab[n, "token"]
    n <- n + 1
  } else {
    RcssParseError(lextab, n, "AA RcssParseSelector", ".")
  }
  
  ## after the first ident, can have classes
  while (n <= nrow(lextab) & lextab[n,"token"] == ".") {
    n <- n + 1
    if (lextab[n,"type"] == "IDENT") {
      ans[length(ans)+1] = PSZ(lextab[n, "token"])
      n <- n +1
    } else {
      RcssParseError(lextab, n, "RcssParseSelector", "IDENT")
    }
  }

  ## at this point, collected all IDENT and classes
  return(list(n = n, Selector = ans))  
}



##
## Parse a set of selectors (i.e. selector [ ',' selector ]*)
##
## lextab - data frame with tokens
## n - the current row in the data frame 
##
RcssParseSelectorSet <- function(lextab, n) {

  ans <- list()
  
  ## get the first selector and advance counter
  sel <- RcssParseSelector(lextab, n)
  ans[[1]] <- sel$Selector;
  n <- as.integer(sel$n);
  
  ## now parse other selectors as long as they are separated by ','
  while (n <= nrow(lextab) & lextab[n,"token"] == ",") {
    sel = RcssParseSelector(lextab, n + 1)
    ans[[length(ans) + 1]] = sel$Selector
    n <- sel$n
  }

  return(list(n = n, SelectorSet = ans));  
}



##
## Parse a set of declarations
## (expects to start with a '{' and end with a '}')
##
## lextab - data frame with tokens
## n - the current row in the data.frame to process
##
RcssParseDeclarationSet <- function(lextab, n) {

  ## check the parsing starts with an open brace
  if (lextab[n,"token"] != "{") {
    RcssParseError(lextab, n, "RcssParseDeclarationSet", "{");
  }
  
  n <- n + 1;
  ans <- list();

  ## keep reading declarations until hit a }
  while(n <= nrow(lextab) & lextab[n, "token"] != "}") {
    ## parse a property/expr pair
    exprprop = RcssParseDeclaration(lextab, n)
    
    ## format and store in a list
    ans[[length(ans) + 1]] <- exprprop$Expr    
    names(ans)[length(ans)] <- exprprop$Property
    
    ## advance the n counter over this property/expr pair
    n <- exprprop$n
  }

  ## when the loop ends, the current state is a "}"
  n <- n + 1
  return(list(n = n, DeclarationSet = ans));
}



##
## Parse one declaration (expects an IDENT)
##
RcssParseDeclaration <- function(lextab, n) {
  
  ## get name of the property, then move over the property
  property <- RcssParseProperty(lextab, n)
  n <- property$n
  property <- property$Property
  
  ## move over the ':'
  if (lextab[n,"token"] != ":") {
    RcssParseError(lextab, n, "RcssParseDeclaration", ":");
  }
  n <- n + 1
  
  ## keep reading the declarations until hit a ';' or a '}'
  expr <- c()
  while (n <= nrow(lextab) &
         lextab[n,"token"] != ";" & lextab[n, "token"] != "}") {
    
    ## allowed tokens are IDENT, NUMBER, HEXCOLOR, STRING
    if (lextab[n, "type"] %in% c("IDENT", "STRING", "HEXCOLOR")) {
      expr[length(expr) + 1] <- lextab[n,"token"]
    } else if (lextab[n, "type"]=="NUMBER") {
      expr[length(expr)+1] <- as.numeric(lextab[n,"token"])
    } else {
      RcssParseError(lextab, n, "RcssParseDeclaration",
                     "IDENT|NUMBER|HEXCOLOR|STRING")
    }

    n <- n + 1    
  }

  ## if current token is an end of declaration, skip over it
  if (lextab[n, "token"] == ";") {
    n <- n + 1
  }

  ## return an object with the property/expr pair, but also
  ## the number of the next non-trivial token
  ans <- list(n = n, Property = property, Expr = expr)
  return(ans)
}




## parse the name of a property
## This is usually one token,
## but if a property has dots (e.g. cex.axis) this function
## will concatenate the text together
RcssParseProperty <- function(lextab, n) {

  if (lextab[n,"type"] != "IDENT") {
    RcssParseError(lextab, n, "RcssParseProperty", "IDENT");
  }

  ## deal with the expected case (one token)
  property <- lextab[n, "token"]
  n <- n + 1

  ## deal with extension of property when the next tokens are ".IDENT"
  while (n < nrow(lextab) &
         lextab[n,"type"] == "TERMINAL" & lextab[n, "token"] == ".") {
    
    if (lextab[n + 1, "type"] == "IDENT") {
      property <- c(property, ".", lextab[n + 1, "token"])
    }
    
    n <- n + 2;
  }
  
  ## build a unified property
  property <- paste(property, collapse = "")

  return(list(n = n, Property = property))
}
