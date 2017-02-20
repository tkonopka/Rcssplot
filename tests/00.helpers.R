## Helper functions used in Rcssplot tests
##

library("Rcssplot")

## naive check function
## uses == and !=, i.e. only works for simple inputs
checkcat = function(observed, expected, label="") {
    ok = TRUE;
    if (length(expected)!=length(observed)) {
        ok = FALSE;
    } else {
        for (i in 1:length(observed)) {
            if (observed[i]!=expected[i]) {
                ok = FALSE;
            }
        }
    }    
    if (!ok) {
        cat(paste0(label, ":\tFAIL\n"));
    } else {
        cat(paste0(label, ":\tOK\n"))
    }
}


cattitle = function(x, n=40) {
  cat(x,"\n")
  cat(paste(rep("-",n), collapse=""), "\n")
}
