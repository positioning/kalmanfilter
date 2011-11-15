print.kfsst <-
function (x, ...)
{
    headvec <- strsplit(x$header, split = "\n")[[1]]
    "%+%" <- function(s1, s2) paste(s1, s2, sep = "")
    out <- "\n\n#R-KFtrack fit\n" %+% headvec[2] %+% "\n" %+%
        "#Number of observations: " %+% x$nobs %+% "\n" %+% "#Negative log likelihood: " %+%
        x$nlog %+% "\n" %+% "#The convergence criteria was " %+%
        ifelse(abs(x$max.grad.comp) > 1e-04, "NOT ", "") %+%
        "met\n\n"
    cat(out)
    if (abs(x$max.grad.comp) < 1e-04) {
        cat("Parameters:\n")
        print(rbind("Estimates:" = x$estimates, "Std. dev.:" = x$std.dev))
        cat("\nThis object contains the following sub-items:\n")
        print(names(x))
    }
}

