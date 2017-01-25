# ----------------------------------------------------------- #
# Modified computeAUC function from drexplorer package v1.1.2 #
# ----------------------------------------------------------- #

# Original function can be found here:
# https://github.com/nickytong/drexplorer/blob/master/R/drexplorer.R
compAUC <- function (fit, curveid, dmin, dmax, ref = 1, islogd = TRUE) 
{
    f_response <- function(dose, islogd = F) {
        if (islogd) {
            dd <- 10^(dose)
        } else {
            dd <- dose
        }
        dd <- data.frame(dose=dd, CURVE=curveid)
        # print(dd)
        res <- predict(fit, dd)
        res
    }
    f_responseRef <- function(dose, reference = ref) {
        res <- rep(reference, length(dose))
        res
    }
    AUC <- try(integrate(f_response, lower = dmin, upper = dmax, 
        islogd = islogd)$value, silent = TRUE)
    AUC0 <- try(integrate(f_responseRef, lower = dmin, upper = dmax)$value, 
        silent = TRUE)
    if (inherits(AUC, "try-error")) {
        cat(AUC)
        AUC = NA
    }
    if (inherits(AUC0, "try-error")) {
        cat(AUC0)
        AUC0 = NA
    }
    res <- c(AUC = AUC, AUC0 = AUC0, AUCs = AUC/AUC0)
    res
}