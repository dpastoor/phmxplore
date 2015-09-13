#' Xplore_database
#' @export
xplore_database <- setClass("xplore",
                            slots = c(
                                # from tabular output
                                ConvergenceData = "data.frame",
                                Doses           = "data.frame",
                                Eta             = "data.frame",
                                EtaCov          = "data.frame",
                                EtaCovariate    = "data.frame",
                                EtaCovariateCat = "data.frame",
                                EtaEta          = "data.frame",
                                EtaStacked      = "data.frame",
                                NonParEta       = "data.frame",
                                NonParOverall   = "data.frame",
                                NonParStacked   = "data.frame",
                                NonParSupport   = "data.frame",
                                Omega           = "data.frame",
                                OmegaStderr     = "data.frame",
                                Overall         = "data.frame",
                                Residuals       = "data.frame",
                                Secondary       = "data.frame",
                                StrCovariate    = "data.frame",
                                StrCovariateCat = "data.frame",
                                Theta           = "data.frame",
                                ThetaCovariance = "data.frame",
                                VarCovar        = "data.frame",
                                Notes           = "character",
                                ObsName         = "character",
                                ObsUnits        = "character",
                                # from dmp.txt
                                coefficients   = "list",
                                logLik          = "numeric",
                                nObs            = "numeric",
                                nSubj           = "numeric",
                                nParm           = "numeric",
                                returnCode      = "numeric",
                                omega           = "matrix",
                                method          = "character",
                                varFix          = "matrix",
                                Covariance      = "matrix",
                                Correlation     = "matrix",
                                Inverse         = "matrix",
                                idTable         = "data.frame",
                                residuals       = "data.frame",
                                test.mdl        = "character",
                                cols1.txt       = "character"
                            )
)


#' @export
setMethod("show",
          signature = "xplore",
          definition = function(object){
              cat("An object of class ", class(object), "\n", sep = "")
              cat("Contains ",
                  length(unique(object@Doses[["ID"]])),
                  " unique ID's", " for ",
                  length(unique(object@Doses[["amt"]])),
                  " dose level(s): ", unique(object@Doses[["amt"]]), ", for ",
                  length(unique(object@Doses[["REP"]])), " Reps", "\n", sep = "")
              cat("Notes associated with run: ", object@Notes)
          })

#' get_residuals
#' @export
setGeneric("get_residuals", function(object,...) standardGeneric("get_residuals"))

#' getResiduals
#' @export
setMethod(f="get_residuals",
          signature="xplore",
          definition= function(object) object@Residuals)
#' Notes
#' @export
setGeneric("Notes", function(object,...) standardGeneric("Notes"))

setMethod(f="Notes",
          signature="Xplore",
          definition= function(object) object@Notes)
#' Notes<-
#' @export
setGeneric("Notes<-", function(object, value) standardGeneric("Notes<-"))
setReplaceMethod(f="Notes",
                 signature="Xplore",
                 definition= function(object, value) {object@Notes <- value
                 return(object)})
# Notes(xpldb) <- "new note 2"
# Notes(xpldb)
#
# residuals <- head(get_residuals(xpldb))
# setMethod(
#   f = "[",
#   signature = "xplore",
#   definition = function(x, i, j, ..., drop="missing") {
#             .Residuals <- x@Residuals[i, j]
#             xplore_database(Residuals = .Residuals)