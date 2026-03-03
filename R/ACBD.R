##' @title Concrete interface for ACBD designs
##' @description Specialized Concrete Definition.
##' @name ACBD-class
##' @exportClass ACBD
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods sommer emmeans
##' @examples
##' data(stg12025, package = "yabaf")
##' b <- Breeder(stg12025)
##' l <- ACBD(b)
##' l$report$fit$sigma
##' @export ACBD
ACBD <- setRefClass(
  Class = "ACBD",
  contains = "Breeder",
  fields = c("report" = "list"),
  methods = list(
    initialize = function(d) {
      if(inherits(d, "Breeder")) {
        .self$data <- d$data
        .self$log <- d$log
        .self$report <- .self$analyze()
      }
    },
    analyze = function() {
      fit <- sommer::mmer(response ~ treat + role, random=~block, rcov=~ar1(row):ar1(column),
                        data = .self$data)
      ll   <- as.numeric(fit$logLik)
      reml <- "REML"
      sigma <- sqrt(fit$sigma2)
      anova <- summary(fit)$fixed
      vc <- summary(fit)$varcomp
      means <- emmeans::emmeans(fit, specs = ~ treat)
      .self$appendLog(event = "report")
      .self$report <-  list("fit" = list("LogLik" = ll, "REML" = reml, "sigma" = sigma),
                            "anova" = anova,
                            "variance" = as.data.frame(vc),
                            "means" = means)
    },
    asList = function() {
      return(list(data = .self$data, report = .self$report, log = .self$log))
    }
  )
)

setValidity(
  Class = "ACBD",
  method = function(object) {
    if (all(c("role", "row", "column", "block", "treat", "response") %in% names(object$data))) {
      TRUE
    } else {
      "ACBD must contain role, row, column, block, treat and response data"            
    }
  }
)
