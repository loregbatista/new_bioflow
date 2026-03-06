##' @title Concrete interface for Randomized Complete Blocks
##' @description Specialized Concrete Definition.
##' @name RCBD-class
##' @exportClass RCBD
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods lme4 emmeans
##' @examples
##' data(ergoStool, package = "nlme")
##' names(ergoStool) <- c("response", "treat", "block")
##' b <- Breeder(as.data.frame(ergoStool))
##' r <- RCBD(b)
##' r$report$fit$sigma
##' @export RCBD
RCBD <- setRefClass(
    Class = "RCBD",
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
            fit <- lme4::lmer(response ~ treat + (1 | block), data = .self$data)
            ll   <- as.numeric(logLik(fit))
            reml <- lme4::REMLcrit(fit)
            sigma <- sigma(fit)
            anova <- emmeans::joint_tests(fit, lmer.df = "kenward-roger")
            vc <- lme4::VarCorr(fit)
            means <- emmeans::emmeans(fit, specs = ~ treat, lmer.df = "kenward-roger")
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
    Class = "RCBD",
    method = function(object) {
        if (all(c("block", "treat", "response") %in% names(object$data))) {
            TRUE
        } else {
            "randomized blocks must contain block, treat and response data"            
        }
    }
)
