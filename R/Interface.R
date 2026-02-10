##' @title Virtual interface for mapped tabular objects
##' @description Pure abstract Definition.
##' @name Breeder-class
##' @exportClass Breeder
##' @seealso [SplitPlot()] for a the in-memory concrete implementation.
##' @field data Tabulated data instance.
##' @field log  Character vector history.
##' @import methods
##' @examples
##' data(std1997, package = "yabaf")
##' b <- Breeder(std1997)
##' class(b)
##' @export Breeder
Breeder <- setRefClass(
    Class = "Breeder",
    fields = c("data" = "data.frame", "log" = "character"),
    methods = list(
        initialize = function(d) {
            if(inherits(d, "data.frame"))
                .self$data <- d
            else if(inherits(d, "character"))
                .self$data <- jsonlite::fromJSON(d)
            .self$log <- setNames(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "start")
            },
        asData = function() {
            d <- .self$data
            attr(d, "log") <- .self$log
            return(d)
        },
        appendLog = function(event) {
            .self$log <- c(.self$log, setNames(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), event))
        }
    )
)

##' @title Concrete interface for Split Plot designs 
##' @description Sample of Concrete Definition.
##' @name SplitPlot-class
##' @exportClass SplitPlot
##' @seealso [Breeder()] for the pure virtual abstract template.
##' @field report list or analytical summary.
##' @import methods lme4 emmeans
##' @examples
##' data(std1997, package = "yabaf")
##' b <- Breeder(std1997)
##' s <- SplitPlot(b)
##' s$report$fit$sigma
##' @export SplitPlot
SplitPlot <- setRefClass(
    Class = "SplitPlot",
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
            fit <- lme4::lmer(response ~  plot * sub + (1 | block) + (1 | block:plot),
                              data = .self$data)
            ll   <- as.numeric(logLik(fit))
            reml <- lme4::REMLcrit(fit)
            sigma <- sigma(fit)
            anova <- emmeans::joint_tests(fit, lmer.df = "kenward-roger")
            vc <- lme4::VarCorr(fit)
            mplot <- emmeans::emmeans(fit, specs = ~ plot, lmer.df = "kenward-roger")
            msub <- emmeans::emmeans(fit, specs = ~ sub, lmer.df = "kenward-roger")
            cplot <- emmeans::emmeans(fit, specs = ~ plot, by = "sub", lmer.df = "kenward-roger")
            csub <- emmeans::emmeans(fit, specs = ~ sub, by = "plot", lmer.df = "kenward-roger")
            means <- list("mplot" = as.data.frame(mplot), 
                          "msub" = as.data.frame(msub),
                          "cplot" = as.data.frame(cplot),
                          "csub" = as.data.frame(csub))
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
    Class = "SplitPlot",
    method = function(object) {
        if (all(c("block", "plot", "sub", "response") %in% names(object$data))) {
            TRUE
        } else {
            "split plot must contain block, plot, sub and response data"            
        }
    }
)
