##' @title Concrete interface for Lattice designs
##' @description Specialized Concrete Definition.
##' @name Lattice-class
##' @exportClass Lattice
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods lme4 emmeans
##' @examples
##' data(gg1984, package = "yabaf")
##' b <- Breeder(gg1984)
##' l <- Lattice(b)
##' l$report$fit$sigma
##' @export Lattice
Lattice <- setRefClass(
    Class = "Lattice",
    contains = "Breeder",
    fields = c("report" = "list", "layout" = "logical", "check" = "logical"),
    methods = list(
        initialize = function(d) {
            
            if(inherits(d, "Breeder")) {
                
                .self$data <- d$data
                .self$log <- d$log
                .self$layout <- .self$check <- FALSE

                if (!is.factor(.self$data$replication))
                    .self$data$replication <- factor(.self$data$replication)
                
                if (!is.factor(.self$data$block))
                    .self$data$block <- factor(.self$data$block)

                if (!is.factor(.self$data$treat))
                    .self$data$treat <- factor(.self$data$treat)

                if (all(c("row", "column") %in% names(.self$data))) {

                    if (all(!is.na(.self$data$row)) & all(!is.na(.self$data$column))) {

                        .self$data$row <- factor(.self$data$row)
                        .self$data$column <- factor(.self$data$column)
                        .self$data <- .self$data[with(.self$data, order(row, column)), ]
                        .self$layout <- TRUE
                        
                    } else {
                        
                        .self$layout <- FALSE
                        
                    }

                }

                if ("role" %in% names(.self$data)) {

                    if (all(!is.na(.self$data$role))) {

                        .self$data$role <- factor(.self$data$role)

                        if (all(levels(.self$data$role) %in% c("check", "test"))) 
                            .self$check <- TRUE
                    
                    } else {
                        
                        .self$check <- FALSE
                        
                    }

                }
                
                .self$report <- .self$analyze()
                
            }
            
        },
        analyze = function() {

            asreml::asreml.options(maxit = 100, workspace = 3E8, pworkspace = 1E8, trace = FALSE)
            
            if (.self$layout & .self$check) {

                fit <- asreml::asreml(response ~ role + at(role, "check"):treat,
                                      random = ~ replication + replication:block + at(role, "test"):treat,
                                      residual = ~ ar1(row):ar1(column),
                                      data = .self$data)

                checks <- subset(predict(fit, classify = "role:treat", levels = list(role = "check"))$pvals,
                                 status == "Estimable", select = -status)
            
                tests <- subset(predict(fit, classify = "role:treat", levels = list(role = "test"))$pvals,
                                !(treat %in% as.character(checks$treat)) & status == "Estimable", select = -status)

            } else if (.self$layout & !(.self$check)) {

                fit <- asreml::asreml(response ~ 1,
                                      random = ~ replication + replication:block + treat,
                                      residual = ~ ar1(row):ar1(column),
                                      data = .self$data)

                means <- subset(predict(fit, classify = "treat")$pvals,
                                status == "Estimable", select = -status)
                
            } else if (!(.self$layout) & .self$check) {

                fit <- asreml::asreml(response ~ role + at(role, "check"):treat,
                                      random = ~ replication + replication:block + at(role, "test"):treat,
                                      data = .self$data)

                checks <- subset(predict(fit, classify = "role:treat", levels = list(role = "check"))$pvals,
                                 status == "Estimable", select = -status)
            
                tests <- subset(predict(fit, classify = "role:treat", levels = list(role = "test"))$pvals,
                                !(treat %in% as.character(checks$treat)) & status == "Estimable", select = -status)

            } else if (!(.self$layout) & !(.self$check)) {

                fit <- asreml::asreml(response ~ 1,
                                      random = ~ replication + replication:block + treat,
                                      data = .self$data)

                means <- subset(predict(fit, classify = "treat")$pvals,
                                status == "Estimable", select = -status)

            }

            ll <- summary(fit)$loglik
            reml <- -2 * ll
            sigma <- summary(fit)$sigma
            anova <- asreml::wald(fit)
            vc <- summary(fit)$varcomp

            if (.self$check)
                means <- rbind(checks, tests)
                                                
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
    Class = "Lattice",
    method = function(object) {
        if (all(c("replication", "block", "treat", "response") %in% names(object$data))) {
            TRUE
        } else {
            "lattice must contain replication, block, treat and response data"            
        }
    }
)
