##' @title Concrete interface for Randomized Complete Blocks
##' @description Specialized Concrete Definition.
##' @name Trial-class
##' @exportClass Trial
##' @seealso [Breeder() and SplitPlot()] for the pure virtual and concrete
##'     abstract template.
##' @field report list or analytical summary.
##' @import methods
##' @examples
##' data(gg1984, package = "yabaf")
##' gg1984$design <- "Lattice"
##' b1 <- Breeder(gg1984)
##' l <- Trial(b1)
##' l$report$fit$sigma
##' data(wheat, package = "yabaf")
##' wheat$design <- "Augmented"
##' b2 <- Breeder(wheat)
##' a <- ACBD(b2)
##' a$report$fit$sigma
##' @export Trial
Trial <- setRefClass(
    Class = "Trial",
    contains = "Breeder",
    fields = c("report" = "list", "design" = "character"),
    methods = list(
        initialize = function(d) {
            if(inherits(d, "Breeder")) {
                .self$data <- d$data
                .self$log <- d$log

                if (length(unique(.self$data$design)) == 1)
                    .self$design <- unique(.self$data$design)

                .self$report <- .self$analyze(.self$design)
                
            }
        },
        analyze = function(design) {

            out <- switch(design, 
                          Lattice = { Lattice(.self) },
                          Augmented = { ACBD(.self) },
                          { list() })

            .self$appendLog(event = "report")
            .self$report <- out$report
            
        },
        asList = function() {
            return(list(data = .self$data, report = .self$report, log = .self$log))
        }
    )
)
