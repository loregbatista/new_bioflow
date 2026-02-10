library(yabaf)
library(shiny)

front <- fluidPage(
    titlePanel("Single trial analysis — one-click interface"),    
    fileInput("file", "Select CSV file with: block, plot, sub, response and index (optional)",accept = ".csv"),
    downloadButton("report", "analysis report as RData")
)

back <- function(input, output) { 
    reply <- reactiveVal(NULL)
    observe({
        req(input$file)        
        tryCatch({
            data <- read.csv(input$file$datapath)
            if ("index" %in% names(data)) {
                chuncks <- lapply(split(subset(data, select = -index), data$index), yabaf::Breeder)
                analyzes <- lapply(chuncks, yabaf::SplitPlot)
                reply(lapply(analyzes, function(chunck) chunck$asList()))
            } else {
                breeder <- yabaf::Breeder(data)
                analysis <- yabaf::SplitPlot(breeder)
                reply(analysis$asList())
            }
        }, error = function(e) {
            reply(NULL)
        })
    })
    output$report <- downloadHandler(
        filename = function() gsub("\\.csv$", "_report.RData", input$file$name),
        content = function(file) {
            req(reply())
            report <- reply()
            save(report, file = file, compress = TRUE)
        }
    )
}

shinyApp(ui = front, server = back)
