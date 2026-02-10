#!/usr/bin/Rscript

request <- readLines(file("stdin"), n = 1) 
data <- jsonlite::fromJSON(request)

if ("index" %in% names(data$data)) {
    chuncks <- lapply(split(subset(data$data, select = -index), data$data$index), yabaf::Breeder)
    analyzes <- lapply(chuncks, yabaf::SplitPlot)
    reply <- lapply(analyzes, function(chunck) chunck$asList())
} else {
    breeder <- yabaf::Breeder(data$data)
    analysis <- yabaf::SplitPlot(breeder)
    reply <- analysis$asList()
}

cat("Content-type: text/plain\n\n")
cat(jsonlite::toJSON(reply))
cat("\n")
