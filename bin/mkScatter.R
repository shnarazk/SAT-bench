#!/usr/bin/env Rscript
# USAGE: R_LIBS_USER=~/Sources/R scatter.R data1.csv data2.csv [graphName]")
library("ggplot2")
version="0.1"

drawGraph = function (d, lx, ly, subt) {
    names <- colnames(d)
    title <- paste("Scatter comparison", " with mkScatter (version ", version, ")", sep="")
    g <- ggplot(d, aes(X, Y))
    g <- g + geom_point()
    g <- g + stat_function(fun = function(x) x)
#   g <- g + theme(legend.position = c(0.88,0.1), legend.justification = c(1,0))
#   g <- g + scale_x_continuous(limits=c(0,1000),breaks=seq(0,1100,100))
#   g <- g + scale_y_continuous(limits=c(0,810) ,breaks=seq(0,810,100))
#   g <- g + xlab("#solved") + ylab("execution time [sec]") # + ggtitle("Comparison of two results of the benchmark")
    g <- g + labs(title=title, subtitle=subt, x=lx, y=ly)
#    g <- g + xlab(lx) + ylab(ly)
    print(g)
}

(function () {
    args <- commandArgs(trailingOnly=TRUE)
    if (2 <= length(args)){
        data1 <- args[1]
        data2 <- args[2]
        dataX <- read.csv(data1, header=T, sep=",", comment="#")
        dataY <- read.csv(data2, header=T, sep=",", comment="#")
        dataX = dataX[order(dataX[,2]),]         	# sort by num
        dataY = dataY[order(dataY[,2]),]         	# sort by num
        l <- min(nrow(dataX), nrow(dataY))
        dataX <- dataX[seq(1,l),]
        dataY <- dataY[seq(1,l),]
        data <- cbind(dataX["num"], dataX[4], dataY[4])
        colnames(data) <- c("num", "X", "Y")
    } else {
        print("USAGE: scatter.R data1.csv data2.csv [graphName]")
    }
    subt <- paste(" X : ", data1, "\n Y : ", data2, sep="")
    if (3 <= length(args)){
        name <- args[3]
        subt <- name
        targetPDF <- paste("scatter-", name, ".pdf", sep="")
        targetPNG <- paste("scatter-", name, ".png", sep="")
    } else {
        targetPDF <- "scatter.pdf"
        targetPNG <- "scatter.png"
    }
    cairo_pdf(filename=targetPDF, width=10, height=8, antialias="subpixel", onefile=TRUE)
    drawGraph(data, data1, data2, subt)
    ggsave(filename=targetPDF, width=10, height=8)
    ggsave(filename=targetPNG, width=10, height=8, dpi=200)
})()
