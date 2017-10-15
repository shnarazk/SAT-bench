#!/usr/bin/env Rscript
# USAGE:
# R_LIBS_USER=~/Sources/R
# cat mkSATgraph.R | R --vanilla
library("ggplot2")

graph <- function (d, lx, ly) {
      names <- colnames(d)
      g <- ggplot(d, aes(X, Y))
      g <- g + geom_point()
      g <- g + stat_function(fun = function(x) x)
#      g <- g + theme(legend.position = c(0.88,0.1), legend.justification = c(1,0))
#      g <- g + scale_x_continuous(limits=c(0,1000),breaks=seq(0,1100,100))
#      g <- g + scale_y_continuous(limits=c(0,810) ,breaks=seq(0,810,100))
#      g <- g + xlab("#solved") + ylab("execution time [sec]") # + ggtitle("Cactus Plot on SAT-Race 2017 main track small subset")
       g <- g + xlab(lx) + ylab(ly)
      print(g)
}

args <- commandArgs(trailingOnly=TRUE)
if (2 == length(args)){
    dataX <- read.csv(args[1], header=T, sep=",", comment="#")
    dataY <- read.csv(args[2], header=T, sep=",", comment="#")
    l <- min(nrow(dataX), nrow(dataY))
    dataX <- dataX[seq(1,l),]
    dataY <- dataY[seq(1,l),]
    data <- cbind(dataX["num"], dataX[4], dataY[4])
    colnames(data) <- c("num", "X", "Y")
    targetPDF=paste("scatter", ".pdf", sep="")
    targetPNG=paste("scatter", ".png", sep="")
  } else {
  print("USAGE: scatter.R data1 data2")
  }

pdf(targetPDF, width=10, height=7)
graph(data,args[1], args[2])
ggsave(targetPDF)
ggsave(file=targetPNG, width=9, height=6, dpi=200)
dev.off()
