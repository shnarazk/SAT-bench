#!/usr/bin/env Rscript
# USAGE:
# R_LIBS_USER=~/Sources/R
# cat mkSATgraph.R | R --vanilla
library("ggplot2")

getData <- function (t, p) {
	df1 = read.csv(as.character(t[[1]]), header=T, sep=",", comment="#")
	df2 = df1[order(df1[,4]),]         	# sort by time (the 4th column)
	if (p && t[[2]] != "") { df2[1] = sub("^ +", "", as.character(t[[2]])) }
	df2[[2]] = 1:nrow(df2)            	# reassign rownumbers
	df2
	}

graph <- function (d = merged) {
      g <- ggplot(merged, aes(num, time, color=solver))
      g <- g + geom_point()
      g <- g + geom_line(data=merged,size=0.6)
      g <- g + theme(legend.position = c(0.38,0.5), legend.justification = c(1,0))
      g <- g + scale_x_continuous(limits=c(0,130),breaks=seq(0,130,10))
      g <- g + scale_y_continuous(limits=c(0,1210) ,breaks=seq(0,1250,200))
      g <- g + xlab("#solved") + ylab("execution time [sec]") # + ggtitle("Cactus Plot on SAT-Race 2015 main track subset")
      print(g)
}

merged = list()

args <- commandArgs(trailingOnly=TRUE)
if (0 < length(args)){
    exps = args[1]
    targetPDF=paste("cactus-SR15m131-", exps, ".pdf", sep="")
    targetPNG=paste("cactus-SR15m131-", exps, ".png", sep="")
  } else {
     exps = "runs"
     targetPDF="cactus-SR15m131.pdf"
     targetPNG="cactus-SR15m131.png"
  }
runs <- read.csv(exps, comment="#", sep=",", header=F)
withTag <- 1 < ncol(runs)
for (i in seq(nrow(runs))) { merged = rbind(merged, getData(runs[i,], withTag)); }

pdf(targetPDF, width=10, height=7)
graph(merged)
ggsave(targetPDF)
ggsave(file=targetPNG, width=9, height=6, dpi=200)
dev.off()
