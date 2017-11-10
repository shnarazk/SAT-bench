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

graph <- function (d, subt) {
      g <- ggplot(d, aes(num, time, color=solver, size=solver, alpha=solver))
      g <- g + geom_point(size=0.5)
      g <- g + geom_line(data=d)
      g <- g + scale_size_manual(values=rep(0.6, times=nrow(d)))
      g <- g + scale_alpha_manual(values=rep(1.0, times=nrow(d)))
#     g <- g + scale_size_manual(values=c(0.7,0.7,0.7,0.7,0.3,0.3,0.3))  -- by aes(size=solver)
#     g <- g + scale_alpha_manual(values=c(1.0,1.0,1.0,1.0,0.5,0.5,0.5)) -- by aes(alpha=solver)
      g <- g + theme(legend.position = c(0.88,0.1), legend.justification = c(1,0))
      g <- g + scale_x_continuous(limits=c(0,350), breaks=seq(00,350,10), expand=c(0,4))
      g <- g + scale_y_continuous(expand=c(0,4))
#      g <- g + scale_y_continuous(limits=c(0,410) ,breaks=seq(0,410,100))
      if (subt == "") {
         g <- g + labs(title="Cactus plot on SAT-Race 2017 Main track (a short timeout)"
	      	       , x="#solved", y="execution time [sec]") }
      else {
         g <- g + labs(title="Cactus plot on SAT-Race 2017 Main track (a short timeout)"
	               , subtitle=subt
	      	       , x="#solved", y="execution time [sec]") }
      print(g)
}

merged = list()

args <- commandArgs(trailingOnly=TRUE)
subt <- ""
if (0 < length(args)){
    exps <- args[1]
    name <- gsub("\\.[^.]+$", "", exps)
    targetPDF <- paste("cactus-", name, ".pdf", sep="")
    targetPNG <- paste("cactus-", name, ".png", sep="")
    if (1 < length(args)) { subt <- args[2] }
  } else {
     exps <- "runs"
     name <- "runs"
     targetPDF <- "cactus-SC17main.pdf"
     targetPNG <- "cactus-SC17main.png"
  }
runs <- read.csv(exps, comment="#", sep=",", header=F)
withTag <- 1 < ncol(runs)
for (i in seq(nrow(runs))) { merged = rbind(merged, getData(runs[i,], withTag)); }

pdf(targetPDF, width=10, height=7)
graph(merged)
ggsave(targetPDF)
ggsave(file=targetPNG, width=9, height=6, dpi=200)
dev.off()
