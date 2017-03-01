#!/usr/bin/env Rscript
# USAGE:
# cat mkSATgraph.R | R --vanilla --quiet --args benchmark-2014-05-24T103329

args <- commandArgs(trailingOnly = T)

mkg <- function (filename) {
    df = read.table(filename, header=F, sep=",")
    makeGraphFixed(df, filename)
}

makeGraphFixed <- function (x, filename) {
    p =  "problem"
    s = "scale"
    c = "conflict"
    outfile = makeOutputFilename(filename, x$problem, s, c, "pdf")
    pdf(outfile)
    ptext = paste(p, "E", sep="")
    xl = "T"
    yl = "max decision level"
    # plot(x[[1]], x[[2]], pch=20, cex=0.3, xlab=xl, ylab=yl, ylim=c(-40,10))
    plot(x[[1]], x[[2]], pch=20, cex=0.3, xlab=xl, ylab=yl)
    title(filename)
    dev.off()
    list(outfile)
    outfile[[1]]
}

makeOutputFilename <- function (inPath, p, s, c, suffix) {
		   inFileVec = strsplit(inPath, "/")[[1]]
		   inFile = inFileVec[[length(inFileVec)]]
		   inBase = sub("(.*)\\.[^.]*", "\\1", inFile)
		   paste(inBase, ".", suffix, sep="")
}		   

mkg(args[1])
