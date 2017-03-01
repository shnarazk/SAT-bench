#!/usr/bin/env Rscript
# USAGE:
# cat tmkgraph.R | R --vanilla --quiet --args benchmark-2014-05-24T103329

args <- commandArgs(trailingOnly = T)
infile <- paste(args[1], "df", sep=".")

mkg <- function (filename) {
    df = read.table(filename, header=T, sep=",")
    pcat = split(df, df$problem)
    o = lapply(pcat, function(p) {
        scat = split (p, p$scale)
	lapply(scat, function(s) {
	    ccat = split(s, s$conflict)
	    q = lapply(ccat, function(run) makeGraphFixed(run, filename))
	    lapply(q, function(x) x[[1]][[1]])
	    })
        })
    unlist(lapply(unique(o), function(x) x[[1]][[1]]))
}

makeGraph <- function (x, filename) {
    p = x[[1, "problem"]]
    s = as.character(x[[1,"scale"]])
    c = as.character(x[[1, "conflict"]])
    outfile = makeOutputFilename(filename, x$problem, s, c, "pdf")
    pdf(outfile)
    plot(x$weight, x$time,type="l", log="y", xlab="weight for backtrack ratio (-s)", ylab="time [sec]")
    title(paste("Execution time of", p), paste("scale = ", s, ", conflict = ", c))
    abline(h = x$time[1],col="red")
    text(0.005, x$time[1] + 0.05 * (max(x$time) - min(x$time)), as.character(min(x$time[1])),col="red")
    abline(h=min(x$time), col="blue")
    text(0.005, min(x$time) + 0.05 * (max(x$time) - min(x$time)), as.character(min(x$time)),col="blue")
    dev.off()
    list(outfile)
}

makeGraphFixed <- function (x, filename) {
    p = x[[1, "problem"]]
    s = as.character(x[[1,"scale"]])
    c = as.character(x[[1, "conflict"]])
    outfile = makeOutputFilename(filename, x$problem, s, c, "pdf")
    pdf(outfile)
    ptext = paste(p, "E", sep="")
    xl = "weight for backtrack ratio (-s)"
    yl = "time [sec]"
    switch (ptext,
        "UF125E" = plot(x$weight, x$time,type="l", xlab=xl, ylab=yl, ylim=c(5,10)),
	"UF150E" = plot(x$weight, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,20)),
	"UF175E" = plot(x$weight, x$time,type="l", xlab=xl, ylab=yl, ylim=c(35,50)),
	"UF200E" = plot(x$weight, x$time,type="l", xlab=xl, ylab=yl, ylim=c(100,200)),
		plot(x$weight, x$time,type="l", xlab=xlabel, ylab=ylabel)
	)
    title(paste("Execution time of", p), paste("scale = ", s, ", conflict = ", c))
    abline(h = x$time[1],col="red")
    labx = 0.02 * (max(x$weight) - min(x$weight))
    offsety = 0.05 * (max(x$time) - min(x$time))
    text(labx, x$time[1] + offsety, as.character(min(x$time[1])),col="red")
    abline(h=min(x$time), col="blue")
    text(labx, min(x$time) + offsety, as.character(min(x$time)),col="blue")
    switch (ptext,
	"UF125E" = abline(h=5.15, col="green"),
	"UF150E" = abline(h=11.63, col="green"),
	"UF175E" = abline(h=42.00, col="green"),
	"UF200E" = abline(h=133.68, col="green"),
	"UF225E" = abline(h=379.67, col="green"),
	"UF250E" = abline(h=2292.09, col="green"),
	"ibmE" = abline(h=13.98, col="green"),
	"eenE" = abline(h=24.03, col="green"),
	      text(5, min(x$time) + 0.05 * (max(x$time) - min(x$time)), p,col="black")
	)
    dev.off()
    list(outfile)
}

makeOutputFilename <- function (inPath, p, s, c, suffix) {
		   inFileVec = strsplit(inPath, "/")[[1]]
		   inFile = inFileVec[[length(inFileVec)]]
		   inBase = sub("(.*)\\.[^.]*", "\\1", inFile)
		   paste(inBase, "--", p, "-scale_", s, "-conflict_", c, ".", suffix, sep="")
}		   

# oldMakeGraph("benchmark-2014-05-24T103329.df", "UF200", 1.5, 10)
oldMakeGraph <- function (filename, key, i, c) {
    df = read.table(filename, header=T, sep=",")
    x = subset(subset(df, problem == key), scale == i && conflict == c)
    pdf(file=paste(filename, "-", key, ".pdf", sep=""))
    plot(x$weight, x$time,type="l", log="y", xlab="weight for backtrack ratio (-s)", ylab="time [sec]")
    title(paste("Execution time of", key), paste("scaling = ", as.character(i), ", conflict = ", as.character(c)))
    abline(h = x$time[1],col="red")
    text(0.005, x$time[1] + 0.05 * (max(x$time) - min(x$time)), as.character(min(x$time[1])),col="red")
    abline(h=min(x$time), col="blue")
    text(0.005, min(x$time) + 0.05 * (max(x$time) - min(x$time)), as.character(min(x$time)),col="blue")
    #    dev.copy(pdf, file=paste(file, "-", key, ".pdf", sep=""))
    dev.off()
}

mkg(infile)
# mkg("benchmark-2014-05-24T103329.df")
