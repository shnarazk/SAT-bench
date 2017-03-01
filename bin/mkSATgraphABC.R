#!/usr/bin/env Rscript
# USAGE:
# cat tmkgraph.R | R --vanilla --quiet --args benchmark-2014-05-24T103329

args <- commandArgs(trailingOnly = T)
gType <- args[1]
infile <- paste(args[2], "df", sep=".")

colors=c("black", "red", "green", "blue", "purple", "gray","red", "green", "blue", "purple", "gray", "red", "green", "blue", "purple", "gray")
ltypes=c(1,1,1,1,1,1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
curveIndex = 0

mkg <- function (filename) {
    df = read.table(filename, header=T, sep=",")
    o = lapply(split(df, df$problem), function(p) {
        outfile = makeOutputFilename(filename, p$problem, "pdf")
	pdf(outfile)
	q = switch (gType,
		"AB" = lapply(split(p, p$C), function(run) makeGraphFixedAB(run, filename)),
		"AC" = lapply(split(p, p$B), function(run) makeGraphFixedAC(run, filename)),
		"CA" = lapply(split(p, p$B), function(run) makeGraphFixedCA(run, filename))
		)
	lapply(q, function(x) x[[1]][[1]])
	list(outfile)
        })
    dev.off()
    unlist(lapply(unique(o), function(x) x[[1]][[1]]))
}

makeGraphFixedAB <- function (x, filename) {
    p = x[[1, "problem"]]
    a = as.character(x[[1, "A"]])
    b = as.character(x[[1, "B"]])
    c = as.character(x[[1, "C"]])
    curveIndex <<- 0
    lines = split (x, x$B)
    lapply(lines, function(seq) makeGraphCurveA(seq, paste(p, "E", sep="")))
    title(paste("Execution time of", p), paste("parameter C =", c))
    names = sapply(lines, function(v) paste("B =", as.character(v$B[1])))
    legend('topright', legend=names, col=colors, bty='n', cex=0.75, lty =ltypes)
}

makeGraphFixedAC <- function (x, filename) {
    p = x[[1, "problem"]]
    a = as.character(x[[1, "A"]])
    b = as.character(x[[1, "B"]])
    c = as.character(x[[1, "C"]])
    curveIndex <<- 0
    lines = split (x, x$C)
    lapply(lines, function(seq) makeGraphCurveA(seq, paste(p, "E", sep="")))
    title(paste("Execution time of", p), paste("parameter B =", b))
    names = sapply(lines, function(v) paste("C =", as.character(v$C[1])))
    legend('topright', legend=names, col=colors, bty='n', cex=0.75, lty =ltypes)
}

makeGraphFixedCA <- function (x, filename) {
    p = x[[1, "problem"]]
    a = as.character(x[[1, "A"]])
    b = as.character(x[[1, "B"]])
    c = as.character(x[[1, "C"]])
    curveIndex <<- 0
    lines = split (x, x$A)
    lapply(lines, function(seq) makeGraphCurveC(seq, paste(p, "E", sep="")))
    title(paste("Execution time of", p), paste("parameter B =", b))
    names = sapply(lines, function(v) paste("A =", as.character(v$A[1])))
    legend('topright', legend=names, col=colors, bty='n', cex=0.75, lty =ltypes)
}

# draw along A
makeGraphCurveA <- function (x, ptext) {
    curveIndex <<- curveIndex + 1
    lclr = "black"
    lclr = colors[curveIndex]
    ltype = ltypes[curveIndex]
    xl = if (curveIndex == 1) "parameter A" else ""
    yl = if (curveIndex == 1) "time [sec]" else ""
    switch (ptext,
       "UF125E" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(4,10), col=lclr, lty=ltype),
       "UF150E" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,18), col=lclr, lty=ltype),
       "UF175E" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(30,50), col=lclr, lty=ltype),
       "UF200E" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(110,190), col=lclr, lty=ltype),
       "UF225E" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(400,900), col=lclr, lty=ltype),
       "eenE" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,300), col=lclr, lty=ltype),
       "ibmE" = plot(x$A, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,300), col=lclr, lty=ltype),
	plot(x$A, x$time,type="l", xlab=xl, ylab=yl)
	)
    if (curveIndex == 1) {
        #labx = 0.02 * (max(x$A) - min(x$A))
	#offsety = 0.05 * (max(x$time) - min(x$time))
	#abline(h=min(x$time), col="blue")
 	#text(labx, min(x$time) + offsety, as.character(min(x$time)),col="blue")
	switch (ptext,
	       "UF125E" = abline(h=8.843, col="gray"),
	       "UF150E" = abline(h=16.297, col="gray"),
	       "UF175E" = abline(h=43.352, col="gray"),
	       "UF200E" = abline(h=167.990, col="gray"),
	       "UF225E" = abline(h=451.72, col="gray"),
	       "UF250E" = abline(h=2741.00, col="gray"),
	       "eenE" = abline(h=14.86, col="gray"),
	       "ibmE" = abline(h=23.11, col="gray") #,
#		      text(5, min(x$time) + 0.05 * (max(x$time) - min(x$time)), p,col="black")
		)
	}
   par(new=T)
}

# draw along C
makeGraphCurveC <- function (x, ptext) {
    curveIndex <<- curveIndex + 1
    lclr = "black"
    lclr = colors[curveIndex]
    ltype = ltypes[curveIndex]
    xl = if (curveIndex == 1) "parameter C" else ""
    yl = if (curveIndex == 1) "time [sec]" else ""
    switch (ptext,
       "UF125E" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(4,10), col=lclr, lty=ltype),
       "UF150E" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,18), col=lclr, lty=ltype),
       "UF175E" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(30,50), col=lclr, lty=ltype),
       "UF200E" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(110,190), col=lclr, lty=ltype),
       "UF225E" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(400,900), col=lclr, lty=ltype),
       "eenE" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,300), col=lclr, lty=ltype),
       "ibmE" = plot(x$C, x$time,type="l", xlab=xl, ylab=yl, ylim=c(10,300), col=lclr, lty=ltype),
	plot(x$C, x$time,type="l", xlab=xl, ylab=yl)
	)
    if (curveIndex == 1) {
	switch (ptext,
	       "UF125E" = abline(h=8.843, col="gray"),
	       "UF150E" = abline(h=16.297, col="gray"),
	       "UF175E" = abline(h=43.352, col="gray"),
	       "UF200E" = abline(h=167.990, col="gray"),
	       "UF225E" = abline(h=451.72, col="gray"),
	       "UF250E" = abline(h=2741.00, col="gray"),
	       "eenE" = abline(h=14.86, col="gray"),
	       "ibmE" = abline(h=23.11, col="gray") #,
#		      text(5, min(x$time) + 0.05 * (max(x$time) - min(x$time)), p,col="black")
		)
	}
   par(new=T)
}

makeOutputFilename <- function (inPath, p, suffix) {
		   inFileVec = strsplit(inPath, "/")[[1]]
		   inFile = inFileVec[[length(inFileVec)]]
		   inBase = sub("(.*)\\.[^.]*", "\\1", inFile)
		   paste(inBase, "--", p, "--", gType, ".", suffix, sep="")
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
