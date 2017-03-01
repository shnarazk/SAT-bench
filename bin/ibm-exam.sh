#!/bin/sh

# 2014-08-18 
# FIT2014発表の RestartByConflict （将来的には RestartOfRestart） を使うと、構造
# 化問題の中でもibmは非常に性能が悪くなることがある。その理由を調べるために
# 最大決定レベルの変化をグラフ化してみることにした。

# 2014-08-25T09:36:33.00+9:00
# パラメータによって最大決定レベルの発展は大きく3種類に分類できることがわかった。
# http://text.is-saved.org/2014/08/24/3-types-of-sat/
# これによれば、毎回の初期最大決定レベルに合わせて、スパン長（あるいは傾き）、
# 閾値を決定することが必要だと思われる。

export LC_ALL=C export TIMEFORMAT=" %2R "

GITREP="."
SOLVER="sih4-4.0.4"
OPTS=" +RTS -K240M -RTS "
LOPTS=" +RTS -K600M -RTS "
ARANGE="1.0 1.1 1.2"
BRANGE="1.9 2.0 2.1" # `seq 1.1 0.1 2.2`
CRANGE="0.05 0.10 0.15 0.20" # `seq 0 0.05 0.4`

while getopts A:B:C:g:s:o:O: OPT
do
    case $OPT in
	A) ARANGE=$OPTARG ;;
	B) BRANGE=$OPTARG ;;
	C) CRANGE=$OPTARG ;;
	g) GITREP=$OPTARG ;;
	s) SOLVER=$OPTARG ;;
	o) OPTS="$OPTS $OPTARG" ;;
	O) OPTS=$OPTARG ;;
	?)
	    echo "sih4-lab for sih4-4.0.3.1 or upper"
	    echo "$0 [-A range] [-B range] [-C range] [-[oO] sih4-options] -g gitdir -s solver"
	    exit 1
	    ;;
    esac
done

shift $((OPTIND - 1))

IBM=$HOME/SATbench/SAT-Race_TS_1/stric-bmc-ibm-10.cnf

RUNAT=`date --iso-8601=seconds | sed -r "s/\+[0-9]+//" | sed -r "s/://g"`
LOGFILE=ibm-mdl-${RUNAT}.log
FINISHED=$HOME/Nash/bin/finished

# start logging
echo "sih4 ibm MDL log file: ${LOGFILE} by $0"  | tee -a ${LOGFILE}
echo "${NTHREADS} thread run on `hostname` at `date --iso-8601=seconds`" | tee -a ${LOGFILE}
echo "solver = ${SOLVER}" | tee -a ${LOGFILE}
echo "# `${SOLVER} --version`" | tee -a ${LOGFILE}
echo -n "# designating " | tee -a ${LOGFILE}
ls -g -G `which ${SOLVER}` | sed 's/[-rwx]* [1-9] [1-9][0-9]* //' | tee -a ${LOGFILE}
echo "---------------------------------------- git diff"
(cd ${GITREP}; git diff) | tee -a ${LOGFILE}
echo "---------------------------------------- git log"
(cd ${GITREP}; git log) | head -6 | tee -a ${LOGFILE}
echo "----------------------------------------"
echo "" | tee -a ${LOGFILE}

for A in  ${ARANGE} ;
do
    for B in ${BRANGE} ;
    do
	for C in ${CRANGE} ;
	do
	    echo -n "${A}, ${B}, ${C}	" | tee -a ${LOGFILE}
	    (time ${SOLVER} ${OPTS} -X -A ${A} -B ${B} -C ${C} ${IBM}) 2>&1 | tee -a ${LOGFILE}
	    ${SOLVER} ${LOPTS} -D -L [19] -X -A ${A} -B ${B} -C ${C} ${IBM} | analyzeAssignLogToDF -l 2 > ibm-${A}-${B}-${C}.df
	done
    done
done

parallel -j-1 "mkSATgraphMDL {}" ::: ibm-*.df
pdfjoin -o ibm.pdf ibm-*.pdf
echo "done" | tee -a ${LOGFILE}
${FINISHED}
