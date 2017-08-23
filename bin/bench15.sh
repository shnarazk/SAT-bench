#!/bin/sh
version="0.39"

# default vaules
BENCHDIR="$HOME/Documents/SAT-RACE"
DUMPDIR="$HOME/Documents/ownCloud/mios-exp"
GITDIR="$HOME/Repositories/mios15"
LogNumber=1
Benchsuit="SR15easy"
MiosExecutable="mios" # set if the name of executable is something like 'mios-1.3.0'
MiosOptions=""
timeout=1260
timestamp=`date --iso-8601 | sed -re 's/^[0-9]+-//'`
upload=`which syncCloud > /dev/null 2>&1; if [ $? = 0 ] ; then echo "syncCloud" ; else echo; fi`
SkipCompile=0

INSTALLOPTS=""
# INSTALLOPTS="--flag mios:devel"

help () {
    cmd=`basename $0`
    echo "Usage of ${cmd} (version ${version}): "
    echo " ${cmd} -r            - Run the bencmark suit"
    echo " ${cmd} -r -P SET     - Select dataset: 'SR15easy' or 'SR15m131' (default: '${Benchsuit}'"
    echo " ${cmd} -r -o 'OPTS'  - Set solver's options"
    echo " ${cmd} -r -i ID      - Select solver by commit id (skip the build phase)"
    echo " ${cmd} -r -e EXE     - Set the executable name to EXE (defualt: '${MiosExecutable}')"
    echo " ${cmd} -r -E EXE     - Use the executable name to EXE without compilation"
    echo " ${cmd} -r -B DIR     - Set the benchmark dir to DIR (default: '${BENCHDIR}')"
    echo " ${cmd} -r -D DIR     - Set the dump dir to DIR (default: '${DUMPDIR}')"
    echo " ${cmd} -r -G DIR     - Set the repository dir to DIR (default: '${GITDIR}')"
    echo " ${cmd} -r -t T       - Set timeout to T (default: ${timeout})"
    echo " ${cmd} -r -T         - Set timeout to 310"
    echo " ${cmd} -r -n N       - Set log sequence number to N (default: ${LogNumber})"
    echo " ${cmd} -r -S         - Force owncloud syhchronization"
    echo " ${cmd} -c            - Cat the current benchmark's result"
    echo " ${cmd} -g            - run mkCactus.R to make a graph"
    echo " ${cmd} -s            - sync the owncloud directory"
    echo " ${cmd} -k            - Kill the current benchmark"
    echo " ${cmd} -h            - Display this message"
 }

showLog () {
    if [ ! -f ${log} ] ; then
	log="${DUMPDIR}/${MiosWithId}-*-${LogNumber}.csv"
    fi
    cat ${log}
    echo "# end of $log"
}

mode="unknown"
forceSync=0
while getopts brcgsSTkhuli::n:e:E:o:P:t:B:D:G: OPT
do
    case $OPT in
	b) mode="build"
	   ;;
	r) mode="run"
	   ;;
	i)
	    id=$OPTARG
 	    mios="mios-${id}"
	    MiosWithId="mios-${id}"
	    ;;
	P) Benchsuit=$OPTARG
	   ;;
	o) MiosOptions=$OPTARG
	   ;;
	t) timeout=$OPTARG
	   ;;
	T) timeout="310"
	   ;;
	n) LogNumber=$OPTARG
	   ;;
	D) DUMPIDR=$OPTARG
	   ;;
	B) BENCHDIR="$OPTARG"
	   ;;
	D) DUMPDIR="$OPTARG"
	   ;;
	G) GITDIR="$OPTARG"
	   ;;
	e) MiosExecutable="$OPTARG"
	   ;;
	E) MiosExecutable="$OPTARG"
	   mode="run"
	   SkipCompile=1
	   ;;
	S) forceSync=1
	   ;;
	c) mode="log"
	   ;;
	s) mode="sync"
	   forceSync=1
	   ;;
	u) mode="sync"
	   forceSync=1
	   ;;
	g) mode="graph"
	   ;;
	k) echo "kill benchmark"
	   parallel -j1 "echo {} > /dev/null; pkill -9 mios" ::: `seq 1 300`
	   exit 0
	   ;;
	h) mode="help"
	   ;;
	*) mode="unknown"
	   ;;
    esac
done
shift $((OPTIND - 1))

case ${mode} in
    "build")
	;;
    "run")
	;;
    "sync")
	echo -n "sync dir..."
	eval ${upload} > /dev/null 2>&1
	echo "done."
	exit 0
	;;
    "graph")
	echo -n "make a graph..."
	eval "(cd ${DUMPDIR}; mkCactus131.R)"
	eval "(cd ${DUMPDIR}; mkCactusSU131.R)"
	if [ ${forceSync} == 1 ] ; then
	    eval ${upload} > /dev/null 2>&1
	fi
	exit 0
	;;
    "unknown")
	echo "?"
	if [ -f ${log} ] ; then
	    showLog
	else
	    help
	fi
	exit 0
	;;
    "help")
	help
	exit 0
	;;
    "log")
	showLog
	if [ ${forceSync} == 1 ] ; then
	    eval ${upload} > /dev/null 2>&1
	fi
	exit 0
	;;
esac

if [ ! -d "${GITDIR}" ] ;
then
    echo "No git directory: ${GITDIR}"
    exit -1
fi

if [ $(cd ${GITDIR}; git status | grep -q modified; echo $?) == "0" ] ; then
    if [ "${SkipCompile}" == "0" ] ; then
	echo "ABORT: the mios repository is not clean. Please commit before benchmark."
	exit 0
    fi
fi

# update variables
if [ $SkipCompile == "1" ] ;
then
    id=''
    MiosWithId="${MiosExecutable}"
else
    id=`cd ${GITDIR}; git log -1 --format="%h" HEAD`
    MiosWithId="${MiosExecutable}-${id}"
fi
log="${DUMPDIR}/${MiosWithId}-`date --iso-8601`-${LogNumber}.csv"

# echo "mode=${mode}"

if [ $mode = "build" ] ;
then
    if [ ! -f $HOME/.local/bin/${MiosWithId} ] ; then
	echo "building ${MiosWithId} ..."
	(cd ${GITDIR}; stack clean; stack install $INSTALLOPTS)
	mv $HOME/.local/bin/${MiosExecutable} $HOME/.local/bin/${MiosWithId}
	echo "done"
    fi
    ls -l $HOME/.local/bin/${MiosWithId}
    exit 0;
fi

# if there is no exectable, install
if [ ! -f $HOME/.local/bin/${MiosWithId} ] ; then
    (cd ${GITDIR}; stack clean; stack install $INSTALLOPTS)
    # set unique name
    mv $HOME/.local/bin/${MiosExecutable} $HOME/.local/bin/${MiosWithId}
fi

# run benchmark
if [ -f ${log} ] ;
then
    echo "Abort: ${log} exists now"
    exit 255
fi

# display configuration
echo "# Benchmark configuration:"
echo " * solver: `ls -l $HOME/.local/bin/${MiosWithId}`"
echo " * result: ${log}"

echo "\"`basename ${log}`\"" >> ${DUMPDIR}/runs

# run the benchmark
cd $BENCHDIR
if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1
else
    upload=""
fi
echo "cd $BENCHDIR; sat-benchmark -K '@${timestamp}' -t "${Benchsuit}/*.cnf" -T ${timeout} -o '${MiosOptions}' ${MiosWithId} > ${log}"
sat-benchmark -K "@${timestamp}" -t "${Benchsuit}/*.cnf" -T ${timeout} -o "${MiosOptions}" ${MiosWithId} > ${log}

# build the report
cd ${DUMPDIR};
PATH="${PATH}:."
case "$Benchsuit" in
    "SR15easy")
	which mkCactusEasy.R > /dev/null 2>&1 && mkcactus.R ;;
    "SR15m131")
	which mkCactus131.R > /dev/null 2>&1 && mkCactus131.R ;;
    "*")
	;;
esac

if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1
fi

postSlack livestream "MIOS Research Project: the ${MiosWithId} benchmark has just done!"
echo "done."
