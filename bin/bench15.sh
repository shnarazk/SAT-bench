#!/bin/sh
version="0.28"

# default vaules
DIR="$HOME/Documents/ownCloud"
DUMPDIR="."
LogNumber=1
GITDIR=$HOME/Repositories/mios15
Benchsuit="SR2015subset1"
MiosExecutable="mios" # set if the name of executable is something like 'mios-1.3.0'
MiosOptions=""
timeout=1260
timestamp=`date --iso-8601 | sed -re 's/^[0-9]+-//'`
upload=`which syncCloud > /dev/null 2>&1; if [ $? = 0 ] ; then echo "syncCloud" ; else echo; fi`

INSTALLOPTS=""
# INSTALLOPTS="--flag mios:devel"

help () {
    cmd=`basename $0`
    echo "Usage of ${cmd} (version ${version}): "
    echo " ${cmd} -r [-o OPTIONS]  - Run the bencmark suit"
    echo " ${cmd} -r -P PROBLEM    - Select dataset: 'SR2015subset1' or 'SR15m'"
    echo " ${cmd} -r -i ID         - Select solver by ID (as '${id}') then run the bencmark"
    echo " ${cmd} -r -t T          - Set the timeout to T then run the bencmark"
    echo " ${cmd} -r -T            - Set the timeout to 310 then run the bencmark"
    echo " ${cmd} -r -n N          - Set the sequence number to N then run the bencmark"
    echo " ${cmd} -r -G DIR       - Set  the solver repository dir to DIR (like '${GITDIR}')"
    echo " ${cmd} -r -e EXE       - Set the executable name to EXE (something like '${MiosExecutable}')"
    echo " ${cmd} -r -S            - Set to force owncloud syhchronization mode and run"
    echo " ${cmd} -c               - Cat the current benchmark's result"
    echo " ${cmd} -g               - run mkcactus.R to make a graph"
    echo " ${cmd} -s               - sync the owncloud directory"
    echo " ${cmd} -k               - Kill the current benchmark"
    echo " ${cmd} -h               - Display this message"
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
while getopts brcgsSTkhuli::n:e:D:o:P:t:G: OPT
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
	G ) GITDIR="$OPTARG"
	    ;;
	e) MiosExecutable="$OPTARG"
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
	eval "(cd ${DUMPDIR}; ./mkcactus.R)"
	eval "(cd ${DUMPDIR}; ./mkcactusSU.R)"
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
    echo "ABORT: the mios repository is not clean. Please commit before benchmark."
    exit 0
fi

# update variables
id=`cd ${GITDIR}; git log -1 --format="%h" HEAD`
MiosWithId="${MiosExecutable}-${id}"
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
echo -n "running ... "
cd $DIR
if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1
else
    upload=""
fi
echo "cd $DIR; sat-benchmark -K '@${timestamp}' -t "${Benchsuit}/*.cnf" -T ${timeout} -o '${MiosOptions}' ${MiosWithId} > ${DUMPDIR}/${log}"
sat-benchmark -K "@${timestamp}" -t "${Benchsuit}/*.cnf" -T ${timeout} -o "${MiosOptions}" ${MiosWithId} > ${DUMPDIR}/${log}

if [ -f ${DUMPDIR}/mkcactus.R ] ;
then
    (cd ${DUMPDIR}; ./mkcactus.R)
fi

if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1 
fi

postSlack watching "The ${MiosWithId} benchmark has just done!"
echo "done."
