#!/bin/sh
version="0.62"

# default vaules
BENCHDIR="$HOME/Documents/SAT-RACE"
DUMPDIR="$HOME/Documents/ownCloud/mios-exp"
GITDIR="$HOME/Repositories/mios"
LogNumber=1
Benchsuit="SC17main" # SR15m131, SC17m54
timeout=510         # for SC17main
MiosExecutable="mios" # set if the name of executable is something like 'mios-1.3.0'
MiosOptions=""
jobs="1"
timestamp=`date --iso-8601 | sed -re 's/^[0-9]+-//'`
upload=`which syncCloud > /dev/null 2>&1; if [ $? = 0 ] ; then echo "syncCloud" ; else echo; fi`
SkipCompile=0
INSTALLOPTS="" # "--flag mios:devel"

# help()
help () {
    cmd=`basename $0`
    echo "Usage of ${cmd} (version ${version}): "
    echo " ${cmd} -r            - Run the bencmark suit"
    echo " ${cmd} -r -P SET     - Select dataset: 'SC17m54' or 'SR15m131' (default: '${Benchsuit}')"
    echo " ${cmd} -r -o 'OPTS'  - Set solver's options"
    echo " ${cmd} -r -j n       - Number of jobs in parallel"
    echo " ${cmd} -r -i ID      - Select solver by commit id (skip the build phase)"
    echo " ${cmd} -r -t T       - Set timeout to T (default: ${timeout})"
    echo " ${cmd} -r -n N       - Set log sequence number to N (default: ${LogNumber})"
    echo " ${cmd} -r -m         - Use a strict benchmark environment for mios"
    echo " ${cmd} -r -e EXE     - Set the executable name to EXE (defualt: '${MiosExecutable}')"
    echo " ${cmd} -r -E EXE     - Use the executable name to EXE without compilation"
    echo " ${cmd} -r -B DIR     - Set the benchmark dir to DIR (default: '${BENCHDIR}')"
    echo " ${cmd} -r -D DIR     - Set the dump dir to DIR (default: '${DUMPDIR}')"
    echo " ${cmd} -r -G DIR     - Set the repository dir to DIR (default: '${GITDIR}')"
    echo " ${cmd} -r -S         - Force owncloud syhchronization"
    echo " ${cmd} -c            - Cat the current benchmark's result"
    echo " ${cmd} -g            - run mkCactus.R to make a graph"
    echo " ${cmd} -s            - sync the owncloud directory"
    echo " ${cmd} -k            - Kill the current benchmark"
    echo " ${cmd} -h            - Display this message"
 }

# showLog (logfile)
showLog () {
    log=${1:-${DUMPDIR}/${Benchsuit}-${timeout}-${MiosWithId}--${HOSTNAME}-*-${LogNumber}.csv}
    cat ${log}
    echo "# end of $log"
}

# makeCactus(dir, runfile)
makeCactus () {
    cd $1;
    case "$Benchsuit" in
	"SC17main")
	    cactus=cactus-$(basename $2 .runs).png
	    if [ "jobs"==1 ] ;
	    then
		subtitle="Sequential execution with a ${timeout} second timeout"
	    else
		subtitle="${jobs} parallel execution with a ${timeout} second timeout"
	    fi
	    which mkCactus.R > /dev/null 2>&1 && mkCactus.R $2 '$subtitle' > /dev/null 2>&1
	    which uploadSlack > /dev/null 2>&1 && uploadSlack livestream ${cactus} > /dev/null 2>&1
	    echo " - cactus  : $1/${cactus}"
	    ;;
	"*")
	    ;;
    esac
}

# postToSlack(channel, post, message)
postToSlack () {
    which postSlack > /dev/null 2>&1 && postSlack $1 "$2" > /dev/null 2>&1 && echo "$3"
}

# makeSync()
makeSync () {
    if [ ${forceSync} == 1 ] ; then
	eval ${upload} > /dev/null 2>&1
    fi
}

mode="unknown"
useMiosBench=0
forceSync=0
while getopts brcgsSkhuli::n:e:E:o:j:P:t:B:D:G:m OPT
do
    case $OPT in
	b) mode="build"
	   ;;
	r) mode="run"
	   ;;
	m) useMiosBench="1"
	   ;;
	i)
	    id=$OPTARG
 	    mios="mios-${id}"
	    MiosWithId="mios-${id}"
	    ;;
	P) Benchsuit=$OPTARG
	   case ${Benchsuit} in
	       SC17main) timeout="510"  ;;
	       SC17m54)  timeout="810"  ;;
	       SR15m131) timeout="1260" ;;
	       *) ;;
	   esac
	   ;;
	o) MiosOptions=$OPTARG
	   ;;
	j) jobs=$OPTARG
	   ;;
	t) timeout=$OPTARG
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

# update variables
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
if [ $SkipCompile == "1" ] ;
then
    id=''
    MiosWithId="${MiosExecutable}"
else
    id=`cd ${GITDIR}; git log -1 --format="%h" HEAD`
    MiosWithId="${MiosExecutable}-${id}"
fi
log="${DUMPDIR}/${Benchsuit}-${timeout}-${MiosWithId}--${HOSTNAME}-`date --iso-8601`-${LogNumber}.csv"
if [ -f ${log} ] ;
then
    echo "Abort: ${log} exists now"
    exit 255
fi
RUNS=${Benchsuit}-${timeout}-$(hostname).runs

case ${mode} in
    "build")
	;;
    "run")
	;;
    "sync")
	echo -n "sync dir..."
	makeSync
	echo "done."
	exit 0
	;;
    "graph")
	echo -n "make a graph..."
	makeCactus ${DUMPDIR} ${RUNS}
	makeSync
	exit 0
	;;
    "unknown")
	echo "?"
	if [ -f ${log} ] ; then
	    showLog ${log}
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
	showLog ${log}
	makeSync
	exit 0
	;;
esac

# build phase
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

# install phase if there is no exectable
if [ ! -f $HOME/.local/bin/${MiosWithId} ] ; then
    (cd ${GITDIR}; stack clean; stack install $INSTALLOPTS)
    # set unique name
    mv $HOME/.local/bin/${MiosExecutable} $HOME/.local/bin/${MiosWithId}
fi

# display configuration
echo "# SAT-Solver Benchmark (version $version) configuration:"
echo " - solver  : `ls -l $HOME/.local/bin/${MiosWithId}`"
echo " - options : jobs=${jobs}, timeout=${timeout}, option='${MiosOptions}'"
echo " - log file: ${log}"

# status update
cd $BENCHDIR
if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1
    (sleep  3600; ${upload} > /dev/null 2>&1) &
    (sleep  7200; ${upload} > /dev/null 2>&1) &
    (sleep 10800; ${upload} > /dev/null 2>&1) &
    (sleep 14400; ${upload} > /dev/null 2>&1) &
    (sleep 18000; ${upload} > /dev/null 2>&1) &
    (sleep 21600; ${upload} > /dev/null 2>&1) &
    (sleep 25200; ${upload} > /dev/null 2>&1) &
    (sleep 28800; ${upload} > /dev/null 2>&1) &
    (sleep 32400; ${upload} > /dev/null 2>&1) &
    (sleep 36000; ${upload} > /dev/null 2>&1) &
    (sleep 39600; ${upload} > /dev/null 2>&1) &
    (sleep 43200; ${upload} > /dev/null 2>&1) &
    (sleep 46800; ${upload} > /dev/null 2>&1) &
    (sleep 50400; ${upload} > /dev/null 2>&1) &
    (sleep 54000; ${upload} > /dev/null 2>&1) &
    (sleep 57600; ${upload} > /dev/null 2>&1) &
else
    upload=""
fi

makeSync
echo "\"`basename ${log}`\"" >> ${DUMPDIR}/${RUNS}
makeSync

if [ $useMiosBench == "1" ]
then
    echo "# $(date --iso-8601=seconds), ${MiosWithId}" > ${log}
    echo "# bench15.sh ${version}, m=1, j=${jobs}, t=${timeout}, ${MiosOptions} on $(hostname) @ ${timestamp}" >> ${log}
    echo "solver, num, target, time, valid" >> ${log}
    (cd $BENCHDIR; parallel -k -j ${jobs} "${MiosWithId} --benchmark=${timeout} --sequence={#} ${MiosOptions} {}" ::: ${Benchsuit}/*.cnf >> ${log})
else
    sat-benchmark -j ${jobs} -K "@${timestamp}" -t "${Benchsuit}/*.cnf" -T ${timeout} -o "${MiosOptions}" ${MiosWithId} > ${log}
fi

# build the report
makeSync
postToSlack livestream "MIOS Research Project: the ${MiosWithId} benchmark has just done!" " - post to your slack"
makeCactus ${DUMPDIR} ${RUNS}
makeSync
echo "done."
