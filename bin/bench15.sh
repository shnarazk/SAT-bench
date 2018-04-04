#!/bin/sh
version="0.86"

#################### variables ####################
## directory and external commands settings [uppercase]
BENCHDIR="$HOME/Documents/SAT-RACE"
DUMPDIR="$HOME/Documents/ownCloud/mios-exp"
GITDIR="$HOME/Repositories/mios"
MAKECACTUS="mkCactus.R"
POSTSLACK="postSlack"
UPLOAD="syncCloud"
UPLOADSLACK="uploadSlack"
## internal settings [captilized sytle]
ForceSync=0
LogNumber=1
Mode="unknown"
SkipCompile=0
StackInstallOpts=""		# "--flag mios:devel" used in 'stack install'
Timestamp=`date --iso-8601 | sed -re 's/^[0-9]+-//'`
UseMiosBench=1
## for sat-benchmark [lowercase]
benchmarkSuite="SC17main"	# SR15m131, SC17m54
timeout=510			# for SC17main
miosExecutable="mios"		# set if the name of executable is something like 'mios-1.3.0'
miosOptions=""
jobs="1"

#################### functions ####################
# help()
help () {
    cmd=`basename $0`
    echo "Usage of ${cmd} (version ${version}): "
    echo " ${cmd} -s            - Force owncloud syhchronization"
    echo " ${cmd} -r            - Run the bencmark suit"
    echo " ${cmd} -r -P SET     - Select dataset: 'SC17m54' or 'SR15m131' (default: '${benchmarkSuite}')"
    echo " ${cmd} -r -o 'OPTS'  - Set solver's options"
    echo " ${cmd} -r -j n       - Number of jobs in parallel"
    echo " ${cmd} -r -i ID      - Select solver by commit id (skip the build phase)"
    echo " ${cmd} -r -t T       - Set timeout to T (default: ${timeout})"
    echo " ${cmd} -r -n N       - Set log sequence number to N (default: ${LogNumber})"
    echo " ${cmd} -r -M         - Don't use a strict benchmark environment for mios"
    echo " ${cmd} -r -e EXE     - Set the executable name to EXE (defualt: '${miosExecutable}')"
    echo " ${cmd} -r -E EXE     - Use the executable name to EXE without compilation"
    echo " ${cmd} -r -B DIR     - Set the benchmark dir to DIR (default: '${BENCHDIR}')"
    echo " ${cmd} -r -D DIR     - Set the dump dir to DIR (default: '${DUMPDIR}')"
    echo " ${cmd} -r -G DIR     - Set the repository dir to DIR (default: '${GITDIR}')"
    echo " ${cmd} -c            - Cat the current benchmark's result"
    echo " ${cmd} -g            - run ${MAKECACTUS} to make a graph"
    echo " ${cmd} -k            - Kill the current benchmark"
    echo " ${cmd} -h            - Display this message"
 }

# showLog (logfile)
showLog () {
    log=${1:-${DUMPDIR}/${benchmarkSuite}-${timeout}-$(basename {MiosWithId})--${HOSTNAME}-*-${LogNumber}.csv}
    cat ${log}
    echo "# end of $log"
}

# makeCactus(dir, runfile)
makeCactus () {
    cd $1;
    case "$benchmarkSuite" in
	"SC17main")
	    cactus=cactus-$(basename $2 .runs).png
	    if [ ${jobs} == 1 ] ;
	    then
		subtitle="A sequential execution with a ${timeout} second timeout"
	    else
		subtitle="${jobs} parallel execution with a ${timeout} second timeout"
	    fi
	    which ${MAKECACTUS} > /dev/null 2>&1 && ${MAKECACTUS} $2 "${subtitle}" > /dev/null 2>&1
	    which ${UPLOADSLACK} > /dev/null 2>&1 && ${UPLOADSLACK} livestream ${cactus} > /dev/null 2>&1
	    echo " - cactus  : $1/${cactus}"
	    ;;
	"*")
	    ;;
    esac
}

# postToSlack(channel, post, message)
postToSlack () {
    which ${POSTSLACK} > /dev/null 2>&1 && ${POSTSLACK} $1 "$2" > /dev/null 2>&1
}

# makeSync()
makeSync () {
    if [ ${ForceSync} == 1 ] ; then
	which ${UPLOAD} > /dev/null 2>&1 && ${UPLOAD} > /dev/null 2>&1
    fi
}

#################### parse options ####################
while getopts brcgskhli::n:e:E:o:j:P:t:B:D:G:M OPT
do
    case $OPT in
	b) Mode="build"
	   ;;
	r) Mode="run"
	   ;;
	M) UseMiosBench="0"
	   ;;
	i)
	    id=$OPTARG
 	    mios="mios-${id}"
	    MiosWithId="mios-${id}"
	    ;;
	P) benchmarkSuite=$OPTARG
	   case ${benchmarkSuite} in
	       SC17main) timeout="510"  ;;
	       SC17m54)  timeout="810"  ;;
	       SR15m131) timeout="1260" ;;
	       *) ;;
	   esac
	   ;;
	o) miosOptions=$OPTARG
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
	e) miosExecutable="$OPTARG"
	   ;;
	E) miosExecutable="$OPTARG"
	   Mode="run"
	   SkipCompile=1
	   ;;
	c) Mode="log"
	   ;;
	s) ForceSync=1
	   ;;
	g) Mode="graph"
	   ;;
	k) echo "kill benchmark"
	   parallel -j1 "echo {} > /dev/null; pkill -9 mios" ::: `seq 1 300`
	   exit 0
	   ;;
	h) Mode="help"
	   ;;
	*) Mode="unknown"
	   ;;
    esac
done
shift $((OPTIND - 1))

#################### let's start ####################
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
    MiosWithId="${miosExecutable}"
else
    id=`cd ${GITDIR}; git log -1 --format="%h" HEAD`
    MiosWithId="${miosExecutable}-${id}"
fi
log="${DUMPDIR}/${benchmarkSuite}-${timeout}-$(basename ${MiosWithId})--${HOSTNAME}-`date --iso-8601`-${LogNumber}.csv"
if [ -f ${log} ] ;
then
    echo "Abort: ${log} exists now"
    exit 255
fi
RUNS=${benchmarkSuite}-${timeout}-$(hostname).runs

case ${Mode} in
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
	if [ ${ForceSync}=1 ] ;	then
	    makeSync
	elif [ -f ${log} ] ; then
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
if [ $Mode = "build" ] ;
then
    if [ ! -f $HOME/.local/bin/${MiosWithId} ] ; then
	echo "building ${MiosWithId} ..."
	(cd ${GITDIR}; stack clean; stack install $StackInstallOpts)
	mv $HOME/.local/bin/${miosExecutable} $HOME/.local/bin/${MiosWithId}
	echo "done"
    fi
    ls -l $HOME/.local/bin/${MiosWithId}
    exit 0;
fi

# install phase if there is no exectable
if [ -f ${MiosWithId} ] ; then
    echo use ${MiosWithId} ;
elif [ -f $HOME/.local/bin/${MiosWithId} ] ; then
    MiosWithId=$HOME/.local/bin/${MiosWithId}
else
    if [ "$SkipCompile" == "1" ] ;
    then
	echo "ABORT: the $HOME/.local/bin/${MiosWithId} does not exist."
	exit 0 ;
    else
	(cd ${GITDIR}; stack clean; stack install $StackInstallOpts)
	# set unique name
	mv $HOME/.local/bin/${miosExecutable} $HOME/.local/bin/${MiosWithId}
	MiosWithId=$HOME/.local/bin/${MiosWithId}
    fi
fi

##############################################################################
cd $BENCHDIR
makeSync

# display configuration
echo "# SAT-Solver Benchmark (version $version) configuration:"
echo " - solver  : `ls -l ${MiosWithId}`"
echo " - options : jobs=${jobs}, timeout=${timeout}, option='${miosOptions}', m=$UseMiosBench"
echo " - log file: ${log}"

# update the RUNS file
if [ ${LogNumber} == 1 ] ;
then
    echo "\"`basename ${log}`\",\"$(basename ${MiosWithId})\"" >> ${DUMPDIR}/${RUNS}
else
    echo "\"`basename ${log}`\",\"$(basename ${MiosWithId})(${LogNumber})\"" >> ${DUMPDIR}/${RUNS}
fi

# run monitor
monitor () {
    while ps | fgrep -q $1 ;
    do makeSync
       sleep 2200;
    done
}

if [ ${ForceSync} == 1 ] ;then
    monitor $$ &
fi

# build log header and start a benchmark
echo "# $(date --iso-8601=seconds), `basename ${log}`" > ${log}
echo "# bench15.sh ${version}, $(basename ${MiosWithId}), m=${UseMiosBench}, j=${jobs}, t=${timeout}, ${miosOptions} on $(hostname) @ ${Timestamp}" >> ${log}
if [ $UseMiosBench == "1" ]
then
    echo -n "# " >> ${log}	# L.3
    ${MiosWithId} --version >> ${log}
    echo "# 0=UNSAT, 1=SAT, 2=OutOfMemory, 3=TimeOut, 4=Bug" >> ${log} # L.4
    echo "solver, num, target, time, valid" >> ${log}		       # L.5
    parallel -k -j ${jobs} "${MiosWithId} --benchmark=${timeout} --sequence={#} ${miosOptions} {}" ::: ${benchmarkSuite}/*.cnf >> ${log}
else
#    echo "solver, num, target, time" >> ${log}
#    echo "# " >> ${log}
    sat-benchmark -j ${jobs} -K "@${Timestamp}" -t "${benchmarkSuite}/*.cnf" -T ${timeout} -o "${miosOptions}" ${MiosWithId} >> ${log}
fi

# build the report
makeSync
#postToSlack livestream "<@U02HB72U2> the ${MiosWithId} benchmark on $(hostname) has just done!"
# makeCactus ${DUMPDIR} ${RUNS}
# makeSync
postToDiscord  "The $(basename ${MiosWithId}) benchmark on $(hostname) has just done!" "Mios Benchmark"
(cd ${DUMPDIR}; make)

echo "done."
