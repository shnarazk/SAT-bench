#!/bin/sh
version="0.18"

DIR="$HOME/Desktop/owncloud"
miosVersion="" # set if the name of executable is something like 'mios-1.3.0'
timeout=1260
timestamp=`date --iso-8601 | sed -re 's/^[0-9]+-//'`
upload=`which syncCloud > /dev/null 2>&1; if [ $? = 0 ] ; then echo "syncCloud" ; else echo; fi`
id=`cd $HOME/mios; git log -1 --format="%h" HEAD`
miosWithId="mios-${id}"
log="${DIR}/mios-exp/${miosWithId}-`date --iso-8601`-1.csv"
INSTALLOPTS=""
# INSTALLOPTS="--flag mios:devel"

help () {
    cmd=`basename $0`
    echo "Usage of ${cmd} (version ${version}): "
    echo " ${cmd} -r       - Run the bencmark suit"
    echo " ${cmd} -r -i ID - Select solver by ID (as '${id}') then run the bencmark"
    echo " ${cmd} -r -t T  - Set the timeout to T then run the bencmark"
    echo " ${cmd} -r -T     - Set the timeout to 310 then run the bencmark"
    echo " ${cmd} -r -n N  - Set the sequence number to N then run the bencmark"
    echo " ${cmd} -r -v V  - Set the mios version to V (something like '${miosVersion}')"
    echo " ${cmd} -r -S    - Set to force owncloud syhchronization mode and run"
    echo " ${cmd} -c       - Cat the current benchmark's result"
    echo " ${cmd} -g       - run mkcactus.R to make a graph"
    echo " ${cmd} -s       - sync the owncloud directory"
    echo " ${cmd} -k       - Kill the current benchmark"
    echo " ${cmd} -h       - Display this message"
 }   

showLog () {
    if [ ! -f ${log} ] ; then
	log="${DIR}/mios-exp/${miosWithId}-*-1.csv"
    fi
    cat ${log}
    echo "# end of $log"
}

mode="unknown"
forceSync=0
while getopts brcgsSTkhuli::n:v: OPT
do
    case $OPT in
	b) mode="build"
	   ;;
	r) mode="run"
	   ;;
	i)
	    id=$OPTARG
 	    mios="mios-${id}"
	    miosWithId="mios-${id}"
	    log="${DIR}/mios-exp/${miosWithId}-`date --iso-8601`-1.csv"
	    ;;
	t) timeout=$OPTARG
	   ;;
	T) timeout="310"
	   ;;
	n) log="${DIR}/mios-exp/${miosWithId}-`date --iso-8601`-$OPTARG.csv"
	   ;;
	v) miosVersion="$OPTARG"
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
	eval "(cd ${DIR}/mios-exp; ./mkcactus.R)"
	eval "(cd ${DIR}/mios-exp; ./mkcactusSU.R)"
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

if [ $(cd $HOME/mios; git status | grep -q modified; echo $?) == "0" ] ; then
    echo "ABORT: the mios repository is not clean. Please commit before benchmark."
    exit 0
fi

# echo "mode=${mode}"

if [ $mode = "build" ] ;
then
    echo "mode=${mode}??"
    if [ ! -f $HOME/.local/bin/${miosWithId} ] ; then
	echo "building ${miosWithId} ..."
	(cd $HOME/mios; stack clean; stack install $INSTALLOPTS --flag mios:llvm)
	if [ "x${miosVersion}" == "x" ] ; then
	    mv $HOME/.local/bin/mios $HOME/.local/bin/${miosWithId}
	else
	    mv $HOME/.local/bin/mios-${miosVersion} $HOME/.local/bin/${miosWithId}
	fi
	echo "done"
    fi
    ls -l $HOME/.local/bin/${miosWithId}
    exit 0;
fi

# if there is no exectable, install
if [ ! -f $HOME/.local/bin/${miosWithId} ] ; then
    (cd $HOME/mios; stack clean; stack install $INSTALLOPTS --flag mios:llvm)
    # set unique name
    if [ "x${miosVersion}" == "x" ] ; then
	mv $HOME/.local/bin/mios $HOME/.local/bin/${miosWithId}
    else
	mv $HOME/.local/bin/mios-${miosVersion} $HOME/.local/bin/${miosWithId}
    fi
fi

# run benchmark
if [ -f ${log} ] ;
then
    echo "Abort: ${log} exists now"
    exit 255
fi

# display configuration
echo "# Benchmark configuration:"
echo " * solver: `ls -l $HOME/.local/bin/${miosWithId}`"
echo " * result: ${log}"

echo "\"`basename ${log}`\"" >> ${DIR}/mios-exp/runs

# run the benchmark
echo -n "running ... "
cd $DIR
if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1
else
    upload=""
fi
sat-benchmark -K "@${timestamp}" -t 'SR15m/*.cnf' -T ${timeout} ${miosWithId} > ${log}
race -g
if [ ${forceSync} == 1 ] ; then
    eval ${upload} > /dev/null 2>&1 
fi
postSlack watching "The ${MiosWithId} benchmark has just done!"
echo "done."
