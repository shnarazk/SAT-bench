#!env sh
cert=`basename $1 .cnf`
timeout=${2:-2000}

if [ ! -f $1 ] ; then
    echo "$1 doesn't exist"
fi
if [ "$1" = "-q" ] ; then
    VERBOSE="-q"
    shift
else
    VERBOSE=${UCPC_MODE:- }
fi

splr -c -p ${cert}.drat -t ${timeout} ${VERBOSE} $1
result=$?

if [ $result == 20 ] ; then
    if [ "$VERBOSE" = '-q' ] ; then
        gratgen $1 ${cert}.drat -o ${cert}.grat 2>&1 | egrep  "^s "
        # gratchk unsat $1 ${cert}.grat | egrep -v "^c "
    else
        gratgen $1 ${cert}.drat -o ${cert}.grat
        echo NG
    fi
elif [ $result == 10 ] ; then
    dmcr $1
else
    echo "could not solve (returned $result)"
fi
