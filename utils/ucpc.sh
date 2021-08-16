#!env sh
cert=`basename $1 .cnf`
timeout=${2:-2000}

if [ ! -f $1 ] ; then
    echo "$1 doesn't exitst"
fi

VERBOSE=${UCPC_MODE:- }
splr -c -p ${cert}.drat -t ${timeout} ${VERBOSE} $1

result=$?
if [ $result == 20 ] ;
then
    gratgen $1 ${cert}.drat -o ${cert}.grat
elif [ $result == 10 ] ;
then
    dmcr $1
else
    echo "could not solve (returned $result)"
fi
