#!env sh
cert=`basename $1 .cnf`
timeout=${2:-2000}

splr -c -p ${cert}.drat -t ${timeout} $1
if [ $? == 20 ] ;
then
    gratgen $1 ${cert}.drat -o ${cert}.grat
else if [ $? == 10 ] ;
     then
         echo "satisfied"
     else
         echo "could not solve"
     fi
fi
