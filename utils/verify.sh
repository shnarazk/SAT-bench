#!/bin/sh

function solve() {
    cnf=$1
    name=`basename ${cnf} .cnf`
    ~/.nix-profile/bin/echo -n "${name}:"
    rm -f ans_${name}.cnf ${name}.drat ${name}.grat
    splr -c -p ${name}.drat ${cnf} > /dev/null
    gratgen ${cnf} ${name}.drat -o ${name}.grat -j 4 >& /dev/null
    gratchk unsat ${cnf} ${name}.grat | egrep "^s" | tr -d s
}

for cnf in ../SAT-bench/3-SAT/UUF250/*.cnf ; do
    solve $cnf
done
for cnf in ../SAT-bench/SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/*.cnf ; do
    solve $cnf
done
