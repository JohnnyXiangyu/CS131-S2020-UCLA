#! /bin/bash

make
cat /proc/meminfo | grep "MemTotal" > measurements.log
printf "Number of processors: " >> measurements.log 
cat /proc/cpuinfo | grep "processor" | wc -l >> measurements.log

for size in 5 100 114514
do
    for threads in 1 8 30 40
    do
        for i in Synchronized Null Unsynchronized AcmeSafe
        do 
            echo "" >> measurements.log 
            echo "$i class with $threads threads, on $size-entry array:" >> measurements.log
            (time timeout 3600 java UnsafeMemory $i $threads 100000000 $size) &>> measurements.log
        done 
    done 
done
make clean
