#! /bin/bash

make
cat /proc/meminfo | grep "MemTotal" > report/measurements.log
printf "Number of processors: " >> report/measurements.log 
cat /proc/cpuinfo | grep "processor" | wc -l >> report/measurements.log

cd compile && for size in 5 100 114514
do
    for threads in 1 8 30 40
    do
        for i in Synchronized Null Unsynchronized AcmeSafe
        do 
            echo "" >> ../report/measurements.log 
            echo "$i class with $threads threads, on $size-entry array:" >> ../report/measurements.log
            (time timeout 3600 java UnsafeMemory $i $threads 100000000 $size) &>> ../report/measurements.log
        done 
    done 
done
cd ..
make clean
