#!/bin/bash
fin=Configs/Testing.txt
#fin=Configs/OrderProb.txt

sed 1d ${fin} | while read line
do
	# Read parameter configuration.
	n=$(echo ${line} | awk "{print \$1}")
	med1=$(echo ${line} | awk "{print \$2}")
	med2=$(echo ${line} | awk "{print \$3}")
	cens=$(echo ${line} | awk "{print \$4}")

	# Run simulation.
	Rscript Rscripts/order_prob_sim.R --n ${n} --med1 ${med1} --med2 ${med2} --cens ${cens} \
		--reps 1000 --out "Simulations/OrderProb";
done