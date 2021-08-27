#!/bin/bash
fin=Configs/exp_diff_3.txt

sed 1d ${fin} | while read line
do
	# Read parameter configuration.
	shape1=$(echo ${line} | awk "{print \$1}")
	shape2=$(echo ${line} | awk "{print \$2}")
	rate1=$(echo ${line} | awk "{print \$3}")
	rate2=$(echo ${line} | awk "{print \$4}")
	cens=$(echo ${line} | awk "{print \$5}")
	marg=$(echo ${line} | awk "{print \$6}")

	# Run simulation.
	Rscript Rscripts/equiv_prob_sim.R --shape1 ${shape1} \
	--shape2 ${shape2} \
	--rate1 ${rate1} \
	--rate2 ${rate2} \
	--cens ${cens} \
	--marg ${marg} \
	--reps 500 \
	--out "Simulations/ExpDiff3";
done