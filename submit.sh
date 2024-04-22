#!/bin/bash


module load R
rm -rf slurm_out; mkdir slurm_out


# Submit an array job for processing data from 2018 to 2024
jobId1=$(sbatch --output="slurm_out/slurm-%A_%a.out" \
               --error="slurm_out/slurm-%A_%a.err" \
               --array=2023-2024 \
               getData.sh)

jobId1=$(echo $jobId1 | sed 's/Submitted batch job //')


#jobId2=$(sbatch --output="slurm_out/slurm-%A_%a.out" \
 #              --error="slurm_out/slurm-%A_%a.err" \
#	       --dependency=afterok:$jobId1 \
 #              combine_files.sh)

#jobId2=$(echo $jobId2 | sed 's/Submitted batch job //')
