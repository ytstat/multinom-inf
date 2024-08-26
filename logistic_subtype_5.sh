#!/bin/sh
#SBATCH -A stats                # The account name for the job.
#SBATCH -J logistic_subtype_5      # The job name.
#SBATCH -c 5                    # The number of cpu cores to use.
#SBATCH -t 08:20:00                   # The time the job will take to run.
#SBATCH --mem-per-cpu 2gb        # The memory the job will use per cpu core.
#SBATCH --output=/moto/home/yt2661/trash/out/slurm-%A_%a.out        # The memory the job will use per cpu core.

module load R/4.0.1

#Command to execute R code
R CMD BATCH --no-save --vanilla logistic_subtype_5.R /moto/home/yt2661/work/multinom-inf/experiments/logistic_subtype_5/out/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt
# End of script
