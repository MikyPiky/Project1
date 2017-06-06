#! /bin/bash
#-----------------------------------------------------
#$ -S /bin/bash            # Working Shell
#$ -N sm_8_crossval_2     # Set job name
#$ -o /home/peichl/projects/crossvalid/jobs/$JOB_NAME.$JOB_ID # Log file
#$ -j y                    # Write error in log file
#$ -l h_rt=48:00:00         # Request resource: hard run time hours:minutes:seconds
#$ -l h_vmem=3G           # Request resource: memory requirement

#$ -binding linear:1 

# #$ -l highmem=true         # run on highmem nodes
# #$ -l centos6=true         # OS
# #$ -cwd                    # Change into directory where you wrote qsub
#$ -m ea                   # mail notification e - in case of job ended a - in case of abortion 
#$ -M michael.peichl@ufz.de # mail address
#----------------------------------------------------

module load R/3.1.2-3 

Rscript /home/peichl/projects/crossvalid/siloMaize/8_sm_crossval.R


