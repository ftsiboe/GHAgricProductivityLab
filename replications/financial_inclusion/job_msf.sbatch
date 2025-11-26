#!/bin/bash -l

#SBATCH --job-name=msf_fin
#SBATCH --time=0-40:00:00    
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=3
#SBATCH --mem=28G 
#SBATCH --array=1-27
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load R/4.2.1-foss-2022a GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
R --no-save -q < replications/financial_inclusion/004_MSF_financial_inclusion_study.R
