#!/bin/bash -l

#SBATCH --job-name=te_land
#SBATCH --time=0-30:00:00    
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=20
#SBATCH --mem=5G
#SBATCH --array=1-808
#SBATCH --mail-user=ftsiboe@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load R/4.2.1-foss-2022a GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
R --no-save -q < replications/land_tenure/003_TREATMENT_land_tenure_study.R

