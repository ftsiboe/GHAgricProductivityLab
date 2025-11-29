#!/bin/bash -l

#SBATCH --job-name=run_all 
#SBATCH --time=0-05:00:00    
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=10G
#SBATCH --array=1-5
#SBATCH --mail-user=ftsiboe@ksu.edu
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0
module load R/4.2.1-foss-2022a

case "$SLURM_ARRAY_TASK_ID" in
  1)
    echo "Running Disability Study"
    R --no-save -q < replications/disability/001_DATA_disability_study.R
    R --no-save -q < replications/disability/002_MATCHING_disability_study.R
    R --no-save -q < replications/disability/003_TREATMENT_disability_study.R
    ;;

  2)
    echo "Running Resource Extraction Study"
    R --no-save -q < replications/resource_extraction/001_DATA_resource_extraction_study.R
    R --no-save -q < replications/resource_extraction/002_MATCHING_resource_extraction_study.R
    R --no-save -q < replications/resource_extraction/003_TREATMENT_resource_extraction_study.R
    ;;

  3)
    echo "Running Education Study"
    R --no-save -q < replications/education/001_DATA_education_study.R
    R --no-save -q < replications/education/002_MATCHING_education_study.R
    R --no-save -q < replications/education/003_TREATMENT_education_study.R
    ;;

  4)
    echo "Running Financial Inclusion Study"
    R --no-save -q < replications/financial_inclusion/001_DATA_financial_inclusion_study.R
    R --no-save -q < replications/financial_inclusion/002_MATCHING_financial_inclusion_study.R
    R --no-save -q < replications/financial_inclusion/003_TREATMENT_financial_inclusion_study.R
    ;;

  5)
    echo "Running Land Tenure Study"
    R --no-save -q < replications/land_tenure/001_DATA_land_tenure_study.R
    R --no-save -q < replications/land_tenure/002_MATCHING_land_tenure_study.R
    R --no-save -q < replications/land_tenure/003_TREATMENT_land_tenure_study.R
    ;;
esac



