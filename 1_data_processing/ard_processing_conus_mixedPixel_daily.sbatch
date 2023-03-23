#!/bin/csh
#SBATCH -t 24:00:00
#SBATCH -J Daily_16
#SBATCH --ntasks=16
#SBATCH --export=NONE
#SBATCH --partition=ord
#SBATCH --mem-per-cpu=8g
#SBATCH --gid=hab4cast
#SBATCH --account=hab4cast


cd $SLURM_SUBMIT_DIR
module load intel/21.4
module load gcc/9.1.0
module load Rstan/4.2.0
module load geos/3.10.2
module load gdal-3.4.2/intel-21
module load proj-8.2.1/intel-21
module load udunits-2.2.28/intel-21
module load geos/3.10.2
module load gdal-3.4.2/intel-21
module load proj-8.2.1/intel-21
module load udunits-2.2.28/intel-21
module load geos/3.10.2
module load gdal-3.4.2/intel-21
module load proj-8.2.1/intel-21
R --no-save <ard_processing_conus_mixedPixel_daily.R