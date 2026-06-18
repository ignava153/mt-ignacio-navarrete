#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J v1-v6-joint-paso4
#SBATCH -p general
#SBATCH -n 6
#SBATCH --ntasks-per-node=6
#SBATCH -c 1
#SBATCH --mem-per-cpu=1200
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 14-00:00:00
#SBATCH -o bashout/v1-v6_joint_paso4_%j.out
#SBATCH -e bashout/v1-v6_joint_paso4_%j.err

#-----------------Toolchain---------------------------
module load r/4.4.0

#-----------------Comandos----------------------------

srun --mpi=none --exclusive -n 1 -c 1 Rscript "v1 joint (paso 4).R" &
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v2 joint (paso 4).R" &
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v3 joint (paso 4).R" &
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v4 joint (paso 4).R" &
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v5 joint (paso 4).R" &
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v6 joint (paso 4).R" &
wait
