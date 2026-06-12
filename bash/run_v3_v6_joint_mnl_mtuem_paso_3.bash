#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J v3-v6-joint-mnl-mtuem
#SBATCH -p general
#SBATCH -n 4
#SBATCH --ntasks-per-node=4
#SBATCH -c 1
#SBATCH --mem-per-cpu=2200
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 7-00:0:0
#SBATCH -o bashout/v3-v6_joint_mnl_mtuem_%A_%a.err.out
#SBATCH -e bashout/v3-v6_joint_mnl_mtuem_%A_%a.err.out

#-----------------Toolchain---------------------------
# ----------------Modulos----------------------------
module load r/4.4.0
# ----------------Comando--------------------------

srun --mpi=none --exclusive -n 1 -c 1 Rscript "v3 joint mnl mtuem (paso 3).R"& 
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v4 joint mnl mtuem (paso 3).R" & 
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v5 joint mnl mtuem (paso 3).R"& 
srun --mpi=none --exclusive -n 1 -c 1 Rscript "v6 joint mnl mtuem (paso 3).R"


