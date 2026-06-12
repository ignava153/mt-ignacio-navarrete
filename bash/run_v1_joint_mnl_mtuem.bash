#!/bin/bash
#---------------Script SBATCH - NLHPC ----------------
#SBATCH -J joint-mnl-mtuem
#SBATCH -p general
#SBATCH -n 1
#SBATCH --ntasks-per-node=1
#SBATCH -c 1
#SBATCH --mem-per-cpu=2200
#SBATCH --mail-user=pareyes2018@udec.cl
#SBATCH --mail-type=ALL
#SBATCH -t 7-0:0:0
#SBATCH -o joint_mnl_mtuem_%A_%a.err.out
#SBATCH -e joint_mnl_mtuem_%A_%a.err.out

#-----------------Toolchain---------------------------
# ----------------Modulos----------------------------
module load r/4.4.0
# ----------------Comando--------------------------

Rscript v1_joint_mnl_mtuem.R
