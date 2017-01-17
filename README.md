# fortranClustering

This package provides an example of how to use FORTRAN code within R.
As example a 2D clustering routine is chosen, where the performance boost by
using FORTRAN becomes obvious (although R is not really optimized).

The fortran code is located in the *src* folder and the R wrapper in the *R*
folder.

For more information and a step-wise implementation of FORTRAN routines, without
the package structure, is given in the [vignette](vignettes/fortranClustering.Rmd).

<!--
##
# Files
##

clustering2d.f90
  This file contains a 2D clustering algorithm in FORTRAN90.

clustering2df90.R
  This file contains the wrapper function for conveniently calling the FORTRAN subroutine within R.

clusteringR.R
  This is a implementation of the 2D clustering algorithm written in R code.

sample_precip_data2d.gz
  This is a R object. Loading it in R will give you a sample 2D precipitation field in units mm/s.

example.R
  An example script with R commands to source the functions, load example data and run the two versions of the clustering algorithm.

##
# Building the subroutine
##

To create a shared object file from FORTRAN source code, run
  
  $ R CMD SHLIB clustering2d.f90

This will compile the source and create the file clustering2d.so which can be loaded into R (see clustering2df90.R).

##
# Calling the FORTRAN subroutine in R
##

The file clustering2df90.R contains a wrapper function 
  clusteringf90(x,startID=1,thres=0)
which hides most of the arguments needed by the fortran subroutine.
It also loads the shared object file and makes sure that all arguments are propper data types before passing them and calling the subroutine.
Missing values (NA) are handled, too.
--> 

