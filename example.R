
# load sample data
load("sample_precip_data2d.gz")
sample_data <- precip.matrix
rm(precip.matrix)

# set values below 0.1 mm/10 min to NA
sample_data[sample_data<0.000166666666667] <- NA

# plot it
filled.contour(sample_data*60*10,color.palette=rainbow)

# now call the fortran clustering function
system.time({
  sample_output.f90 <- clusteringf90(sample_data)$tcl
})

# now the R implementation
system.time({
  sample_output.R <- clusteringR(sample_data)
})

# plot the clusters
filled.contour(sample_output.f90,color.palette=rainbow)
filled.contour(sample_output.R,color.palette=rainbow)

# now use a bigger matrix
sample_data_expanded <- sample_data[rep(seq(1,dim(sample_data)[1]),3),rep(seq(1,dim(sample_data)[2]),3)]

# plot it
filled.contour(sample_data_expanded*60*10,color.palette=rainbow)

# now call the fortran clustering function
system.time({
  sample_output_expanded.f90 <- clusteringf90(sample_data_expanded)$tcl
})

# now the R implementation
system.time({
  sample_output_expanded.R <- clusteringR(sample_data_expanded)
})

# plot the clusters
filled.contour(sample_output_expanded.f90,color.palette=rainbow)
filled.contour(sample_output_expanded.R,color.palette=rainbow)

# run it and save the run times
sys.times <- matrix(NA,ncol=2,nrow=10)
colnames(sys.times) <- c("FORTRAN","R")
rownames(sys.times) <- seq(1,10)**2

for(i in 1:10){
  print(i)
  # create the input matrix
  sample_data_expanded <- sample_data[rep(seq(1,dim(sample_data)[1]),i),rep(seq(1,dim(sample_data)[2]),i)]
  # now call the fortran clustering function
  sys.times[i,1] <- system.time({
    sample_output_expanded.f90 <- clusteringf90(sample_data_expanded)$tcl
  })[3]

  # now the R implementation
  sys.times[i,2] <- system.time({
    sample_output_expanded.R <- clusteringR(sample_data_expanded)
  })[3]
}

png("runtimes.png",width=600,height=800)
par(cex=1.5)
barplot(sys.times,beside=T,ylab="run time [s]")
dev.off()
