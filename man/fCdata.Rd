\name{fCdata}
\alias{fCdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Fortran clustering example gridded precipitation field.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A matrix containing a gridded precipitation field in mm/10 min. The number of grid points in each dimension is 288. The grid spacing is 200 m.
}
\usage{data("fCdata")}
\format{
  matrix[1:288,1:288]
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fCdata)
filled.contour(fCdata,color.palette=rainbow)
}
\keyword{datasets}
