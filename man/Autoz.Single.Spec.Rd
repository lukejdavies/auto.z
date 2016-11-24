\name{Autoz.Single.Spec}
\alias{Autoz.Single.Spec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automatic redshifting tool 
}
\description{
Redshfiting toold originaly developed by Ivan Baldry in IDL (Baldry et al., 2014 MNRAS). Repurposed in R by Luke Davies and  Leon Drygala
}
\usage{
Autoz.Single.Spec(spec, error, logLambdaData, logVlambda, tempData, plan, verbose = TRUE, tileFile = paste(.libPaths(), "/auto.z/data/", "SG23_Y7_003.fits", sep = ""), specNum = 2, tempFile = paste(.libPaths(), "/auto.z/data/", "filtered-templates.fits", sep = ""), oversample = 5, num = 5, templateNumbers = c(2:14, 16:22, 40:47), useInvCorrection = TRUE, stLambda = 400, endLambda = 9000, minval = -10000, maxval = 1e+06, z_prior = c(-1, 1000), UTMJD = 56835.65, longitude = 0, latitude = 0, altitude = 0, RA = 0, DEC = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{
%%     ~~Describe \code{spec} here~~
}
  \item{error}{
%%     ~~Describe \code{error} here~~
}
  \item{logLambdaData}{
%%     ~~Describe \code{logLambdaData} here~~
}
  \item{logVlambda}{
%%     ~~Describe \code{logVlambda} here~~
}
  \item{tempData}{
%%     ~~Describe \code{tempData} here~~
}
  \item{plan}{
%%     ~~Describe \code{plan} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{tileFile}{
%%     ~~Describe \code{tileFile} here~~
}
  \item{specNum}{
%%     ~~Describe \code{specNum} here~~
}
  \item{tempFile}{
%%     ~~Describe \code{tempFile} here~~
}
  \item{oversample}{
%%     ~~Describe \code{oversample} here~~
}
  \item{num}{
%%     ~~Describe \code{num} here~~
}
  \item{templateNumbers}{
%%     ~~Describe \code{templateNumbers} here~~
}
  \item{useInvCorrection}{
%%     ~~Describe \code{useInvCorrection} here~~
}
  \item{stLambda}{
%%     ~~Describe \code{stLambda} here~~
}
  \item{endLambda}{
%%     ~~Describe \code{endLambda} here~~
}
  \item{minval}{
%%     ~~Describe \code{minval} here~~
}
  \item{maxval}{
%%     ~~Describe \code{maxval} here~~
}
  \item{z_prior}{
%%     ~~Describe \code{z_prior} here~~
}
  \item{UTMJD}{
%%     ~~Describe \code{UTMJD} here~~
}
  \item{longitude}{
%%     ~~Describe \code{longitude} here~~
}
  \item{latitude}{
%%     ~~Describe \code{latitude} here~~
}
  \item{altitude}{
%%     ~~Describe \code{altitude} here~~
}
  \item{RA}{
%%     ~~Describe \code{RA} here~~
}
  \item{DEC}{
%%     ~~Describe \code{DEC} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

file=paste(.libPaths(),'/fourXPS/data/ExampleSpectrum.fits',sep='')
spec<-get.spec(paste(.libPaths(),'/fourXPS/data/ExampleSpec.fits',sep=''))
error<-get.spec(paste(.libPaths(),'/fourXPS/data/ExampleSpec.fits',sep=''), row=T)$flux

SN<-spec$flux/error

spec$flux<-(spec$flux/max(spec$flux, na.rm=T))*10^(-18) 
spec$error<-spec$flux/SN

AutozOut<-Autoz.Single.Spec(spec)

print(AutozOut$best)

SimSpec<-obs.Single.4MOST("TestSim", 1, 4800,4,'test.fits', writeFITSOut=FALSE, airMass=1.4, IQ=1.1, skyBright=21.77,tilt=6.0, misAlign=0.1, systemModelDir='/Users/lukehome/work/ICRAR_work/4MOST/IWG8/Mock_test/4FS-ETC_app/4FS_ETC_system_model_v0.2/', verbose=F)

AutozOut<-Autoz.Single.Spec(SimSpec)

print(SimSpec$best)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
