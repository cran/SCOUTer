% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcame.R
\name{pcame}
\alias{pcame}
\title{pcame}
\usage{
pcame(X, pcaref)
}
\arguments{
\item{X}{Matrix with observations that will be projected onto the PCA model.}

\item{pcaref}{A list with the elemements of a PCA model:
\itemize{
\item \code{m}: mean.
\item \code{s}: standard deviation.
\item \code{prepro}: preprocessing: \code{"none"}, \code{"cent"} or \code{"autosc"}.
\item \code{P}: loading matrix.
\item \code{lambda}: vector with variances of each PC.
}}
}
\value{
list with elements containing information about X in the PCA model:
\itemize{
\item \code{Xpreprocessed}: matrix \code{X} preprocessed.
\item \code{Tscores}: score matrix with the projection of \code{X} on each one of the A PCs.
\item \code{E}: error matrix with the par of \code{X} not explained by the PCA model.
\item \code{SPE}: vector with the SPE for each observation of \code{X}.
\item \code{T2}: vector with the T^_A for each observation of \code{X}.
\item \code{T2matrix}: matrix with the contributions of each PC to the T^2_A for each observation
of \code{X}.
\item \code{Xrec}: matrix with the reconstructed part of \code{X}, i.e. the part of \code{X}
explained by the PCA model.
}
}
\description{
Projection of X onto a Principal Component Analysis (PCA) model.
}
\details{
\code{pcame} performs the projection of the data in X onto the PCA model stored as a
list of parameters. It returns the projection of the observations in X, along with the
Squared Prediction Errors (SPE), Hotelling's T^2_A, contribution elements and the
reconstruction of X obtained by the PCA model.
}
\examples{
X <- as.matrix(X)
pcamodel.ref <- pcamb_classic(X, 3, 0.1, "autosc") # PCA-MB with all observations
pcame(X, pcamodel.ref) # Project all observations onto PCA model of pcamodel.ref

pcamodel.ref <- pcamb_classic(X[1:40,], 2, 0.05, "cent") # PCA-MB with first 40 observations
pcame(X[-c(1:40),], pcamodel.ref) # Project observations not used in PCA-MB onto PCA model 
# of pcamodel.ref
}
