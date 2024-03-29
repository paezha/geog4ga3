% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geog4ga3.R
\docType{data}
\name{travel_time_car}
\alias{travel_time_car}
\title{"travel_time_car"}
\format{
An excel file with 270 rows and 10 variables
}
\source{
Calculated by A. Paez
}
\usage{
data(travel_time_car)
}
\description{
A dataframe with travel distance/time from TAZ centroids to Jackson Square in downtown Hamilton, Canada.
}
\details{
\itemize{
   \item GTA06. idenitifer used for spatial joins (4050--6020)
   \item group. (4050.1--6020.1)
   \item from. TAZ centroid address
   \item to. Jackson Square, Hamilton, Ontario
   \item m. travel distance between origin and Jackson Square in meters (493--37558)
   \item km. travel distance between origin and Jackson Square in kilometers (0.493--37.558)
   \item miles. travel distance between origin and Jackson Square in miles (0.3063502--23.338541)
   \item seconds. travel time between origin and Jackson Square in seconds (115--2100)
   \item minutes. travel time between origin and Jackson Square in minutes (1.91667--35.0000)
   \item hours. travel time between origin and Jackson Square in hours (0.03194444--0.5833333)
}
}
\keyword{datasets}
