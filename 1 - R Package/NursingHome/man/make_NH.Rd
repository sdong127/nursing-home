% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nh_abm_master.R
\name{make_NH}
\alias{make_NH}
\title{Structure nursing home and staff/visitor-resident relationships}
\usage{
make_NH(synthpop, cohorting = T, visitors = T, temp_staff = F)
}
\arguments{
\item{synthpop}{synthetic population; load synthpop.csv file}

\item{cohorting}{assign staff to residents; defaults to T (note to self that setting this to F does not change level of viral spread)}

\item{visitors}{allow visitors; defaults to T}

\item{temp_staff}{add temporary staff; defaults to F}
}
\value{
out data frame of structured nursing home
}
\description{
This function sorts nursing home residents into rooms (42 doubles, 36 singles), adds staffing shifts,
option for cohorting among staff and residents, and allows/assigns visitors to residents.
}
