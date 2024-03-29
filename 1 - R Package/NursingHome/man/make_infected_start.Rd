% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nh_abm_master.R
\name{make_infected_start}
\alias{make_infected_start}
\title{Set infection parameters for seeded infections}
\usage{
make_infected_start(
  df.u,
  time = 30,
  days_inf = 5,
  days_isolate_res = 10,
  days_isolate_staff = 7,
  days_isolate_visit = 5,
  set = NA,
  seed_asymp = F
)
}
\arguments{
\item{df.u}{dataframe in run_model() of infected individuals}

\item{time}{number of days to run model; defaults to 30}

\item{days_inf}{length of infectious period, defaults to 5}

\item{days_isolate_res}{length of isolation for residents, defaults to 10}

\item{days_isolate_staff}{length of isolation for staff, defaults to 7}

\item{days_isolate_visit}{length of isolation for visitors, defaults to 5}

\item{set}{indication of seeding model vs. creating infections}

\item{seed_asymp}{when making a seed, force to be asymptomatic; default is false}
}
\value{
df.u with updated parameters
}
\description{
Set infection parameters for individuals infected before looping through timesteps
}
