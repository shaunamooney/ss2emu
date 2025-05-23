% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/country_ss_to_emu.R
\name{country_ss_to_emu}
\alias{country_ss_to_emu}
\title{Calculating EMU from excel spreadsheet using modelled supply-share estimates, and saving output in specified files}
\usage{
country_ss_to_emu(
  country_tools_info,
  input_type = NULL,
  method_summary = FALSE,
  save_samples = FALSE
)
}
\arguments{
\item{country_tools_info}{Country tools info (get_tools_info() output)}

\item{input_type}{The chosen service type (in the Shiny UI)}

\item{method_summary}{Summarise by method or not (TRUE/FALSE)}

\item{save_samples}{Save samples for analysis (TRUE/FALSE)}
}
\description{
Calculating EMU from excel spreadsheet using modelled supply-share estimates, and saving output in specified files
}
