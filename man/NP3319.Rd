% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NP3319.R
\name{NP3319}
\alias{NP3319}
\title{Odette's Model (EPRI NP-3319) for Transition Temperature Shift (TTS) Calculation}
\usage{
NP3319(
  product_form = NULL,
  Cu = NULL,
  Ni = NULL,
  fluence = NULL,
  output = c("TTS", "CF", "FF", "SD"),
  output_unit = c("Celsius", "Fahrenheit")
)
}
\arguments{
\item{product_form}{Character vector. One of \code{"B"}, \code{"F"}, \code{"P"}, or \code{"W"}.
\code{"F"} (forgings) and \code{"P"} (plates) are treated as base metal (\code{"B"}).}

\item{Cu}{Numeric vector. Copper content in weight percent (wt\%). Must be between 0 and 100.}

\item{Ni}{Numeric vector. Nickel content in weight percent (wt\%). Must be between 0 and 100.}

\item{fluence}{Numeric vector. Neutron fluence in n/cm².}

\item{output}{Character. One of:
\itemize{
\item \code{"TTS"} – Transition Temperature Shift
\item \code{"CF"} – Chemistry Factor
\item \code{"FF"} – Fluence Factor
\item \code{"SD"} – Standard Deviation
\item \code{"Margin"} – Regulatory Margin
}}

\item{output_unit}{Character. Unit of the output result. One of:
\itemize{
\item \code{"Celsius"} – Return results in degrees Celsius (default)
\item \code{"Fahrenheit"} – Return results in degrees Fahrenheit
}}
}
\value{
A numeric vector of computed values for the selected \code{output}. For TTS, CF, and SD,
the unit depends on \code{output_unit}. FF is unitless.
}
\description{
Computes the Transition Temperature Shift (TTS), Chemistry Factor (CF),
Fluence Factor (FF), or Standard Deviation (SD) based on the embrittlement model
described in EPRI NP-3319 (1984).
}
\details{
This model estimates radiation-induced embrittlement in reactor pressure vessel materials
based on copper and nickel content and neutron fluence. It distinguishes between base and weld metals.
}
\examples{
# Compute TTS in Celsius
NP3319(
  product_form = "B", Cu = 0.1, Ni = 0.6, fluence = 1e19,
  output = "TTS", output_unit = "Celsius"
)

# Compute CF in Fahrenheit
NP3319(
  product_form = "W", Cu = 0.2, Ni = 0.5,
  output = "CF", output_unit = "Fahrenheit"
)

# Compute FF for fluence only
NP3319(
  product_form = "F", fluence = 1e19,
  output = "FF"
)

# Compute SD in Celsius
NP3319(
  product_form = "B", output = "SD", output_unit = "Celsius"
)

}
\seealso{
\code{\link{CR3391}}, \code{\link{RG199R2_P1}}, \code{\link{RG199R2_P2}}
}
