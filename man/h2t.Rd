% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h2t.R
\name{h2t}
\alias{h2t}
\title{How to make herb to symbol and fullname}
\usage{
h2t(x, type = "latin", output = "symbol")
}
\arguments{
\item{x}{is the information adout herb}

\item{type}{is the type of the x}

\item{output}{is the type of return}
}
\value{
symbol and fullname
}
\description{
How to make herb to symbol and fullname
}
\examples{
.h2t(x='Ziziphi Spinosae Semen',type='latin',output='symbol')
.h2t(x='huangqi',type='pinyin',output='both')
h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='symbol')
h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='both')
}
