% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FactorDataset.R
\name{contingency}
\alias{contingency}
\title{Contingency table}
\usage{
contingency(object, var)
}
\arguments{
\item{object}{An object of class ccdata}

\item{var}{A data vector of an active category variable}
}
\value{
A cross table between the two variables.
The same table with margins and sum
The table with percentage by lines
Number of lines (which means number of cluster)
Number of column (which means number of modalities in the other variables)
}
\description{
Create a cross table between a cluster variable and a chosen category variable.
}
\examples{
data(BankCustomer)
obj <- Dataset(BankCustomer, BankCustomer$Cluster)
table <- contingency(obj, BankCustomer$profession)
effectiveTable <- table[[2]]
}