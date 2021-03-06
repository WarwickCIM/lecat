% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_search.R
\name{run_search}
\alias{run_search}
\title{Searches a series of strings for a specific query. Used internally by run_lecat_analysis}
\usage{
run_search(
  strings,
  query,
  regex = "\\\\bquery\\\\b",
  type,
  category,
  ids,
  column
)
}
\arguments{
\item{strings}{Vector of strings to search through. Strings are from the chosen corpus.}

\item{query}{String query term to search for in the strings. Taken from the chosen lexicon.}

\item{regex}{String regular expression defining the strings should be searched. Defaults to searching for the query term with non word characters either side}

\item{type}{String query type. Derived from lexicon type.}

\item{category}{String query type. Derived from lexicon type.}

\item{ids}{id of the string}

\item{column}{String column name examined included in the returned dataframe}
}
\value{
dataframe with counts of the query in each string
}
\description{
Search through a series for queries based on a regex query.
}
