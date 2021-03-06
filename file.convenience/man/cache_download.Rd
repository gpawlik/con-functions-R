% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/cache_download.R
\name{cache_download}
\alias{cache_download}
\title{cache_download}
\usage{
cache_download(url, dataDir, localName)
}
\arguments{
\item{url}{(string) the URL to download from}

\item{dataDir}{(string) The directory where the local file will be
stored in}

\item{localName}{(string) The name you want the file to be called locally}
}
\description{
Downloads a file from the internet, and caches it locally. So future calls to
the same function simply load the local file instead of downloading all over
again.
}
\note{
This function has only been tested on Linux, it might not work on other
      operating systems yet.
}
\author{
Ronny Restrepo
}

