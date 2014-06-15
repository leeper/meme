\name{get_memes}
\alias{get_memes}
\title{Get memes}
\description{Get memes from a site}
\usage{
get_memes(site = "imgflip", type = NULL, query = NULL, ...)
}
\arguments{
  \item{site}{The site used to generate the meme. This is set by default if \code{template} is an object of class \dQuote{meme_template}. One of \dQuote{imgflip}, \dQuote{memegenerator}, and \dQuote{memecaptain}.}
  \item{type}{If \code{site} is \dQuote{memegenerator}, optionally one of \dQuote{new}, \dQuote{popular} (the implicit default), \dQuote{trending}, or \dQuote{topic} to return a different subset of template images. For \dQuote{trending} and \dQuote{topic}, \code{query} should specify an image name or search term, respectively.}
  \item{query}{When \code{site} is \dQuote{memegenerator} and \code{type} is \dQuote{trending} or \dQuote{topic}, \code{query} should specify an image name or search term, respectively.}
  \item{...}{Additional arguments to \code{curlPerform}.}
}
\details{}
\value{A list of objects of class \dQuote{meme}.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{get_templates}}

\code{\link{create_meme}}
}
\examples{
\dontrun{
# use memegenerator
get_memes("memegenerator")
}
}
