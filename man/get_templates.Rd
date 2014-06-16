\name{get_templates}
\alias{get_templates}
\alias{plot.meme_template}
\title{Get meme templates}
\description{Get a list of meme templates}
\usage{
get_templates(site = "memecaptain", type = NULL, query = NULL, ...)
}
\arguments{
  \item{site}{The site used to generate the meme. This is set by default if \code{template} is an object of class \dQuote{meme_template}. One of \dQuote{imgflip}, \dQuote{memegenerator}, or \dQuote{memecaptain} (the default).}
  \item{type}{If \code{site} is \dQuote{memegenerator}, optionally one of \dQuote{new}, \dQuote{popular} (the implicit default), \dQuote{trending}, \dQuote{related}, or \dQuote{search} to return a different subset of template images. For \dQuote{related} and \dQuote{search}, \code{query} should specify an image name or search term, respectively.}
  \item{query}{When \code{site} is \dQuote{memegenerator} and \code{type} is \dQuote{related} or \dQuote{search}, \code{query} should specify an image name or search term, respectively.}
  \item{...}{Additional arguments to \code{curlPerform}.}
}
\details{}
\value{A list of objects of class \dQuote{meme_template}.}
\references{
\href{http://version1.api.memegenerator.net/}{memegenerator API}

\href{https://api.imgflip.com/}{imgflip API}

\href{http://memecaptain.com/}{memecaptain}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{\code{\link{create_meme}}}
\examples{
\dontrun{
# use imgflip
t1 <- get_templates("imgflip")

# use memegenerator
t2 <- get_templates("memegenerator")

# use memecaptain
t3 <- get_templates("memecaptain")
}
}
