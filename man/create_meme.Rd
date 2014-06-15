\name{create_meme}
\alias{create_meme}
\alias{plot.meme}
\title{Create meme}
\description{Create a meme from a template image}
\usage{
create_meme(template, upper, lower, username, password, site = NULL, 
            font = NULL, language = NULL, ...)
}
\arguments{
  \item{template}{Either an object of class \dQuote{meme_template} (returned by \code{\link{get_templates}}) or the URL of an image to caption. If the latter, \code{site} is set to \dQuote{memecaptain}.}
  \item{upper}{The text to display at the top of the meme image.}
  \item{lower}{The text to display at the bottom of the meme image.}
  \item{username}{A username for the relevant meme site. Note: For memegenerator, username and password are passed as plain text, so use at your own risk.}
  \item{password}{A password for the relevant meme site. Note: For memegenerator, username and password are passed as plain text, so use at your own risk.}
  \item{site}{The site used to generate the meme. This is set by default if \code{template} is an object of class \dQuote{meme_template}. One of \dQuote{imgflip}, \dQuote{memegenerator}, and \dQuote{memecaptain}.}
  \item{font}{If \code{site="imgflip"}, optionally one of \dQuote{impact} (the default) or \dQuote{arial}. Otherwise \code{NULL}.}
  \item{language}{If \code{site="memegenerator"}, optionally one of \dQuote{en} (English), \dQuote{es} (Spanish), \dQuote{ru} (Russia), or \dQuote{--} (other). Otherwise \code{NULL} (the default).}
  \item{...}{Additional arguments to \code{curlPerform}.}
}
\details{}
\value{An object of class \dQuote{meme}.}
\references{
\href{http://version1.api.memegenerator.net/}{memegenerator API}

\href{https://api.imgflip.com/}{imgflip API}

\href{http://memecaptain.com/}{memecaptain}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{\code{\link{get_templates}}}
\examples{
\dontrun{
# use imgflip
t1 <- get_templates("imgflip")
m1 <- create_meme(t1[[1]], "Upper text", "Lower text", "username", "password")
plot(m1)

# use memegenerator
t2 <- get_templates("memegenerator")
m2 <- create_meme(t2[[1]], "Upper text", "Lower text", "username", "password")
plot(m2)

# use memecaptain
t3 <- get_templates("memecaptain")
m3 <- create_meme(t3[[1]], "Upper text", "Lower text")
plot(m3)

# use memecaptain with arbitrary image URL
u <- ""
m4 <- create_meme(u, "Upper text", "Lower text")
plot(m4)
}
}
