\name{mkhtml}
\alias{mkhtml}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
make HTML report with text and images
}
\description{
Make HTML report from rayout dataframe. This is a wrapper function of "hwrite" in  the "hwriter" package.
}
\usage{
mkhtml(filename, data, foot = TRUE, charset = "CP932", lang = "JP")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
character.A filename for output.
}
  \item{data}{
3 columns dataframe. left column:text or a image file name, middle column:choose a text size LL/L/M/S, right column:image file or not, image file=1, others =0. See Example.
}
  \item{foot}{
logical. If TRUE, the bottom row is converted into foot note.
}
  \item{charset}{
character. Default charset is "CP932".
}
  \item{lang}{
character. Default language code is "JP".
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
NULL(a HTML file)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#prepare a image file.
png("sample.png")
plot(iris$Sepal.Length, iris$Sepal.Width)
dev.off()

#make a dataframe for the report rayout
df <- data.frame("This is a sample report.", "LL", stringsAsFactors=FALSE)
df[2,] <- c("This is a caption.", "L")
df[3,] <- c("This is a caption2.", "M")
df[4,] <- c("This is a text.", "S")
df[5,] <- c("sample.png", "S")
df[6,] <- c("This is a foot note", "S")

mkhtml("sample.html", df)

#If you use a web-browser supporting CSS3, you can edit text parts on it.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
