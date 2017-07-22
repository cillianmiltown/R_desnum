\name{p_report}
\alias{p_report}
\title{p_report}
\usage{
p_report(x)
}
\description{
For use inline in RMarkdown documents, for reporting p values in APA.  The input is a number between 0 and 1.  The output is this number as an APA reported p value, three decimal places, no preceeding zero, and the correct symbol "=" or "<".  If the value passed through the function is less than 0.001 (e.g., 0.00034), then the function will return "< .001".  When using inline it needs to be used in conjunction with paste.
}
\examples{
In a code chunk, save a test to an object e.g.,
c <- chisq.test(x,y)
The p value can be reported inline
*p* `r paste(p_report(c$p.value))`
}
