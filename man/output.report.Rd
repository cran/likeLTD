\name{output.report}
\alias{output.report}
\title{likeLTD::output.report}
\usage{output.report(prosecutionHypothesis,defenceHypothesis, 
                      results,file=NULL)}
\arguments{
  \item{prosecutionHypothesis}{The output from \code{prosecution.hypothesis}.}
  \item{defenceHypothesis}{The output from \code{defence.hypothesis}.}
  \item{results}{The output from \code{evaluate}.}
  \item{file}{A file name for the output report. By default a filename is 
created to avoid over writing a previous file.}
}
\description{Outputs a docx report of results.}
\details{
Creates a docx containing all results, placed in the directory specified 
by admin$outputPath.
Results include both the pre-evaluation assessment of the alleles from both the reference 
profiles and the crime scene profile, and the post-evaluation results of the likelihoods 
and weight of evidence.
}
\examples{
\dontrun{
# datapath to example files
datapath = file.path(system.file("extdata", package="likeLTD"),"hammer")

# File paths and case name for allele report
admin = pack.admin.input(
            cspFile = file.path(datapath, 'hammer-CSP.csv'),
            refFile = file.path(datapath, 'hammer-reference.csv'),
            caseName = "hammer",
	    kit= "SGMplus"
             )

# Enter arguments
args = list(
        nUnknowns = 1,
        doDropin = FALSE,
        ethnic = "EA1",
        adj = 1,
        fst = 0.02,
        relatedness = c(0,0)
        )

# Create hypotheses
hypP = do.call(prosecution.hypothesis, append(admin,args))
hypD = do.call(defence.hypothesis, append(admin,args))

# Get parameters for optimisation
paramsP = optimisation.params(hypP)
paramsD = optimisation.params(hypD)

# Run optimisation
# n.steps set for demonstration purposes
results = evaluate(paramsP, paramsD, n.steps=1)

# Generate output report
output.report(hypP,hypD,results)
}
}
