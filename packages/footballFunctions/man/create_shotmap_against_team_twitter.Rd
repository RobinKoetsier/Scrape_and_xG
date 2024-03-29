% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_shotmap_against_team_twitter.R
\name{create_shotmap_against_team_twitter}
\alias{create_shotmap_against_team_twitter}
\title{A plot function
makes a shotmap of shots against Eredivisie Team}
\usage{
create_shotmap_against_team_twitter(name_of_team = "Ajax", showstats = FALSE)
}
\arguments{
\item{showstats}{show the xG and means. default = FALSE}

\item{name_of_player}{Name of the team defined in the excel file, default = Ajax.}
}
\description{
A plot function
makes a shotmap of shots against Eredivisie Team
}
\examples{
create_shotmap_team_twitter(name_of_team = "Ajax", showstats = FALSE)

ggsave("name.png",dpi=300,width=8,height=7,device=png)
}
\keyword{shotmap}
