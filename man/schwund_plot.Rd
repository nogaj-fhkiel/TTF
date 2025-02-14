% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schwund_plot.R
\name{schwund_plot}
\alias{schwund_plot}
\title{Schwund Plot}
\usage{
schwund_plot(basisdaten_roh, Sem, Abschl, Stg)
}
\arguments{
\item{basisdaten_roh}{data.table der Basisdaten zu den Studierenden enthält}

\item{Sem}{charakter_vektor der das Semester enthält}

\item{Abschl}{charakter_vektor der die Abschlussart enthält}

\item{Stg}{charakter_vektor der den Studiengang enthält}
}
\value{
ggplotobjekt
}
\description{
Zeigt die Anzahl der Exmatrikulierten nach Semester und Fachsemester
}
\examples{
schwund_plot(basisdaten_roh,
c(
  "WiSe 2017/2018",
 "SoSe 2018",
  "WiSe 2018/2019",
  "SoSe 2019",
  "WiSe 2019/2020"
),
"Bachelor",
"Betriebswirtschaft"
)
}
