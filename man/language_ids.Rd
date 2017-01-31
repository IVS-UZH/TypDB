\name{language_ids}
\alias{language_ids}
\docType{data}
\title{Standard language identifiers mapping table}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A table containing mappings between various language codes, including
Glottolog, ISO-639-3 and WALS. Retrieved from \url{https://github.com/ddediu/phon-class-counts/blob/master/input/code_mappings_iso_wals_autotyp_glottolog.csv}.
}
\usage{language_ids}
\format{
  A data frame containing mappings between various language codes for a
  total of 17256 code combinations.
  \describe{
    \item{\code{UULID}}{Unique Universal Language IDentifier}
    \item{\code{iso.639.3}}{ISO 639-3 language name identifier}
    \item{\code{glottocode}}{Glottolog ID}
    \item{\code{wals.code}}{WALS ID}
    \item{\code{autotyp.code}}{AUTOTYP ID}
    \item{\code{ethnologue.name}}{Ethnologue canonical language name}
    \item{\code{wals.name}}{WALS canonical language name}
    \item{\code{autotyp.name}}{AUTOTYP canonical language name}
    \item{\code{glottolog.name}}{Glottolog canonical language name}
  }
}

\references{
  Dediu, D., & Moisik, S. (2016). Defining and counting phonological classes in cross-linguistic segment databases. In N. Calzolari, K. Choukri, T. Declerck, S. Goggi, M. Grobelnik, B. Maegaard, J. Mariani, H. Mazo, A. Moreno, J. Odijk, & S. Piperidis (Eds.), Proceedings of LREC 2016: 10th International Conference on Language Resources and Evaluation (pp. 1955-1962). Paris: European Language Resources Association (ELRA).
}
\keyword{datasets}
