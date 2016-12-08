\name{scan_sm}
\alias{scan_sm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SCAN IPS/SM: Extraer de index_period_source y source_master de Scantrack
%%  ~~function to do ... ~~
}
\description{

Las funciones de scan_ips y scan_sm sirven para hacer extracciones del SMS de Scantrack para index_period_source y de source_master de manera rapida. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

scan_sm(variables,args=NA)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables}{
%%     ~~Describe \code{x} here~~
cualquier variable valida para index_period_source o de source_master (segun la funcion que se utilice) del SMS de Scantrack Brasil. Si se trata de multiples variables, se deben separar por una coma. Se puede utilizar el comodin "*" cuando se desea extraer todas las variables de la base.
}

  \item{args}{
%%     ~~Describe \code{x} here~~
Este argumento es opcional. Si se desea filtrar la extraccion por alguna variable, se debe especificar aqui. Los argumentos deben escribirse en formato SQL, cada variable debe separarse por AND/OR. Es posible utilizar parentesis.

}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion scan_ips() hace una extraccion del ultimo periodo cerrado de Scantrack de retail para el indice 1 (SOT).

La funcion scan_sm() hace una extraccion del source_master de Scantrack (no tiene periodos).

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

Las funciones devuelven a la consola la informacion extraida del SMS de Retail utilizando SQL. Los datos son devueltos con formato de "data.frame".

}
\author{
%%  ~~who you are~~
Roberto Franco
}
\examples{
## Para llamar a la funcion scan_ips, para el ultimo periodo cerrado:
scan_ips("source_id")
scan_ips("source_id, source_acv")
scan_ips("*")
scan_ips("source_acv","source_id=12177")

## Para llamar a la funcion scan_sm:
scan_sm("uf")
scan_sm("uf, munic")
scan_sm("uf","source_id=12177")
scan_sm("*","source_id=12177 OR source_id=12227")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~scan_ips }
\keyword{ ~scan_sm }
