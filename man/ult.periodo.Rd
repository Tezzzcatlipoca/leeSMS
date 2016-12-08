\name{ult.periodo}
\alias{ult.periodo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extraer el ultimo periodo cerrado de un indice de Retail
%%  ~~function to do ... ~~
}
\description{

La funcion sirve para extraer el ultimo periodo cerrado para un indice del Retail. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

ult.periodo(index_id)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index_id}{
%%     ~~Describe \code{x} here~~
cualquier indice valido del SMS de Retail para Brasil.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion ult.periodo() hace una extraccion del ultimo periodo cerrado de SMS de retail para el indice en cuestion.

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

Las funciones devuelven a la consola la informacion extraida del SMS de Retail utilizando SQL. Los datos son devueltos con formato de "integer".

}
\author{
%%  ~~who you are~~
Roberto Franco
}
\examples{
## Para llamar a la funcion, para el ultimo periodo cerrado:
ult.periodo(27)
2016022

ult.periodo(62)
2016009

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ult.periodo }


