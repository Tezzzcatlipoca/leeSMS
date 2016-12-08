\name{scan.ult.periodo}
\alias{scan.ult.periodo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extraer el ultimo periodo cerrado de un indice de Scantrack
%%  ~~function to do ... ~~
}
\description{

La funcion sirve para extraer el ultimo periodo cerrado para un indice del Scantrack. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

scan.ult.periodo()

}
%- maybe also 'usage' for other objects documented here.
\arguments{
La funcion no utiliza ningun argumento, ya que existe solo un indice utilizado en Scan (1).

}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion scan.ult.periodo() hace una extraccion del ultimo periodo cerrado de SMS de Scantrack para el indice de SOT (1).

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

Las funciones devuelven a la consola la informacion extraida del SMS de Scantrack utilizando SQL. Los datos son devueltos con formato de "integer".

}
\author{
%%  ~~who you are~~
Roberto Franco
}
\examples{
## Para llamar a la funcion, para el ultimo periodo cerrado:
scan.ult.periodo()
2016072


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~scan.ult.periodo }


