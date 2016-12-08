\name{sm}
\alias{sm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SM: Extraer de source_master
%%  ~~function to do ... ~~
}
\description{

La funcion de SM sirve para hacer extracciones del SMS de Retail para source_master de manera rapida. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

sm(variables,args=NA)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variables}{
%%     ~~Describe \code{x} here~~
cualquier variable valida para source_master del SMS de Retail Brasil. Si se trata de multiples variables, se deben separar por una coma. Se puede utilizar el comodin "*" cuando se desea extraer todas las variables de la base.
}

  \item{args}{
%%     ~~Describe \code{x} here~~
Este argumento es opcional. Si se desea filtrar la extraccion por alguna variable, se debe especificar aqui. Los argumentos deben escribirse en formato SQL, cada variable debe separarse por AND/OR. Es posible utilizar parentesis.

}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion sm() hace una extraccion de source_master del SMS de retail (no existe en source_master informacion de indice o de periodo).

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
## Para llamar a la funcion:
sm("source_id")
sm("estibge, uf, source_id")
sm("*")
sm("estibge","source_id=12177")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~sm }
