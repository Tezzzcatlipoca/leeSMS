\name{sm_ips}
\alias{sm_ips}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IPS: Extraer de index_period_source y source_master a la vez
%%  ~~function to do ... ~~
}
\description{

La funcion de sm_ips sirve para hacer extracciones del SMS de Retail para index_period_source y source_master de manera rapida. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

sm_ips(index_id,variables,args=NA)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index_id}{
%%     ~~Describe \code{x} here~~
cualquier indice valido del SMS de Retail para Brasil.
}
  \item{variables}{
%%     ~~Describe \code{x} here~~
cualquier variable valida para index_period_source y/o source_master del SMS de Retail Brasil. Si se trata de multiples variables, se deben separar por una coma. Se puede utilizar el comodin "*" cuando se desea extraer todas las variables de la base.
}

  \item{args}{
%%     ~~Describe \code{x} here~~
Este argumento es opcional. Si se desea filtrar la extraccion por alguna variable, se debe especificar aqui. Los argumentos deben escribirse en formato SQL, cada variable debe separarse por AND/OR. Es posible utilizar parentesis.

}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion sm_ips() hace una extraccion del ultimo periodo cerrado de SMS de retail para el indice en cuestion y hace una extraccion de source_master para todas las tiendas encontradas en index_period_source dados los criterios proporcionados. Finalmente, la funcion extrae solo las variables solicitadas. Es posible utilizar el comodin * para extraer todas las variables de ips y sm.

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
## Para llamar a la funcion, para el ultimo periodo cerrado:
sm_ips(27,"source_id")
sm_ips(27,"cell_id, source_name, uf")
sm_ips(27,"*")
sm_ips(27,"estibge, ciudad, cell_id","source_id=2177")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~sm_ips }% use one of  RShowDoc("KEYWORDS")

