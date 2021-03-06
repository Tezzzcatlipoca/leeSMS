\name{ips_cur}
\alias{ips_cur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IPS: Extraer de index_period_source
%%  ~~function to do ... ~~
}
\description{

Las funciones de IPS sirven para hacer extracciones del SMS de Retail para index_period_source de manera rapida. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

ips_cur(index_id,variables,args=NA)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index_id}{
%%     ~~Describe \code{x} here~~
cualquier indice valido del SMS de Retail para Brasil.
}
  \item{variables}{
%%     ~~Describe \code{x} here~~
cualquier variable valida para index_period_source del SMS de Retail Brasil. Si se trata de multiples variables, se deben separar por una coma. Se puede utilizar el comodin "*" cuando se desea extraer todas las variables de la base.
}

  \item{args}{
%%     ~~Describe \code{x} here~~
Este argumento es opcional. Si se desea filtrar la extraccion por alguna variable, se debe especificar aqui. Los argumentos deben escribirse en formato SQL, cada variable debe separarse por AND/OR. Es posible utilizar parentesis.

}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion ips() hace una extraccion del ultimo periodo cerrado de SMS de retail para el indice en cuestion.

La funcion ips_cur() hace una extraccion del periodo CURRENT (sin cerrar).

La funcion ips_raw() hace una extraccion del historico SIN especificar un periodo particular. Si se necesita un periodo particular (que no sea el ultimo), se puede definir utilizando el argumento "args".

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
ips(27,"source_id")
ips(27,"source_id, source_name")
ips(27,"*")
ips(27,"source_name","source_id=2177")

## Para llamar a la funcion, para el periodo CURRENT:
ips_cur(27,"source_name")
ips_cur(27,"source_name, source_name")
ips_cur(27,"source_name","source_id=2177")
ips_cur(27,"*","source_id=2177 OR source_id=2227")

## Para llamar a la funcion, para cualquier periodo historico:
ips_raw(27,"source_name")
ips_raw(27,"source_name, source_name")
ips_raw(27,"source_name","source_id=2177 AND period_id=2016018")
ips_raw(27,"*","source_id=2177")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ips }
\keyword{ ~ips_raw }
\keyword{ ~ips_curr }% __ONLY ONE__ keyword per line
