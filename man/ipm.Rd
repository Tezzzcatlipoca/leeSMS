\name{ipm}
\alias{ipm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IPM: Extraer de index_period_market
%%  ~~function to do ... ~~
}
\description{

Las funciones de IPM sirven para hacer extracciones del SMS de Retail para index_period_market de manera rapida. Con la menor cantidad de texto posible.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

ipm(index_id,variables,args=NA)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index_id}{
%%     ~~Describe \code{x} here~~
cualquier indice valido del SMS de Retail para Brasil.
}
  \item{variables}{
%%     ~~Describe \code{x} here~~
cualquier variable valida para index_period_market del SMS de Retail Brasil. Si se trata de multiples variables, se deben separar por una coma. Se puede utilizar el comodin "*" cuando se desea extraer todas las variables de la base.
}

  \item{args}{
%%     ~~Describe \code{x} here~~
Este argumento es opcional. Si se desea filtrar la extraccion por alguna variable, se debe especificar aqui. Los argumentos deben escribirse en formato SQL, cada variable debe separarse por AND/OR. Es posible utilizar parentesis.

}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
La funcion ipm() hace una extraccion del ultimo periodo cerrado de SMS de retail para el indice en cuestion.

La funcion ipm_cur() hace una extraccion del periodo CURRENT (sin cerrar).

La funcion ipm_raw() hace una extraccion del historico SIN especificar un periodo particular. Si se necesita un periodo particular (que no sea el ultimo), se puede definir utilizando el argumento "args".

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
ipm(27,"mbd_id")
ipm(27,"mbd_id, mbd_name")
ipm(27,"*")
ipm(27,"mbd_name","mbd_id=2177")

## Para llamar a la funcion, para el periodo CURRENT:
ipm_cur(27,"mbd_name")
ipm_cur(27,"mbd_name, mbd_name")
ipm_cur(27,"mbd_name","mbd_id=2177")
ipm_cur(27,"*","mbd_id=2177 OR mbd_id=2227")

## Para llamar a la funcion, para cualquier periodo historico:
ipm_raw(27,"mbd_name")
ipm_raw(27,"mbd_name, mbd_name")
ipm_raw(27,"mbd_name","mbd_id=2177 AND period_id=2016018")
ipm_raw(27,"*","mbd_id=2177")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~ipm }
\keyword{ ~ipm_raw }
\keyword{ ~ipm_curr }% __ONLY ONE__ keyword per line
