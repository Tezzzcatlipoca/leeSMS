\name{consolidados}
\alias{consolidados}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Celdas Consolidadas en ultimo periodo
%%  ~~function to do ... ~~
}
\description{

La funcion "consolidados" sirve para extraer facilmente las relaciones de consolidacion entre celdas de cualquier indice de Retail para Brasil.

La funcion extrae la informacion del SMS de Retail y devuelve unicamente las celdas que tengan consolidaciones.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
consolidados(index_id, graba = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{index_id}{
  cualquier indice valido del SMS de Retail para Brasil.
%%     ~~Describe \code{index_id} here~~
}
  \item{graba}{
  los valores posibles son 0 o 1. El valor por default es 0. Cuando el valor es 1, la funcion exporta a un archivo formato .CSV la informacion extraida. El archivo se graba en el directorio actual (consultarlo usando getwd() ).
%%     ~~Describe \code{graba} here~~
}
}
\details{
La funcion necesita solamente una variable de input (el indice), ya que la segunda variable por default esta definida en 0.

Si se proporciona solamente la informacion de indice, la funcion devuelve la extraccion directamente a la consola. Si se iguala el argumento graba = 1, entonces la funcion no devuelve la extraccion a la consola, sino solamente a un archivo con formato .CSV en el directorio actual (para saber cual es, utilizar getwd() ).

%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
La funcion debe devolver una extraccion similar a la siguiente, para el indice especificado:

> head(consolidados(60))

cell_id  cell_name  consolidada  cezinho



Esta misma informacion se graba directo al archivo .CSV en caso de utilizar el siguiente llamado:

>consolidados(60,1)

En el caso anterior, se genera un archivo con el siguiente nombre (segun el ultimo periodo cerrado de SMS para el indice en cuestion):

"Celdas_60_2016022.csv"

Los ultimos 7 digitos del nombre corresponden al ultimo periodo cerrado de SMS de Retail para el indice en cuestion.

}

\author{
Roberto Franco
%%  ~~who you are~~
}

\examples{
## Para ejecturar la funcion:
output<-consolidados(60)
head(consolidados(60))

## Para que la funcion exporte las consolidaciones a un archivo en el directorio
## actual:
consolidados(60,1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~consolidados }
