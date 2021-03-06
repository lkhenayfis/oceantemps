% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extracao.r
\name{extraiserie}
\alias{extraiserie}
\title{Extracao do regioes do dado completo}
\usage{
extraiserie(dado, lon, lat, data, FUN = mean, plot = TRUE, ...)
}
\arguments{
\item{dado}{o dado do qual extrair serie. Deve ter formato igual ao retornado por
\code{\link{leFROMdir}} ou \code{\link{leFROMurl}}}

\item{lon}{longitude da regiao desejada. Ver Detalhes}

\item{lat}{latitude da regiao desejada. Ver Detalhes}

\item{data}{data das observacoes desejadas no formato AAAAMM. Ver Detalhes}

\item{FUN}{funcao utilizada para agregacao de observacoes por data no caso de \code{lon} ou
\code{lat} serem vetores. Por padrao se usa a media}

\item{plot}{booleano indicando se deve ser plotado um mapa apontando a regiao extraida.}

\item{...}{parametros extras passados a \code{FUN}. \bold{Nao deve ser passado na.rm por aqui,
pois e incorporado automaticamente}}
}
\value{
objeto \code{ts} contendo serie temporal da regiao especificada na janela especificada
}
\description{
Permite extrair de dado de temperaturas da superficie uma determinada regiao numa janela de tempo
}
\details{
Os parametros \code{lon} e \code{lat}podem ser fornecidos como um escalar ou vetor. No primeiro
caso sera feito matching exato do valor fornecido; no segundo, quando sao informados vetores,
sera buscado no dado registros entre o primeiro e ultimo valores do vetor, inclusive.

Ainda com relacao aos vetores de coordenadas, e tambem possivel fornecer \code{lon} em duas
referencias distintas: na escala de 0 a 360 graus ou -180 a 180. O segundo caso e mais
conveniente quando se estao extraindo series no atlantico, cujas coordenadas no sistema 0, 360
acabam sendo quebradas nas duas pontas do mapa. Caso algum valor de \code{lon} seja negativo, o
codigo automaticamente entedera a mudanca de paradigma. \bold{Nao sao suportados ainda valores
de \code{lon} 'circulares", isto e, que implicariam o contorno ao redor do globo (ex.
\code{lon = c(350, 370)} -- para isso, use \code{lon = c(-10, 10)})}. Ver Exemplos

No caso das datas, algo similar pode ser feito fornecendo uma string no formato "AAAAMM:AAAAMM",
indicando a janela de tempo desejada. Nao e permitida a extracao de datas nao sequenciais, dado
que assim ja nao configura mais uma serie temporal.
}
\examples{

# usando datexemplo

# Extracao da coordenada lon = 38, lat = -44 ao longo de todo 2020
extraiserie(datexemplo, lon = 38, lat = -44)

# Extracao da faixa lon = [38, 40], lat = [-44, -40] ao longo de 2020, combinando por media (padrao)
extraiserie(datexemplo, c(38, 40), c(-44, -40))

# Mesma regiao mas apenas na janela 202001:202006
extraiserie(datexemplo, c(38, 40), c(-44, -40), "202001:202006")

# Extraindo series no atlantico, usando escala [-180, 180]
extraiserie(datexemplo, c(-15, -10), c(-10, 0))

# Equivalente ao anterior na referencia [0, 360]
extraiserie(datexemplo, c(345, 350), c(-10, 0))

}
