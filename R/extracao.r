################################# EXTRACAO DE REGIOES ESPECIFICAS ##################################

#' Extracao do regioes do dado completo
#' 
#' Permite extrair de dado de temperaturas da superficie uma determinada regiao numa janela de tempo
#' 
#' Os parametros \code{lon} e \code{lat}podem ser fornecidos como um escalar ou vetor. No primeiro
#' caso sera feito matching exato do valor fornecido; no segundo, quando sao informados vetores,
#' sera buscado no dado registros entre o primeiro e ultimo valores do vetor, inclusive.
#' 
#' Ainda com relacao aos vetores de coordenadas, e tambem possivel fornecer \code{lon} em duas 
#' referencias distintas: na escala de 0 a 360 graus ou -180 a 180. O segundo caso e mais 
#' conveniente quando se estao extraindo series no atlantico, cujas coordenadas no sistema 0, 360 
#' acabam sendo quebradas nas duas pontas do mapa. Caso algum valor de \code{lon} seja negativo, o 
#' codigo automaticamente entedera a mudanca de paradigma. \bold{Nao sao suportados ainda valores
#' de \code{lon} 'circulares", isto e, que implicariam o contorno ao redor do globo (ex. 
#' \code{lon = c(350, 370)} -- para isso, use \code{lon = c(-10, 10)})}. Ver Exemplos
#' 
#' No caso das datas, algo similar pode ser feito fornecendo uma string no formato "AAAAMM:AAAAMM", 
#' indicando a janela de tempo desejada. Nao e permitida a extracao de datas nao sequenciais, dado 
#' que assim ja nao configura mais uma serie temporal.
#' 
#' @param dado o dado do qual extrair serie. Deve ter formato igual ao retornado por 
#'     \code{\link{leFROMdir}} ou \code{\link{leFROMurl}}
#' @param lon longitude da regiao desejada. Ver Detalhes
#' @param lat latitude da regiao desejada. Ver Detalhes
#' @param data data das observacoes desejadas no formato AAAAMM. Ver Detalhes
#' @param FUN funcao utilizada para agregacao de observacoes por data no caso de \code{lon} ou 
#'     \code{lat} serem vetores. Por padrao se usa a media
#' @param plot booleano indicando se deve ser plotado um mapa apontando a regiao extraida.
#' @param ... parametros extras passados a \code{FUN}. \bold{Nao deve ser passado na.rm por aqui,
#'     pois e incorporado automaticamente}
#' 
#' @examples 
#' 
#' # usando datexemplo
#' 
#' # Extracao da coordenada lon = 38, lat = -44 ao longo de todo 2020
#' extraiserie(datexemplo, lon = 38, lat = -44)
#' 
#' # Extracao da faixa lon = [38, 40], lat = [-44, -40] ao longo de 2020, combinando por media (padrao)
#' extraiserie(datexemplo, c(38, 40), c(-44, -40))
#' 
#' # Mesma regiao mas apenas na janela 202001:202006
#' extraiserie(datexemplo, c(38, 40), c(-44, -40), "202001:202006")
#' 
#' # Extraindo series no atlantico, usando escala [-180, 180]
#' extraiserie(datexemplo, c(-15, -10), c(-10, 0))
#' 
#' # Equivalente ao anterior na referencia [0, 360]
#' extraiserie(datexemplo, c(345, 350), c(-10, 0))
#' 
#' @return objeto \code{ts} contendo serie temporal da regiao especificada na janela especificada
#' 
#' @export

extraiserie <- function(dado, lon, lat, data, FUN = mean, plot = TRUE, ...) {

    if(missing("lat")) lat <- dado[, range(LAT)]
    if(missing("lon")) lon <- dado[, range(LON)]
    if(missing("data")) {
        data <- dado[, range(DATE)]
    } else {
        data <- range(dateparse(data))
    }

    if(length(lat) == 1) lat <- rep(lat, 2)
    if(length(lon) == 1) lon <- rep(lon, 2)
    if(length(data) == 1) data <- rep(data, 2)

    lat <- sort(lat)
    lon <- sort(lon)
    data <- sort(data)

    # caso haja valores nagativos em lon, processa o dado para wrap adequado
    if(any(sign(lon) == -1)) {
        wrap <- c(-180, 180)
        dado <- aplicawrap(dado, wrap)
    } else {
        wrap <- c(0, 360)
    }

    if(is.character(FUN)) FUN <- eval(parse(text = FUN))

    out <- dado[(LAT %between% lat) & (LON %between% lon) & (DATE %between% data),
        .("SST" = FUN(SST, na.rm = TRUE, ...)), by = DATE]
    out <- ts(out$SST, start = c(year(data[1]), month(data[1])), freq = 12)

    reg <- data.frame(xmin = lon[1], xmax = lon[2], ymin = lat[1], ymax = lat[2])

    if(plot) {
        g <- plotamapa(dado, wrap = wrap)
        g <- g + geom_rect(data = reg, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                    fill = NA, color = "#000000", lwd = 1.1)

        print(g)
    }

    return(out)
}