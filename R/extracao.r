################################# EXTRACAO DE REGIOES ESPECIFICAS ##################################

#' Extracao do regioes do dado completo
#' 
#' Permite extrair de dado de temperaturas da superficie uma determinada regiao numa janela de tempo
#' 
#' Os parametros \code{lon} e \code{lat}podem ser fornecidos como um escalar ou vetor. No primeiro
#' caso sera feito matching exato do valor fornecido; no segundo, quando sao informados vetores,
#' sera buscado no dado registros entre o primeiro e ultimo valores do vetor, inclusive.
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
#' @return objeto \code{ts} contendo serie temporal da regiao especificada na janela especificada
#' 
#' @export

extraiserie <- function(dado, lon, lat, data, FUN = mean, ...) {

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

    if(is.character(FUN)) FUN <- eval(parse(text = FUN))

    out <- dado[(LAT %between% lat) & (LON %between% lon) & (DATE %between% data),
        .("SST" = FUN(SST, na.rm = TRUE, ...)), by = DATE]
    out <- ts(out$SST, start = c(year(data[1]), month(data[1])), freq = 12)

    return(out)
}