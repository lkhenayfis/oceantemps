# Pacote oceantemps

Inclui um conjunto de funções para facilitar o trabalho com dados de temperatura dos oceanos

## Exemplo de uso

```r
# leitura de dados de temperatura na superficie no periodo 2020-01 a 2020-12
dados <- leFROMurl(c("202001", "202012"))

# extrai a serie temporal de temperatura media na regiao lon = [38, 40], lat = [-44, -40]
serie <- extraiserie(datexemplo, c(38, 40), c(-44, -40))
plot(serie)

# visualizacao das temperaturas no mapa mundi
plotamapa(dados, data = "202001")
```