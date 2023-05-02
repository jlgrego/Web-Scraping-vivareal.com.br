#' ---
#' title: "Relação do mercado imobiliário e índices socioeconômicos em São Paulo - SP"
#' author: "José Luiz Grego"
#' ---

#carregando pacotes utilizados
pacotes <- c( "rgdal","dplyr", "sf", "tmap", "plotly",
              "ggplot2", "cowplot", "tibble", "stringi",
              "corrplot", "metan", "readxl", "spdep", "kableExtra")


options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
#                     Carregando bases auxiliares                              #
################################################################################

#Shapefile do município de São Paulo - SP. Fonte: Geosampa.
shp_saopaulo <- readOGR("shapefile_municipio", "municipio_sp")

#Transformando o shapefile para o Datum WGS84:
shp_long_lat <- spTransform(shp_saopaulo, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 

#Transformando o shapefile em um objeto simplefeature: 
sf_shp <- spTransform(shp_long_lat, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% 
  sf::st_as_sf()

#Alterando nome dos bairros para realização de join posterior
sf_shp <- sf_shp %>% mutate(NOME_DIST = gsub(" ", "-", sf_shp$NOME_DIST) %>% tolower())

#Base de dados com índices sociais e econômicos por distrito de São Paulo. Fonte: Geosampa
idh_sp <- read_excel("idh_sp.xls", sheet = "dados")

#carregando objeto sp do metrô de São Paulo          
sp_metro_utm <- readOGR("metro", "metro_sp", use_iconv = TRUE, encoding = "UTF-8")

#alterando datum para utm
proj4string(sp_metro_utm) <- CRS("+init=epsg:22523")

#alterando datum para WGS 84
sp_metro_geod <- spTransform(x = sp_metro_utm,
                             CRSobj = CRS("+proj=longlat"))

#Data frame auxiliar com as regiões de São Paulo para pulverizar a busca de dados pela malha do município;
regioes <- data.frame("distrito" = idh_sp$DISTRITO,
                      "zona" = idh_sp$REGIAO8)

#Modificando o nome das regiões para análises posteriores
regioes <- regioes %>% mutate(distrito = stringi::stri_trans_general(str = distrito, id = "Latin-ASCII") %>% tolower(),
                              zona = gsub("[0-9]", "", regioes$zona) %>% tolower()) %>% 
  mutate(zona = gsub("leste", "zona-leste", zona),
         zona = gsub("oeste", "zona-oeste", zona),
         zona = gsub("sul",  "zona-sul", zona),
         zona = gsub("norte", "zona-norte", zona),
         zona = gsub(" ", "", zona),
         distrito = gsub(" ", "-", distrito))

#Removendo os dados duplicados:
regioes <- regioes[!duplicated(regioes[c(1:2)]),]

#Removendo bairros que não foram localizados no site Viva Real com o padrão adotado:
regioes <- subset(regioes,!grepl(c("carrao|morumbi|cachoeirinha|jaguara|pari|marsilac|perus|jardim-paulista|itaim-bibi|sao-domingos|sao-rafael|iguatemi|sao-miguel|cambuci"), regioes$distrito))

#Carregando base de dados de imóveis:
dados_sp_pt <- read_excel("dados_sp.xlsx")


################################################################################
#                     Análises exploratória e espacial                         #
################################################################################

#Variáveis categóricas em factor
dados_sp_pt[, 10:12] <- lapply(dados_sp_pt[,10:12], as.factor)

#Transformando o dataset em um objeto simple feature:
sf_dados_sp <- st_as_sf(x = dados_sp_pt, 
                        coords = c("lon", "lat"), 
                        crs = "+proj=longlat")

#Disposição dos dados na malha urbana de São Paulo:
tmap_mode("view")

a <- tm_shape(shp = shp_long_lat) + 
  tm_borders(alpha = 0.9) +
  tm_shape(shp = sf_dados_sp) + 
  tm_dots(col = "#1A5276",
          size = 0.03,
          popup.vars = c("Endereço" = "endereco", "Valor total (R$)" = "valor_total", "Unitário (R$/m²):" = "unit",
                         "Area (m²):" = "area_util", "Quartos:" = "quartos", "Suites:" = "suites",
                         "Banheiros" = "banheiros", "Vagas:" = "vagas","Condomínio" = "condominio",  
                         "Piscina:" = "piscina", "Academia" = "academia", "Quadra" = "quadra")) +
  tm_layout(title= "Antes da intersecção")

#Plotando o mapa
a

#Excluindo dados fora dos limites de São Paulo através
#de uma interseção dos dados com um shapefile do município:
inter <- st_intersection(sf_dados_sp, sf_shp) %>% 
  sf::st_as_sf()

#Com a interseção:
b <- tm_shape(shp = shp_long_lat) + 
  tm_borders(alpha = 0.9) +
  tm_shape(shp = inter) + 
  tm_dots(col = "#1A5276",
          size = 0.03,
          popup.vars = c("Endereço" = "endereco", "Valor total (R$)" = "valor_total", "Unitário (R$/m²):" = "unit",
                         "Area (m²):" = "area_util", "Quartos:" = "quartos", "Suites:" = "suites",
                         "Banheiros" = "banheiros", "Vagas:" = "vagas","Condomínio" = "condominio",  
                         "Piscina:" = "piscina", "Academia" = "academia", "Quadra" = "quadra")) +
  tm_layout(title= "Depois da intersecção")

#Comparação
tmap_arrange(a,b)

# Quantidade de dados que estavam fora dos limites de São Paulo:
print(length(dados_sp_pt$id) - length(inter$id))

#Boxplot da variável dependente "Valor Unitário/m²":

# Calculando  medidas resumo para legenda
media <- mean(dados_sp_pt$unit)
mediana <- median(dados_sp_pt$unit)
primeiro_quartil <- quantile(dados_sp_pt$unit, 0.25)
terceiro_quartil <- quantile(dados_sp_pt$unit, 0.75)
minimo <- min(dados_sp_pt$unit)
maximo <- max(dados_sp_pt$unit)
quant_dados <- length(dados_sp_pt$unit)


# Cria a string com os valores resumo
texto_resumo <- paste0("Valor mínimo = R$ ", round(minimo, 2), "\n",
                       "1º quartil = R$ ", round(primeiro_quartil, 2), "\n",
                       "Mediana = R$ ", round(mediana, 2), "\n",
                       "Média = R$ ", round(media, 2), "\n",
                       "3º quartil = R$ ", round(terceiro_quartil, 2), "\n",
                       "Valor Máximo = R$ ", round(maximo, 2), "\n",
                       "Quantidade de dados = ", quant_dados)
                       
                       
box_unit <- ggplot(dados_sp_pt) +
  aes(x = "", y = unit) +
  geom_boxplot(fill = "#0C4C8A") +
  labs(y = "Valor Unitário (R$/m²)", 
       title = "Boxplot do Valor Unitário (R$/m²)") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, 125000), breaks = seq(0, 125000, by = 10000)) +
  annotate("text", x = 1.3, y = 90000, label = texto_resumo)        

#Histograma dos valores unitários:
hist_unit <-  ggplot(dados_sp_pt) +
  aes(x = unit) +
  geom_histogram(bins = 100, fill = "#0C4C8A") +
  labs(
    x = "Valor Unitário (R$/m²)",
    y = "Frequência",
    title = "Histograma de Valor Unitário (R$/m²) "
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"))


summary(dados_sp_pt$unit)
ggplotly(hist_unit)
ggplotly(box_unit)


#Os possíveis outliers poderão ser removidos, desde que devidamente justificado, conforme
#previsto na NBR 14653-2. É pertinente também que sejam tratados com base em literatura 
#subjacente. No caso, foi criada uma função que elimina os chamados outliers extremos 
#da variável resposta, ou seja Valor unitário > Q3 + 3 * AIQ. 

#Função para detectar outliers extremos na variável dependente "valor unitário, que 
#são quaisquer valores situados abaixo de Q1 ou acima de Q3 por 
#mais 3 vezes a amplitude interquartil.
outliers_extreme = function(x){
  x > quantile(x)[4] + 3*IQR(x) | x < quantile(x)[2] - 3*IQR(x)
}

remove_outliers <- function(inter, cols = names(inter)) {
  for (col in cols) {
    inter <- inter[!outliers_extreme(inter[[col]]),]
  }
  inter
}

#Removendo outliers extremos na variável "unit"
inter <- inter %>% remove_outliers(col = 3) %>% st_as_sf(coords = c("lon", "lat"), 
                                                         crs = "+proj=longlat")

#Novo Boxplot dos valores unitários

# Calculando  medidas resumo para legenda
media_inter <- mean(inter$unit)
mediana_inter <- median(inter$unit)
primeiro_quartil_inter <- quantile(inter$unit, 0.25)
terceiro_quartil_inter <- quantile(inter$unit, 0.75)
minimo_inter <- min(inter$unit)
maximo_inter <- max(inter$unit)
quant_dados_inter <- length(inter$unit)


# Cria a string com os valores resumo
texto_resumo_inter <- paste0("Valor mínimo = R$ ", round(minimo_inter, 2), "\n",
                       "1º quartil = R$ ", round(primeiro_quartil_inter, 2), "\n",
                       "Mediana = R$ ", round(mediana_inter, 2), "\n",
                       "Média = R$ ", round(media_inter, 2), "\n",
                       "3º quartil = R$ ", round(terceiro_quartil_inter, 2), "\n",
                       "Valor Máximo = R$ ", round(maximo_inter, 2), "\n",
                       "Quantidade de dados = ", quant_dados_inter)

box_unit_out <- ggplot(inter) +
  aes(x = "", y = unit) +
  geom_boxplot(fill = "#0C4C8A") +
  labs(y = "Valor Unitário (R$/m²)", 
       title = "Boxplot do Valor Unitário (R$/m²)") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, 20000), breaks = seq(0, 20000, by = 2000)) +
  annotate("text", x = 1.3, y = 15000, label = texto_resumo_inter)

#Novo Histograma dos valores unitários
hist_unit_out <-  ggplot(inter) +
  aes(x = unit) +
  geom_histogram(bins = 30, fill = "#0C4C8A") +
  labs(
    x = "Valor Unitário (R$/m²)",
    y = "Frequência",
    title = "Histograma de Valor Unitário (R$/m²) "
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )

plot_grid(box_unit_out, hist_unit_out, labels = "AUTO")

#Calculando a média dos valores unitários por bairro:
mean_bairro <- inter[,c(3,18)] %>% group_by(NOME_DIST) %>% 
  summarise(media = mean(unit)) %>% rename(NOME_DIST = 1) %>% 
  as.tibble() %>% select(1:2)

#Quantidade de dados por bairro:
qtd_bairro <- inter %>% group_by(NOME_DIST) %>% summarise(count=n()) %>% 
  as.tibble() %>% select(1:2)

#Fazendo join:
sf_shp <- sf_shp %>% left_join(mean_bairro, sf_shp,
                               by = "NOME_DIST")
sf_shp <- sf_shp %>% left_join(qtd_bairro, sf_shp,
                               by = "NOME_DIST")

#Deixando o prefixo JD no arquivo shapefile similar ao dataframe "filtro_idh":
sf_shp$NOME_DIST <-  sub("jd", "jardim", sf_shp$NOME_DIST)

#Levando informação sobre qual é região do imóvel:

regioes <- rename(regioes, NOME_DIST = distrito)
regioes$NOME_DIST <- sub("cidade", "cid", regioes$NOME_DIST) 


sf_shp <- sf_shp %>% left_join(regioes, sf_shp,
                               by = "NOME_DIST")

#filtrando índices socioeconômicos para complementar as variáveis do estudo:
filtro_idh <- idh_sp %>% filter(ANO == 2010) %>%  group_by(DISTRITO) %>% 
  summarise(media_idh = mean(IDHM),
            media_gini = mean(GINI),
            expectativa = mean(ESPVIDA),
            renda_percapita = mean(RDPC))

#Removendo acentos para realizar join:
filtro_idh[, "DISTRITO"] <-  stri_trans_general(str = filtro_idh$DISTRITO, 
                                                id = "Latin-ASCII") %>% tolower() 

#Deixando o prefixo CID similar ao dataframe do shapefile:
filtro_idh$DISTRITO <-  sub("cidade", "cid", filtro_idh$DISTRITO)
filtro_idh$DISTRITO <- sub(" ", "-", filtro_idh$DISTRITO)
filtro_idh$DISTRITO <- sub(" ", "-", filtro_idh$DISTRITO)

#Deixando variavel com mesmo nome:
filtro_idh <- filtro_idh %>% rename("NOME_DIST" = "DISTRITO")

#Join
sf_shp <- sf_shp %>% left_join(filtro_idh, sf_shp,
                               by = "NOME_DIST")
#Trazendo as novas variáveis para o data set principal:
inter_mean <- st_intersection(inter, sf_shp) 

#Renomeando as novas variáveis:
inter_mean <- inter_mean %>% rename(bairro = 18,
                                    media_bairro = 33,
                                    qtd_dados_bairro = 34,
                                    zona = 35,
                                    expectativa_vida = 38)%>% 
  select(everything(),-c(15:17, 19:32)) 

#Plotando os resultados:

#BoxPlot por região da cidade
ggplotly(ggplot(data = inter_mean, mapping = aes(x = zona, y = unit)) +
  geom_boxplot(fill = "#0C4C8A") +
  labs(y = "Valor Unitário (R$/m²)", 
       title = "Boxplot do Valor Unitário (R$/m²) por região"))

#Os 5 bairros com maiores e menores valor unitários médios em oferta
df_ordenado <- sf_shp[, c(4,12,10)] %>% st_drop_geometry() 
df_ordenado <- df_ordenado[order(df_ordenado$media, decreasing = TRUE), ]
df_ordenado <- df_ordenado[complete.cases(df_ordenado),]
top_5 <- head(df_ordenado, 5)
bottom_5 <- tail(df_ordenado, 5)
df_tops <- rbind(top_5, bottom_5)

#Plotando os resultados

ggplot(df_tops, aes(x = reorder(NOME_DIST, -media), y = media, fill = zona)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label(aes(x= NOME_DIST, label = round(media,0)),
             show.legend = F,
             fill = "white") +
  labs(title = "Bairros com os maiores e menores valores unitários médios por m² em São Paulo", 
       x = "Bairro", y = "Valor Unitário (R$/m²)", fill = "Região")

#Mapas: 
  
#Unitário
unit_plot <- tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_text("NOME_DIST", size = 0.3) +
  tm_fill(col = "media",
          title = "Valor Unitário médio (R$/m²)",alpha = 0.9,
          popup.vars = c("Bairro" = "NOME_DIST", 
                         "Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa"))

#IDH
idh_plot <- tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_fill(col = "media_idh",
          title = "Média IDH (2010)",alpha = 0.7,
          popup.vars = c("Bairro" = "NOME_DIST", 
                         "Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa"))

#Índice Gini (desigualdade):
gini_plot <- tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_fill(col = "media_gini",
          title = "Média GINI (2010)",alpha = 0.7,
          popup.vars = c("Bairro" = "NOME_DIST", 
                         "Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa"))

#Renda percapita
renda_plot <- tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_fill(col = "renda_percapita",
          title = "Renda Média (2010)",alpha = 0.7,
          popup.vars = c("Bairro" = "NOME_DIST", 
                         "Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa"))

#Expectativa de vida
vida_plot <- tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_fill(col = "expectativa",
          title = "Expectativa de vida (2010)",alpha = 0.7,
          popup.vars = c("Bairro" = "NOME_DIST", 
                         "Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa"))

#Comparativos
tmap_mode("view")
tmap_arrange(unit_plot, renda_plot, vida_plot, idh_plot,ncol = 2)

#Correlação entre os índices socioeconômicos e o valor médio por m² do bairro:
vars <- as.data.frame(inter_mean[, c(16, 19:22)]) 
vars <- vars %>% select(1:5)
vars %>%
  corr_plot(media_bairro, media_idh, expectativa_vida, renda_percapita,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl",)

###############################################################################
#              Cálculo das distâncias para estações do metrô                  #
###############################################################################

#objeto simple feature dos dados em utm
sf_aptos_utm <- inter_mean %>% st_transform(CRS("+init=epsg:22523"))

#data frame aptos em utm
dados_utm <- sf_aptos_utm %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,2],
                lon = sf::st_coordinates(.)[,1]) %>% st_drop_geometry()

#data frame estacoes em WGS 84
estacoes_geod <- sp_metro_geod@data
estacoes_geod <- estacoes_geod %>% mutate(lon = sp_metro_geod@coords[,1],
                                          lat = sp_metro_geod@coords[,2])

#data frame estacoes em utm
estacoes_utm <- sp_metro_utm@data
estacoes_utm <- estacoes_utm %>% mutate(lon = sp_metro_utm@coords[,1],
                                        lat = sp_metro_utm@coords[,2])

#data frames auxiliares para calcular distancias para o metrô
df1 <- dados_utm
df2 <- estacoes_utm
df1$x1 <- dados_utm$lon
df1$y1 <- dados_utm$lat
df2$x2 <- estacoes_utm$lon
df2$y2 <- estacoes_utm$lat

#funcao
dist_func = function (x1, y1, x2, y2){
  dist = sqrt(((x2 - x1)^2) + ((y2-y1)^2))
  return(dist)
}

#criando colunas para registro
df1$estacao_prox = NA
df1$linha_prox = NA
df1$dist = NA

#looping
for(i in 1:nrow(df1)){
  min_dist = 0
  x1 = df1$x1[i]
  y1 = df1$y1[i]
  for(j in 1:nrow(df2)){
    x2 = df2$x2[j]
    y2 = df2$y2[j]
    current_dist = dist_func(x1, y1, x2, y2)
    if(j == 1){
      min_dist = current_dist
      estacao_prox = df2$emt_nome[j]
      linha_prox = df2$emt_linha[j]
    }
    if(current_dist < min_dist){
      min_dist = current_dist
      estacao_prox = df2$emt_nome[j]
      linha_prox = df2$emt_linha[j]
    }
  }
  df1$estacao_prox[i] = estacao_prox
  df1$linha_prox[i] = linha_prox
  df1$dist[i] = min_dist
}

#salvando dados no dataset original
dados_utm <- df1[,c(1:24,27:29)]

#transformando novamente em simple feature utm
sf_aptos_utm <- st_as_sf(x = dados_utm, 
                         coords = c("lon", "lat"), 
                         crs = "+init=epsg:22523")

#transformando novamente em simple feature WGS 
sf_aptos_geod <- sf_aptos_utm %>% st_transform(CRS("+proj=longlat"))

#Plotando pontos mais próximos de cada linha do metrô:

#Geral
dist_geral <- ggplot(sf_aptos_geod) +
    geom_point(aes(x = dist, y = unit), size = 1.5, 
               colour = "#1A5276") +
    geom_smooth(aes(x = dist, y = unit),
                method = "lm", formula = y ~ log(x), se = F, size = 0.5, legend = F) +
    labs(x = "Distância ao metrô (m)", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 14L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold", size = 12), 
          axis.title.x = element_text(face = "bold", size = 12))


#Linha azul
linha_azul <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "AZUL")) +
    geom_point(aes(x = dist, y = unit), size = 1.5, 
               colour = "#0C4C8A") +
    geom_smooth(aes(x = dist, y = unit),
                method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
    labs(x = "Distância ao metrô", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"))


#Linha amarela
linha_amarela <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "AMARELA")) +
    geom_point(aes(x = dist, y = unit), size = 1.5, 
               colour = "#0C4C8A") +
    geom_smooth(aes(x = dist, y = unit),
                method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
    labs(x = "Distância ao metrô", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"))


#Linha vermelha
linha_vermelha <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "VERMELHA")) +
    geom_point(aes(x = dist, y = unit), size = 1.5, 
               colour = "#0C4C8A") +
    geom_smooth(aes(x = dist, y = unit),
                method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
    labs(x = "Distância ao metrô", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"))


#Linha verde
linha_verde <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "VERDE")) +
    geom_point(aes(x = dist, y = unit), size = 1.5, 
               colour = "#0C4C8A") +
    geom_smooth(aes(x = dist, y = unit),
                method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
    labs(x = "Distância ao metrô", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"))


#Linha lilás
linha_lilas <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "LILAS")) +
    aes(x = dist, y = unit) +
    geom_point(size = 1.5, 
               colour = "#0C4C8A") +
    geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
    labs(x = "Distância ao metrô", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"))


#Linha prata
linha_prata <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "PRATA")) +
    aes(x = dist, y = unit) +
    geom_point(size = 1.5, 
               colour = "#0C4C8A") +
    geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) + 
    labs(x = "Distância ao metrô", y = "Unitário (R$/m²)") +
    theme_light() +
    theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"))

plot_grid(dist_geral, linha_amarela, linha_azul, linha_verde, linha_lilas, linha_vermelha, linha_prata, 
          labels = c("Dist. geral", "Linha Amarela", "Linha Azul",
                     "Linha Verde", "Linha Lilás", "Linha Vermelha","Linha Prata"),
          label_size = 10,
          ncol = 3, nrow = 3)

#Plotando com os pontos das estações
tmap_mode("view")

tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_fill(col = "media",
          title = "Média do Bairro (R$/m²)",alpha = 0.7,
          popup.vars = c("Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa"),
          id = "NOME_DIST") +
  tm_shape(shp = sf_aptos_geod) + 
  tm_dots(size = 0.05,
          col = "unit",
          title = "R$/m²",
          id = "endereco",
          popup.vars = c("Valor total (R$)" = "valor_total", "Unitário (R$/m²):" = "unit",
                         "Area (m²):" = "area_util", "Quartos:" = "quartos",
                         "Banheiros" = "banheiros","Suites:" = "suites", "Vagas:" = "vagas",
                         "Piscina:" = "piscina", "Endereço" = "endereco", "Bairro" = "bairro", 
                         "Linha mais próxima" = "linha_prox", "Estação mais próxima" = "estacao_prox", 
                         "Distância até a estação (m)" = "dist")) +
  tm_shape(shp = sp_metro_geod)+
  tm_dots(col = "emt_linha", palette=c(VERMELHA='red', AZUL='blue', LILAS='pink', VERDE='green', AMARELA='yellow', PRATA='black'), 
          stretch.palette = FALSE,size = 0.05,
          id = "emt_nome",
          title = "Linha do metrô",
          popup.vars = c("Linha" = "emt_linha", 
                         "Estação" = "emt_nome")) 

###############################################################################
#    Diagnóstico de Autocorrelação Espacial Global - Estatística I de Moran   #                         #
###############################################################################

#levando a variável "média" para o shapefile
shp_long_lat@data$media <- sf_shp$media

#Verificando bairros com NAs
shp_long_lat@data[!complete.cases(shp_long_lat@data),]

#Função para excluir dados faltantes do shapefile
moran.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("Linhas excluídas: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("Colunas excluídas: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

#Removendo os bairros a partir da função sp.na.omit
shp_moran_test <- moran.na.omit(shp_long_lat)

#Estabelecendo vizinhanças por contiguidade, critério queen:
vizinhos_queen <- poly2nb(pl = shp_moran_test,
                          queen = TRUE,
                          row.names = shp_moran_test@data$NOME_DIST)

# Informações relevantes sobre a vizinhança queen estabelecida:
summary(vizinhos_queen)

#Matriz W?
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B")

#Para facilitar o estudo da matriz W:
colnames(matrizW_queen) <- shp_moran_test@data$NOME_DIST
listw_queen <- mat2listw(matrizW_queen)
class(listw_queen)

#Aplicando teste de Moran
moran.test(x = shp_moran_test@data$media, 
           listw = listw_queen, 
           zero.policy = TRUE)


#Diagrama da Estatística I de Moran 
moran.plot(x = shp_moran_test@data$media, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Valor unitário médio", 
           ylab = "Valor unitário médio Espacialmente Defasado",
           pch = 15)

#Outra forma de plotar o diagrama de Moran
vetor_unit <- shp_moran_test@data$media

wunit <- lag.listw(x = listw_queen,
                   var = shp_moran_test@data$media,
                   zero.policy = TRUE)

base_moran <- data.frame(bairro = shp_moran_test@data$NOME_DIST,
                         media_unit = vetor_unit,
                         wunit)

#Plotagem
ggplotly(
  base_moran %>% 
    ggplot(aes(label = bairro)) +
    geom_point(aes(x = media_unit, y = wunit)) +
    geom_text(aes(x = media_unit, y = wunit, label = bairro), size = 2.4, nudge_y = 800) +
    geom_smooth(aes(x = media_unit, y = wunit), method = "lm", se = F) +
    geom_hline(yintercept = mean(base_moran$wunit), lty = 2) +
    geom_vline(xintercept = mean(base_moran$media_unit), lty = 2) +
    labs(x = "Valor unitário médio (R$/m²)", y = "Valor unitário defasado", title = "Diagrama de Moran") +
    annotate("text", x = 3200, y = 80000, label = "Low-High", col = "#0C4C8A") +
    annotate("text", x = 3200, y = 3000, label = "Low-Low", col = "#0C4C8A" ) +
    annotate("text", x = 14000, y = 80000, label = "High-High", col = "#0C4C8A") +
    annotate("text", x = 14000, y = 3000, label = "High-Low", col = "#0C4C8A") +
    theme_grey()
) 
