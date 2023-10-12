#' ---
#' title: "Linguagem R para avaliação de mercado de apartamentos em São Paulo – SP - Tcc - Usp/Esalq"
#' author: "José Luiz Grego"
#' ---

#carregando pacotes utilizados
pacotes <- c("httr","rvest","jsonlite", "tidyr", "dplyr", "stringr", "ggmap",
             "mapview","rgdal","tmap","maptools","sf","rgeos","sp","adehabitatHR",
             "rayshader","knitr","kableExtra","RColorBrewer","profvis", "ggrepel",
             "plotly", "spdep", "neuralnet","leaflet", "leafgl", "leafpop", "rstatix",
             "readr", "tinytex", "tibble", "stringi", "Hmisc", "data.table", 
             "PerformanceAnalytics", "equatiomatic", "cowplot", "nortest", "car", "readxl",
             "stargazer", "see", "ggraph", "metan", "gridExtra", "grid", "graphics",
             "plyr", "openxlsx", "olsrr")


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

#Transformando o shapefile para o Datum WGS84
shp_long_lat <- spTransform(shp_saopaulo, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 

#Transformando o shapefile em um objeto simplefeature 
sf_shp <- spTransform(shp_long_lat, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>% 
  sf::st_as_sf()

#Nomes dos bairros em letras minúsculas
sf_shp <- sf_shp %>% mutate(NOME_DIST = gsub(" ", "-", sf_shp$NOME_DIST) %>% tolower())

#Base de dados com índices sociais e econômicos por distrito de São Paulo, que será utilizado
#como auxiliar para obter as regiões de pesquisa dos dados. Fonte: Geosampa
idh_sp <- read_excel("idh_sp.xls", sheet = "dados")

#carregando sp do metro          
sp_metro_utm <- readOGR("metro", "metro_sp", use_iconv = TRUE, encoding = "UTF-8")

#alterando datum para utm
proj4string(sp_metro_utm) <- CRS("+init=epsg:22523")

#alterando datum para WGS 84
sp_metro_geod <- spTransform(x = sp_metro_utm,
                             CRSobj = CRS("+proj=longlat"))

#Data frame auxiliar com as regiões de São Paulo para pulverizar a busca de dados pela malha do município;
regioes <- data.frame("distrito" = idh_sp$DISTRITO,
                      "zona" = idh_sp$REGIAO8)

#Modificando o nome das regiões conforme padrão de pesquisa no site Viva Real:
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

################################################################################
#                              Web Scraping                                    #
################################################################################

#data set para armazenar os links capturados:
base_links <- data.frame()
i <- 1
set.seed(36666)

#link raiz de onde se iniciará a análise:
link_geral <- paste("https://www.vivareal.com.br/venda/sp/sao-paulo/",regioes[i,2],"/",regioes[i,1],"/apartamento_residencial/", sep = "")

#Instalar a extensão "SelectorGadget" no google chrome  para obter os "nós" dos
#campos de variáveis desejadas

#Fazendo a busca de links por bairros do município:
for (regiao in 1:length(regioes$distrito)){
  Sys.sleep(4)
  #lendo o link
  page_anuncio <- read_html(link_geral)
  
  #capturando somente os sufixos de links de anúncios:
  link_anuncio <- page_anuncio %>% html_nodes("a") %>% html_attr("href") %>% 
    grep('/imovel/apartamento', ., value = TRUE) %>% unique() 
  
  #colando o prefixo comum aos links:
  link_anuncio <- paste("https://www.vivareal.com.br", link_anuncio, sep = "")
  
  #Armazenando os dados do loop e mudando contador
  base_links <- rbind(base_links, data.frame(link_anuncio))
  print(link_geral)
  i <- i + 1
  link_geral <- paste("https://www.vivareal.com.br/venda/sp/sao-paulo/",regioes[i,2],"/",regioes[i,1],"/apartamento_residencial/", sep = "")
}

#Removendo links duplicados
base_links_unique <- unique(base_links)

#Inicicar o scraping por página:
link <- (base_links[1,])

#Base de dados para armazenar as variáveis coletadas:
dados <- data.frame()

#Looping:
for (pagina in 1:length(base_links$link_anuncio)){
  
  #ler o link:
  page <- read_html(link)
  
  #capturando as features:
  endereco <- page %>% 
    html_node(".js-address") %>% 
    html_text() 
  
  valor_total <- page %>% 
    html_node(".js-price-sale") %>% 
    html_text()
  
  area_util <-  page %>% 
    html_node(".js-area") %>% 
    html_text()
  
  condominio <- page %>%
    html_node(".js-condominium") %>% 
    html_text()
  
  quartos <- page %>%
    html_node(".js-bedrooms") %>% 
    html_text()
  
  banheiros_suites <- page %>%
    html_node(".js-bathrooms") %>% 
    html_text()
  
  vagas <- page %>%
    html_node(".js-parking") %>% 
    html_text()
  
  #coluna para capturar equipamentos do imóvel (piscina, quadra, salão de festas etc.)
  descricao <- page %>%
    html_node(".description__text") %>% 
    html_text()
  
  #Armazenando os dados do loop e mudando contador
  dados <- rbind(dados, data.frame(endereco, valor_total, area_util, quartos, banheiros_suites, vagas, descricao, condominio,link))
  link <- (base_links[1+pagina,])
  print(link)
  print(pagina)
  Sys.sleep(5)
}

#Limpando espaços, caracteres não utilizados das variáveis e alterando
#os tipos, quando pertinente:

#Valor total do imóvel
dados$valor_total <-  gsub("[^[:digit:]]", "", dados$valor_total) %>% as.numeric

#Valor do condompinio:
dados$condominio <- gsub("[^[:digit:]]", "", dados$condominio) %>% as.numeric

#Área útil do imóvel:
dados$area_util <-  gsub("[^[:digit:]]", "", dados$area_util) %>% as.numeric

#Número de vagas de garagem:
dados$vagas <- gsub("[^[:digit:]]", "", dados$vagas) %>% as.numeric()

#Quantidade de quartos:
dados$quartos <-gsub("[^[:digit:]]", "", dados$quartos) %>% as.numeric()

#Quantidade de suítes:
dados$suites <- str_extract(dados$banheiros_suites, "\\d+(?= suíte)") %>% as.numeric()

#Quantidade de banheiros:
dados$banheiros <- str_extract(dados$banheiros_suites, "\\d+(?= banheiro)") %>% as.numeric()

#O condompinio possui piscina? 1 = Sim, 0 = Não
dados$piscina <-  +(grepl("Piscina | piscina", dados$descricao )) %>% as.factor()

#O condomínio possui academia? 1 = Sim, 0 = Não
dados$academia <- +(grepl("Academia | academia | Fitness | fitness", dados$descricao )) %>% as.factor

#O condomínio possui quadra esportiva? 1 = Sim, 0 = Não
dados$quadra <- +(grepl("Quadra | quadra ", dados$descricao )) %>% as.factor()

#Valor Unitário:
dados$unit <- dados$valor_total/dados$area_util

################################################################################
#                     Análise espacial e exploratória dos dados                #
################################################################################

#Acessando API do Google Maps para coletar coordenadas geográficas
#de cada elemento. Mais informações de como obter a chave para acessar a API
#em: https://mapsplatform.google.com/. O pacote ggmap também dá orientações 
#de como obter e utilizar a API: https://search.r-project.org/CRAN/refmans/ggmap/html/register_google.html
register_google(key = "cole aqui a sua chave API do Maps", write = TRUE) #chave pessoal

#Removendo dados que não possuem endereço e, na sequência,
#coletando as latitudes e longitudes:
dados <- dados[complete.cases(dados$endereco),]
dados <- mutate_geocode(dados, location = endereco, output = "latlona")

#Removendo dados que não foram encontradas coordenadas:
dados <- dados[complete.cases(dados$lon, dados$area_util),]

#Renomeando e organizando algumas variáveis:
dados_sp_pt <- dados %>% 
  mutate(
    id = (1:length(valor_total)),
    id = as.character(id),
    across(unit, round, digits = 2),
    across(c(vagas,suites), replace_na, 0),
    across(c(banheiros, quartos), replace_na, 1)) %>%
  select(-c(5, 7, 18)) %>% 
  select(id, valor_total, unit, everything(), -c(link, lat, lon, endereco), c(endereco, lat, lon, link ))

#visualizando a base de dados
dados_sp_pt %>%
  head(n = 5) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

write.xlsx(dados_sp_pt[1:10,], "dados_sp_exemplo.xlsx")

#Plotagem de histograma e correlações das variáveis quanti
chart.Correlation(dados_sp_pt[, 2:9])

#Salvando base para testes futuros
write.xlsx(dados_sp_pt, "dados_sp.xlsx")

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
a
#Excluindo dados que possam estar fora dos limites de São Paulo, através
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

#Boxplot da variável dependente "Valor Unitário/m²":                      
box_unit <- ggplot(dados_sp_pt) +
  aes(x = "", y = unit) +
  geom_boxplot(fill = "#A52A2A") +
  labs(y = "Valor Unitário (R$/m²)", 
       title = "Boxplot do Valor Unitário (R$/m²)") +
  coord_flip() +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", 
                                  hjust = 0.5),
        axis.title.x = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0, 125000), breaks = seq(0, 125000, by = 20000)) +
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

#Função para detectar outliers extremos na variável, que 
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

# Calculando as novas medidas resumo para legenda
media_inter <- mean(inter$unit)
mediana_inter <- median(inter$unit)
primeiro_quartil_inter <- quantile(inter$unit, 0.25)
terceiro_quartil_inter <- quantile(inter$unit, 0.75)
minimo_inter <- min(inter$unit)
maximo_inter <- max(inter$unit)
quant_dados_inter <- length(inter$unit)


# Criando a string com os valores resumo
texto_resumo_inter <- paste0("Valor mínimo = R$ ", round(minimo_inter, 2), "\n",
                             "1º quartil = R$ ", round(primeiro_quartil_inter, 2), "\n",
                             "Mediana = R$ ", round(mediana_inter, 2), "\n",
                             "Média = R$ ", round(media_inter, 2), "\n",
                             "3º quartil = R$ ", round(terceiro_quartil_inter, 2), "\n",
                             "Valor Máximo = R$ ", round(maximo_inter, 2), "\n",
                             "Quantidade de dados = ", quant_dados_inter)

#Novo Boxplot dos valores unitários
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

#Comparação dos boxplots
plot_grid(box_unit, box_unit_out, labels = "AUTO")

#Após a eliminação, nota-se que os dados ficaram com distribuição mais próxima à normal. 
#É possível que os valores excluídos sejam de fato dados de mercado, porém, conforme NBR 14653-2,
#é pertinente que esse mercado seja tratado em modelos com amostra formada por dados mais 
#semelhantes, nesse caso por imóveis de altíssimo padrão. 

#Calculando a média por bairro, tendo por objetivo verificar o grau de 
#associação espacial dos dados (Índice de Moran):
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

ggplot(df_tops, aes(x = reorder(NOME_DIST, -media), y = media,fill = zona, label = round(media))) +
  geom_bar(stat = "identity", position = "dodge",) +
  geom_text(size = 5, position = position_stack(vjust = 1.04)) +
  labs(title = "Bairros com os maiores e menores valores unitários médios por m² em São Paulo", 
       x = "Bairro", y = "Valor Unitário (R$/m²)", fill = "Região")


#Unitário
unit_plot <- tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 0.4) +
  tm_fill(col = "media",
          title = "Valor Unitário médio (R$/m²)",alpha = 0.7,
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
            lab.position = "bl")

#Cálculo das distâncias para estações do metrô

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
  labs(x = "Distância ao metrô (m)", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Visão geral") +
  theme_light() +
  theme(plot.title = element_text(size = 18L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold", size = 16), 
        axis.title.x = element_text(face = "bold", size = 16)) +
  scale_x_continuous(limits = c(0, 35000), breaks = seq(0, 35000, by = 5000))


#Linha azul
linha_azul <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "AZUL")) +
  geom_point(aes(x = dist, y = unit), size = 1.5, 
             colour = "#0C4C8A") +
  geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
  labs(x = "Distância ao metrô", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Linha Azul") +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))


#Linha amarela
linha_amarela <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "AMARELA")) +
  geom_point(aes(x = dist, y = unit), size = 1.5, 
             colour = "#0C4C8A") +
  geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
  labs(x = "Distância ao metrô", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Linha Amarela") +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))


#Linha vermelha
linha_vermelha <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "VERMELHA")) +
  geom_point(aes(x = dist, y = unit), size = 1.5, 
             colour = "#0C4C8A") +
  geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
  labs(x = "Distância ao metrô", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Linha Vermelha") +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))


#Linha verde
linha_verde <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "VERDE")) +
  geom_point(aes(x = dist, y = unit), size = 1.5, 
             colour = "#0C4C8A") +
  geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) +
  labs(x = "Distância ao metrô", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Linha Verde") +
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
  labs(x = "Distância ao metrô", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Linha Lilás") +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))


#Linha prata
linha_prata <- ggplot(sf_aptos_geod %>% filter(sf_aptos_geod$linha_prox == "PRATA")) +
  aes(x = dist, y = unit) +
  geom_point(size = 1.5, 
             colour = "#0C4C8A") +
  geom_smooth(aes(x = dist, y = unit),
              method = "lm", formula = y ~ x, se = F, size = 0.5, legend = F) 
labs(x = "Distância ao metrô", y = "Unitário (R$/m²)", title = "Distância metrô x Valor Unitário (R$/m²) - Linha Prata") +
  theme_light() +
  theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
        axis.title.x = element_text(face = "bold"))

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


#Distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias:
df_auxiliar <- as.data.frame(inter_mean)
chart.Correlation(df_auxiliar[2:9], histogram = TRUE)

#Frequência de dados por distrito
freq_bairros <- table(df_auxiliar$NOME_DIST)
freq_bairros <- freq_bairros[order(freq_bairros, decreasing = TRUE)]
freq_bairros %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#data frame aptos em utm
dados_wgs <- sf_aptos_geod %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,2],
                lon = sf::st_coordinates(.)[,1]) %>% st_drop_geometry()

#Salvando base para testes futuros
write.xlsx(dados_wgs, "dados_wgs.xlsx")

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


###############################################################################
#                     Análise de Regressão múltipla                           #
###############################################################################

#Transformando em data frame para iniciar modelo de regressão
base_regre_total <- sf_aptos_geod %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,2],
                lon = sf::st_coordinates(.)[,1]) %>% st_drop_geometry() 

#Removendo colunas não utilizadas
base_regre_total <- as.data.frame((na.omit(base_regre_total[c(2:12,16,18:21,24)])))

#Removendo elemento com quantidade improvável de vagas de garagem
base_regre_total <- base_regre_total %>% filter(vagas < 39)

#Transformando todas as colunas em numéricas
base_regre_total[] <- lapply(base_regre_total, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

#Regressão linear múltipla:
aptos_model_total <- lm(formula = unit~. -valor_total -media_bairro,  base_regre_total)
summary(aptos_model_total)

################################################################################
#                            PROCEDIMENTO STEPWISE                             #
################################################################################

#Aplicando o procedimento Stepwise, temos o seguinte código:
step_aptos_total <- step(aptos_model_total, k = 3.841459)
summary(step_aptos_total)

#Teste de Shapiro-Francia
sf.test(step_aptos_total$residuals)

#Plotando os resíduos do modelo step_empresas
residuos_total <- base_regre_total %>%
  mutate(residuos = step_aptos_total$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_aptos_total$residuals),
                            sd = sd(step_aptos_total$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  ggtitle("Resíduos do modelo inicial")
theme_bw()

##############################################################################
#                         BOX-COX                                            #
##############################################################################

#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(base_regre_total$unit)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
base_regre_total$bcunit <- (((base_regre_total$unit ^ lambda_BC$lambda) - 1) / 
                              lambda_BC$lambda)

#Gerando novo modelo
aptos_model_total_bc <- lm(formula = bcunit~. -valor_total -media_bairro -unit,  base_regre_total)
summary(aptos_model_total_bc)

#Aplicando procedimento stepwise
aptos_model_total_bc_step <- step(aptos_model_total_bc)
summary(aptos_model_total_bc_step)

#Teste de Shapiro-Francia
sf.test(aptos_model_total_bc_step$residuals)

#Plotando os resíduos do modelo aptos_model_total_bc_step
residuos_total_bc <- base_regre_total %>%
  mutate(residuos = aptos_model_total_bc_step$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(aptos_model_total_bc_step$residuals),
                            sd = sd(aptos_model_total_bc_step$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  ggtitle("Resíduos após Box-cox")
theme_bw()

#Comparação dos resíduos plotados
grid.arrange(residuos_total,residuos_total_bc)

#Valores observados x Valores calculados
ggplotly(ggplot(base_regre_total, aes(x = aptos_model_total_bc_step$fitted.values, y = unit)) + geom_point() + geom_smooth(method = "lm"))

#Diagnóstico do modelo
par(mfrow = c(2,2))
plot(aptos_model_total_bc_step, which=c(1:4), pch = 20)

#Multicolinearidade
#NIHIL

#Teste de Breush-Pagan
#NIHIL

#############################################################################
#                     Modelo por bairros contíguos                          #                     
#############################################################################

grupos <- list()

# loop pelas colunas da matriz
for (i in 1:ncol(matrizW_queen)) {
  # obter o nome da coluna
  nome_coluna <- colnames(matrizW_queen)[i]
  # obter os índices dos elementos iguais a 1 na coluna
  indices <- which(matrizW_queen[, i] == 1)
  # adicionar os índices à lista de grupos, com o nome da coluna como chave
  grupos[[nome_coluna]] <- indices
}

#Analisando os bairros que fazem fronteira com o distrito da Saúde
grupos$SAUDE

#Data frame com os dados dos bairros da Saúde e seus vizinhos 
base_regre_saude <- sf_aptos_geod %>%
  filter(bairro == "CAMPO BELO"|
           bairro == "MOEMA"|
           bairro == "CURSINO"|
           bairro == "SAUDE"|
           bairro == "JABAQUARA"|
           bairro == "VILA MARIANA") %>% 
  dplyr::mutate(lat = sf::st_coordinates(.)[,2],
                lon = sf::st_coordinates(.)[,1]) %>% st_drop_geometry() 

summary(base_regre_saude)

#visualizando bairros selecionados

tm_shape(shp = sf_shp %>% filter(NOME_DIST == "campo-belo"|
                                   NOME_DIST == "moema"|
                                   NOME_DIST == "cursino"|
                                   NOME_DIST == "saude"|
                                   NOME_DIST == "jabaquara"|
                                   NOME_DIST == "vila-mariana")) + 
  tm_borders(alpha = 0.4) +
  tm_text("NOME_DIST", size = 0.8) +
  tm_fill(col = "media",
          title = "Valor Unitário médio (R$/m²)",alpha = 0.9,
          popup.vars = c("Bairro" = "NOME_DIST", 
                         "Média do bairro" = "media",
                         "Dados no bairro" = "count",
                         "Média do IDH do bairro" = "media_idh",
                         "Média índice Gini" = "media_gini",
                         "Renda per capita média" = "renda_percapita",
                         "Expectativa de vida média do bairro" = "expectativa")) +
  tm_shape(shp = sf_shp) + 
  tm_borders(alpha = 1)


#Removendo colunas não utilizadas
base_regre_saude <- as.data.frame((na.omit(base_regre_saude[c(2:12,16,18:21,24)])))

#Resumo da base
summary(base_regre_saude)

#Transformando todas as colunas em numéricas
base_regre_saude[] <- lapply(base_regre_saude, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

#Regressão linear múltipla:
aptos_model_saude <- lm(formula = unit~. -valor_total -media_bairro,  base_regre_saude)
summary(aptos_model_saude)

################################################################################
#                            PROCEDIMENTO STEPWISE                             #
################################################################################

#Aplicando o procedimento Stepwise, temos o seguinte código:
step_aptos_saude <- step(aptos_model_saude, k = 3.841459)
summary(step_aptos_saude)

#Teste de Shapiro-Francia
sf.test(step_aptos_saude$residuals)

#Plotando os resíduos do modelo step_empresas
residuos_saude <- base_regre_saude %>%
  mutate(residuos = step_aptos_saude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_aptos_saude$residuals),
                            sd = sd(step_aptos_saude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  ggtitle("Resíduos do modelo inicial") +
  theme_bw()

##############################################################################
#                         BOX-COX                                            #
##############################################################################

#Para calcular o lambda de Box-Cox
lambda_BC_saude <- powerTransform(base_regre_saude$unit)
lambda_BC_saude$lambda

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
#novo modelo
base_regre_saude$bcunit <- (((base_regre_saude$unit ^ lambda_BC_saude$lambda) - 1) / 
                              lambda_BC_saude$lambda)

#Gerando novo modelo
aptos_model_saude_bc <- lm(formula = bcunit~. -valor_total -unit -media_bairro,  base_regre_saude)
summary(aptos_model_saude_bc)

#Apliando procedimento stepwise
aptos_model_saude_bc_step <- step(aptos_model_saude_bc)
summary(aptos_model_saude_bc_step)

#Teste de Shapiro-Francia
sf.test(aptos_model_saude_bc_step$residuals)

#Multicolinearidade
ols_vif_tol(aptos_model_saude_bc_step)

#Removendo a variável "media_idh" devido a multicolinearidade (Tolerance próximo a 0 e VIF elevado)
aptos_model_saude_bc <- lm(formula = bcunit~. -valor_total -media_bairro -unit -media_idh,  base_regre_saude)
summary(aptos_model_saude_bc)

#Aplicando procedimento stepwise após remover media_idh
aptos_model_saude_bc_step <- step(aptos_model_saude_bc)
summary(aptos_model_saude_bc_step)

#Teste de Shapiro-Francia após remover media_idh
sf.test(aptos_model_saude_bc_step$residuals)

#Multicolinearidade após remover media_idh
ols_vif_tol(aptos_model_saude_bc_step)

#Teste de Breush-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(aptos_model_saude_bc_step)

#Diagnóstico do modelo
par(mfrow = c(2,2))
plot(aptos_model_saude_bc_step, which=c(1:4), pch = 20)

#Inserindo os valores preditos na base original 
base_regre_saude$yhat_step_saude_bc <- (((aptos_model_saude_bc_step$fitted.values*(lambda_BC_saude$lambda))+
                                           1))^(1/(lambda_BC_saude$lambda))

#Valores observados x Valores calculados
ggplotly(ggplot(base_regre_saude, aes(x = unit, y = yhat_step_saude_bc)) + 
           geom_point() + geom_smooth(method = "lm"))


#Ajustes dos modelos: valores previstos (fitted values) X valores reais
base_regre_saude %>%
  ggplot() +
  geom_smooth(aes(x = unit, y = yhat_step_saude_bc, color = "Stepwise Box-Cox - bairro Saúde, São Paulo - SP"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = unit, y = yhat_step_saude_bc),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = unit, y = unit), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelo:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Valores unitários observados (R$/m²)", y = "Valores previstos (R$/m²)") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom") +
  theme_light()


#Plotando os resíduos do modelo aptos_model_total_bc_step
residuos_saude_bc_step <- base_regre_saude %>%
  mutate(residuos = aptos_model_saude_bc_step$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(aptos_model_saude_bc_step$residuals),
                            sd = sd(aptos_model_saude_bc_step$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  ggtitle("Resíduos após Box-cox") +
  theme_bw()

#Comparação dos resíduos plotados
grid.arrange(residuos_saude,residuos_saude_bc_step)

#Alfa e Betas do modelo final:
aptos_model_saude_bc_step$coefficients
