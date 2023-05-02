# Análises espaciais sobre o mercado imobiliário de São Paulo - SP

![mapa geral](https://github.com/jlgrego/imoveis_sp/blob/main/assets/valores_unitarios.png)

# Sobre o projeto

Repositório que compõem uma parte do meu projeto de conclusão de curso no MBA em Data Science e Analytics pela USP/Esalq, previsto para junho de 2023. 

Em suma, o código permite, dentre outras coisas:

a) Impacto da distância para o metrô sobre os valores dos imóveis. 

![distancia_versus_unitario](https://github.com/jlgrego/imoveis_sp/blob/main/assets/dist_versus_unitario.png)

Foi possivel perceber, por exemplo, que a queda dos valores é mais acentuada para aqueles que estão numa distância para o metrô entre de 0 a 2.500 metros. A partir daí a nuvem de pontos comoeça a ficar homogêgena e sem um padrão percepitível. Uma hipótese seria que outros modais de transporte passem a influenciar essas regiões mais distantes, como por exemplo trens ou linhas de ônibus, fazendo com que os preços pouco sejam alterados em decorrência da proximidade ao metrô.

O script possui também as análises por linha do metrô.

b) Qual a estação de metrô mais próxima de cada imóvel

![base de dados]()

Essa análise pode ser muito útil, por exemplo, para verificar a média de preços praticados num raio de distância para determinada estação ou grupo delas. 

c) Quais as médias de valores das ofertas de apartamentos por bairro e zonas

![medias_bairros](https://github.com/jlgrego/imoveis_sp/blob/main/assets/medias_bairros.png)

Nesse caso foi possível analisar os bairros que registram as maiores e menores médias de valores. As zonas sul e oeste detém juntas, 4 dos 5 bairros com os valores mais altos, enquanto que a zona leste, sozinha, detém a mesma proporção,só que dos menores valores. 

d) Relação dos valores dos imóveis e os índices econômicos de cada bairro

![indices economicos](https://github.com/jlgrego/imoveis_sp/blob/main/assets/valor%20e%20indices.PNG)

Os mapas deixem evidente a correlação que existe entre os preços praticados e os índices socioeconômicos da cidade de São Paulo - SP.

A matriz de correlações corrobora a percepção

![matriz_correlacoes]()

e) Cálculo da estatística I de Moran para detecção de autocorrelação espacial

![estatistica_moran]()

Foi calculado, também, a estatística I de Moran. No caso em questão, esta foi positiva e estatisticamente significante ao nível de 5%, concluindo que os dados sofrem de um efeito de transbordo, o chamado "spillover", que ocorre quando os dados são cercados de vizinhos similares.

![moran_diagrama]()

A partir dela, foi possível a construção do diagrama de Moran. Nele, os dados são distribuídos em quatro quadrantes: Alto-Alto ou “High-High”, que são dados com altos valores próximos a dados com altos valores; Baixo-Baixo ou “Low-Low”, que são dados com baixos valores próximos a dados com baixos valores; Baixo-Alto ou “Low-High”, que são dados com baixos valores próximos a dados com altos valores e Alto-Baixo ou “High-Low”, que são dados com altos valores próximos a dados com baixos valores. 

# Sobre os arquivos

## analise_sp.R

Script geral

## dados_sp.xlsx

Em breve complementarei esse repositório com o script de web scraping, 100% em linguagem R, criado para coleta dos 2.500 dados analisados e que estão à venda num site especializado em anúncios imobiliários. O projeto vai conter, também, como se deu a geocodificação desses imóveis e um modelo supervisionado para estimação dos valores.  

## idh_sp

Base obtida no site https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx, utilizada para coleta de índices socioeconômicos do município de São Paulo - SP. 

## shapefile_municipio

Arquivo shapefile do município de São Paulo - SP, também obtido no site https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx
, sendo essencial para as análises espaciais.

## metro

Arquivo spatial point das estações de metrô da cidade de São Paulo - SP, também obtido no site https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx. 

O arquivo possui, dentre outras, as coordenadas de cada estação e, assim, possibilita a medição das distâncias de cada uma delas para os imóveis da base. 


