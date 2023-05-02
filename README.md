# Análises espaciais sobre o mercado imobiliário de São Paulo - SP


![Mapa geral](https://github.com/jlgrego/imoveis_sp/blob/main/assets/valores%20unitarios.PNG)


# Sobre o projeto

Repositório que compõem uma parte do meu projeto de conclusão de curso no MBA em Data Science e Analytics pela USP/Esalq, previsto para junho de 2023. 

Em suma, o código permite, dentre outras coisas:

a) Entender aspectos espaciais do mercado imobiliário de apartamentos em São Paulo

b) Qual a estação de metrô mais proxi de cada imóvel

c) Quais as médias de valores das ofertas de apartamentos por bairro

d) Qual a média dos valores unitários dos imóveis em relação a uma determinada estação ou linha do metrô

e) Relação dos valores dos imóveis e os índices econômicos de cada bairro

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


