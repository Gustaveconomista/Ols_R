### TRABALHO APP ###
# Carregar pacotes
library(stargazer)
library(basedosdados)
library(tidyverse)
library(wooldridge)
library(lmtest)
library(sandwich)
# Defina o seu projeto no Google Cloud
set_billing_id("projetoapp-340617")
# Para carregar o dado direto no R
query <- bdplyr("br_sp_seduc_inse.escola")
df <- bd_collect(query)
View(df)

df = df %>% 
  select(id_escola, nivel_socio_economico)
View(df)

query1 <- bdplyr("br_sp_seduc_idesp.escola")
df1 <- bd_collect(query1)
View(df1)

df1 = df1 %>% 
  select(ano, id_escola, nota_idesp_em) %>% 
  filter(ano == "2018") %>% 
  select(id_escola, nota_idesp_em)
View(df1)

df2 = inner_join(df, df1)
View(df2)

y = df2$nota_idesp_em
x = df2$nivel_socio_economico
reg = lm(y ~ x)
result = stargazer(reg, type = "text", keep.stat = c("n","rsq"), float = FALSE, font.size = "small", digits=2, keep = 1)
View(result)
