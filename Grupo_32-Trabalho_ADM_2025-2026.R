# ==============================================================================
# PROJETO ADM - PONTOS 1, 2 e 3 (VERSÃO FINAL CONSOLIDADA)
# Grupo: 32 - Montenegro
# ==============================================================================

# --- 0. PREPARAÇÃO ---
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(psych)) install.packages("psych")   
if(!require(car)) install.packages("car")
if(!require(ggcorrplot)) install.packages("ggcorrplot")


library(ggcorrplot)
library(tidyverse)
library(psych)
library(car)

rm(list = ls()) # Limpar a memória antes de começar

# ==============================================================================
# 0. CARREGAMENTO E TRATAMENTO
# ==============================================================================

# 0.1 Carregar o Ficheiro Bruto
if(!exists("dados_total")) {
  cat(">>> Seleciona o ficheiro RDS original na janela...\n")
  file_path <- file.choose() 
  dados_total <- readRDS(file_path)
}

# 0.2 Filtrar Montenegro e Confirmar Amostra
CODIGO_PAIS <- "Montenegro"
dados_me <- dados_total %>% filter(cntry == CODIGO_PAIS)

# --- NOVO: Output para o Relatório ---
cat("\n================================================\n")
cat(" RELATÓRIO DE AMOSTRA - MONTENEGRO \n")
cat("================================================\n")
cat("Total de observações importadas (N):", nrow(dados_me), "\n")
cat("Variáveis originais disponíveis:", ncol(dados_me), "\n")
cat("================================================\n\n")

# 0.3 "ABRIR" O FICHEIRO
# View(dados_me)

# 0.4 APLICAR O TRATAMENTO
dados_tratados <- dados_me %>%
  mutate(
    agea = as.numeric(as.character(agea)), 
    
    # --- ESTADO CIVIL ---
    marsts_rec = case_when(
      str_detect(marsts, regex("married|civil union", ignore_case = TRUE)) &
        !str_detect(marsts, regex("Never|divorced|separated|died|widowed", ignore_case = TRUE)) ~ "In relationship",
      
      str_detect(marsts, regex("divorced|separated|died|widowed", ignore_case = TRUE)) ~ 
        "Formerly in relationship",
      
      str_detect(marsts, regex("Never", ignore_case = TRUE)) ~ "Never in relationship",
      
      TRUE ~ NA_character_
    ),
    
    # --- ESCOLARIDADE (RECODIFICAÇÃO CORRETA) ---
    edulvlb_rec = case_when(
      # LOW
      edulvlb %in% c(
        "Not completed ISCED level 1",
        "ISCED 1, completed primary education",
        "Vocational ISCED 2C < 2 years, no access ISCED 3",
        "General/pre-vocational ISCED 2A/2B, access ISCED 3 vocational",
        "General ISCED 2A, access ISCED 3A general/all 3",
        "Vocational ISCED 2C >= 2 years, no access ISCED 3",
        "Vocational ISCED 2A/2B, access ISCED 3 vocational",
        "Vocational ISCED 2, access ISCED 3 general/all"
      ) ~ "EduLevel:Low",
      
      # MEDIUM
      edulvlb %in% c(
        "Vocational ISCED 3C < 2 years, no access ISCED 5",
        "General ISCED 3 >=2 years, no access ISCED 5",
        "General ISCED 3A/3B, access ISCED 5B/lower tier 5A",
        "General ISCED 3A, access upper tier ISCED 5A/all 5",
        "Vocational ISCED 3C >= 2 years, no access ISCED 5",
        "Vocational ISCED 3A, access ISCED 5B/ lower tier 5A",
        "Vocational ISCED 3A, access upper tier ISCED 5A/all 5",
        "General ISCED 4A/4B, access ISCED 5B/lower tier 5A",
        "General ISCED 4A, access upper tier ISCED 5A/all 5",
        "ISCED 4 programmes without access ISCED 5",
        "Vocational ISCED 4A/4B, access ISCED 5B/lower tier 5A",
        "Vocational ISCED 4A, access upper tier ISCED 5A/all 5"
      ) ~ "EduLevel:Medium",
      
      # HIGH
      edulvlb %in% c(
        "ISCED 5A short, intermediate/academic/general tertiary below bachelor",
        "ISCED 5B short, advanced vocational qualifications",
        "ISCED 5A medium, bachelor/equivalent from lower tier tertiary",
        "ISCED 5A medium, bachelor/equivalent from upper/single tier tertiary",
        "ISCED 5A long, master/equivalent from lower tier tertiary",
        "ISCED 5A long, master/equivalent from upper/single tier tertiary",
        "ISCED 6, doctoral degree"
      ) ~ "EduLevel:High",
      
      # OTHER
      edulvlb == "Other" ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  )

# Colocar ordem correta FORA do mutate
dados_tratados$edulvlb_rec <- factor(
  dados_tratados$edulvlb_rec,
  levels = c("EduLevel:Low", "EduLevel:Medium", "EduLevel:High")
)

# ==============================================================================
# PONTO 1: DESCRITIVA
# ==============================================================================
cat("\n--- PONTO 1: GRÁFICOS E ESTATÍSTICAS ---\n")

# A) IDADE
# 1. Cálculos Estatísticos para o Relatório
cat("\n>> Estatísticas da Idade:\n")
print(summary(dados_tratados$agea))
cat("Desvio Padrão:", sd(dados_tratados$agea, na.rm = TRUE), "\n")

# 2. O Gráfico
print(ggplot(dados_tratados, aes(x = agea)) +
        geom_histogram(binwidth = 5, fill = "#4E79A7", color = "white") +
        labs(title = "A) Distribuição de Idades", x = "Idade", y = "Frequência") +
        theme_minimal())

# B) GÉNERO
cat("\n>> Estatísticas de Género (Copiar para o relatório):\n")

# 1. Calcular Tabela
tabela_genero <- dados_tratados %>% 
  mutate(gndr = replace_na(as.character(gndr), "Sem Resposta")) %>%
  count(gndr) %>% 
  mutate(Percentagem = round(n/sum(n)*100, 2)) # Calcula % e arredonda

print(tabela_genero)

# 2. O Gráfico
print(dados_tratados %>% 
        mutate(gndr = replace_na(as.character(gndr), "Sem Resposta")) %>%
        ggplot(aes(x = gndr, fill = gndr)) + # Nota: Tirei o y=n porque o geom_bar conta sozinho
        geom_bar() + 
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) + # Põe o número em cima da barra
        labs(title = "B) Distribuição por Género", y = "Contagem", x = NULL) + 
        theme_minimal() +
        theme(legend.position = "none"))

# C) ESCOLARIDADE
cat("\n>> Estatísticas de EscolaridadeS:\n")

# 1. Calcular Tabela
tabela_escolaridade <- dados_tratados %>% 
  mutate(cat_plot = replace_na(as.character(edulvlb_rec), "Sem Resposta")) %>%
  mutate(cat_plot = factor(cat_plot, levels = c("EduLevel:Low", "EduLevel:Medium", "EduLevel:High", "Sem Resposta"))) %>%
  count(cat_plot) %>% 
  mutate(Percentagem = round(n/sum(n)*100, 2))

print(tabela_escolaridade)

# 2. O Gráfico
print(dados_tratados %>% 
        mutate(cat_plot = replace_na(as.character(edulvlb_rec), "Sem Resposta")) %>%
        mutate(cat_plot = factor(cat_plot, levels = c("EduLevel:Low", "EduLevel:Medium", "EduLevel:High", "Sem Resposta"))) %>%
        ggplot(aes(x = cat_plot)) +
        geom_bar(fill = "#59A14F") +
        geom_text(stat='count', aes(label=..count..), vjust=-0.5) + # Números em cima das barras
        labs(title = "C) Nível de Escolaridade", x = NULL, y = "Contagem") +
        theme_minimal())

# D) ESTADO CIVIL
cat("\n>> Estatísticas de Estado Civil (Copiar para o relatório):\n")

# 1. Calcular Tabela para o Pie Chart
pie_data <- dados_tratados %>%
  mutate(cat_plot = if_else(is.na(marsts_rec), "Sem Resposta / Outro", marsts_rec)) %>%
  count(cat_plot) %>% 
  mutate(prop = round(n / sum(n) * 100, 2)) %>% 
  arrange(desc(prop))

print(pie_data)

# 2. O Gráfico (Pie Chart)
print(ggplot(pie_data, aes(x = "", y = prop, fill = cat_plot)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) + 
        theme_void() +
        geom_text(aes(label = paste0(round(prop, 1), "%")), 
                  position = position_stack(vjust = 0.5), 
                  fontface = "bold", color = "black") + # Pus a cor preta para se ler melhor
        labs(title = "D) Estado Civil Recodificado", fill = "Situação") +
        scale_fill_brewer(palette = "Set3"))


# ==============================================================================
# PONTO 2: ACP - VALORES DE VIDA
# ==============================================================================
cat("\n--- PONTO 2: ANÁLISE COMPONENTES PRINCIPAIS ---\n")

vars_valores <- c("BeCreative", "BeRich", "EqualOpport", "BeAdmired", "BeSecure", 
                  "TryNew", "FollowRules", "UnderstandPeople", "BeHumble", 
                  "HaveGoodTime", "BeFree", "HelpPeople", "Success", "StrongGov", 
                  "SeekAdv", "BehaveProp", "GetRespect", "BeLoyal", "CareNature", 
                  "Traditionalism", "SeekFun")

# Preparar dados LIMPOS para cálculo (sem NAs)
dados_analise <- dados_tratados %>% 
  select(idno, all_of(vars_valores), edulvlb_rec) %>% 
  drop_na()


# Matriz e Testes
cor_matrix <- cor(dados_analise %>% select(all_of(vars_valores)))
cat(">> KMO:", KMO(cor_matrix)$MSA, "\n")
cat(">> Bartlett p-value:", cortest.bartlett(cor_matrix, n=nrow(dados_analise))$p.value, "\n")

# Scree Plot
scree(cor_matrix, pc = TRUE, main = "Scree Plot - Critério de Kaiser")

# Executar ACP (Kaiser Rule: Eigenvalues > 1)
n_comp <- sum(eigen(cor_matrix)$values > 1)
pca_result <- principal(dados_analise %>% select(all_of(vars_valores)), nfactors = n_comp, rotate = "varimax", scores = TRUE)

print(pca_result$loadings, cutoff = 0.4, sort = TRUE)
print(pca_result$Vaccounted)

# Guardar Scores
scores_df <- as.data.frame(pca_result$scores)
colnames(scores_df) <- paste0("Dim_", 1:n_comp)
dados_finais <- cbind(dados_analise, scores_df)

# Gerar Matriz Visual
cor_matrix_visual <- cor(dados_analise %>% select(all_of(vars_valores)))
print(ggcorrplot(cor_matrix_visual, 
                 method = "square", 
                 type = "lower", 
                 lab = FALSE, # Não mete números para não poluir
                 tl.cex = 8, # Tamanho da letra
                 title = "Matriz de Correlação - Valores de Vida"))


# ==============================================================================
# PONTO 3: TESTES DE HIPÓTESES (Valores vs Escolaridade)
# ==============================================================================
cat("\n--- PONTO 3: ANOVA ---\n")

for(dim in colnames(scores_df)) {
  cat("\n>> Dimensão:", dim, "\n")
  
  # Boxplot (Sem NAs, pois usámos dados_analise)
  print(ggplot(dados_finais, aes(x = edulvlb_rec, y = .data[[dim]], fill = edulvlb_rec)) +
          geom_boxplot() + labs(title = paste(dim, "por Escolaridade")) + theme_minimal() + theme(legend.position="none"))
  
  # Testes
  print(leveneTest(as.formula(paste(dim, "~ edulvlb_rec")), data = dados_finais))
  
  anova_mod <- aov(as.formula(paste(dim, "~ edulvlb_rec")), data = dados_finais)
  print(summary(anova_mod))
  
  # Tukey se significativo
  if(summary(anova_mod)[[1]][["Pr(>F)"]][1] < 0.05) print(TukeyHSD(anova_mod))
}



# ==============================================================================
# PONTO 4: Indicador de Confiança Institucional (ACP)
# ==============================================================================
cat("\n--- PONTO 4: ACP (Confiança Institucional) ---\n")

# 4.1 Seleção de Variáveis
vars_confianca <- c(
  "trstprl", "trstlgl", "trstplc",
  "trstplt", "trstprt", "trstep", "trstun"
)

# 4.2 Preparação do Dataframe
# IMPORTANTE: Estamos a usar 'dados_tratados' porque é lá que está o 'edulvlb_rec'
df_confianca <- dados_tratados %>% 
  select(idno, all_of(vars_confianca), edulvlb_rec, gndr) %>% 
  drop_na() # Remove linhas com NAs nestas variáveis específicas

# 4.3 Testes de Adequabilidade
cor_matrix_conf <- cor(df_confianca %>% select(all_of(vars_confianca)))

cat("\n>> KMO (Confiança):\n"); print(KMO(cor_matrix_conf))
cat("\n>> Bartlett (Confiança):\n"); print(cortest.bartlett(cor_matrix_conf, n = nrow(df_confianca)))

# 4.4 Decisão de Componentes (Critério Eigenvalue > 1)
ev_conf <- eigen(cor_matrix_conf)$values
n_comp_conf <- sum(ev_conf > 1)
cat(paste("\n>>> Componentes a reter (Kaiser > 1):", n_comp_conf, "\n"))

# Scree Plot
scree(cor_matrix_conf, pc = TRUE, main = "Scree Plot - Confiança Institucional")

# 4.5 ACP (Forçado a 1 Componente - Indicador Único)
pca_result_conf <- principal(df_confianca %>% select(all_of(vars_confianca)), nfactors = 1, rotate = "none", scores = TRUE)

cat("\n>> Loadings (Confiança - 1 Componente):\n")
print(pca_result_conf$loadings, sort = TRUE, cutoff = 0.4)

cat("\n>> Variância Explicada:\n")
print(pca_result_conf$Vaccounted)

# 4.6 Criação da Variável de Indicador
df_confianca$Confianca_Inst_Score <- as.numeric(pca_result_conf$scores[,1])
cat("\nScore criado e guardado no df_confianca.\n")

# ==============================================================================
# PONTO 5: Influência de Escolaridade e Género na Confiança
# ==============================================================================
cat("\n--- PONTO 5: ANOVA DE DOIS FATORES ---\n")

# Preparação ligeira para garantir que a ANOVA corre sem erros
# (Convertemos gndr para factor apenas para esta análise, sem mexer no original)
dados_ponto5 <- df_confianca %>%
  mutate(gndr = as.factor(gndr)) 

# O Modelo: Confiança ~ Escolaridade * Género
modelo_anova <- aov(Confianca_Inst_Score ~ edulvlb_rec * gndr, data = dados_ponto5)

# 5.1 Validação de Pressupostos
cat("\n>> 5.1 Validação de Pressupostos:\n")

# Shapiro-Wilk (apenas se N < 5000, senão demora muito/falha)
if(nrow(dados_ponto5) <= 5000) {
  cat("Teste de Shapiro-Wilk:\n")
  print(shapiro.test(residuals(modelo_anova)))
} else {
  cat("Amostra > 5000. Verificar normalidade pelo Q-Q Plot.\n")
}

# Q-Q Plot
plot(modelo_anova, which = 2, main = "Q-Q Plot dos Resíduos (ANOVA)")

# Levene (Homogeneidade)
cat("\nTeste de Levene:\n")
print(leveneTest(Confianca_Inst_Score ~ edulvlb_rec * gndr, data = dados_ponto5))


# 5.2 Resultados da ANOVA
cat("\n>> 5.2 Tabela ANOVA:\n")
print(summary(modelo_anova))


# 5.3 Post-Hoc (Tukey)
# Só executamos se houver significância estatística
p_valor_escolaridade <- summary(modelo_anova)[[1]]["edulvlb_rec", "Pr(>F)"]

cat("\n>> 5.3 Análise Post-Hoc:\n")
if(!is.na(p_valor_escolaridade) && p_valor_escolaridade < 0.05) {
  cat("Diferenças significativas encontradas na Escolaridade. A executar TukeyHSD...\n")
  print(TukeyHSD(modelo_anova, which = "edulvlb_rec"))
} else {
  cat("A escolaridade não teve efeito significativo principal.\n")
}


# 5.4 Gráfico de Interação
cat("\n>> 5.4 Gráfico de Interação:\n")

plot_interacao <- dados_ponto5 %>%
  group_by(edulvlb_rec, gndr) %>%
  summarise(media_confianca = mean(Confianca_Inst_Score, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = edulvlb_rec, y = media_confianca, group = gndr, color = gndr)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Interação: Escolaridade x Género na Confiança",
       y = "Média do Score de Confiança", x = "Nível de Escolaridade") +
  theme_minimal()

print(plot_interacao)

# ==============================================================================
# PONTO 6: REGRESSÃO MÚLTIPLA – Determinantes da Felicidade
# ==============================================================================

cat("\n--- PONTO 6: REGRESSÃO MÚLTIPLA (Felicidade) ---\n")

# 6.1 Criar dataset base completo com todas as variáveis necessárias
dados_reg <- dados_tratados %>%
  select(idno, happy, gndr, agea, edulvlb_rec, marsts_rec) %>%
  left_join(scores_df %>% mutate(idno = dados_analise$idno), by = "idno") %>%
  left_join(df_confianca %>% select(idno, Confianca_Inst_Score), by = "idno") %>%
  drop_na(happy)

cat("Observações disponíveis para regressão:", nrow(dados_reg), "\n")

# 6.2 Modelo
modelo6 <- lm(
  happy ~ Dim_1 + Dim_2 + Dim_3 +
    agea + gndr + edulvlb_rec +
    Confianca_Inst_Score,
  data = dados_reg
)

cat("\n>> Resumo do Modelo:\n")
print(summary(modelo6))

# 6.3 Diagnósticos
cat("\n>> Teste VIF:\n")
print(car::vif(modelo6))

par(mfrow = c(2,2))
plot(modelo6)
par(mfrow = c(1,1))

# 6.4 Interpretação automática
cat("\n\n================ INTERPRETAÇÃO AUTOMÁTICA (Para o relatório) =================\n")

sum_out <- summary(modelo6)$coefficients
signifs <- rownames(sum_out)[sum_out[,4] < 0.05]

cat("Variáveis com efeito significativo na felicidade (p < 0.05):\n")
print(signifs)

cat("\nResumo interpretativo:\n")
if("Dim_1" %in% signifs) cat("- Dimensão 1 influencia significativamente a felicidade.\n")
if("Dim_2" %in% signifs) cat("- Dimensão 2 influencia significativamente a felicidade.\n")
if("Dim_3" %in% signifs) cat("- Dimensão 3 influencia significativamente a felicidade.\n")
if("agea" %in% signifs) cat("- A idade afeta significativamente a felicidade.\n")
if("gndrMale" %in% signifs) cat("- O género masculino tem efeito significativo na felicidade.\n")
if("edulvlb_recEduLevel:Medium" %in% signifs) cat("- Escolaridade média difere da baixa.\n")
if("edulvlb_recEduLevel:High" %in% signifs) cat("- Escolaridade alta difere da baixa.\n")
if("Confianca_Inst_Score" %in% signifs) cat("- A confiança institucional influencia a felicidade.\n")

cat("==========================================================================\n\n")

# ==============================================================================
# PONTO 7: ANÁLISE DE CLUSTERS (K-MEANS)
# ==============================================================================

cat("\n--- PONTO 7: ANÁLISE DE CLUSTERS ---\n")

# ---------------------------------------------------------------------
# 7.1 Criar dataset para clustering
# ---------------------------------------------------------------------

dados_clusters <- dados_reg %>%     # vem do Ponto 6
  select(Dim_1, Dim_2, Dim_3, Confianca_Inst_Score) %>%
  drop_na()

cat("Observações usadas no clustering:", nrow(dados_clusters), "\n")

# Normalizar variáveis (muito importante para K-means!)
dados_scaled <- scale(dados_clusters)

# ---------------------------------------------------------------------
# 7.2 Encontrar o número ideal de clusters (cotovelo)
# ---------------------------------------------------------------------

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- kmeans(dados_scaled, centers = k, nstart = 20)$tot.withinss
}

plot(1:10, wss, type = "b", pch = 19,
     xlab = "Número de clusters (k)",
     ylab = "Soma dos Quadrados Internos",
     main = "Método do Cotovelo")

cat("=> Observa o gráfico do cotovelo e identifica o melhor k.\n")

# ---------------------------------------------------------------------
# 7.3 Calcular K-means com 2, 3 e 4 clusters
# ---------------------------------------------------------------------

set.seed(123)

k2 <- kmeans(dados_scaled, centers = 2, nstart = 25)
k3 <- kmeans(dados_scaled, centers = 3, nstart = 25)
k4 <- kmeans(dados_scaled, centers = 4, nstart = 25)

cat("\nCluster sizes (k = 2):\n"); print(k2$size)
cat("\nCluster sizes (k = 3):\n"); print(k3$size)
cat("\nCluster sizes (k = 4):\n"); print(k4$size)

# 7.4 Adicionar clusters ao dataset de forma segura

# 1) Criar dataset reduzido com idno + clusters
cluster_df <- dados_reg %>% 
  drop_na(Dim_1, Dim_2, Dim_3, Confianca_Inst_Score) %>% 
  select(idno) %>%
  mutate(
    cluster2 = k2$cluster,
    cluster3 = k3$cluster,
    cluster4 = k4$cluster
  )

# 2) Juntar de volta ao dados_reg
dados_reg <- dados_reg %>%
  left_join(cluster_df, by = "idno")

cat("Clusters adicionados ao dataset final.\n")

# ---------------------------------------------------------------------
# 7.5 Visualização dos clusters (Dim1 x Dim2)
# ---------------------------------------------------------------------

library(ggplot2)

plot_k2 <- ggplot(dados_reg, aes(x = Dim_1, y = Dim_2, color = factor(cluster2))) +
  geom_point(alpha = 0.7) +
  labs(title = "Clusters k=2 (Dimensões dos Valores)",
       color = "Cluster") +
  theme_minimal()

plot_k3 <- ggplot(dados_reg, aes(x = Dim_1, y = Dim_2, color = factor(cluster3))) +
  geom_point(alpha = 0.7) +
  labs(title = "Clusters k=3 (Dimensões dos Valores)",
       color = "Cluster") +
  theme_minimal()

plot_k4 <- ggplot(dados_reg, aes(x = Dim_1, y = Dim_2, color = factor(cluster4))) +
  geom_point(alpha = 0.7) +
  labs(title = "Clusters k=4 (Dimensões dos Valores)",
       color = "Cluster") +
  theme_minimal()

print(plot_k2)
print(plot_k3)
print(plot_k4)

# ---------------------------------------------------------------------
# 7.6 Perfil Sociodemográfico dos Clusters
# ---------------------------------------------------------------------

cat("\n==== TABELAS DE PERFIL (K = 3) ====\n")

# Idade média por cluster
print(dados_reg %>% group_by(cluster3) %>% summarise(Idade_Media = mean(agea, na.rm=TRUE)))

# Felicidade média
print(dados_reg %>% group_by(cluster3) %>% summarise(Happy_Medio = mean(happy, na.rm=TRUE)))

# Escolaridade
print(dados_reg %>% group_by(cluster3, edulvlb_rec) %>% summarise(n = n()))

# Género
print(dados_reg %>% group_by(cluster3, gndr) %>% summarise(n = n()))

# Confiança institucional média
print(dados_reg %>% group_by(cluster3) %>% summarise(Confianca = mean(Confianca_Inst_Score, na.rm=TRUE)))

cat("\n=> Usar estas tabelas para descrever os perfis no relatório.\n")









cat("\n--- FIM DO PROJETO ---\n")

