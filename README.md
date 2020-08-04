# Electricity-Consumption-Regression

Descrição detalhada neste link.

- **Projeto**: Modelo preditivo de regressão.
- **Ferramenta Utilizada**: Linguagem R
- **Dataset**: Dados fornecidos para o Projeto Final do Curso de Machine Learning da **Data Science Academy**. 

Variáveis: *Date, Appliances (target) , lights, T1, RH_1, T2, RH_2, T3, RH_3, T4, RH_4, T5, RH_5, T6, RH_6, T7, RH_7, T8, RH_8, T9, RH_9, T_out, Press_mm_hg, RH_out, Windspeed, Visibility, Tdewpoint, rv1, rv2, NSM, WeekStatus, Day_of_week*.


**Objetivo**:
Previsão de consumo de energia de eletrodomésticos baseado em dados de sensores de temperatura e umidade relativa do ar (Internet of Things), bem como dados meteorológicos provenientes de uma estação de aeroporto.

**Passos Realizados**:
1. Exploração dos dados
  o Estatística Descritiva (médias, quantiles, máximos)
  
2. Visualização dos dados
  o Histogramas, Boxplot, Gráficos de Barras

3. Limpeza e Transformação
  o Criação de novas variáveis
  o Remoção de Outliers da variável Target
  
4. Engenharia de Atributos
  o Criação de modelo simples de Random Forest para verificar importância de variáveis
  
5. Criação de modelos XGB e SVM.

6. Previsões.
