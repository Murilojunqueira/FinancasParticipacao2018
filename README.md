README
================
Murilo Junqueira
28/02/2020

# Participation and Budget 2018 (article repo)

**Carla de Paiva Bezerra**  
**Murilo Junqueira**

## 1\. Short Description

This is the repo for the working paper “Why has the Participatory
Budgeting declined in Brazil?”.

For full paper: carlabezerra.info -\> Research

## 2\. Dependencies

1.  R version 3.4.3 (2017-11-30) – “Kite-Eating Tree”

## 3\. Files

### 3.1. Data

A. Raw Data

1.  Spada, P. 2012. “Brazilian Participatory Budgeting Census:
    1989-2012.” Available at:
    <http://participedia.net/en/content/brazilian-participatory-budgeting-census>
    / Access date: April 20, 2018. (licensed under CC BY-NC-SA 3.0).
    00\_Spada\_05112017\_PB CENSUS 2016.xlsx
    01\_Participedia\_Spada\_PBcensus1989-2012.xlsx

2.  IPEADATA: <http://www.ipeadata.gov.br>  
    A portal of the Brazilian Government Institute for Applied Economics
    Research, that puts together several data sources of the Brazilian
    Governement. The data is downloaded. We accessed the following
    sources:

<!-- end list -->

  - IBGE: Brazilian Institute of Geography and Statistics. For
    demographic variables. 02\_IPEA\_IBGE\_
    latitude\_longitude1998.xls  
    03\_IPEA\_IBGE\_PopulacaoMunic\_1992-2017.xls
    04\_IPEA\_IBGE\_PIBMunic\_1996-2017.xls

<!-- end list -->

3.  CEPESP:

<!-- end list -->

  - TSE: organized and documented access to the Brazilian Electoral
    Superior Court data on elections in Brazil from 1998 to 2016.

<!-- end list -->

4.  FINBRA: MPF/STN: Offical Accounting Data for Brazilian
    Municipalities (1989-2012). Downloaded anually. Accesse files were
    convereted to csv.

B. FINAL DATASET

1.  Data.Analysis.csv: Final analysis dataset derived from the raw data
    above. It includes muncipality-year values for all Brazilian
    municipalities above 50.0000 inhabintants. The variables are
    detailes in the “variables.txt”.

### 3.2. Code

Dataset importing and merging  
1.Importa CEPESPdata.R  
2.Importa Dados Spada.R  
3.Importa Finbra Funcoes.R  
4.Importa Finbra.R  
5.Importa IPEAData.R  
6.Importa TSE.R

Spada model replication 1.Reproducao Spada.R  
2.AditionalVars.R  
3.Cria Variaveis.R  
4.Full replication Spada - Modelos.R  
5.Full replication Spada - Variaveis.R

Final Model  
1.PaperFunctions.R  
2.PaperGraphs.R  
3.PaperModels.R

### 4\. Results

TBD

###
