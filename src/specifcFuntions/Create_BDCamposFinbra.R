
# Create and format table BDCamposFinbra

# By Murilo Junqueira (m.junqueira@yahoo.com.br)

# Created at 2019-09-13

# Dependencies:
library(dplyr)
library(stringr)
################## External Functions ##################

source("src/generalFunctions/AutosequentialVar.R")

################## Functions Create_BDCamposFinbra ##################

# Debug:
# ListFieldsExcel_df <- ListFieldsExcel_df
# OutputDir <- "data/dataset/"



Create_BDCamposFinbra <- function(ListFieldsExcel_df, OutputDir) {
  
  # Empty data frame to gather the data
  BDCamposFinbra <- data.frame()
  
  
  temp_df <- ListFieldsExcel_df %>% 
    magrittr::set_names(c("FinbraCampo_ArquivoXls", "FinbraCampo_FilePath", "FinbraCampo_Tabela",
                          "FinbraCampo_Campo"))
  
  message("Formating 1989 - 1996 data")
  
  # Formating years 89-96

  temp_df_89_96 <- temp_df %>% 
    dplyr::filter(FinbraCampo_ArquivoXls %in% c("Finbra89-93.xlsx", "Finbra94-96.xlsx")) %>% 
    mutate(FinbraCampo_DataTab = ifelse(str_detect(FinbraCampo_Tabela, "Quadro"), 1, 0)) %>% 
    mutate(tm = ifelse(str_detect(FinbraCampo_Tabela, "Quadro"), FinbraCampo_Tabela, NA)) %>% 
    mutate(FinbraCampo_Ano = str_sub(tm, nchar(tm) - 1, nchar(tm))) %>%
    mutate(FinbraCampo_Ano = as.integer(paste0(19, FinbraCampo_Ano))) %>% 
    mutate(temp = str_detect(FinbraCampo_Campo, 
                             "CodIBGE|município|MUNICIPIOS|MUNICÍPIOS|NOME|REGIÕES E MUNICÍPIOS|uf|UF|UG")) %>% 
    mutate(FinbraCampo_IDMunic = ifelse(temp,1, 0)) %>% 
    select(FinbraCampo_ArquivoXls, FinbraCampo_Tabela, FinbraCampo_Campo, FinbraCampo_Ano, 
           FinbraCampo_DataTab, FinbraCampo_IDMunic) %>% 
    suppressMessages()
  
  # Bind to main data frame
  BDCamposFinbra <- bind_rows(BDCamposFinbra, temp_df_89_96)
  
  rm(temp_df_89_96)
  
  message("Formating 1997 data")
  
  # Formating year 1997
  temp_df_97 <- temp_df %>% 
    dplyr::filter(FinbraCampo_ArquivoXls == "Finbra1997.xlsx") %>% 
    mutate(FinbraCampo_Ano = 1997) %>% 
    mutate(tm = ifelse(str_detect(FinbraCampo_Tabela, "Quadro"), FinbraCampo_Tabela, NA)) %>% 
    mutate(FinbraCampo_DataTab = ifelse(is.na(tm), 0, 1)) %>% 
    mutate(temp = str_detect(FinbraCampo_Campo, "MUNICIPIO|NOME|UF|UG|Estados")) %>% 
    mutate(FinbraCampo_IDMunic = ifelse(temp,1, 0)) %>% 
    select(FinbraCampo_ArquivoXls, FinbraCampo_Tabela, FinbraCampo_Campo, FinbraCampo_Ano, 
           FinbraCampo_DataTab, FinbraCampo_IDMunic)
    
  # Bind to main data frame
  BDCamposFinbra <- bind_rows(BDCamposFinbra, temp_df_97)
  
  rm(temp_df_97)
  
  message("Formating 1998 - 2012 data")
  
  # Formating year 1998 to 2012
  temp_df_98_2012 <- temp_df %>% 
    dplyr::filter(FinbraCampo_ArquivoXls %in% paste0("Finbra", 1998:2012, ".xlsx")) %>% 
    mutate(fl = FinbraCampo_ArquivoXls) %>% 
    mutate(FinbraCampo_Ano = str_sub(fl, nchar(fl) - 8, nchar(fl) - 5)) %>% 
    mutate(FinbraCampo_Ano = as.integer(FinbraCampo_Ano)) %>% 
    mutate(FinbraCampo_DataTab = ifelse(str_detect(FinbraCampo_Tabela, "^Receita$|^Despesa$|^RecDesp$"), 1, 0)) %>% 
    mutate(temp = ifelse(str_detect(FinbraCampo_Campo, "MUNICIPIO|MUNICÍPIO|NOME"), 1, 0)) %>% 
    mutate(temp = ifelse(str_detect(FinbraCampo_Campo, "MUNICÍPIO/UF|MUNICÍPIO/UF"), 1, temp)) %>% 
    mutate(temp = ifelse(str_detect(FinbraCampo_Campo, "CD_MUN|CdMun|Cod Mun|NU_MUN"), 1, temp)) %>% 
    mutate(temp = ifelse(str_detect(FinbraCampo_Campo, "CodUfMun|CD_UF"), 1, temp)) %>% 
    mutate(temp = ifelse(str_detect(FinbraCampo_Campo, "ESTADO|UF|CdUF"), 1, temp)) %>% 
    mutate(temp = ifelse(str_detect(FinbraCampo_Campo, "CodUG|UG"), 1, temp)) %>% 
    rename(FinbraCampo_IDMunic = temp) %>% 
    arrange(FinbraCampo_ArquivoXls, FinbraCampo_Tabela) %>% 
    select(FinbraCampo_ArquivoXls, FinbraCampo_Tabela, FinbraCampo_Campo, FinbraCampo_Ano, 
           FinbraCampo_DataTab, FinbraCampo_IDMunic)
  
  
  # Correct wromg 2010 columns
  remove_rows <- which(temp_df_98_2012$FinbraCampo_Ano == 2010 & 
                         temp_df_98_2012$FinbraCampo_Tabela == "DSubFuncao")[171:221]
  
  temp_df_98_2012 <- temp_df_98_2012 %>% 
    slice(-remove_rows)
    
  # Bind to main data frame
  BDCamposFinbra <- bind_rows(BDCamposFinbra, temp_df_98_2012)
  
  rm(temp_df_98_2012, remove_rows)
  
  # Create ID:
  BDCamposFinbra <- AutosequentialVar(BDCamposFinbra, "FinbraCampo_Id") %>% 
    select(FinbraCampo_Id, everything())
  
  
  write.table(BDCamposFinbra, 
              file = paste0(OutputDir, "BDCamposFinbra.csv"),
              sep = ";", dec = ",",
              row.names = FALSE,
              fileEncoding = "UTF-8")
  
  
  message("File ", paste0(OutputDir, "BDCamposFinbra.csv"), " saved")
}

#End