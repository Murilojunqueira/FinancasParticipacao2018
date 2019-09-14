# Remove acentos

# Criado por Murilo Junqueira (m.junqueira@yahoo.com.br)

# Criado em: 2017-02-16

RemoveAccents <- function(x) iconv(x, to = "ASCII//TRANSLIT")
