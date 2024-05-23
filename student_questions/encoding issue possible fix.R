text <- read.csv('coffee.csv')
stringi::stri_enc_detect(text$text)

text$text <- iconv(text$text, "latin1", "ISO-8859-1", sub="")
