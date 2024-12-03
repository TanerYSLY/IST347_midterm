file_path <- "C:/Users/taner/OneDrive/Masa\u00fcst\u00fc/projeler/ist analiz/arasinav.txt"

# TXT dosyas??n?? oku
veri_kitle <- read.table(file_path, header = TRUE, sep = " ")  # E??er dosya tab ile ayr??lm????sa

print(class(veri_kitle))
print(nrow(veri_kitle))

# Rastgele sat??r ve s??tun se??
set.seed(123)  # Tekrar edilebilirlik i??in
rastgele_satir <- sample(nrow(veri_kitle), 2000)  # Rastgele bir sat??r
                                     
# Se??ilen sat??r ve s??tunu alma
veri_orneklem <- veri_kitle[rastgele_satir,]


str(ver_orneklem) #structure of an R object
#veri turleri dogru tan??mlanm??s
summary(veri_orneklem)





