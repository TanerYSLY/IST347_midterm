library(readr)

########################################
# VERILERI ICERI AKTARMA
########################################

veri_kitle <- read.table("arasinav.txt", header = TRUE, sep = " ")  
set.seed(123) 
satir_no <- sample(1:nrow(veri_kitle),size = 2000)
veri_orneklem <- veri_kitle[satir_no,]

########################################
# BÖLÜM 9.1
########################################

# Veri kümemizin basit istatistiksel özetini verir.
# Girdilerimiz veri kümemizdir.
# Çıktı olarak ise, veri kümemizdeki her bir sayısal değişken için...
# Min.,Q1,Medyan,Ortalama,Q3 ve Max. değerleridir.
# Veri kümemizdeki her bir kategorik değişken için ise uzunluğunu verir.

summary(veri_orneklem)

# Çıktı yorumu...

# Medyan ve ortalama ile verimizin dağılımını yorumlayabiliriz.
# Ortalama ve medyana sahip olmak çarpıklığı tespit etmek için yeterlidir.
for(col in names(veri_orneklem)){
  if(is.numeric(veri_orneklem[[col]])){ 
    if(mean(veri_orneklem[[col]]) > median(veri_orneklem[[col]])){
      print(paste(col,"sağa çarpık"))
    }
    if(mean(veri_orneklem[[col]]) < median(veri_orneklem[[col]])){
      print(paste(col,"sola çarpık"))
    }
  }
}
# summary fonksiyonumuz standart sapma vb. ölçüleri vermez.

########################################
# BÖLÜM 9.5
########################################

# Quantile fonksiyonu ile yüzdelik değerleri hesaplayabiliriz
# İlk argüman gözlemlerimiz, ikinci argüman hesaplamak
# istediğimiz yüzdelik değerdir.
# Çıktı olarak girdiğimiz yüzdelik değerin hangi gözleme karşılık
# geldiğini verir.

quantile(veri_orneklem$Income, 0.30)

# Bireylerin yüzde 30'unun geliri 19834.58'dan az diyebiliriz.
# Ya da yüzde 70'inin geliri 19834.58 den fazla diyebiliriz.

# İkinci argümanı kaldırırsak,
# Çeyreklik değerleri elde ederiz.

quantile(veri_orneklem$Income)

# Gelirler için medyan 30261.258'dir.

########################################
# BÖLÜM 9.7
########################################

# Verileri z-skoruna dönüştürme (Normalleştirme)
# scale fonksiyonu ile dönüşümü yapabiliriz.
# Girdi olarak vektör,matris ve data frame girebiliriz.
# Girdi olarak vektör girdiysek çıktı olarak normalize edilmiş vektörleri elde ederiz.
# Girdi olarak matris ve data frame girdiysek her bir sütunun normalize edilmiş
# halini döndürür.

scale(veri_orneklem$Income)

# Gelir verimizin normalize edilmiş değerleri

########################################
# BÖLÜM 9.9
########################################

# Ortalama için güven aralığı
# t.test fonksiyonu ile güven aralığını bulabiliriz.
# Örneklem büyüklüğümüz büyük olduğu için MLT devreye girer ve
# test istatistiğimizin dağılımı normal dağılım olur.
# Normal dağıldığı varsayımı ile anlamlı bir güven aralığı uygulayabiliriz.
# Fonksiyona girdi olarak verimiz, çıktı olarak t tablo değeri, serbestlik derecesi,
# p değeri, altenratif hipotez, güven aralığı ve örneklem ortalaması elde ederiz

for(col in names(veri_orneklem)){
  if(is.numeric(veri_orneklem[[col]])){ 
    ortalama_ga = t.test(veri_orneklem[[col]])
    print(paste(col,"değişkeninin ortalama için güven aralığı"))
    print(ortalama_ga)
  }
}

# Güven aralığı yorumu...WIP 

# Fonksiyon varsayılan olarak %95 güven düzeyi ile çalışıyor özel olarak
# güven düzeyi belirlemek istersek conf.level argümanını kullanabiliriz.

t.test(veri_orneklem$Income, conf.level = 0.99)

# Yorum...WIP

########################################
# BÖLÜM 9.10
########################################

# Medyan için güven aralığı
# Medyan için güven aralığı oluştururken Wilcoxon İşaretli Sıra Testini kullanıyoruz.
# wilcox.test fonksiyonu ile Wilcoxon İşaretli Sıra Testini yapabiliriz.
# Girdi olarak verimizi ve güven aralığı istediğimiz için conf.int argümanını TRUE
# alıyoruz.
# Özel olarak güven düzeyi belirlemek istersek conf.level argümanını değiştirebiliriz.
# Çıktı olarak V değeri, p-değeri, alternatif hipotez, güven aralığı ve
# pseudomedian elde ediyoruz.
# ÖNEMLİ: pseudomedian, medyan ile karıştırılmamalıdır.
 
for(col in names(veri_orneklem)){
  if(is.numeric(veri_orneklem[[col]])){ 
    medyan_ga = wilcox.test(veri_orneklem[[col]],conf.int = TRUE)
    print(paste(col,"değişkeninin medyan için güven aralığı"))
    print(medyan_ga)
  }
}

# Çıktı yorumu...WIP

########################################
# BÖLÜM 9.11
########################################

# Oran için hipotez testi
# Veri setimizdeki bireylerin yarısından fazlası borç ödüyor iddasını test etmek
# isteyelim. Bunun için prop.test fonksiyonu ile oran için test yapabiliriz.

x = sum(veri_orneklem['Loan_Repayment']>0) # Örneklemimizdeki borç ödeyenlerin sayısı
n = nrow(veri_orneklem) # Örneklem büyüklüğümüz

# Fonksiyonumuza girdi olarak örneklem büyüklüğü, başarı sayısı bu durumda borç ödemek
# ve test etmek istediğimiz oranı giriyoruz.
# Örneğimizde büyüktürü test etmek istediğimiz için alternative argümanını 'greater'
# olarak giriyoruz. Boş bırakırsak varsayılan olarak eşit değildir hipotezini test ediyor.

# Çıktı olarak X-squared değeri, serbestlik derecesi, p-değeri, alternatif hipotezi
# güven arlaığını ve örneklem oranını elde ediyoruz.

prop.test(x,n,0.5, alternative = 'greater')

# p-değeri 1 geliyor. Bu durumda sıfır hipotezi reddedilemez yani veri setimizdeki
# bireylerin yarısından fazlası borç ödüyor diyemeyiz.

########################################
# BÖLÜM 9.20
########################################

# Kolmogorov–Smirnov testi
# İki dağılımın aynı dağılımdan geldiğini test etmek için kullanılır.
# Veri setimizdeki yaş değişkeninin normal dağılıp dağılmadığını test etmek isteyelim.

# Test etmek için bir normal dağılım oluşturalım.
z <- rnorm(2000)

# ks.test fonksiyonu ile testimizi gerçekleştirebiliriz.
# Girdi olarak karşılaştırmak istediğimiz dağılımları giriyoruz.
# Çıktı olarak D değeri ve p-değerini elde ediyoruz.

ks.test(veri_orneklem$Age,z)

# p-değerimiz 0.05 den küçük olduğu için sıfır hipotezi reddedilir yani 
# yaş değişkeninin dağılımı normal dağılıma uymaz.
