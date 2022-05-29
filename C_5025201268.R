# No.1

# a.
oxy = data.frame(Responden = c(1,2,3,4,5,6,7,8,9), 
                 X=c(78,75,67,77,70,72,78,74,77),
                 Y=c(100,95,70,90,90,90,89,90,100))
diff <- oxy$Y-oxy$X
oxy$diff <- diff
oxy
sd(oxy$diff)

# b.
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")

library("tidyverse")
library("ggpubr")
library("rstatix")

oxy.long <- oxy %>%
  gather(key = "group", value = "saturation", X, Y)
head(oxy.long, 3)

oxy.long %>%
  group_by(group) %>%
  get_summary_stats(saturation, type = "mean_sd")

# Dengan base function
res <- t.test(saturation ~ group, data = oxy.long, paired = TRUE)
res

# Dengan rstatix
stat.test <- oxy.long  %>% 
  t_test(saturation ~ group, paired = TRUE) %>%
  add_significance()
stat.test

# c.
# p-value dari test kurang dari tingkat signifikansi 5%. 
# Kita bisa menolak hipotesis null dan menyimpulkan bahwa kadar saturasi oksigen sebelum aktivitas A berbeda secara signifikan dari kadar saturasi oksigen setelah aktivitas A 
# dengan p-value = 0.00006003

# No.2

# a.
# Iya, setuju

# b.
install.packages("BSDA")
library(BSDA)
tsum.test(mean.x=23500, s.x=3900, n.x=100, mu=20000, alternative="greater")

# c.
# Misal rata-rata lebih dari 20000 kita jadikan sebagai hipotesis alternatif
# Maka hipotesis null-nya adalah rata-rata tidak lebih dari 20000
# p-value yang dihasilkan adalah 9.437e-15. Karena p-value tersebut dibawah tingkat signifikansi 5%, maka kita bisa menolak hipotesis null.

# No.3

# a.
# H0 = Tidak ada perbedaan pada rata-rata saham Bandung dan Bali
# H1 = Ada perbedaan pada rata-rata saham Bandung dan Bali

# b.
library(BSDA)
tsum.test(mean.x=3.64, s.x=1.67, n.x=19, mean.y=2.79, s.y=1.32, n.y=27, alternative="greater", mu = 0, var.equal = TRUE, conf.level=0.95)

# c.
install.packages("mosaic")
library(mosaic)
plotDist(dist='t', df=2, col="black")

# d.
qchisq(.05, df=2, lower.tail=FALSE)   

# e.
# p-value yang dihasilkan adalah 0.03024. Karena p-value tersebut dibawah tingkat signifikansi 5%, maka kita bisa menolak hipotesis null. 

# f.
# Jadi, kesimpulannya, ada perbedaan pada rata-rata saham Bandung dan Bali. 
