# Analisis Eksplorasi Data untuk Dataset Bank Marketing ----

library(tidyverse)
library(skimr)
library(ggcorrplot)
library(gridExtra)
library(GGally)
library(scales)
library(colorspace)

# set default ggplot theme
theme_set(theme_minimal())

# Memuat dataset Bank Marketing ----
bank_full <- read_delim("data/bank-additional-full.csv", 
                        delim = ";", 
                        locale = locale(decimal_mark = "."))

## 1. Pemahaman awal tentang dataset ----

# Dimensi dataset bank_full 
dim(bank_full)

# Enam baris pertama dataset
head(bank_full)

# Ringkasan struktur data
glimpse(bank_full)

# Melihat nama-nama variabel yang tersedia
names(bank_full)

# Ringkasan statistik dasar
summary(bank_full)

# Ringkasan data lebih mendalam dengan skimr
skim_without_charts(bank_full)


# Analisis variabel target (y)

# Variabel y
# 
# | Aspek          | Penjelasan                                                    |
# | -------------- | ------------------------------------------------------------- |
# | Nama variabel  | `y`                                                           |
# | Tipe data      | Kategorik biner (`yes` / `no`)                                |
# | `yes` artinya  | Klien **menjadi nasabah deposito berjangka** setelah kampanye |
# | `no` artinya   | Klien **tidak berlangganan** setelah kampanye                 |
# | Peran variabel | **Target variabel (output)** dalam supervised learning        |
# | Jenis masalah  | **Klasifikasi biner**                                         |

  
bank_full %>% 
  count(y) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  count(y) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = y, y = pct)) + 
  geom_col(fill = "steelblue") + 
  geom_text(aes(label = paste0(comma(n), " (", percent(pct, accuracy = 0.01), ")")), 
            vjust = -0.5) + 
  scale_y_continuous(labels = percent_format(), 
                     limits = c(0,1)) + 
  labs(
    x = "variable Target", 
    y = "Proporsi"
  ) 

# Analisis predictor  ---- 

p1 <- bank_full %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "steelblue", color = "grey") + 
  scale_x_continuous(limits = c(10, 120)) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Usia", 
    y = "Frekuensi Pelanggan"
  )

p2 <- bank_full %>% 
  ggplot(aes(x = age, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(labels = comma_format(), 
                     limits = c(10, 120)) + 
  labs(
    x = "Usia", 
    y = ""
  )

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))

p1 <- bank_full %>% 
  ggplot(aes(x = age, fill = y, color = y)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(limits = c(10, 120)) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Usia", 
    y = "Frekuensi Pelanggan"
  )

p2 <- bank_full %>% 
  ggplot(aes(x = age, fill = y)) + 
  geom_boxplot() + 
  scale_x_continuous(labels = comma_format(), 
                     limits = c(10, 120)) + 
  labs(
    x = "Usia", 
    y = ""
  )

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))



p3 <- bank_full %>% 
  filter(pdays < 999) %>% 
  ggplot(aes(x = pdays)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "grey") + 
  scale_x_continuous(breaks = 1:30) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Hari Setelah Kontak dari Campaign Terakhir", 
    y = "Frekuensi Pelanggan"
  )

p4 <- bank_full %>% 
  filter(pdays < 999) %>% 
  ggplot(aes(x = pdays, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(breaks = 1:30) +
  labs(
    x = "Hari Setelah Kontak dari Campaign Terakhir", 
    y = ""
  ) + 
  theme()

grid.arrange(p3, p4, ncol = 1, heights = c(2, 1))


bank_full %>%
  mutate(
    pdays_bin = case_when(
      pdays == 999 ~ "never",
      pdays <= 5 ~ "recently",
      pdays <= 30 ~ "in_last_month",
      TRUE ~ "more_than_month")
  ) %>% 
  count(pdays_bin)

bank_full %>%
  mutate(
    pdays_bin = case_when(
      pdays == 999 ~ "never",
      pdays <= 5 ~ "recently",
      pdays <= 30 ~ "in_last_month",
      TRUE ~ "more_than_month")
  ) %>% 
  ggplot(aes(x = pdays_bin, fill = y)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Campaign", 
    y = "Frekuensi Pelanggan"
  )

p5 <- bank_full %>% 
  ggplot(aes(x = campaign-1)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "grey") + 
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Frekuensi Dihubungi dari Campaign Ini", 
    y = "Frekuensi Pelanggan"
  )

p6 <- bank_full %>% 
  ggplot(aes(x = campaign-1, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  labs(
    x = "Frekuensi Dihubungi dari Campaign Ini", 
    y = ""
  ) + 
  theme()

grid.arrange(p5, p6, ncol = 1, heights = c(2, 1))

p5 <- bank_full %>% 
  ggplot(aes(x = campaign-1, fill = y, color = y)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Frekuensi Dihubungi dari Campaign Ini", 
    y = "Density", 
    fill = "Kategori Respon", 
    color = "Kategori Respon"
  ) +
  theme(legend.position = "top")

p6 <- bank_full %>% 
  ggplot(aes(x = campaign-1, fill = y)) + 
  geom_boxplot() + 
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  labs(
    x = "Frekuensi Dihubungi dari Campaign Ini", 
    y = ""
  ) + 
  theme(legend.position = "none")

grid.arrange(p5, p6, ncol = 1, heights = c(2, 1))


p7 <- bank_full %>% 
  ggplot(aes(x = previous)) + 
  geom_bar(fill = "steelblue", color = "grey") + 
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Frekuensi Dihubungi dari Campaign Sebelumnya", 
    y = "Frekuensi Pelanggan"
  )

p8 <- bank_full %>% 
  ggplot(aes(x = previous, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(breaks = seq(0, 10)) +
  labs(
    x = "Frekuensi Dihubungi dari Campaign Sebelumnya", 
    y = ""
  ) + 
  theme()

grid.arrange(p7, p8, ncol = 1, heights = c(2, 1))

bank_full %>% 
  ggplot(aes(x = previous, fill = y)) + 
  geom_bar(position = "fill") + 
  scale_x_continuous(breaks = seq(0, 10)) +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Frekuensi Dihubungi dari Campaign Sebelumnya", 
    y = "Proporsi Pelanggan"
  )


# Variabel emp.var.rate
# 
# | Aspek              | Penjelasan                                                              |
# | ------------------ | ----------------------------------------------------------------------- |
# | Nama variabel      | `employment_variation_rate`                                             |
# | Satuan             | Persentase (biasanya negatif, contoh: `-1.8`)                           |
# | Interpretasi       | Kenaikan atau penurunan tingkat pekerjaan dalam ekonomi                 |
# | Sumber             | Data makro dari ekonomi Portugal                                        |
# | Arti nilai negatif | **Penurunan** jumlah orang yang bekerja dibandingkan kuartal sebelumnya |
# | Arti nilai positif | **Kenaikan** jumlah orang yang bekerja dibandingkan kuartal sebelumnya  |
  
  
p9 <- bank_full %>% 
  ggplot(aes(x = emp.var.rate)) + 
  geom_histogram(fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Tingkat Variasi Pekerjaan - Qurterly", 
    y = "Frekuensi Pelanggan"
  )

p10 <- bank_full %>% 
  ggplot(aes(x = emp.var.rate, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Tingkat Variasi Pekerjaan - Qurterly", 
    y = ""
  ) 

grid.arrange(p9, p10, ncol = 1, heights = c(2, 1))

p9 <- bank_full %>% 
  ggplot(aes(x = emp.var.rate, fill = y, color = y)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Tingkat Variasi Pekerjaan - Qurterly", 
    y = "Density"
  )

p10 <- bank_full %>% 
  ggplot(aes(x = emp.var.rate, fill = y)) + 
  geom_boxplot() + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Tingkat Variasi Pekerjaan - Qurterly", 
    y = ""
  ) 

grid.arrange(p9, p10, ncol = 1, heights = c(2, 1))

# Variable cons.price.idx
# Indeks harga konsumen (inflasi)	
# Inflasi tinggi → orang lebih hati-hati berinvestasi
# 
# | Aspek                | Penjelasan                                                       |
# | -------------------- | ---------------------------------------------------------------- |
# | Nama variabel        | `cons.price.idx`                                                 |
# | Satuan               | Indeks (biasanya dalam skala seperti `93.2`, `94.5`, dll)        |
# | Tipe data            | Numeric (kontinu)                                                |
# | Sumber               | Statistik ekonomi makro Portugal                                 |
# | Fungsi               | Mengukur inflasi dari perspektif konsumen                        |
# | Arti kenaikan nilai  | Harga barang/jasa naik → inflasi meningkat                       |
# | Arti penurunan nilai | Harga barang/jasa turun → bisa jadi deflasi atau tekanan ekonomi |


p11 <- bank_full %>% 
  ggplot(aes(x = cons.price.idx)) + 
  geom_histogram(fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Indeks Harga Konsumen (Inflasi)", 
    y = "Frekuensi Pelanggan"
  )

p12 <- bank_full %>% 
  ggplot(aes(x = cons.price.idx, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Indeks Harga Konsumen (Inflasi)", 
    y = ""
  ) 

grid.arrange(p11, p12, ncol = 1, heights = c(2, 1))



# Variable cons.conf.idx
# Indeks Kepercayaan Konsumen	
# 
# | Aspek                                     | Penjelasan                                                            |
# | ----------------------------------------- | --------------------------------------------------------------------- |
# | Nama variabel                             | `cons.conf.idx`                                                       |
# | Satuan                                    | Indeks (biasanya berupa angka negatif, contoh: `-36.4`, `-45.0`, dst) |
# | Tipe data                                 | Numeric (kontinu)                                                     |
# | Makna nilai negatif                       | Kepercayaan konsumen rendah (pesimis terhadap ekonomi)                |
# | Makna nilai lebih tinggi (kurang negatif) | Kepercayaan konsumen membaik (lebih optimis)                          |
# | Sumber data                               | Statistik ekonomi makro dari Portugal                                 |


p13 <- bank_full %>% 
  ggplot(aes(x = cons.conf.idx)) + 
  geom_histogram(fill = "steelblue", color = "grey") + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Indeks Kepercayaan Konsumen", 
    y = "Frekuensi Pelanggan"
  )

p14 <- bank_full %>% 
  ggplot(aes(x = cons.conf.idx, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Indeks Kepercayaan Konsumen", 
    y = ""
  ) 

grid.arrange(p13, p14, ncol = 1, heights = c(2, 1))

p13 <- bank_full %>% 
  ggplot(aes(x = cons.conf.idx, fill = y, color = y)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Indeks Harga Konsumen (Inflasi)", 
    y = "Density", 
    fill = "Kategori Respon", 
    color = "Kategori Respon"
  ) + 
  theme(legend.position = "top")

p14 <- bank_full %>% 
  ggplot(aes(x = cons.price.idx, fill = y)) + 
  geom_boxplot() + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Indeks Harga Konsumen (Inflasi)", 
    y = ""
  ) + 
  theme(legend.position = "none") 

grid.arrange(p13, p14, ncol = 1, heights = c(2, 1))

# Variabel euribor3m
# Euro Interbank Offered Rate (3-month)
#  suku bunga tinggi (euribor naik), orang lebih tertarik buka deposito → positif untuk kampanye marketing bank.
# suku bunga acuan antarbank di zona Euro untuk 
# pinjaman jangka pendek 3 bulan.
# 
# | Aspek                | Penjelasan                                              |
# | -------------------- | ------------------------------------------------------- |
# | Nama variabel        | `euribor3m`                                             |
# | Arti                 | Suku bunga antarbank Eropa untuk tenor 3 bulan          |
# | Satuan               | Persentase (contoh: `4.857`, `1.344`)                   |
# | Tipe                 | Numeric kontinu                                         |
# | Sumber               | Statistik pasar keuangan zona Euro                      |
# | Kategori indikator   | **Makroekonomi – pasar uang**                           |
# | Nilai tinggi artinya | Biaya pinjaman antarbank naik (kredit jadi lebih mahal) |
# | Nilai rendah artinya | Biaya pinjaman turun (mendorong kredit dan investasi)   |


p15 <- bank_full %>% 
  ggplot(aes(x = euribor3m)) + 
  geom_histogram(fill = "steelblue", color = "grey") + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Indeks Harga Konsumen", 
    y = "Frekuensi Pelanggan"
  )

p16 <- bank_full %>% 
  ggplot(aes(x = euribor3m, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  labs(
    x = "Indeks Harga Konsumen", 
    y = ""
  ) 

grid.arrange(p15, p16, ncol = 1, heights = c(2, 1))


p15 <- bank_full %>% 
  ggplot(aes(x = euribor3m, fill = y, color = y)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Suku Bunga Euro Interbank", 
    y = "Density", 
    fill = "Kategori Respon", 
    color = "Kategori Respon"
  ) + 
  theme(legend.position = "top")

p16 <- bank_full %>% 
  ggplot(aes(x = euribor3m, fill = y)) + 
  geom_boxplot() + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Suku Bunga Euro Interbank", 
    y = ""
  ) + 
  theme(legend.position = "none") 

grid.arrange(p15, p16, ncol = 1, heights = c(2, 1))


# Variabel nr.employed
# Total jumlah tenaga kerja di pasar kerja saat ini (dalam ribuan)
# Semakin tinggi → ekonomi sehat, konsumsi meningkat
# Jika nr.employed tinggi:
# - Ekonomi sedang tumbuh
# - Banyak orang punya penghasilan
# - Peluang sukses kampanye marketing bank lebih besar
# 
# | Aspek         | Penjelasan                                                    |
# | ------------- | ------------------------------------------------------------- |
# | Nama variabel | `nr.employed`                                                 |
# | Arti          | Jumlah total pekerja di pasar tenaga kerja nasional           |
# | Satuan        | Ribu orang (contoh: `5099.1` berarti 5.099.100 orang)         |
# | Tipe data     | Numerik kontinu                                               |
# | Sumber data   | Statistik makroekonomi dari pemerintah Portugal atau Eurostat |
# | Kategori      | Indikator makroekonomi — sektor ketenagakerjaan               |
# | Contoh nilai  | `5099.1`, `5191`, `5228.1`, dsb                               |


p17 <- bank_full %>% 
  ggplot(aes(x = nr.employed)) + 
  geom_histogram(fill = "steelblue", color = "grey") + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Jumlah Total Tenaga Kerja", 
    y = "Frekuensi Pelanggan"
  )

p18 <- bank_full %>% 
  ggplot(aes(x = nr.employed, y = "")) + 
  geom_boxplot(fill = "steelblue") + 
  labs(
    x = "Jumlah Total Tenaga Kerja", 
    y = ""
  ) 

grid.arrange(p17, p18, ncol = 1, heights = c(2, 1))


p17 <- bank_full %>% 
  ggplot(aes(x = nr.employed, fill = y, color = y)) + 
  geom_density(alpha = 0.5) + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Jumlah Total Tenaga Kerja", 
    y = "Density", 
    fill = "Kategori Respon", 
    color = "Kategori Respon"
  ) + 
  theme(legend.position = "top")

p18 <- bank_full %>% 
  ggplot(aes(x = nr.employed, fill = y)) + 
  geom_boxplot() + 
  scale_x_continuous(labels = comma_format(accuracy = 0.01)) +
  labs(
    x = "Jumlah Total Tenaga Kerja", 
    y = ""
  ) + 
  theme(legend.position = "none") 

grid.arrange(p17, p18, ncol = 1, heights = c(2, 1))


# Analisi Korelasi 

# Analisis korelasi untuk variabel numerik
# Memilih kolom numerik saja untuk analisis korelasi
library(ggcorrplot)

num_vars <- bank_full %>%
  select_if(is.numeric) 

# Menghitung korelasi
cor_matrix <- cor(num_vars, use = "complete.obs")

pvmat <- cor_pmat(num_vars)

# Visualisasi matriks korelasi
ggcorrplot(cor_matrix, type = "full", 
           colors = c("firebrick", "white", "steelblue"), 
           lab = TRUE, digits = 1,lab_size = 3, 
           p.mat = pvmat)

bank_full %>% 
  ggplot(aes(x = euribor3m, y = emp.var.rate)) + 
  geom_point() + 
  geom_smooth(method = "lm")

bank_full %>% 
  ggplot(aes(x = cons.price.idx, y = emp.var.rate)) + 
  geom_point() + 
  geom_smooth(method = "lm")

bank_full %>% 
  ggplot(aes(x = euribor3m, y = nr.employed)) + 
  geom_point() + 
  geom_smooth(method = "lm")

bank_full %>% 
  ggplot(aes(x = emp.var.rate, y = nr.employed)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# Analisis Variabel Kategorik

# Variabel job
# 
# | Aspek           | Penjelasan                                                                                     |
# | --------------- | ---------------------------------------------------------------------------------------------- |
# | Nama variabel   | `job`                                                                                          |
# | Tipe data       | Kategorikal (nominal)                                                                          |
# | Jumlah kategori | 12 kategori pekerjaan + 1 kategori "unknown"                                                   |
# | Peran           | Fitur (predictor)                                                                              |
# | Tujuan analisis | Mengetahui apakah jenis pekerjaan memengaruhi minat nasabah terhadap produk deposito berjangka |

bank_full %>% 
  count(job, sort = TRUE) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(y = job, fill = y)) + 
  geom_bar(position = "fill") + 
  scale_x_continuous(labels = percent_format()) + 
  labs(
    x = "Proporsi", 
    y = "Pekerjaan"
  )


# selain admin., blue-collar, technician: other


# Variabel marital
# 
# | Aspek           | Penjelasan                                                             |
# | --------------- | ---------------------------------------------------------------------- |
# | Nama variabel   | `marital`                                                              |
# | Tipe data       | Kategorikal (nominal)                                                  |
# | Jumlah kategori | 4 (termasuk “unknown”)                                                 |
# | Peran           | Fitur (predictor)                                                      |
# | Tujuan analisis | Mengamati hubungan status pernikahan dengan keputusan membuka deposito |


bank_full %>% 
  count(marital) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(x = marital)) + 
  geom_bar() + 
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    x = "Status Pernikahan",
    y = "Jumlah"
  )

bank_full %>% 
  ggplot(aes(x = marital, fill = y)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Status Pernikahan",
    y = "Proporsi" 
  )


# Variabel education
# 
# | Aspek           | Penjelasan                                                              |
# | --------------- | ----------------------------------------------------------------------- |
# | Nama variabel   | `education`                                                             |
# | Tipe data       | Kategorikal (ordinal atau nominal)                                      |
# | Peran           | Fitur (predictor)                                                       |
# | Tujuan analisis | Mengamati hubungan tingkat pendidikan dengan keputusan membuka deposito |


bank_full %>% 
  count(education, sort = TRUE) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(y = education, fill = y)) + 
  geom_bar(position = "fill") + 
  scale_x_continuous(labels = percent_format()) + 
  labs(
    x = "Proporsi", 
    y = "Pendidikan"
  )

bank_full %>% 
  mutate(
    education_level = case_when(education %in% c("illiterate", "basic.4y", "basic.6y") ~ "low", 
                                education %in% c("basic.9y", "high.school") ~ "mid", 
                                education %in% c("professional.course", "university.degree") ~ "high", 
                                TRUE ~ "unknown") %>% 
      factor(levels = c("unknown", "low", "mid", "high"))
  ) %>% 
  ggplot(aes(x = education_level)) +
  geom_bar(fill = "steelblue") +
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Tingkat Pendidikan", 
    y = "Jumlah"
  )


bank_full %>% 
  mutate(
    education_level = case_when(education %in% c("illiterate", "basic.4y", "basic.6y") ~ "low", 
                                education %in% c("basic.9y", "high.school") ~ "mid", 
                                education %in% c("professional.course", "university.degree") ~ "high", 
                                TRUE ~ "unknown") %>% 
      factor(levels = c("unknown", "low", "mid", "high"))
  ) %>% 
  ggplot(aes(x = education_level, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Tingkat Pendidikan", 
    y = "Proporsi"
  )

# Variabel default
# 
# | Aspek           | Penjelasan                                                                                 |
# | --------------- | ------------------------------------------------------------------------------------------ |
# | Nama variabel   | `default`                                                                                  |
# | Tipe data       | Kategorikal (biner: yes/no/unknown)                                                        |
# | Peran           | Fitur (predictor)                                                                          |
# | Tujuan analisis | Mengetahui apakah riwayat gagal bayar memengaruhi kemungkinan berlangganan produk deposito |


bank_full %>% 
  count(default, sort = TRUE) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(x = default)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Pernah Gagal Bayar",
    y = "Jumlah"
  )

bank_full %>% 
  ggplot(aes(x = default, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Pernah Gagal Bayar", 
    y = "Proporsi"
  )

bank_full %>% 
  mutate(default = if_else(default == "yes", "unknown", default)) %>% 
  ggplot(aes(x = default, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Pernah Gagal Bayar", 
    y = "Proporsi"
  )


# Variabel housing
# 
# | Aspek           | Penjelasan                                                                                   |
# | --------------- | -------------------------------------------------------------------------------------------- |
# | Nama variabel   | `housing`                                                                                    |
# | Tipe data       | Kategorikal (biner: `yes`, `no`, `unknown`)                                                  |
# | Peran           | Fitur (predictor)                                                                            |
# | Tujuan analisis | Melihat apakah kepemilikan pinjaman rumah berkaitan dengan kemungkinan berlangganan deposito |


bank_full %>% 
  count(housing, sort = TRUE) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(x = housing)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Pinjaman Perumahan",
    x = "Jumlah"
  )

bank_full %>% 
  ggplot(aes(x = housing, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Pinjaman Perumahan", 
    y = "Proporsi"
  )


# Variabel loan
# 
# | Aspek           | Penjelasan                                                                                                 |
# | --------------- | ---------------------------------------------------------------------------------------------------------- |
# | Nama variabel   | `loan`                                                                                                     |
# | Tipe data       | Kategorikal (biner: `yes`, `no`, `unknown`)                                                                |
# | Peran           | Fitur (predictor)                                                                                          |
# | Tujuan analisis | Mengetahui apakah klien dengan pinjaman pribadi lebih atau kurang tertarik berlangganan deposito berjangka |


bank_full %>% 
  count(loan, sort = TRUE) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(x = loan)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Pinjaman Pribadi", 
    y = "Jumlah" 
  )

bank_full %>% 
  ggplot(aes(x = loan, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Pinjaman Pribadi", 
    y = "Proporsi"
  )


# Variabel contact
# 
# | Aspek           | Penjelasan                                                                    |
# | --------------- | ----------------------------------------------------------------------------- |
# | Nama variabel   | `contact`                                                                     |
# | Tipe data       | Kategorikal (nominal)                                                         |
# | Peran           | Fitur (predictor)                                                             |
# | Tujuan analisis | Mengetahui apakah metode kontak memengaruhi hasil kampanye pemasaran deposito |



bank_full %>% 
  count(contact, sort = TRUE) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(x = contact)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Media Komunikasi", 
    y = "Jumlah"
  )

bank_full %>% 
  ggplot(aes(x = contact, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Media Komunikasi", 
    y = "Proporsi"
  )


# Variabel month
# 
# | Aspek           | Penjelasan                                                                       |
# | --------------- | -------------------------------------------------------------------------------- |
# | Nama variabel   | `month`                                                                          |
# | Tipe data       | Kategorikal ordinal (berupa nama bulan)                                          |
# | Peran           | Fitur (predictor)                                                                |
# | Tujuan analisis | Melihat apakah waktu dalam setahun memengaruhi hasil kampanye pemasaran deposito |

bank_full %>% 
  mutate(month = factor(month, levels = str_to_lower(month.abb))) %>% 
  count(month) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  mutate(month = factor(month, levels = str_to_lower(month.abb))) %>% 
  ggplot(aes(x = month)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Bulan Dihubungi",
    y = "Proporsi"
  )

bank_full %>% 
  mutate(month = factor(month, levels = str_to_lower(month.abb))) %>% 
  ggplot(aes(x = month, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Bulan Dihubungi", 
    y = "Proporsi"
  )


# Variabel day_of_week
# 
# | Aspek           | Penjelasan                                                                       |
# | --------------- | -------------------------------------------------------------------------------- |
# | Nama variabel   | `day_of_week`                                                                    |
# | Tipe data       | Kategorikal ordinal (berupa nama hari)                                           |
# | Peran           | Fitur (predictor)                                                                |
# | Tujuan analisis | Melihat apakah hari tertentu memengaruhi hasil kampanye pemasaran deposito       |

bank_full %>% 
  mutate(day_of_week = factor(day_of_week, levels =c("mon", "tue", "wed", "thu", "fri"))) %>% 
  count(day_of_week) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  mutate(day_of_week = factor(day_of_week, levels =c("mon", "tue", "wed", "thu", "fri"))) %>% 
  ggplot(aes(x = day_of_week)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Hari Dihubungi",
    y = "Jumlah"
  )

bank_full %>% 
  mutate(day_of_week = factor(day_of_week, levels =c("mon", "tue", "wed", "thu", "fri"))) %>% 
  ggplot(aes(x = day_of_week, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Hari Dihubungi", 
    y = "Proporsi"
  )

# Variabel poutcome
# 
# | Aspek           | Penjelasan                                                                       |
# | --------------- | -------------------------------------------------------------------------------- |
# | Nama variabel   | `day_of_week`                                                                    |
# | Tipe data       | Kategorikal ordinal (berupa nama hari)                                           |
# | Peran           | Fitur (predictor)                                                                |
# | Tujuan analisis | Melihat apakah hari tertentu memengaruhi hasil kampanye pemasaran deposito       |

bank_full %>% 
  count(poutcome) %>% 
  mutate(pct = n/sum(n))

bank_full %>% 
  ggplot(aes(x = poutcome)) + 
  geom_bar() + 
  scale_y_continuous(labels = comma_format()) + 
  labs(
    x = "Hasil Marketing Sebelumnya",
    y = "Jumlah"
  )

bank_full %>% 
  ggplot(aes(x = poutcome, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(
    x = "Hasil Marketing Sebelumnya", 
    y = "Proporsi"
  )

