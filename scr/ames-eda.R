# Analisis Eksplorasi Data untuk Dataset Ames Housing ----
# dari package modeldata di R

library(tidyverse)
library(skimr)
library(ggcorrplot)
library(gridExtra)
library(GGally)
library(leaflet)
library(colorspace)

# Memuat dataset Ames Housing ----
ames <- read_csv("data/ames.csv")

## 1. Pemahaman awal tentang dataset ----

# Dimensi dataset Ames Housing
dim(ames)

# Enam baris pertama dataset
head(ames)

# Ringkasan struktur data
glimpse(ames)

# Melihat nama-nama variabel yang tersedia
names(ames)

# Ringkasan statistik dasar
summary(ames)

# Ringkasan data lebih mendalam dengan skimr
skim_without_charts(ames)

# Analisis harga rumah (variabel target)

# Histogram harga rumah
p1 <- ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(fill = "steelblue", bins = 50) + 
  scale_x_continuous(labels = comma_format(), 
                     limits = c(0, 800000)) + 
  labs(title = "Distribusi Harga Rumah", 
       x = "Harga Rumah ($)",
       y = "Frekuensi") +
  theme_minimal() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

# Boxplot harga rumah
p2 <- ggplot(ames, aes(x = Sale_Price, y = "")) +
  geom_boxplot(fill = "steelblue") + 
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") + 
  scale_x_continuous(labels = comma_format(), 
                     limits = c(0, 800000)) + 
  labs(x = "Harga Rumah ($)", 
       y = "") +
  theme_minimal() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank())

# Log-transform untuk harga rumah
p3 <- ggplot(ames, aes(x = log10(Sale_Price))) +
  geom_histogram(fill = "salmon", bins = 50) +
  scale_x_continuous(limits = c(4, 6)) + 
  labs(title = "Distribusi Log10 Harga Rumah",
       x = "Log10 Harga Rumah",
       y = "Frekuensi") +
  theme_minimal() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

# Boxplot log harga rumah
p4 <- ggplot(ames, aes(x = log10(Sale_Price), y = "")) +
  geom_boxplot(fill = "salmon") +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") + 
  scale_x_continuous(limits = c(4, 6)) + 
  # scale_y_continuous(labels = number_format(accuracy = 0.1)) + 
  labs(x = "Log10 Harga Rumah",
       y = "") +
  theme_minimal() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank())

grid.arrange(p1, p2, p3, p4, ncol = 1, 
             heights = c(0.5, 0.25, 0.5, 0.25))

ames <- ames %>% 
  mutate(Log10Sale_Price = log10(Sale_Price))

# Mengeluarkan transaksi outlier: Ground Living Area > 4000 sqf dan 
# Sale Condition = "Partial"

ames %>% 
  mutate(text = ifelse(Gr_Liv_Area > 4000, Sale_Condition, ""), 
         color = case_when(Gr_Liv_Area > 4000 | Sale_Condition %in% c("Partial", "Abnorml", "Family") ~ "outlier", 
                           TRUE ~ "obs")) %>% 
  ggplot(aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = 0.7) + 
  geom_text(aes(label = text, color = color), size = 3) +
  # geom_smooth(method = "lm") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  scale_color_manual(values = list("obs" = "black", 
                                   "outlier" = "red")) + 
  theme_minimal() + 
  theme(legend.position = "none")

ames %>% 
  mutate(text = ifelse(Gr_Liv_Area > 4000 | Log10Sale_Price < 4.5, Sale_Condition, ""), 
         color = case_when(Gr_Liv_Area > 4000 | Sale_Condition %in% c("Partial", "Abnorml", "Family") ~ "outlier", 
                           TRUE ~ "obs")) %>% 
  ggplot(aes(x = Gr_Liv_Area, y = Log10Sale_Price)) + 
  geom_point(alpha = 0.7) + 
  geom_text(aes(label = text, color = color), size = 3) + 
  # geom_smooth(method = "lm") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  scale_color_manual(values = list("obs" = "black", 
                                   "outlier" = "red")) + 
  theme_minimal() + 
  theme(legend.position = "none")

ames %>% 
  filter(Gr_Liv_Area <= 4000 | Sale_Condition != "Partial") %>% 
  filter(Log10Sale_Price >= 4.5) %>% 
  mutate(text = ifelse(Gr_Liv_Area > 4000 | Log10Sale_Price < 4.5, Sale_Condition, ""), 
         color = case_when(Gr_Liv_Area > 4000 | Log10Sale_Price < 4.5 & Sale_Condition %in% c("Partial", "Abnorml") ~ "outlier", 
                           TRUE ~ "obs")) %>% 
  ggplot(aes(x = Gr_Liv_Area, y = Log10Sale_Price)) + 
  geom_point(alpha = 0.7) + 
  geom_text(aes(label = text, color = color), size = 3) + 
  geom_smooth(method = "lm") + 
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  scale_color_manual(values = list("obs" = "black", 
                                   "outlier" = "red")) + 
  theme_minimal() + 
  theme(legend.position = "none")

ames <- ames %>% 
  filter(Gr_Liv_Area <= 4000 | Sale_Condition != "Partial") %>% 
  filter(Log10Sale_Price >= 4.5) 

# 4. Eksplorasi beberapa variable numerik dan hubungannya dengan harga rumah

# Scatter plots dengan harga rumah
p5 <- ggplot(ames, aes(x = Gr_Liv_Area, y = Log10Sale_Price)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah vs Ground Living Area",
       x = "Ground Living Area (sqft)",
       y = "Log10 Harga Rumah") +
  theme_minimal()

p6 <- ggplot(ames, aes(x = Total_Bsmt_SF, y = Log10Sale_Price)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah vs Luas Basement",
       x = "Luas Basement (sqft)",
       y = "Log10 Harga Rumah") +
  theme_minimal()

grid.arrange(p5, p6, ncol = 1)

p7 <- ggplot(ames, aes(x = Year_Built, y = Log10Sale_Price)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah vs Tahun Pembangunan",
       x = "Tahun Pembangunan",
       y = "Log10 Harga Rumah") +
  theme_minimal()

p8 <- ggplot(ames, aes(x = Year_Remod_Add, y = Log10Sale_Price)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah vs Tahun Renovasi",
       x = "Tahun Renovasi",
       y = "Log10 Harga Rumah") +
  theme_minimal()

grid.arrange(p7, p8, ncol = 1)

ames <- ames %>% 
  mutate(Age = Year_Sold - Year_Built,
         After_Remod_Add = Year_Sold - Year_Remod_Add)

p9 <- ames %>% 
  ggplot(aes(x = Age, y = Log10Sale_Price)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah vs Usia Rumah",
       x = "Usia Rumah",
       y = "Log10 Harga Rumah") +
  theme_minimal()


p10 <- ames %>% 
  ggplot(aes(x = After_Remod_Add, y = Log10Sale_Price)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = comma_format()) + 
  scale_y_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah vs Usia Rumah Setelah Remodeling",
       x = "Usia Rumah Setelah Remodeling",
       y = "Log10 Harga Rumah") +
  theme_minimal()

grid.arrange(p9, p10, ncol = 1)

# 5. Analisis korelasi untuk variabel numerik
# Memilih kolom numerik saja untuk analisis korelasi
numeric_columns <- ames %>%
  select_if(is.numeric) %>% 
  select(-Longitude, -Latitude)

# Menghitung korelasi
cor_matrix <- cor(numeric_columns, use = "complete.obs")

pvmat <- cor_pmat(numeric_columns)

# Visualisasi matriks korelasi
ggcorrplot(cor_matrix, type = "full", 
           colors = c("firebrick", "white", "steelblue"), 
           lab = TRUE, digits = 1,lab_size = 3, 
           p.mat = pvmat)

# Menampilkan variabel numerik dengan korelasi tertinggi terhadap harga rumah
high_corr_vars <- cor_matrix["Log10Sale_Price", ]
high_corr_vars <- high_corr_vars[order(abs(high_corr_vars), decreasing = TRUE)]

pvmat_sale_price <- pvmat["Log10Sale_Price",] %>% 
  sort(decreasing = FALSE)

data.frame(variable = names(high_corr_vars), 
           corr = high_corr_vars, 
           pv = pvmat_sale_price) %>% 
  filter(!variable %in% c("Sale_Price", "Log10Sale_Price")) %>% 
  mutate(text = ifelse(test = pv < 0.05, 
                       yes = round(corr, 2), 
                       no = paste0(round(corr, 2), " (", round(pv, 2), ")")), 
         text_pos = ifelse(corr < 0, 1, -0.25)) %>% 
  ggplot(aes(x = corr, y = reorder(variable, corr), fill = corr)) + 
  geom_col() + 
  geom_text(aes(label = text, hjust = text_pos), size = 3) + 
  # geom_text(aes(label = ifelse(pv < 0.05, round(pv, 2), "")), size = 3) + 
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", 
                       midpoint = 0, limits = c(-1, 1), 
                       breaks = seq(-1, 1, by = 0.2)) + 
  xlim(-1, 1) +
  labs(title = "Korelasi Variable dengan Sale Price",
       x = "Korelasi",
       y = "Variable") + 
  theme_minimal() +
  theme(
    legend.position = "right",  # Posisi legend di kanan
    legend.key.height = unit(0.15, "npc")  # Menyesuaikan tinggi legend
  )

# 6. Eksplorasi variabel kategorikal penting
# Membuat plot untuk melihat hubungan antara beberapa variabel kategorikal dan harga rumah
p10 <- ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Neighborhood, Log10Sale_Price, median, na.rm = TRUE))) + 
  geom_boxplot(fill = "steelblue") + 
  # stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Lingkungan",
       x = "Log10 Harga Rumah",
       y = "Lingkungan") +
  theme_minimal() 

p10

ames %>% 
  mutate(
    Neighborhood_Category = 
      case_when(Neighborhood %in% c("Stone_Brook", "Northridge_Heights", "Northridge", 
                                    "Green_Hills", "Veenker", "Timberland", "Somerset") ~ "High-end", 
                Neighborhood %in% c("Crawford", "College_Creek", "Greens", "Clear_Creek", 
                                    "Bloomington_Heights", "Gilbert", "Northwest_Ames", "Sawyer_West") ~ "Mid-range", 
                Neighborhood %in% c("Meadow_Village", "Briardale", "Iowa_DOT_and_Rail_Road", 
                                    "Old_Town", "Edwards", "Brookside", "Blueste", "Sawyer",  
                                    "South_and_West_of_Iowa_State_University", "Landmark", 
                                    "North_Ames", "Northpark_Villa", "Mitchell") ~ "Affordable", 
                TRUE ~ Neighborhood
                ), 
    Neighborhood_Category = factor(Neighborhood_Category, levels = c("Affordable", "Mid-range", "High-end"))
    ) %>% 
  count(Neighborhood_Category)

ames %>% 
  mutate(
    Neighborhood_Category = 
      case_when(Neighborhood %in% c("Stone_Brook", "Northridge_Heights", "Northridge", 
                                    "Green_Hills", "Veenker", "Timberland", "Somerset") ~ "High-end", 
                Neighborhood %in% c("Crawford", "College_Creek", "Greens", "Clear_Creek", 
                                    "Bloomington_Heights", "Gilbert", "Northwest_Ames", "Sawyer_West") ~ "Mid-range", 
                Neighborhood %in% c("Meadow_Village", "Briardale", "Iowa_DOT_and_Rail_Road", 
                                    "Old_Town", "Edwards", "Brookside", "Blueste", "Sawyer",  
                                    "South_and_West_of_Iowa_State_University", "Landmark", 
                                    "North_Ames", "Northpark_Villa", "Mitchell") ~ "Affordable", 
                TRUE ~ Neighborhood
      ), 
    Neighborhood_Category = factor(Neighborhood_Category, levels = c("Affordable", "Mid-range", "High-end"))
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Neighborhood, Log10Sale_Price, median, na.rm = TRUE), 
             fill = Neighborhood_Category)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Lingkungan",
       x = "Log10 Harga Rumah",
       y = "Lingkungan", 
       fill = "Kategori Lingkungan") +
  theme_minimal()

ames <- ames %>% 
  mutate(
    Neighborhood_Category = 
      case_when(Neighborhood %in% c("Stone_Brook", "Northridge_Heights", "Northridge", 
                                    "Green_Hills", "Veenker", "Timberland", "Somerset") ~ "High-end", 
                Neighborhood %in% c("Crawford", "College_Creek", "Greens", "Clear_Creek", 
                                    "Bloomington_Heights", "Gilbert", "Northwest_Ames", "Sawyer_West") ~ "Mid-range", 
                Neighborhood %in% c("Meadow_Village", "Briardale", "Iowa_DOT_and_Rail_Road", 
                                    "Old_Town", "Edwards", "Brookside", "Blueste", "Sawyer",  
                                    "South_and_West_of_Iowa_State_University", "Landmark", 
                                    "North_Ames", "Northpark_Villa", "Mitchell") ~ "Affordable", 
                TRUE ~ Neighborhood), 
    Neighborhood_Category = factor(Neighborhood_Category, levels = c("Affordable", "Mid-range", "High-end"))
  )

# Map lokasi berdasarkan Neighborhood
pal <- 
  colorFactor(
    qualitative_hcl(28, palette = "Dynamic"), 
    domain = ames$Neighborhood
  )

ames %>% 
  arrange(Neighborhood) %>% 
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 3,
    group = ~Neighborhood, 
    color = ~pal(Neighborhood),
    stroke = FALSE,
    fillOpacity = 0.7, 
    lng = ~Longitude, lat = ~Latitude, 
    popup = ~paste("Neighborhood:", Neighborhood, 
                   "<br>Year Built:", Year_Built, 
                   "<br>MS Zoning:", MS_Zoning)
  ) %>% 
  addLayersControl(overlayGroups = ~Neighborhood, 
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  setView(lng = -93.58429, lat = 42.02712, zoom = 12)


ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Overall_Cond, Log10Sale_Price, median, na.rm = TRUE))) +
  geom_boxplot(fill = "steelblue") + 
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Kondisi Keseluruhan",
       x = "Log10 Harga Rumah",
       y = "Kondisi Keseluruhan") +
  theme_minimal()


# Map lokasi berdasarkan Neighborhood
pal <- 
  colorFactor(
    qualitative_hcl(length(unique(ames$Overall_Cond)), palette = "Dynamic"), 
    domain = ames$Overall_Cond
  )

ames %>% 
  arrange(Overall_Cond) %>% 
  leaflet() %>%
  addCircleMarkers(
    radius = 3,
    group = ~Overall_Cond, 
    color = ~pal(Overall_Cond),
    stroke = FALSE,
    fillOpacity = 0.7, 
    lng = ~Longitude, lat = ~Latitude, 
    popup = ~paste("Neighborhood:", Neighborhood, 
                   "<br>Year Built:", Year_Built, 
                   "<br>MS Zoning:", MS_Zoning, 
                   "<br>Overall Cond:", Overall_Cond)
  ) %>% 
  addLayersControl(overlayGroups = ~Overall_Cond, 
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  setView(lng = -93.58429, lat = 42.02712, zoom = 12)

ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(MS_SubClass, Log10Sale_Price, median, na.rm = TRUE))) +
  geom_boxplot(fill = "steelblue") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Subkelas Penjualan",
       x = "Log10 Harga Rumah",
       y = "Subkelas Penjualan") +
  theme_minimal() 

ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(House_Style, Log10Sale_Price, median, na.rm = TRUE))) +
  geom_boxplot(fill = "steelblue") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Gaya Rumah",
       x = "Log10 Harga Rumah",
       y = "Gaya Rumah") +
  theme_minimal() 


ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(MS_Zoning, Log10Sale_Price, median, na.rm = TRUE))) + 
  geom_boxplot(fill = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Klasifikasi Zona",
       x = "Harga Rumah ($)",
       y = "Klasifikasi Zona") +
  theme_minimal()

ames %>% 
  mutate(
    MS_Zoning = case_when(MS_Zoning %in% c("A_agr", "C_all", "I_all") ~ "Others", 
                          TRUE ~ MS_Zoning)
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(MS_Zoning, Log10Sale_Price, median, na.rm = TRUE))) + 
  geom_boxplot(fill = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Klasifikasi Zona",
       x = "Harga Rumah ($)",
       y = "Klasifikasi Zona") +
  theme_minimal()

ames <- ames %>% 
  mutate(
    MS_Zoning = case_when(MS_Zoning %in% c("A_agr", "C_all", "I_all") ~ "Others", 
                          TRUE ~ MS_Zoning)
  )

ames %>% 
  mutate(
    Lot_Shape = case_when(Lot_Shape %in% c("Irregular", "Moderately_Irregular") ~ "Irregular", 
                          TRUE ~ Lot_Shape)
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Lot_Shape, Log10Sale_Price, median, na.rm = TRUE))) + 
  geom_boxplot(fill = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Bentuk Bidang Tanah",
       x = "Harga Rumah ($)",
       y = "Bentuk Bidang Tanah") +
  theme_minimal()

ames <- ames %>% 
  mutate(
    Lot_Shape = case_when(Lot_Shape %in% c("Irregular", "Moderately_Irregular") ~ "Irregular", 
                          TRUE ~ Lot_Shape)
  )

# Tipe penjualan dikateogrikan ulang
ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Sale_Type, Log10Sale_Price, median, na.rm = TRUE))) + 
  geom_boxplot(fill = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Tipe Penjualan",
       x = "Harga Rumah ($)",
       y = "Tipe Penjualan") +
  theme_minimal()

ames %>% 
  mutate(
    Sale_Type = case_when(Sale_Type %in% c("WD", "CWD", "VWD") ~ "Warranty",
                          Sale_Type %in% c("Con", "ConLI", "ConLw", "ConLD") ~ "Contract",
                          Sale_Type %in% c("COD", "Oth") ~ "COD_Oth",
                          Sale_Type == "New" ~ "New", 
                          TRUE ~ "Unknown")) %>%
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Sale_Type, Log10Sale_Price, median, na.rm = TRUE))) + 
  geom_boxplot(fill = "steelblue") + 
  stat_summary(fun = mean, geom = "point", color = "firebrick") +
  scale_x_continuous(labels = comma_format()) + 
  labs(title = "Harga Rumah berdasarkan Tipe Penjualan",
       x = "Harga Rumah ($)",
       y = "Tipe Penjualan") +
  theme_minimal()

# Menggabungkan jumlah kamar mandi berdasarkan jenisnya
ames %>% 
  mutate(Total_Full_Bath = Full_Bath + Bsmt_Full_Bath) %>% 
  ggplot(aes(x = Total_Full_Bath)) + 
  geom_bar()

ames %>% 
  mutate(Total_Half_Bath = Half_Bath + Bsmt_Half_Bath) %>% 
  ggplot(aes(x = Total_Half_Bath)) + 
  geom_bar()

ames <- ames %>% 
  mutate(Total_Full_Bath = Full_Bath + Bsmt_Full_Bath) %>% 
  mutate(Total_Half_Bath = Half_Bath + Bsmt_Half_Bath) 


# Membuat Kategori Modernitas dan Kualitas Rumah
# Fungsi untuk modernitas
get_modernity <- function(x) {
  dplyr::case_when(
    x %in% c("VinylSd", "CemntBd", "MetalSd", "PreCast") ~ "Modern",
    x %in% c("Wd Sdng", "WdShing", "HdBoard", "Plywood", "Stucco", "ImStucc", "AsphShn") ~ "Traditional",
    TRUE ~ "Unknown"
  )
}

# Fungsi untuk kualitas
get_quality <- function(x) {
  dplyr::case_when(
    x %in% c("Stone", "BrkFace", "BrkComm") ~ "Premium",
    x %in% c("AsbShng", "CBlock", "Other") ~ "Low",
    TRUE ~ "Standard"
  )
}

combine_priority <- function(cat1, cat2, order) {
  pmin <- function(x, y) ifelse(match(x, order) < match(y, order), x, y)
  mapply(pmin, cat1, cat2)
}

modernity_order <- c("Modern", "Traditional", "Unknown")
quality_order <- c("Premium", "Standard", "Low", "Unknown")


ames <- ames %>% 
  mutate(
    Modernity1 = get_modernity(Exterior_1st), 
    Modernity2 = get_modernity(Exterior_2nd), 
    Quality1 = get_quality(Exterior_1st), 
    Quality2 = get_quality(Exterior_2nd), 
    Modernity = combine_priority(Modernity1, Modernity2, order = modernity_order), 
    Quality = combine_priority(Quality1, Quality2, order = quality_order)
  )


ames %>% 
  ggplot(aes(x = Mas_Vnr_Area, 
             y = fct_reorder(Mas_Vnr_Type, Mas_Vnr_Area, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Masonry Veneer Area", 
       y = "Masonry Veneer Type") + 
  theme_minimal()

ames %>% 
  mutate(Has_Masonry = if_else(Mas_Vnr_Type == "None" | Mas_Vnr_Area == 0, "No_Masonry", "Has_Masonry")) %>% 
  ggplot(aes(x = Mas_Vnr_Area, 
             y = fct_reorder(Has_Masonry, Mas_Vnr_Area, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Masonry Veneer Area", 
       y = "Masonry Veneer Type") + 
  theme_minimal()

ames <- ames %>% 
  mutate(Has_Masonry = if_else(Mas_Vnr_Type == "None" | Mas_Vnr_Area == 0, 0, 1))

ames %>% 
  count(Has_Masonry)

ames %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Fence, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Jenis Pagar") + 
  theme_minimal()

ames %>% 
  mutate(
    Fence = case_when(Fence == "No_Fence" ~ "No", 
                      str_detect(Fence, "^Good") ~ "Good", 
                      str_detect(Fence, "^Minimum") ~ "Minimum", 
                      TRUE ~ "unknown"), 
    Fence = 
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Fence, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Jenis Pagar") + 
  theme_minimal()

ames %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Pool_QC, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Kualitas Kolam Renang") + 
  theme_minimal()

ames %>% 
  mutate(
    Pool_QC = case_when(Pool_QC %in% c("Fair", "Good", "Excellent") ~ "Fair_to_Excellent", 
                      TRUE ~ Pool_QC)
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Pool_QC, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Kualitas Kolam Renang") + 
  theme_minimal()

ames %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Bldg_Type, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Jenis Bangunan") + 
  theme_minimal()

ames %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Street, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Jenis Jalan") + 
  theme_minimal()

ames %>% 
  ggplot(aes(x = Log10Sale_Price, y = fct_reorder(Lot_Config, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Jenis Akses Jalan") + 
  theme_minimal()

ames %>% 
  count(Heating) %>% 
  mutate(pct = n/sum(n))

ames %>% 
  count(Heating_QC) %>% 
  mutate(pct = n/sum(n))

ames %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Heating_QC, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Jenis Akses Jalan") + 
  theme_minimal()

ames %>% 
  mutate(Heating_QC = case_when(Heating_QC %in% c("Poor", "Fair") ~ "Poor_Fair", 
                                TRUE ~ Heating_QC) %>% 
           fct_relevel(c("Poor_Fair", "Typical", "Good", "Excellent"))
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Heating_QC, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Kualitas Pemanas") + 
  theme_minimal()

ames %>% 
  mutate(
    Heating_QC = case_when(Heating_QC %in% c("Poor", "Fair") ~ "Poor_Fair", 
                           TRUE ~ Heating_QC) %>% 
      fct_relevel("Poor_Fair", "Typical", "Good", "Excellent")
      ) %>% 
  count(Heating_QC) %>% 
  mutate(pct = n/sum(n))

ames %>% 
  count(Garage_Cond) %>% 
  mutate(pct = n/sum(n))

ames %>% 
  mutate(
    Garage_Cond = case_when(Garage_Cond %in% c("No_Garage", "Typical") ~ Garage_Cond, 
                            TRUE ~ "Others"), 
    Garage_Cond = factor(Garage_Cond, levels = c("No_Garage", "Typical", "Others"))
  ) %>% 
  ggplot(aes(x = Log10Sale_Price, 
             y = fct_reorder(Garage_Cond, Log10Sale_Price, median))) + 
  geom_boxplot(fill = "steelblue") + 
  labs(x = "Log10 Harga Rumah", 
       y = "Kondisi Garasi") + 
  theme_minimal()


# 7. Analisis hubungan multivariate
# Scatter plot matrix untuk beberapa variabel 
ames %>% 
  select(Sale_Price, Gr_Liv_Area, Year_Built, Overall_Cond, Garage_Area) %>%
  mutate(Overall_Cond = as.factor(Overall_Cond)) %>% 
  ggpairs(lower = list(continuous = "smooth"),
        diag = list(continuous = "densityDiag"),
        upper = list(continuous = "cor"),
        title = "Hubungan Multivariate antara Variabel Penting")


