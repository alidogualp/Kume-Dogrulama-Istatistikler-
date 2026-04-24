
install.packages(c("cluster", "factoextra", "fpc", "gridExtra"))

library(cluster)
library(factoextra)
library(fpc)
library(gridExtra)

# Veriyi Oku (Senin dosya yolun)
df <- read.csv("/Users/berka/desktop/crocodile_dataset _2_.csv")

# Sadece sayisal sutunlari al ve Olceklendir
df_num <- df[, c("Observed.Length..m.", "Observed.Weight..kg.")]
df_scaled <- scale(df_num)

# Grafikleri depolamak icin bos listeler
plot_cluster <- list()
plot_silhouette <- list()

# ==============================================================================
# 2. DONGU: MODELLERI KUR VE ISTATISTIKLERI HESAPLA (k = 2, 3, 4, 5)
# ==============================================================================
for (k in 2:5) {
  
  # A) Modeli Kur
  set.seed(123)
  km_res <- kmeans(df_scaled, centers = k, nstart = 25)
  
  # B) Istatistikleri Hesapla (Grafik basligina yazmak icin)
  # Siluet Skoru
  sil <- silhouette(km_res$cluster, dist(df_scaled))
  mean_sil <- round(mean(sil[, 3]), 3)
  
  # Dunn Endeksi
  stats <- cluster.stats(d = dist(df_scaled), clustering = km_res$cluster)
  dunn_val <- round(stats$dunn, 4)
  
  # C) Grafikleri Olustur
  
  # 1. Kume Dagilim Grafigi
  p1 <- fviz_cluster(km_res, data = df_scaled, 
                     geom = "point",
                     palette = "jco", 
                     ggtheme = theme_minimal(),
                     main = paste0("k=", k, " | Siluet:", mean_sil, " | Dunn:", dunn_val)) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  plot_cluster[[length(plot_cluster) + 1]] <- p1
  
  # 2. Siluet Grafigi
  p2 <- fviz_silhouette(sil, 
                        print.summary = FALSE, 
                        palette = "jco", 
                        ggtheme = theme_minimal(),
                        main = paste0("k=", k, " Siluet Analizi (Ort: ", mean_sil, ")")) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  plot_silhouette[[length(plot_silhouette) + 1]] <- p2
}

# ==============================================================================
# 3. GORSELLESTIRME: 2x2 MATRIX (TOPLU BAKIS)
# ==============================================================================
# Burasi grafiklerin hepsini tek sayfada 4'lu kare seklinde gosterir.

# Kume Dagilimlari (2x2)
grid.arrange(grobs = plot_cluster, ncol = 2, 
             top = "KUME DAGILIMLARI (2x2 GORUNUM)")

# Yeni sayfa ac (RStudio Plot panelinde oncekini silmemesi icin)
grid.newpage()

# Siluet Analizleri (2x2)
grid.arrange(grobs = plot_silhouette, ncol = 2, 
             top = "SILUET KALITE ANALIZLERI (2x2 GORUNUM)")


# ==============================================================================
# 4. DUZELTME: 1x1 (TEK SUTUN / ALT ALTA)
# ==============================================================================
# Eger grafikleri buyuk buyuk alt alta gormek istersen 'ncol = 1' yapmalisin.

# Kume Dagilimlari (1x1 - Alt alta dizersen sayfa uzar)
grid.newpage()
grid.arrange(grobs = plot_cluster, ncol = 1, 
             top = "K??ME DA??ILIMLARI (1x1 LISTE GORUNUMU)")

# Siluet Analizleri (1x1)
grid.newpage()
grid.arrange(grobs = plot_silhouette, ncol = 1, 
             top = "SILUET KALITE ANALIZLERI (1x1 LISTE GORUNUMU)")

# NOT: Eger sadece tek bir tanesini (mesela k=3) gormek istersen:
# print(plot_cluster[[2]])  # Listede 2. sirada k=3 vardir.

# ==============================================================================
# 1. KUTUPHANELER VE VERI YUKLEME
# ==============================================================================

# Sadece Boy ve Kilo (6. ve 7. sutunlar)
df_num <- df[, c(6, 7)]
df_scaled <- scale(df_num)

# ==============================================================================
# 2. HIYERARSIK AGACI OLUSTUR
# ==============================================================================
# Agaci bir kere hesapliyoruz, sonra farkli k'lar icin boyayacagiz
dist_mat <- dist(df_scaled, method = "euclidean")
hc_res <- hclust(dist_mat, method = "ward.D2")

# ==============================================================================
# 3. DONGU: k=2, 3, 4, 5 ICIN DENDROGRAMLARI HAZIRLA
# ==============================================================================
plot_list <- list()

for (k in 2:5) {
  
  # Dendrogrami ciz
  p <- fviz_dend(hc_res, 
                 k = k,                 # Kume sayisi (Donguden gelir: 2,3,4,5)
                 cex = 0.5,             # Yazi boyutu (kucuk tutuyoruz)
                 show_labels = FALSE,   # Etiketleri gizle (cok veri var)
                 k_colors = "jco",      # Renk paleti
                 rect = TRUE,           # Kutucuk icine al
                 rect_border = "jco",   
                 rect_fill = TRUE,      
                 main = paste("Dendrogram ( k =", k, ")")) # Baslik
  
  # Listeye ekle
  plot_list[[length(plot_list) + 1]] <- p
}

# ==============================================================================
# 4. GORSELLESTIRME (2x2 MATRIX)
# ==============================================================================
# 4 Grafigi tek sayfada birlestir
grid.arrange(grobs = plot_list, ncol = 2, 
             top = "Farkl?? K De??erleri ????in Hiyerar??ik A??a?? Kesimi")



# Gerekli Paketler (Yoksa kurar)
if(!require(factoextra)) install.packages("factoextra")
if(!require(gridExtra)) install.packages("gridExtra")

library(factoextra)
library(gridExtra)

# 1. VER??Y?? OKU (Senin bilgisayar??ndaki yol)
# E??er hata al??rsan file.choose() ile elle se??ebilirsin
df <- read.csv("/Users/berka/desktop/crocodile_dataset _2_.csv")

# 2. SADECE BOY VE K??LO AL (6. ve 7. s??tunlar)
df_num <- df[, c(6, 7)]
df_scaled <- scale(df_num)

# 3. H??YERAR????K A??ACI OLU??TUR
# Euclidean uzakl??k ve Ward metodu
res_hc <- hclust(dist(df_scaled), method = "ward.D2")

# 4. D??NG??: k=2, 3, 4, 5 ??????N DENDROGRAMLARI HAZIRLA
plot_list <- list()

for(k in 2:5){
  # Her k de??eri i??in a??ac?? boyay??p listeye at??yoruz
  p <- fviz_dend(res_hc, 
                 k = k, 
                 cex = 0.5, 
                 show_labels = FALSE, 
                 k_colors = "jco", 
                 rect = TRUE, 
                 rect_border = "jco", 
                 rect_fill = TRUE,
                 main = paste("Dendrogram (k =", k, ")"))
  
  plot_list[[length(plot_list)+1]] <- p
}

# 5. ????ZD??R (2x2 FORMATINDA)
grid.arrange(grobs = plot_list, ncol = 2, top = "Farkl?? K De??erleri ????in Dendrogramlar")





# Gerekli Paketleri Y??kle
if(!require(factoextra)) install.packages("factoextra")
if(!require(gridExtra)) install.packages("gridExtra")

library(factoextra)
library(gridExtra)

# 1. DOSYAYI SE?? (Pencere A????lacak)
cat("L??tfen crocodile_dataset dosyas??n?? se??in...\n")
df <- read.csv(file.choose())

# 2. VER??Y?? HAZIRLA (Sadece Boy ve Kilo)
# 6. ve 7. s??tunlar?? al??yoruz (??simlere tak??lmamak i??in s??ra no kulland??m)
df_num <- df[, c(6, 7)]
df_scaled <- scale(df_num)

# 3. A??ACI OLU??TUR
# Hiyerar??ik k??meleme hesaplan??yor
res_hc <- hclust(dist(df_scaled), method = "ward.D2")

# 4. GRAF??KLER?? HAZIRLA (k=2, 3, 4, 5 i??in)
# Hepsini bir listeye at??p toplu ??izece??iz
plot_list <- list()

for(k in 2:5){
  p <- fviz_dend(res_hc, 
                 k = k, 
                 cex = 0.5, 
                 show_labels = FALSE,   # ??simleri gizle (??ok veri oldu??u i??in)
                 k_colors = "jco",      # Renk paleti
                 rect = TRUE,           # Kutucuk i??ine al
                 rect_border = "jco", 
                 rect_fill = TRUE,
                 main = paste("Dendrogram (k =", k, ")"))
  
  plot_list[[length(plot_list)+1]] <- p
}

# 5. EKRANA BAS (2x2 Format??nda)
grid.arrange(grobs = plot_list, ncol = 2, top = "Hiyerar??ik K??meleme Kar????la??t??rmas??")


# Gerekli Paketler
if(!require(fpc)) install.packages("fpc")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(factoextra)) install.packages("factoextra")

library(fpc)
library(ggplot2)

# 1. VERIYI OKU VE HAZIRLA
# ---------------------------------------------------------
# Dosya secme penceresi acilir


# Sadece Boy ve Kilo (6. ve 7. sutunlar) ve Olceklendirme
df_num <- df[, c(6, 7)]
df_scaled <- scale(df_num)

# 2. HESAPLAMA (Calinski-Harabasz)
# ---------------------------------------------------------
k_values <- 2:5
ch_scores <- c() # Skorlari buraya biriktirecegiz

for (k in k_values) {
  # K-Means modelini kur
  set.seed(123)
  km <- kmeans(df_scaled, centers = k, nstart = 25)
  
  # CH Skorunu Hesapla (calinhara fonksiyonu fpc paketinden gelir)
  # calinhara(veri, kume_etiketleri)
  ch_val <- calinhara(df_scaled, km$cluster)
  ch_scores <- c(ch_scores, ch_val)
}

# 3. VERIYI TABLOYA DOKME
# ---------------------------------------------------------
results <- data.frame(k = k_values, CH_Score = ch_scores)

print("--- Calinski-Harabasz Skorlar?? ---")
print(results)

# 4. GRAFIK CIZIMI (ggplot2 ile sik bir cizim)
# ---------------------------------------------------------
ggplot(results, aes(x = k, y = CH_Score)) +
  geom_line(color = "purple", size = 1.2) +        # Cizgi
  geom_point(color = "purple", size = 4) +         # Noktalar
  geom_text(aes(label = round(CH_Score, 1)),       # Degerleri yaz
            vjust = -0.8, fontface = "bold") +
  labs(title = "Calinski-Harabasz ??ndeksi Kar????la??t??rmas??",
       subtitle = "Yuksek deger daha iyi ayr??smay?? gosterir",
       x = "Kume Say??s?? (k)",
       y = "CH Skoru") +
  theme_minimal() +
  scale_x_continuous(breaks = k_values)            # Sadece 2,3,4,5 goster




# Gerekli Paketler


library(cluster)
library(factoextra)


# 2. GAP ISTATISTIGINI HESAPLA
# iter.max = 50 ekledik (Warning cozumu)
# nstart = 25 (Guvenilirlik icin)
cat("Hesaplama yapiliyor (biraz zaman alabilir)...\n")
set.seed(123)
gap_stat <- clusGap(df_scaled, FUN = kmeans, nstart = 25,
                    K.max = 8, B = 50, iter.max = 50)

# 3. SONUCLARI GOSTER
print(gap_stat, method = "firstmax")

# 4. GRAFIGI CIZ
# maxColor argumanini kaldirdik (Hata cozumu)
fviz_gap_stat(gap_stat, 
              linecolor = "steelblue") +
  labs(title = "Gap ??statisti??i Sonucu",
       subtitle = "Optimal K??me Say??s?? (Kesikli ??izgi ile g??sterilir)",
       x = "K??me Say??s?? (k)",
       y = "Gap De??eri") +
  theme_minimal()



# 2. GAP ISTATISTIGINI HESAPLA
# iter.max = 50 (Uyariyi engeller)
# B = 50 (Simulasyon sayisi)
cat("Hesaplama yapiliyor (biraz surebilir)...\n")
set.seed(123)
gap_stat <- clusGap(df_scaled, FUN = kmeans, nstart = 25,
                    K.max = 8, B = 50, iter.max = 50)

# 3. VERIYI GRAFIK ICIN DUZENLE
# Gap sonuclarini bir tabloya (dataframe) ceviriyoruz
df_gap_values <- as.data.frame(gap_stat$Tab)
df_gap_values$k <- 1:nrow(df_gap_values) # K degerlerini (1,2,3...) ekle

# 4. DEGERLI GRAFIGI CIZ (ggplot ile Ozel Cizim)
ggplot(df_gap_values, aes(x = k, y = gap)) +
  # Cizgi ve Noktalar
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  # Hata Cubuklari (Opsiyonel - Gap'in guven araligi)
  geom_errorbar(aes(ymin = gap - SE.sim, ymax = gap + SE.sim), 
                width = 0.2, color = "gray") +
  # --- ISTE BURASI DEGERLERI YAZAR ---
  geom_text(aes(label = round(gap, 3)), 
            vjust = -1,    # Yaziyi noktanin biraz ustune al
            fontface = "bold", 
            size = 4) +
  # -----------------------------------
labs(title = "Gap ??statistigi ve Degerleri",
     subtitle = "Yuksek deger daha iyi kumelemeyi gosterir",
     x = "Kume Say??s?? (k)",
     y = "Gap Degeri") +
  theme_minimal() +
  scale_x_continuous(breaks = df_gap_values$k) # X eksenine tam sayilari koy


# Gercek Etiketler (Ground Truth - Karsilastirma icin)
# Timsahlarin "Age Class" (Yetiskin, Yavru vb.) bilgisini alalim
true_labels <- df$Age.Class 

# ==============================================================================
# 3. MODELLERI KURMA (k=3 icin)
# ==============================================================================

# A) K-Means Modeli
set.seed(123)
km_res <- kmeans(df_scaled, centers = 3, nstart = 25)
km_clusters <- km_res$cluster

# B) Hiyerarsik Kumeleme Modeli
dist_mat <- dist(df_scaled, method = "euclidean")
hc_tree <- hclust(dist_mat, method = "ward.D2")
hc_clusters <- cutree(hc_tree, k = 3)

# ==============================================================================
# 4. ARI (ADJUSTED RAND INDEX) HESAPLAMA VE KARSILASTIRMA
# ==============================================================================

# SENARYO 1: Iki Farkli Algoritmanin Karsilastirilmasi
# "K-Means ile Hiyerarsik yontem ne kadar benzer sonuclar buldu?"
ari_algo <- adjustedRandIndex(km_clusters, hc_clusters)

# SENARYO 2: Algoritmanin Gercek Veriyle Karsilastirilmasi (External Validation)
# "K-Means'in buldugu gruplar, gercek yas siniflariyla (Age Class) ne kadar uyusuyor?"
ari_truth_km <- adjustedRandIndex(km_clusters, true_labels)
ari_truth_hc <- adjustedRandIndex(hc_clusters, true_labels)

# ==============================================================================
# 5. SONUCLARI GOSTER (TABLO)
# ==============================================================================
cat("\n--- ARI (Adjusted Rand Index) KARSILASTIRMA RAPORU ---\n")
cat("Deger Araligi: [-1, 1]. 1'e yakin olmasi mukemmel eslesme demektir.\n\n")

comparison_df <- data.frame(
  Karsilastirma = c("K-Means vs Hiyerar??ik (Metot Benzerli??i)", 
                    "K-Means vs Ger??ek Ya?? S??n??f?? (Ground Truth)", 
                    "Hiyerar??ik vs Ger??ek Ya?? S??n??f?? (Ground Truth)"),
  ARI_Skoru = c(ari_algo, ari_truth_km, ari_truth_hc)
)

print(comparison_df)

# ==============================================================================
# 6. GORSEL KARSILASTIRMA (K-Means vs Age Class)
# ==============================================================================
# Gercek etiketlerle Modelin bulduklarini yan yana cizelim
library(ggplot2)
library(gridExtra)

km_clusters
df$KMeans_Cluster <- as.factor(km_clusters)

# Modelin Buldugu
p1 <- ggplot(df, aes(x=Observed.Length..m., y=Observed.Weight..kg., color=KMeans_Cluster)) +
  geom_point(alpha=0.6) +
  labs(title="K-Means (k=3) Sonucu", color="K??me") + theme_minimal()

# Gercek Yas Siniflari
p2 <- ggplot(df, aes(x=Observed.Length..m., y=Observed.Weight..kg., color=Age.Class)) +
  geom_point(alpha=0.6) +
  labs(title="Gercek Yas S??n??flar?? (Ground Truth)", color="S??n??f") + theme_minimal()

grid.arrange(p1, p2, ncol=2, top="Model vs Gercek Veri Kars??last??rmas??")




# ==============================================================================
# 1. KUTUPHANELER VE TEMIZ BASLANGIC
# ==============================================================================
if(!require(mclust)) install.packages("mclust")
if(!require(cluster)) install.packages("cluster")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")

library(mclust)
library(cluster)
library(ggplot2)
library(gridExtra)

# ==============================================================================
# 2. VERIYI HAZIRLA (GARANTILI ESITLEME)
# ==============================================================================
# Dosyayi sec
cat("Lutfen crocodile_dataset dosyasini secin...\n")
df_raw <- read.csv(file.choose())

# Analiz icin gerekli sutunlari secip YENI bir dataframe olusturuyoruz.
# Bu adim cok onemlidir, boylece satir sayilari kesinlikle esitlenir.
# 6: Boy, 7: Kilo, 8: Age Class (Csv yapiniza gore degisebilir, kontrol edin)
# S??tun isimlerinden emin olmak icin isimle seciyoruz:

df_work <- df_num[, c("Observed.Length..m.", "Observed.Weight..kg.", "Age.Class")]

# Bos veri varsa satiri komple sil (Sync islemi)
df_clean <- na.omit(df_work)

# Artik her sey bu 'df_clean' uzerinden gidecek. Kayma imkansiz.
df_model <- df_clean[, 1:2]       # Sadece Boy ve Kilo
true_labels <- df_clean[, 3]      # Sadece Yas Sinifi (Ground Truth)

# Olceklendirme
df_scaled <- scale(df_model)

# ==============================================================================
# 3. MODELLERI KUR
# ==============================================================================

# A) K-Means (k=3)
set.seed(123)
km_res <- kmeans(df_scaled, centers = 3, nstart = 25)
km_clusters <- km_res$cluster

# B) Hiyerarsik Kumeleme (k=3)
dist_mat <- dist(df_scaled, method = "euclidean")
hc_tree <- hclust(dist_mat, method = "ward.D2")
hc_clusters <- cutree(hc_tree, k = 3)

# ==============================================================================
# 4. ARI HESAPLA (Artik hata vermez)
# ==============================================================================

# K-Means vs Gercek Etiketler
ari_km <- adjustedRandIndex(km_clusters, true_labels)

# Hiyerarsik vs Gercek Etiketler
ari_hc <- adjustedRandIndex(hc_clusters, true_labels)

# K-Means vs Hiyerarsik
ari_algo <- adjustedRandIndex(km_clusters, hc_clusters)

# ==============================================================================
# 5. SONUCLARI RAPORLA
# ==============================================================================
results <- data.frame(
  Karsilastirma = c("K-Means vs Ger??ek Ya?? (Age Class)", 
                    "Hiyerar??ik vs Ger??ek Ya?? (Age Class)",
                    "K-Means vs Hiyerar??ik (Metot Benzerli??i)"),
  ARI_Skoru = round(c(ari_km, ari_hc, ari_algo), 4)
)

print("--- ARI SONUCLARI ---")
print(results)

# ==============================================================================
# 6. GORSEL KARSILASTIRMA
# ==============================================================================
# Gorsellestirme icin veriye kume sonuclarini ekleyelim
df_clean$KMeans_Cluster <- as.factor(km_clusters)

# Grafigi Ciz
p1 <- ggplot(df_clean, aes(x=Observed.Length..m., y=Observed.Weight..kg., color=KMeans_Cluster)) +
  geom_point(alpha=0.6, size=2) +
  labs(title="K-Means Tahmini (k=3)", x="Boy", y="Kilo") + theme_minimal()

p2 <- ggplot(df_clean, aes(x=Observed.Length..m., y=Observed.Weight..kg., color=Age.Class)) +
  geom_point(alpha=0.6, size=2) +
  labs(title="Ger??ek S??n??flar (Age Class)", x="Boy", y="Kilo") + theme_minimal()

grid.arrange(p1, p2, ncol=2, top="Model Ba??ar??s?? Kar????la??t??rmas??")



##########################################

# 2. ELBOW GRAFIGINI CIZ
# fviz_nbclust fonksiyonu 'wss' (Within Sum of Square) metoduyla calisir.
fviz_nbclust(df_scaled, kmeans, method = "wss", k.max = 7) +
  labs(title = "Elbow Y??ntemi (Dirsek Analizi)",
       subtitle = "Dirsek noktas?? optimal k??me say??s??n?? i??aret eder (Genellikle k=2 veya k=3)",
       x = "K??me Say??s?? (k)",
       y = "Toplam Kareler Toplam?? (WCSS)") +
  # Gorselligi iyilestirelim
  geom_point(color = "steelblue", size = 3) +
  geom_line(color = "steelblue", size = 1) +
  theme_minimal() +
  # Degerleri noktalarin uzerine yazalim
  geom_text(aes(label = round(y, 1)), vjust = -1, size = 3.5)


# 1. VERIYI HAZIRLA
cat("Lutfen crocodile_dataset dosyasini secin...\n")
df <- read.csv(file.choose())

# Sadece Boy ve Kilo (6. ve 7. sutunlar - kontrol et)
df_num <- df[, c("Observed.Length..m.", "Observed.Weight..kg.")]
df_scaled <- scale(df_num)

# 2. K-MEANS MODELINI KUR (k=3 i??in)
set.seed(123) # Sonuclarin degismemesi icin
k_means_model <- kmeans(df_scaled, centers = 3, nstart = 25)

# 3. CALINSKI-HARABASZ (CH) DEGERINI HESAPLA
# fpc paketindeki 'calinhara' fonksiyonu bunu yapar
ch_score <- calinhara(df_scaled, k_means_model$cluster)

# 4. SONUCU YAZDIR
cat("\n----------------------------------------------------\n")
cat("ANAL??Z SONUCU (k=3 i??in ????sel Ge??erlilik)\n")
cat("----------------------------------------------------\n")
cat("Calinski-Harabasz (CH) ??ndeksi: ", round(ch_score, 2), "\n")
cat("----------------------------------------------------\n")
cat("YORUM: Bu de??er ne kadar Y??KSEK olursa, k??meler o kadar \niyi ayr????m???? (dense & separated) demektir.\n")


# 1. VERIYI HAZIRLA
cat("Lutfen crocodile_dataset dosyasini secin...\n")
df <- read.csv(file.choose())
df_num <- df[, c(6, 7)] # Boy ve Kilo
df_scaled <- scale(df_num)

# 2. D??NG?? ??LE HER K ??????N CH HESAPLA (k=2'den k=8'e)
# Not: CH indeksi k=1 i??in hesaplanamaz, en az 2 k??me gerekir.
k_values <- 2:10
ch_scores <- numeric(length(k_values))

cat("Hesaplama yapiliyor...\n")
for(i in 1:length(k_values)) {
  k <- k_values[i]
  set.seed(123)
  km <- kmeans(df_scaled, centers = k, nstart = 25)
  # calinhara fonksiyonu CH degerini verir
  ch_scores[i] <- calinhara(df_scaled, km$cluster)
}

# Veriyi tabloya ??evir
df_ch <- data.frame(k = k_values, ch = ch_scores)

# 3. GRAF?????? ????Z (ggplot2)
ggplot(df_ch, aes(x = k, y = ch)) +
  # Cizgi ve Noktalar
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(color = "brown", size = 4) +
  
  # De??erleri ??zerine Yaz
  geom_text(aes(label = round(ch, 1)), vjust = -1, fontface = "bold") +
  
  # En y??ksek de??eri (Zirveyi) vurgula (Otomatik bulur)
  geom_point(data = df_ch[which.max(df_ch$ch), ], 
             aes(x = k, y = ch), 
             color = "red", size = 6, shape = 1) +
  
  # Ba??l??k ve Eksenler
  labs(title = "Calinski-Harabasz ??ndeksi (Kars??last??rmal??)",
       subtitle = "Deger ne kadar yuksekse kumeleme o kadar basar??l??d??r",
       x = "Kume Say??s?? (k)",
       y = "CH Degeri") +
  
  scale_x_continuous(breaks = k_values) + # X eksenine tam sayilari koy
  theme_minimal()
summary(data.frame)
