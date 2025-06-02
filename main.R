# Load libraries
library(dplyr)
library(factoextra)
library(cluster)
library(corrplot)

# Load dataset
df <- Student_Performance_Metrics_Dataset

# Cek struktur dan nilai unik variabel kategorikal
str(df)
unique(df$Department)
unique(df$Income)
unique(df$Hometown)
unique(df$Preparation)
unique(df$Gaming)
unique(df$Attendance)
unique(df$Semester)

# Encoding variabel ordinal dan nominal
df_encoded <- df %>%
    mutate(
        # Ordinal Encoding
        Preparation_num = recode(
            Preparation,
            "0-1 Hour" = 1,
            "2-3 Hours" = 2,
            "More than 3 Hours" = 3
        ),
        Gaming_num = recode(
            Gaming,
            "0-1 Hour" = 1,
            "2-3 Hours" = 2,
            "More than 3 Hours" = 3
        ),
        Attendance_num = recode(
            Attendance,
            "Below 40%" = 1,
            "40%-59%" = 2,
            "60%-79%" = 3,
            "80%-100%" = 4
        ),
        Semester_num = as.numeric(gsub("[^0-9]", "", Semester)),
        Income_num = recode(
            Income,
            "Low (Below 15,000)" = 1,
            "Lower middle (15,000-30,000)" = 2,
            "Upper middle (30,000-50,000)" = 3,
            "High (Above 50,000)" = 4
        ),
        
        # Nominal Encoding (binary)
        Job_num = ifelse(Job == "Yes", 1, 0),
        Extra_num = ifelse(Extra == "Yes", 1, 0),
        Hometown_num = ifelse(Hometown == "City", 1, 0),
        Gender_num = ifelse(Gender == "Male", 1, 0)
    )

# Pilih variabel numerik dan encoded
df_ready <- df_encoded %>%
    select(
        HSC,
        SSC,
        Computer,
        English,
        Last,
        Overall,
        # skor numerik
        Preparation_num,
        Gaming_num,
        Attendance_num,
        # ordinal
        Semester_num,
        Income_num,
        # ordinal
        Job_num,
        Extra_num,
        Hometown_num,
        Gender_num        # nominal binary
    )

# One-hot encode Department
dept_onehot <- model.matrix( ~ Department - 1, data = df)

# Gabungkan data
df_ready_full <- cbind(df_ready, dept_onehot)

# Skala data
df_scaled <- scale(df_ready_full)

# Tentukan jumlah cluster dengan Elbow Method
set.seed(6969)
fviz_nbclust(df_scaled, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = "Elbow Method untuk menentukan jumlah cluster")

# Jalankan K-Means
set.seed(6969)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 25)

# Lihat distribusi cluster
table(kmeans_result$cluster)

# Tambahkan cluster hasil K-Means ke data
df_final <- df_encoded %>%
    mutate(Cluster = kmeans_result$cluster)

# Visualisasi cluster K-Means
fviz_cluster(
    kmeans_result,
    data = df_scaled,
    geom = "point",
    ellipse.type = "convex",
    palette = "jco",
    ggtheme = theme_minimal()
)

# Statistik rata-rata fitur utama per cluster
df_final %>%
    group_by(Cluster) %>%
    summarise(across(
        c(
            Last,
            Overall,
            Attendance_num,
            Gaming_num,
            Preparation_num,
            Computer,
            Income_num
        ),
        mean,
        na.rm = TRUE
    ))


# PCA untuk visualisasi
pca <- prcomp(df_scaled)
library(factoextra)

fviz_pca_biplot(
    pca,
    label = "var",
    # tampilkan label variabel
    habillage = kmeans_result$cluster,
    # warna berdasarkan cluster
    addEllipses = TRUE,
    # gambar elips tiap cluster
    palette = "jco",
    # palette warna
    repel = TRUE,
    # supaya label variabel tidak bertumpuk
    ggtheme = theme_minimal()
)

#Fitur akademik
fviz_pca_biplot(
    pca_res,
    label = "var",
    select.var = list(name = c(
        "HSC", "SSC", "Computer", "English", "Last", "Overall"
    )),
    geom = "point",
    habillage = as.factor(kmeans_result$cluster),
    addEllipses = TRUE,
    palette = "jco",
    repel = TRUE,
    ggtheme = theme_minimal()
) +
    ggtitle("PCA Biplot: Fitur Akademik")

# perilaku dan demografi
fviz_pca_biplot(
    pca_res,
    label = "var",
    select.var = list(
        name = c(
            "Preparation_num",
            "Gaming_num",
            "Attendance_num",
            "Semester_num",
            "Income_num",
            "Job_num",
            "Extra_num",
            "Hometown_num",
            "Gender_num"
        )
    ),
    geom = "point",
    habillage = as.factor(kmeans_result$cluster),
    addEllipses = TRUE,
    palette = "jco",
    repel = TRUE,
    ggtheme = theme_minimal()
) +
    ggtitle("PCA Biplot: Perilaku & Demografi")

# department
# Ambil semua nama variabel yang mulai dengan "Department"
dept_vars <- rownames(pca_res$rotation)[grepl("^Department", rownames(pca_res$rotation))]

fviz_pca_biplot(
    pca_res,
    label = "var",
    select.var = list(name = dept_vars),
    geom = "point",
    habillage = as.factor(kmeans_result$cluster),
    addEllipses = TRUE,
    palette = "jco",
    repel = TRUE,
    ggtheme = theme_minimal()
) +
    ggtitle("PCA Biplot: Variabel Departemen")
# Silhouette analysis
sil <- silhouette(kmeans_result$cluster, dist(df_scaled))
summary(sil)
fviz_silhouette(sil)

# Gap Statistic untuk jumlah cluster optimal
set.seed(6969)
gap_stat <- clusGap(
    df_scaled,
    FUN = kmeans,
    nstart = 25,
    K.max = 10,
    B = 50
)
fviz_gap_stat(gap_stat)

# Korelasi antar fitur
num_features <- df_ready_full %>% select(where(is.numeric))
cor_mat <- cor(num_features, use = "complete.obs")
corrplot(cor_mat, method = "color", tl.cex = 0.7)

# Variansi fitur
apply(num_features, 2, var)

# PCA analisis fitur
pca_res <- prcomp(num_features, scale. = TRUE)
summary(pca_res)

# Visualisasi PCA biplot fitur
fviz_pca_biplot(
    pca_res,
    label = "var",
    # hanya label variabel yang muncul
    geom = "point",
    col.ind = "orange",
    col.var = "steelblue",
    title = "PCA Biplot Label Variabel"
)

# Kontribusi fitur pada PC1 dan PC2
fviz_contrib(pca_res,
             choice = "var",
             axes = 1,
             top = 10)  # PC1
fviz_contrib(pca_res,
             choice = "var",
             axes = 2,
             top = 10)  # PC2


# ========== PCA Biplot Manual dengan ggplot2 ==========

# Siapkan data: skor PCA (individu) dan loading (variabel)
library(ggplot2)
library(dplyr)

# PCA sudah dilakukan sebelumnya: pca_res <- prcomp(num_features, scale. = TRUE)

# Skor individu (obs)
scores <- as.data.frame(pca_res$x[, 1:2])
scores$cluster <- as.factor(kmeans_result$cluster)

# Loadings (panah variabel)
loadings <- as.data.frame(pca_res$rotation[, 1:2])
loadings$varname <- rownames(loadings)

# Skala panjang panah biar sebanding
arrow_scale <- 3

# Plot manual dengan ggplot2
ggplot(scores, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.6, size = 2) +
    
    # Tambahkan panah variabel
    geom_segment(
        data = loadings,
        aes(
            x = 0,
            y = 0,
            xend = PC1 * arrow_scale,
            yend = PC2 * arrow_scale,
            color = varname
        ),
        arrow = arrow(length = unit(0.2, "cm")),
        inherit.aes = FALSE
    ) +
    
    # Tambahkan titik akhir panah supaya bisa masuk legenda
    geom_point(
        data = loadings,
        aes(
            x = PC1 * arrow_scale,
            y = PC2 * arrow_scale,
            color = varname
        ),
        size = 3,
        inherit.aes = FALSE
    ) +
    
    # Tata letak dan tema
    theme_minimal() +
    labs(
        title = "PCA Biplot with Variable Arrows in Legend",
        x = "PC1",
        y = "PC2",
        color = "Cluster / Variable"
    ) +
    theme(legend.position = "right")
