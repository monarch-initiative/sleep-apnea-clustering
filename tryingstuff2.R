
df.demo<-datadict %>%
  filter(str_detect(folder, "Demographics"))
terms_demo<-pullTerms(df.demo, shhs1.pruned)
shhs1.demo<-subsetCols(shhs1.pruned, terms_demo)

df.anthro<-datadict %>%
  filter(str_detect(folder, "Anthropometry"))
terms_anthro<-pullTerms(df.anthro, shhs1.pruned)
terms_anthro<-c("obf_pptid", terms_anthro)
shhs1.anthro<-subsetCols(shhs1.pruned, terms_anthro)

shhs1.demo.mut<- shhs1.demo %>% 
  select(-pptid, -age_category_s1, -ethnicity) %>%
  mutate(educat=ordered(educat),
         gender=factor(gender), mstat=factor(mstat),
         race=factor(race))
#age_category_s1=ordered(age_category_s1), ethnicity=factor(ethnicity)

# Remove college name before clustering
# A tibble: 5,804 × 7
#age_category_s1 age_s1 educat ethnicity gender mstat  race
#<int>  <int>  <int>     <int>  <int> <int> <int>
#  1                7     55      3         2      1     1     1
#  2                9     78      2         2      1     1     1
#  3                9     77      3         2      2     3     1
#              ordinal   num    ordinal factor  factor factor factor

gower_dist <- daisy(shhs1.demo.mut[, -5],
                    metric = "gower")
euc_dist <- daisy(shhs1.anthro[, -1],
                    metric = "euclidean")

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair

shhs1.demo[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

shhs1.anthro[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair

shhs1.demo[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

shhs1.anthro[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:9){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

for(i in 3:9){
  
  km_fit <- kmeans(gower_dist,
                 centers=i)
  
  
  sil<-silhouette(km_fit$cluster, gower_dist)
  si.sum<-summary(sil)
  sil_width[i]<-si.sum$avg.width
}

sil<-silhouette(km_fit$cluster, gower_dist)
si.sum <- summary(sil)
# Average silhouette width of each cluster
si.sum$clus.avg.widths
# The total average (mean of all individual silhouette widths)
si.sum$avg.width

plot(1:9, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:9, sil_width)

pam_fit <- pam(gower_dist, diss = TRUE, k = 4)

km_fit <- kmeans(gower_dist, centers=4)
pam_results <- shhs1.demo %>%
  dplyr::select(-obf_pptid, -pptid) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

km_results <- shhs1.demo %>%
  dplyr::select(-obf_pptid, -pptid) %>%
  mutate(cluster = km_fit$cluster) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
km_results$the_summary

shhs1.demo[pam_fit$medoids, ]
shhs1.demo[km_fit$centers, ]

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = shhs1.demo$obf_pptid)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(km_fit$cluster),
         name = shhs1.demo$obf_pptid)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))