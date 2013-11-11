blend_out <- "Submissions/blend_impute_knn.csv"
imp_in <- "Submissions/softimpute_blend.csv"
knn_in <- "Submissions/knn_blend.csv"

imp <- as.matrix(read.csv(imp_in, header = TRUE))
knn <- as.matrix(read.csv(knn_in, header = TRUE))

blend <- (1/2)*(imp + knn)

write.csv(blend, file = blend_out, quote = FALSE, row.names = FALSE)
