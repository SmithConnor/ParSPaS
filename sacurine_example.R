library("ropls")
utils::data("sacurine")

X = sacurine$dataMatrix
Y = sacurine$sampleMetadata$gender

colnames(X) = paste("X",1:109)


test2 = parspas(x = X,
                 y = Y,
                 B = 50,
                 c = 30,
                 family = "binomial")

test2 = metric_clust(test$metrics$avgRankVar)



#####

#Hclust plot

dendrogram_data <- test2
dendrogram_segments <- dendrogram_data$segments

inc = test$baseModel >= 0.5
baseModels = data.frame(sample_name = colnames(x), model = inc)

dendrogram_ends <- dendrogram_segments %>%
  filter(yend == 0) %>%
  left_join(dendrogram_data$labels, by = 'x') %>%
  rename(sample_name = label) %>%
  left_join(baseModels, by = 'sample_name')

p <- ggplot() +
  geom_segment(data = dendrogram_segments,
               aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_segment(data = dendrogram_ends,
               aes(x=x, y=y.x, xend=xend, yend=yend, color = model))

p


###


