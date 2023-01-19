#sampling_variability = function(sample_size, num_samples) {
#  
#  means = replicate(num_samples, mean(sample(iris$Petal.Length, sample_size)))
#  
#  print("Mean of Distribution")
#  print(round(mean(means), 2))
#  print("SD of Distribution")
#  print(round(sd(means), 2))
#  
#  means %>%
#    data.frame(x = .) %>%
#    ggplot(aes(x = x)) +
#    geom_histogram(color = "black",
#                   fill = "grey",
#                   binwidth = 0.5) +
#    geom_vline(aes(xintercept = mean(iris$Petal.Length)), color = "blue",
#               size = 1.75, linetype = "dashed") +
#    labs(title = "Distribution of Sample Means",
#         x = "Mean Score",
#         y = "Number of Samples")
#}

sampling_variability = function(sample_size, num_samples, data){
  sample_means = replicate(num_samples, mean(sample(na.omit(data), sample_size)))
  
  print(paste0("The mean of the ", num_samples, " samples of size ", sample_size, " was:"))
  print(mean(sample_means))
  
  print(paste0("The sd of the ", num_samples, " samples of size ", sample_size, " was:"))
  print(sd(sample_means))
  
  data.frame(x = sample_means) %>%
    ggplot(aes(x = x)) +
    geom_histogram(color = "black",
                   fill = "grey") +
    geom_vline(xintercept = mean(data, na.rm = TRUE), linetype = "dashed", color = "blue", size = 1.5) +
    labs(y = "Frequency",
         x = "Mean")
  
}
