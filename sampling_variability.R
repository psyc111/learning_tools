sampling_variability = function(sample_size, num_samples) {
  
  means = replicate(num_samples, mean(sample(iris$Petal.Length, sample_size)))
  
  print("Mean of Distribution")
  print(round(mean(means), 2))
  print("SD of Distribution")
  print(round(sd(means), 2))
  
  means %>%
    data.frame(x = .) %>%
    ggplot(aes(x = x)) +
    geom_histogram(color = "black",
                   fill = "grey",
                   binwidth = 0.5) +
    geom_vline(aes(xintercept = mean(iris$Petal.Length)), color = "blue",
               size = 1.75, linetype = "dashed") +
    labs(title = "Distribution of Sample Means",
         x = "Mean Score",
         y = "Number of Samples")
}