

df_list <- list(output.sum.deg_10, output.sum.deg_20, output.sum.deg_30, output.sum.deg_40, output.sum.deg_50,
                output.sum.deg_60, output.sum.deg_70, output.sum.deg_80, output.sum.deg_90, output.sum.deg_100,
                output.sum.deg_150, output.sum.deg_200, output.sum.deg_300, output.sum.deg_400, output.sum.deg_500,
                output.sum.deg_600, output.sum.deg_700, output.sum.deg_800, output.sum.deg_900, output.sum.deg_1000)

# Loop through each dataframe in the list and save it as a CSV file
for (i in seq_along(df_list)) {
  write.csv(df_list[[i]], file = paste0("results_iteration", i, ".csv"), row.names = FALSE)
}

a10 <- sum(output.sum.deg_10$Test != "Not Significant")
a20 <- sum(output.sum.deg_20$Test != "Not Significant")
a30 <- sum(output.sum.deg_30$Test != "Not Significant")
a40 <- sum(output.sum.deg_40$Test != "Not Significant")
a50 <- sum(output.sum.deg_50$Test != "Not Significant")
a60 <- sum(output.sum.deg_60$Test != "Not Significant")
a70 <- sum(output.sum.deg_70$Test != "Not Significant")
a80 <- sum(output.sum.deg_80$Test != "Not Significant")
a90 <- sum(output.sum.deg_90$Test != "Not Significant")
a100 <- sum(output.sum.deg_100$Test != "Not Significant")
a150 <- sum(output.sum.deg_150$Test != "Not Significant")
a200 <- sum(output.sum.deg_200$Test != "Not Significant")
a300 <- sum(output.sum.deg_300$Test != "Not Significant")
a400 <- sum(output.sum.deg_400$Test != "Not Significant")
a500 <- sum(output.sum.deg_500$Test != "Not Significant")
a600 <- sum(output.sum.deg_600$Test != "Not Significant")
a700 <- sum(output.sum.deg_700$Test != "Not Significant")
a800 <- sum(output.sum.deg_800$Test != "Not Significant")
a900 <- sum(output.sum.deg_900$Test != "Not Significant")
a1000 <- sum(output.sum.deg_1000$Test != "Not Significant")

vec_list <- list(`10 iterations` = a10,
                 `20 iterations` = a20,
                 `30 iterations` = a30,
                 `40 iterations` = a40,
                 `50 iterations` = a50,
                 `60 iterations` = a60,
                 `70 iterations` = a70,
                 `80 iterations` = a80,
                 `90 iterations` = a90,
                 `100 iterations` = a100,
                 `150 iterations` = a150,
                 `200 iterations` = a200,
                 `300 iterations` = a300,
                 `400 iterations` = a400,
                 `500 iterations` = a500,
                 `600 iterations` = a600,
                 `700 iterations` = a700,
                 `800 iterations` = a800,
                 `900 iterations` = a900,
                 `1000 iterations` = a1000)

# Create a data frame with two columns
results_test <- data.frame(name = character(), value = numeric(), stringsAsFactors = FALSE)
results_test$name <- as.factor(results_test$name)

# Loop through each vector in the list and add its name and values to the data frame
for (name in names(vec_list)) {
  value <- vec_list[[name]]
  results_test <- rbind(results_test, data.frame(name = name, value = paste(value, collapse = ",")))
}

ggplot(results_test, aes(x=factor(name, 
  levels = c("10 iterations", "20 iterations", "30 iterations", "40 iterations", "50 iterations",
  "60 iterations", "70 iterations", "80 iterations", "90 iterations", "100 iterations",
  "150 iterations", "200 iterations", "300 iterations", "400 iterations", "500 iterations",
  "600 iterations", "700 iterations", "800 iterations", "900 iterations", "1000 iterations")), y=as.numeric(value))) +
  geom_line(aes(group=1))+
  geom_point() +
  ylim(20,40) +
  xlab(NULL) + 
  ylab("Significant relationships") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

