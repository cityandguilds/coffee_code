
summary_stats <- function(df){
  df |> reframe(n = n(),
                mean = mean(raw_mark),
                median = median(raw_mark),
                sd = sd(raw_mark),
                skw = e1071::skewness(raw_mark),
                kur = e1071::kurtosis(raw_mark))
}
