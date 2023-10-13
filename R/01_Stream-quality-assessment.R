###
# Stream survey evaluation
###
library(dplyr)
library(ggplot2)

evals <- read.csv('./data/stream_quality.csv')

# Whats the total Stream score?
(total_stream_score <- evals |> 
    group_by(Site) |> 
    summarize(score = sum(Score)))
