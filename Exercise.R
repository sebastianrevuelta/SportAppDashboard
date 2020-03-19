##exercises

dfTrainings %>%
    spread(key = "Sport", value = "Distance")

colnames(dfTrainings)
