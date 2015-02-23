

hw <- HoltWinters(ldeaths)
p <- predict(hw, n.ahead = 36, prediction.interval = TRUE)
all <- cbind(ldeaths, p)

dygraph(all, "Deaths from Lung Disease (UK)") %>%
  dySeries("ldeaths", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")


