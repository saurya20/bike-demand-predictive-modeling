bike = read.csv("data/bikeshare.csv")
head(bike)

# trying to predict count of the bike required in the hour
library(ggplot2)

pf = ggplot(bike, aes(x = temp, y = count)) +
  geom_point(alpha = 0.5)

datetime = as.POSIXct(bike$datetime)

sf = ggplot(bike, aes(x = datetime, y = count, color = temp)) +
  geom_point()

cor(bike[, c("temp", "count")])

bp = ggplot(bike, aes(x = factor(season), y = count, color = factor(season))) +
  geom_boxplot()

bike$datetime <- as.POSIXct(bike$datetime)
bike$hour <- as.factor(format(bike$datetime, "%H"))

head(bike)

bb = bike[bike$workingday == 1, ]

wd = ggplot(bb, aes(x = hour, y = count, color = temp)) +
  geom_point(alpha = 0.6) +
  scale_color_gradientn(
    colors = c("blue", "green", "yellow", "orange", "red")
  )

nb = bike[bike$workingday == 0, ]

nwd = ggplot(nb, aes(x = hour, y = count, color = temp)) +
  geom_point(alpha = 0.6) +
  scale_color_gradientn(
    colors = c("blue", "green", "yellow", "orange", "red")
  )

temp.model = lm(count ~ temp, bike)
summary(temp.model)

# if temp is 25C
count = 6.0462 + (9.1705 * 25)
count

newdata = data.frame(temp = 25)
predict(temp.model, newdata)

bike$hour = sapply(bike$hour, as.numeric)

model = lm(count ~ . - casual - registered - datetime - atemp, bike)
summary(model)