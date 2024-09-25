library("dplyr")
library("geometry")
library("matlib")
library("ggplot2")


# Washington, DC

data <- read.csv("Washington-Q_S.csv")
spearman <- cor(data$Discharge, data$Surge, method = "spearman")
pearson <- cor(data$Discharge, data$Surge, method = "pearson")

q <- data$Discharge
s <- data$Surge

q <- q - mean(q)
s <- s - mean(s)

q.length <- sqrt(sum(q ** 2))
s.length <- sqrt(sum(s ** 2))

cos.theta <- q %*% s / (q.length * s.length)

round(cos.theta, 4) == round(pearson, 4)

theta.angle <- acos(cos.theta)


# Houston, TX

data <- read.csv("Bivariate-Sampling-Q_SWL.csv")

data <- data %>% select(c(2, 4))
names(data) <- c("X", "Y")

cor(data$X, data$Y, method = "kendall")

cor(data$X[5:39], data$Y[5:39])

cor(data$X[27:44], data$Y[27:44])

cor(data$X[45:70], data$Y[45:70])

data$GROUP <-
  c(rep("1946-1971", 23),
    rep("1972-1996", 23),
    rep("1997-2022", 24))

data %>% ggplot(aes(x = X, y = Y)) +
  geom_point(
    aes(fill = GROUP),
    shape = 21,
    color = "black",
    size = 1.5
  ) + theme_bw() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.margin = margin(t = 0, unit = 'cm')
  ) +
  scale_fill_manual(
    values = c(
      "1946-1971" = "purple",
      "1972-1996" = "green",
      "1997-2022" = "red"
    ),
    name = ""
  ) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 375)) +
  labs(y = "Still sea level (m)", x = expression(paste("Discharge (", m ^
                                                         3 / s, ")")))

###
c1 <- cor(data$X[5:39], data$Y[5:39])

c2 <- cor(data$X[10:44], data$Y[10:44])

c3 <- cor(data$X[27:61], data$Y[27:61])

c4 <- cor(data$X[35:69], data$Y[35:69])

theta1 <- acos(c1) * 180 / pi
theta2 <- acos(c2) * 180 / pi
theta3 <- acos(c3) * 180 / pi
theta4 <- acos(c4) * 180 / pi

x <- c(2, 0)
y1 <- c1 * 4 / 2
y2 <- sqrt(4 - y1 ** 2)
y <- c(y1, y2)

xlim <- c(0, 11)
ylim <- c(0, 2)

plot(
  xlim,
  ylim,
  type = "n",
  asp = 1,
  xaxt = "n",
  yaxt = "n",
  xlab = '',
  ylab = ''
)
vectors(
  matrix(c(x, y), nrow = 2, byrow = TRUE),
  col = "red",
  cex.lab = 0.75,
  labels = c(expression(bar(q)), expression(bar(s))),
  lwd = 1.25
)
arc(c(1, 0),
    c(0, 0),
    y,
    d = 0.66,
    col = "red",
    lwd = 1.25)

x <- c(5, 0)
y1 <- c2 * 4 / 2
y2 <- sqrt(4 - y1 ** 2)
y <- c(y1 + 3, y2)

vectors(
  matrix(c(x, y), nrow = 2, byrow = TRUE),
  col = "purple",
  cex.lab = 0.75,
  labels = c(expression(bar(q)), expression(bar(s))),
  lwd = 1.25,
  origin =  c(3, 0)
)
arc(c(5, 0),
    c(3, 0),
    y,
    d = 0.50,
    col = "purple",
    lwd = 1.25)
y1 <- c3 * 4 / 2
y2 <- sqrt(4 - y1 ** 2)
y <- c(y1 + 6, y2)
x <- c(8, 0)
vectors(
  matrix(c(x, y), nrow = 2, byrow = TRUE),
  col = "purple" ,
  labels = c(expression(bar(q)), expression(bar(s))),
  cex.lab = 0.75,
  lwd = 1.25,
  origin = c(6, 0)
)
arc(c(8, 0),
    c(6, 0),
    y,
    d = 0.45,
    col = "purple",
    lwd = 1.25)

y1 <- c4 * 4 / 2
y2 <- sqrt(4 - y1 ** 2)
y <- c(y1 + 9, y2)
x <- c(11, 0)

vectors(
  matrix(c(x, y), nrow = 2, byrow = TRUE),
  col = "red" ,
  labels = c(expression(bar(q)), expression(bar(s))),
  cex.lab = 0.75,
  lwd = 1.25,
  origin = c(9, 0)
)
arc(c(11, 0),
    c(9, 0),
    y,
    d = 0.33,
    col = "red",
    lwd = 1.25)


text(
  x = 1.05,
  y = -0.20,
  label = "1950-1991",
  col = "black",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
text(
  x = 0.60,
  y = 0.50,
  label = expression(theta),
  col = "red",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
text(
  x = 4.05,
  y = -0.20,
  label = "1955-1994",
  col = "black",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
text(
  x = 3.50,
  y = 0.45,
  label = expression(theta),
  col = "purple",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
text(
  x = 7.05,
  y = -0.20,
  label = "1972-2013",
  col = "black",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
text(
  x = 6.50,
  y = 0.45,
  label = expression(theta),
  col = "purple",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
text(
  x = 10.05,
  y = -0.20,
  label = "1982-2021",
  col = "black",
  # Color of the text
  font = 2,
  cex = 0.7
)
text(
  x = 9.45,
  y = 0.35,
  label = expression(theta),
  col = "red",
  # Color of the text
  font = 2,
  # Bold face
  cex = 0.70
)
