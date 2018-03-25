{
{
source('C:/R/Motion model/header.R')
library('data.table')
# X,Y - размер сетки
X <- 500
Y <- 500
# выбор типа модели: , 'random walk model with SIR and specified input'
# 'random walk model with SIR and without specified input', 'random walk model'
# 'random walk model with SIR and obj'
model = 'random walk model with SIR and obj'
# n - количество реализуемых абонентов
n <- 4
data <- create_objects(n, X, Y, model)

plot.new()
plot(x = data$A_x, y = data$A_y, xlim = c(0,X), ylim = c(0,Y), col = 1:n, pch = 19)
points(x = data$B_x, y = data$B_y, col = 1:n, pch = 19)

# созадние объектов-препятствий
# задаются все точки в порядке очередности для каждого из препятствий
# u - количество точек для создаваемого объекта
u <- 4
barrier_point <- data.frame('x' = array(NA, u + 1),'y' = array(NA, u + 1))
barrier_point$x[u+1] <- barrier_point$x[1]
barrier_point$y[u+1] <- barrier_point$y[1]

# plot obj

lines(barrier_point, type = 's')
# создание препятствий
# data_barrier <- create_barrier(X,Y,model,barrier_point, ...)
# прям очень важные фреймы, где tmp.env - координаты для новой точки A_x в t-ой реализации
# tmp_B_x - промежуточный фрейм, использующийся для перезаписи B_x
# temp_mass - фрейм для записи предыдущих A_x для графика
tmp.env <- data.table("A_x" = data$A_x,"A_y" = data$A_y) 
# массивы для записи в table
# удалил: temp_mass_A_x <- array(NA, n)
temp_mass <- data.table("A_x" = array(NA, n), "A_y" = array(NA, n))
tmp_B_x <- data.table("B_x" = NA, "B_y" = NA)
# количество шагов
T <- 800
# скорость шага: задается с помощью вероятности
obj_speed <- array(NA, n)
for (i in 1:n)
{
  obj_speed[i] <- sample(1:3, size = 1, prob = c(0.8,0.15,0.05))
}
# Дистанция между интерферирующими объектами
Int <- array(NA, T)
# Усредняемая
M_int <- array(0, n-2)
# Дистанция между 1 и 2 объектами
Dist <- array(NA, T)
# дистанция между интерферирующими объектами iм и 1м
Dist_int <- array(NA, n-2)
# SIR массив
SIR <- array(NA, T)
# счетчик
t <- 1
k <- 0
}
# цикл для совершения шагов, пока не достигнута T
{
  while (t < T)
  {
    for (i in 1:n)
    {
      # вероятность уйти не туда, которая в данный момент не используется
      prob <- runif(1,0,1)
      if (t == 1)
      {
        # при t = 1 реализация для A(x,y) и B(x,y)
        temp_mass$A_x[i] <- data$A_x[i]
        temp_mass$A_y[i] <- data$A_y[i]
        set_angle <- edit_angle(data$A_x[i],data$A_y[i],data$B_x[i],data$B_y[i])
        tmp.env[i] <- set_direction(prob, set_angle, tmp.env, data$A_x[i], data$A_y[i], 
                                    data$B_x[i], data$B_y[i], X, Y, obj_speed[i])
        segments(x0 = temp_mass$A_x[i], y0 = temp_mass$A_y[i], x1 = tmp.env$A_x[i], 
                 y1 = tmp.env$A_y[i], col = i)
      }
      else
      {
        # при t > 1 реализация для tmp.env(x,y) и B(x,y)
        temp_mass$A_x[i] <- tmp.env$A_x[i]
        temp_mass$A_y[i] <- tmp.env$A_y[i]
        set_angle <- edit_angle(tmp.env$A_x[i],tmp.env$A_y[i],data$B_x[i],data$B_y[i])
        tmp.env[i] <- set_direction(prob, set_angle, tmp.env, tmp.env$A_x[i],
                                    tmp.env$A_y[i], data$B_x[i], data$B_y[i], X, Y, obj_speed[i])
        segments(x0 = temp_mass$A_x[i], y0 = temp_mass$A_y[i], x1 = tmp.env$A_x[i], 
                 y1 = tmp.env$A_y[i], col = i)
      }
      if ( ( abs(data$B_x[i] - tmp.env$A_x[i]) < 10) && ( abs(data$B_y[i] - tmp.env$A_y[i]) < 10) )
      {
        # segments(x0 = tmp.env$A_x[i], y0 = tmp.env$A_y[i], x1 = data$B_x[i], 
        #          y1 = data$B_y[i], col = i)
        tmp.env$A_x[i] <- data$B_x[i]
        tmp.env$A_y[i] <- data$B_y[i]
        tmp_B_x <- new_endl_object(X, Y, tmp.env$A_x[i], tmp.env$A_y[i])
        data$B_x[i] <- tmp_B_x$B_x
        data$B_y[i] <- tmp_B_x$B_y
      }
    }
    # поиск интерферируюшего сигнала
    if ( (model == 'random walk model with SIR and without specified input')
      || (model == 'random walk model with SIR and specified input') 
      || (model == 'random walk model with SIR and obj') )
    {
      # Интерференция
      for (i in 1:n-2)
      {
        M_int[i] <- (((temp_mass$A_x[1]-tmp.env$A_x[i+2])^2 +
                       (temp_mass$A_y[1]-tmp.env$A_y[i+2])^2)^(1/2))^(-3)
        Dist_int[i] <- (((temp_mass$A_x[1]-tmp.env$A_x[i+2])^2 +
                        (temp_mass$A_y[1]-tmp.env$A_y[i+2])^2)^(1/2))
      }
      # усредненное расстояние между интерферирующими объектами
      Int[t] <- sum(Dist_int)/(n-2)
      # дистанция между 1 и 2м объектами
      Dist[t] <- ( (temp_mass$A_x[1]-tmp.env$A_x[2])^2 +
                     (temp_mass$A_y[1]-tmp.env$A_y[2])^2 )^(1/2)
      SIR[t] <- Dist[t]^(-2)/sum(M_int)
    }
    t <- t + 1
  }
}

} 
# конец скрипта
# new_data <- SIR[which(SIR<100)]
# hist(new_data, main = 'Hist of SIR')
# hist(SIR, main = 'Hist of SIR', xlab = 'SIR', breaks = c(0, seq(10,100,10), 101:which.max(SIR)))
# plot(Dist, type = 'l', ylim = c(0,400), xlab = 'Step', ylab = 'Dist', col = 'black', lwd = 1.9)
# lines(Int, type = 'l', col = 'black', lty = 4, lwd = 2)
# legend('topleft', legend = c('Distance between first and second', 'Mean distance between other'),
#        lwd = c(1.9, 2), lty = c(1,4))
# 
# plot(x = c(10,20), y = c(30,50), xlim = c(0,35), ylim = c(0,70), type = 'l')
