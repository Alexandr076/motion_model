{
{
source('C:/R/Motion model/header.R')
library('data.table')
# X,Y - ������ �����
X <- 500
Y <- 500
# ����� ���� ������: , 'random walk model with SIR and specified input'
# 'random walk model with SIR and without specified input', 'random walk model'
# 'random walk model with SIR and obj'
model = 'random walk model with SIR and obj'
# n - ���������� ����������� ���������
n <- 4
data <- create_objects(n, X, Y, model)

plot.new()
plot(x = data$A_x, y = data$A_y, xlim = c(0,X), ylim = c(0,Y), col = 1:n, pch = 19)
points(x = data$B_x, y = data$B_y, col = 1:n, pch = 19)

# �������� ��������-�����������
# �������� ��� ����� � ������� ����������� ��� ������� �� �����������
# u - ���������� ����� ��� ������������ �������
u <- 4
barrier_point <- data.frame('x' = array(NA, u + 1),'y' = array(NA, u + 1))
barrier_point$x[u+1] <- barrier_point$x[1]
barrier_point$y[u+1] <- barrier_point$y[1]

# plot obj

lines(barrier_point, type = 's')
# �������� �����������
# data_barrier <- create_barrier(X,Y,model,barrier_point, ...)
# ���� ����� ������ ������, ��� tmp.env - ���������� ��� ����� ����� A_x � t-�� ����������
# tmp_B_x - ������������� �����, �������������� ��� ���������� B_x
# temp_mass - ����� ��� ������ ���������� A_x ��� �������
tmp.env <- data.table("A_x" = data$A_x,"A_y" = data$A_y) 
# ������� ��� ������ � table
# ������: temp_mass_A_x <- array(NA, n)
temp_mass <- data.table("A_x" = array(NA, n), "A_y" = array(NA, n))
tmp_B_x <- data.table("B_x" = NA, "B_y" = NA)
# ���������� �����
T <- 800
# �������� ����: �������� � ������� �����������
obj_speed <- array(NA, n)
for (i in 1:n)
{
  obj_speed[i] <- sample(1:3, size = 1, prob = c(0.8,0.15,0.05))
}
# ��������� ����� ���������������� ���������
Int <- array(NA, T)
# �����������
M_int <- array(0, n-2)
# ��������� ����� 1 � 2 ���������
Dist <- array(NA, T)
# ��������� ����� ���������������� ��������� i� � 1�
Dist_int <- array(NA, n-2)
# SIR ������
SIR <- array(NA, T)
# �������
t <- 1
k <- 0
}
# ���� ��� ���������� �����, ���� �� ���������� T
{
  while (t < T)
  {
    for (i in 1:n)
    {
      # ����������� ���� �� ����, ������� � ������ ������ �� ������������
      prob <- runif(1,0,1)
      if (t == 1)
      {
        # ��� t = 1 ���������� ��� A(x,y) � B(x,y)
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
        # ��� t > 1 ���������� ��� tmp.env(x,y) � B(x,y)
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
    # ����� ���������������� �������
    if ( (model == 'random walk model with SIR and without specified input')
      || (model == 'random walk model with SIR and specified input') 
      || (model == 'random walk model with SIR and obj') )
    {
      # �������������
      for (i in 1:n-2)
      {
        M_int[i] <- (((temp_mass$A_x[1]-tmp.env$A_x[i+2])^2 +
                       (temp_mass$A_y[1]-tmp.env$A_y[i+2])^2)^(1/2))^(-3)
        Dist_int[i] <- (((temp_mass$A_x[1]-tmp.env$A_x[i+2])^2 +
                        (temp_mass$A_y[1]-tmp.env$A_y[i+2])^2)^(1/2))
      }
      # ����������� ���������� ����� ���������������� ���������
      Int[t] <- sum(Dist_int)/(n-2)
      # ��������� ����� 1 � 2� ���������
      Dist[t] <- ( (temp_mass$A_x[1]-tmp.env$A_x[2])^2 +
                     (temp_mass$A_y[1]-tmp.env$A_y[2])^2 )^(1/2)
      SIR[t] <- Dist[t]^(-2)/sum(M_int)
    }
    t <- t + 1
  }
}

} 
# ����� �������
# new_data <- SIR[which(SIR<100)]
# hist(new_data, main = 'Hist of SIR')
# hist(SIR, main = 'Hist of SIR', xlab = 'SIR', breaks = c(0, seq(10,100,10), 101:which.max(SIR)))
# plot(Dist, type = 'l', ylim = c(0,400), xlab = 'Step', ylab = 'Dist', col = 'black', lwd = 1.9)
# lines(Int, type = 'l', col = 'black', lty = 4, lwd = 2)
# legend('topleft', legend = c('Distance between first and second', 'Mean distance between other'),
#        lwd = c(1.9, 2), lty = c(1,4))
# 
# plot(x = c(10,20), y = c(30,50), xlim = c(0,35), ylim = c(0,70), type = 'l')
