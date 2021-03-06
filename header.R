# �������� �������� �������� � �����, �� ������� �������� ��������������
# �� ����: ���������� ��������, ����������� ��������� X*Y. �� ������ ������ � �������
create_objects <- function(n, X, Y, model)
{
  A_x <- array(NA, n)
  A_y <- array(NA, n)
  B_x <- array(NA, n)
  B_y <- array(NA, n)
  mass <- list("A_x" = A_x,"A_y" = A_y,"B_x" = B_x,"B_y" = B_y)
  if (model == 'random walk model with SIR and specified input')
  {
    mass$A_x <- sample(0:(X/10), size = n) # x0
    mass$A_y <- sample(0:(Y/10), size = n) # y0
  }
  else
  {
    mass$A_x <- sample((X/100):(X-X/100), size = n) # x0
    mass$A_y <- sample((Y/100):(Y-Y/100), size = n) # y0
  }
  mass$B_x <- sample((X/100):(X-X/100), size = n) # x1
  mass$B_y <- sample((Y/100):(Y-Y/100), size = n) # y1
  for (i in 1:n)
  {
    while ( ((((mass$A_x[i]-mass$B_x[i])^2)+((mass$A_y[i]-mass$B_y[i])^2))^(1/2)) < X/5 )
    {
      mass$B_x[i] <- sample((X/100):(X-X/100), size = 1)
      mass$B_y[i] <- sample((Y/100):(Y-Y/100), size = 1)
    }
  }
  return(mass)
}
# create_barrier <- function(X, Y, model, brr_points)
# {
#   
# }
# ����� ������ B. ������� ������: ����������� ����� X*Y, ���������� A(x,y)
new_endl_object <- function(X, Y, A_x, A_y)
{
  mass <- list("B_x" = sample((X/100):(X-X/100), size = 1), "B_y" = sample((Y/100):(Y-Y/100), size = 1))
  while ( ((((A_x-mass$B_x)^2)+((A_y-mass$B_y)^2))^(1/2)) < X/5 )
  {
    mass$B_x <- sample((X/100):(X-X/100), size = 1)
    mass$B_y <- sample((Y/100):(Y-Y/100), size = 1)
  }
  return(mass)
}

# ���������� ���������� ��������� � �������� �����, ����������� ����. �� ������
# ����� ���� � ������������������ �����, ���� ���������
edit_angle <- function(A_x, A_y, B_x, B_y)
{
  angle <- NA
  part <- NA
  tmp_angle <- list("angle" = angle, "part" = part)
  tmp_angle$angle <- NA
  tmp_angle$part <- NA
  # 1
  if ( (A_x < B_x) && (A_y < B_y) )
  {
    tmp_angle$angle <- atan((B_y-A_y)/(B_x-A_x))*180/pi
    tmp_angle$part <- 1
  }
  if ( (A_x == B_x) && (A_y < B_y) )
  {
    tmp_angle$angle <- 90
    tmp_angle$part <- 1
  }
  if ( (A_y == B_y) && (A_x < B_x) )
  {
    tmp_angle$angle <- 0
    tmp_angle$part <- 1
  }
  # 2
  if ((A_x > B_x) && (A_y < B_y))
  {
    tmp_angle$angle <- atan((B_y-A_y)/(A_x-B_x))*180/pi;
    tmp_angle$part <- 2
  }
  if ( (A_y == B_y) && (A_x > B_x))
  {
    tmp_angle$angle <- 0
    tmp_angle$part <- 2
  }
  # 3
  if ((A_x > B_x) && (A_y > B_y))
  {
    tmp_angle$angle <- atan((A_y-B_y)/(A_x-B_x))*180/pi;
    tmp_angle$part <- 3
  }
  if ( (A_x == B_x) && (A_y > B_y))
  {
    tmp_angle$angle <- 90
    tmp_angle$part <- 3
  }
  # 4
  if ((A_x < B_x) && (A_y > B_y))
  {
    tmp_angle$angle <- atan((A_y-B_y)/(B_x-A_x))*180/pi;
    tmp_angle$part <- 4
  }
  return(tmp_angle)
}  

# ������� ��� ������ ����������� ��������. �� ����: ����������� ����� � ���������� �����������, ������� 
# ���� � ����� ���������, ������������ ������� tmp, ����������, ������ ����� X*Y, �������� ��������
# �������� ������ ����� ��� ��������� �������� � ������������ ��� ������������� 
# ������� ��� �����������: set_direction <- function(p, tmp_angle, tmp, A_x, A_y, B_x, B_y, X, Y, speed)
set_direction <- function(p, tmp_angle, tmp, A_x, A_y, B_x, B_y, speed)
{
  if ( (p >= 0) && (p <= 1) )
  {
    # part 1
    if ( (tmp_angle$angle <= 22.5) && (tmp_angle$part == 1) )
    {
      # ������
      A_x <- A_x + speed
      # if (A_x > X)
      # {
      #   A_x <- A_x - 1
      # }
    }
    if ( ( (tmp_angle$angle > 22.5) && (tmp_angle$angle < 67.5) ) && (tmp_angle$part == 1) )
    {
      # ��������� 
      A_x <- A_x + speed
      A_y <- A_y + speed
      # if (A_x > X)
      # {
      #   A_x <- A_x - 1
      # }
      # if (A_y > Y)
      # {
      #   A_y <- A_y - 1
      # }
    }
    if ( (tmp_angle$angle >= 67.5) && (tmp_angle$part == 1) )
    {
      # �����
      A_y <- A_y + speed
      # if (A_y > Y)
      # {
      #   A_y <- A_y - 1
      # }
    }
    # part = 2
    if ( (tmp_angle$angle <= 22.5) && (tmp_angle$part == 2) )
    {
      # �����
      A_x <- A_x - speed
      # if (A_x < 0)
      # {
      #   A_x <- A_x + 1
      # }
    }
    if ( ( (tmp_angle$angle > 22.5) && (tmp_angle$angle < 67.5) ) && (tmp_angle$part == 2) )
    {
      # ��������� 
      A_x <- A_x - speed
      A_y <- A_y + speed
      # if (A_x < 0) 
      # {
      #   A_x <- A_x + 1
      # }
      # if (A_y > Y)
      # {
      #   A_y <- A_y - 1
      # }
    }
    if ( (tmp_angle$angle >= 67.5) && (tmp_angle$part == 2) )
    {
      # �����
      A_y <- A_y + speed
      # if (A_y > Y)
      # {
      #   A_y <- A_y - 1
      # }
    }
    # part = 3
    if ( (tmp_angle$angle <= 22.5) && (tmp_angle$part == 3) )
    {
      # �����
      A_x <- A_x - speed
      # if (A_x < 0) 
      # {
      #   A_x <- A_x + 1
      # }
    }
    if ( ( (tmp_angle$angle > 22.5) && (tmp_angle$angle < 67.5) ) && (tmp_angle$part == 3) )
    {
      # ���������
      A_x <- A_x - speed
      A_y <- A_y - speed
      # if (A_x < 0) 
      # {
      #   A_x <- A_x + 1
      # }
      # if (A_y < 0)
      # {
      #   A_y <- A_y + 1
      # }
    }
    if ( (tmp_angle$angle >= 67.5) && (tmp_angle$part == 3) )
    {
      # ����
      A_y <- A_y - speed
      # if (A_y < 0)
      # {
      #   A_y <- A_y + 1
      # }
    }
    # part = 4
    if ( (tmp_angle$angle <= 22.5) && (tmp_angle$part == 4) )
    {
      # ������
      A_x <- A_x + speed
      # if (A_x > X) 
      # {
      #   A_x <- A_x - 1
      # }
    }
    if ( ( (tmp_angle$angle > 22.5) || (tmp_angle$angle < 67.5) ) && (tmp_angle$part == 4) )
    {
      # ���������
      A_x <- A_x + speed
      A_y <- A_y - speed
      # if (A_x > X) 
      # {
      #   A_x <- A_x - 1
      # }
      # if (A_y < 0)
      # {
      #   A_y <- A_y + 1
      # }
    }
    if ( (tmp_angle$angle >= 67.5) && (tmp_angle$part == 4) )
    {
      # ����
      A_y <- A_y - speed
      # if (A_y < 0)
      # {
      #   A_y <- A_y + 1
      # }
    }
  }
  # �������� �����������
  # if ( p > 0.9 )
  # {
  #   # �������� ����� ��� part = 1
  #   if ( tmp_angle$part == 1 )
  #   {
  #     ins_p <- runif(1,0,1)
  #     if ( ins_p < 0.33 )
  #     {
  #       A_x <- A_x - 1
  #     }
  #     if ( (ins_p > 0.33) && (ins_p < 0.66) )
  #     {
  #       A_x <- A_x - 1
  #       A_y <- A_y - 1
  #     }
  #     if ( ins_p > 0.66 )
  #     {
  #       A_y <- A_y - 1
  #     }
  #   }
  #   # �������� ����� ��� part = 2
  #   if ( tmp_angle$part == 2 )
  #   {
  #     ins_p <- runif(1,0,1)
  #     if ( ins_p < 0.33 )
  #     {
  #       A_x <- A_x + 1
  #     }
  #     if ( (ins_p > 0.33) && (ins_p < 0.66) )
  #     {
  #       A_x <- A_x + 1
  #       A_y <- A_y - 1
  #     }
  #     if ( ins_p > 0.66 )
  #     {
  #       A_y <- A_y - 1
  #     }
  #   }
  #   # �������� ����� ��� part = 3
  #   if ( tmp_angle$part == 3 )
  #   {
  #     ins_p <- runif(1,0,1)
  #     if ( ins_p < 0.33 )
  #     {
  #       A_x <- A_x + 1
  #     }
  #     if ( (ins_p > 0.33) && (ins_p < 0.66) )
  #     {
  #       A_x <- A_x + 1
  #       A_y <- A_y + 1
  #     }
  #     if ( ins_p > 0.66 )
  #     {
  #       A_y <- A_y + 1
  #     }
  #   }
  #   # �������� ����� ��� part = 4
  #   if ( tmp_angle$part == 4 )
  #   {
  #     ins_p <- runif(1,0,1)
  #     if ( ins_p < 0.33 )
  #     {
  #       A_x <- A_x - 1
  #     }
  #     if ( (ins_p > 0.33) && (ins_p < 0.66) )
  #     {
  #       A_x <- A_x - 1
  #       A_y <- A_y + 1
  #     }
  #     if ( ins_p > 0.66 )
  #     {
  #       A_y <- A_y + 1
  #     }
  #   }
  # }
  # ��������� tmp <- list(A_x,A_y)
  tmp <- list("A_x" = A_x,"A_y" = A_y)
  return(tmp)
}


# 
# if ((prob > 0.93) && (prob <= 0.99))
# {
#   # ���������
# }
# 
# 
# if ((prob > 0.99))
# {
#   inside_prob <- runif(1,0,1)
#   if ((angle >= 0) && (angle <= 0.33))
#   {
#     # ����
#   }
#   if ((angle > 0.33) && (angle <= 66.6))
#   {
#     # ��������� �����
#   }
#   else 
#   {
#     # �����
#   }
# }
# 
# 

# best <- function(tmp)
# {
#   tmp$A_x[1] = tmp$A_x[1] + 100
#   return(tmp)
# }





