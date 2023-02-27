# script to calculate the average *variable on colour scale* with regards to *variables on x- and y-axis*
# 2019-07-05
# by: Jasper Denissen

calc_boxes <- function(x_array, # array carrying the values on the x-axis
                       y_array, # array carrying the values on the y-axis
                       col_array, # array carrying the values on the color scale
                       func, # median, mean, standard deviation, IQR or MLR (for MLR, stack the response variable [,,1] and all the explanatory variables in one array --> col_array)
                       x_bounds, # from first to last tick on x-axis
                       y_bounds, # from first to last tick on y-axis
                       min_points, # the minimum amount of points that should be present within the box in order to calculate val_per_box
                       min_models # the minimum amount of models that should contribute results to the box in order to calculate val_per_box
  ){
  # if(func == "MLR"){
  #   print("MLR only with 5 explanatory variables or now. If this changes, go into the function code and adapt")
  # }
  val_per_box <- 
    most_sign_var_per_box <-
    points_per_box <- 
    dom_var_1 <-
    dom_var_2 <-
    sum_dom_var_1 <-
    sum_dom_var_2 <-
    array(NaN,c(length(x_bounds)-1,length(y_bounds)-1)) # because x_bounds and y_bounds contain the outer ticks, the boxes in between them is -1
  if(length(dim(x_array)) == length(dim(y_array)) & length(dim(x_array)) == length(dim(col_array))){ # check if the dimensions of the arguments are the same
    for(x in 1:(length(x_bounds)-1)){
      for(y in 1:(length(y_bounds)-1)){
        # calculate the number of points used (points_per_box) to calculate val_per_box
        points_per_box[x,y] <- length(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                        y_array > y_bounds[y] & y_array < y_bounds[y+1])])
        if(points_per_box[x,y] > min_points){
          count <- 0
          if(!is.na(dim(col_array)[3])){ # The third dimension of the input should be the models. If there is this third dimension, continue
            for(i in 1:dim(col_array)[3]){ # loop over all available models
              if(length(col_array[,,i][which(x_array[,,i] > x_bounds[x] & x_array[,,i] < x_bounds[x+1] &
                                                          y_array[,,i] > y_bounds[y] & y_array[,,i] < y_bounds[y+1])]) > 0){
                count <- count + 1
              }
            }
            if(count >= min_models){
              if(func == "median"){
                val_per_box[x,y] <- median(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                           y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
              }else if(func == "mean"){
                val_per_box[x,y] <- mean(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                         y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
              }else if(func == "sd"){
                val_per_box[x,y] <- sd(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                       y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
              # }else if(func == "dom_var"){
              #   dom_var_1[x,y] <- which(rank(-col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
              #                                                    y_array > y_bounds[y] & y_array < y_bounds[y+1])]) == 1)
              #   dom_var_2[x,y] <- which(rank(-col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
              #                                             y_array > y_bounds[y] & y_array < y_bounds[y+1])]) == 2)
              #   sum_dom_var_1[x,y] <- length(which(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] & y_array > y_bounds[y] & y_array < y_bounds[y+1])] == dom_var_1[x,y]))
              #   sum_dom_var_2[x,y] <- length(which(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] & y_array > y_bounds[y] & y_array < y_bounds[y+1])] == dom_var_2[x,y]))
              }else if(func == "IQR"){
                val_per_box[x,y] <- IQR(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                        y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
              }else{
                print("error, choose: func = 'median','mean', 'sd', 'dom_var' or 'IQR'")
              }
            }
          }else{
            if(func == "median"){
              val_per_box[x,y] <- median(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                         y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
            }else if(func == "mean"){
              val_per_box[x,y] <- mean(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                       y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
            }else if(func == "sd"){
              val_per_box[x,y] <- sd(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                     y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
            }else if(func == "dom_var"){
              count_var <- c()
              for(j in 1:5){
                count_var[j] <- length(which(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                               y_array > y_bounds[y] & y_array < y_bounds[y+1])] == j))
              }
              if(length(which(rank(-count_var, ties.method = 'min') == 1)) == 1){ # if variables are equally important, ranks will be the same and length(unique()) < 5
                dom_var_1[x,y] <- which(rank(-count_var) == 1)
                sum_dom_var_1[x,y] <- count_var[dom_var_1[x,y]]
              }else if(length(which(count_var == max(count_var))) == 2){ # if there is a tie of exactly two variables for the most important variable, just take both randomly for 1st and second
                dom_var_1[x,y] <- which(count_var == max(count_var))[1]
                dom_var_2[x,y] <- which(count_var == max(count_var))[2]
                sum_dom_var_1[x,y] <- sum_dom_var_2[x,y] <- count_var[which(count_var == max(count_var))[1]]
              }else if(length(which(count_var == max(count_var))) > 2){
                dom_var_1[x,y] <- dom_var_2[x,y] <- 6
                sum_dom_var_1[x,y] <- sum_dom_var_2[x,y] <- count_var[which(count_var == max(count_var))[1]]
              }
              if(length(which(rank(-count_var, ties.method = 'min') == 2)) == 1){
                dom_var_2[x,y] <- which(rank(-count_var, ties.method = 'min') == 2)
                sum_dom_var_2[x,y] <- count_var[dom_var_2[x,y]]
              }else if(length(which(rank(-count_var, ties.method = 'min') == 2)) > 1){
                dom_var_2[x,y] <- 6
                sum_dom_var_2[x,y] <- count_var[which(rank(-count_var, ties.method = 'min') == 2)[1]]
              }
            }else if(func == "IQR"){
              val_per_box[x,y] <- IQR(col_array[which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                      y_array > y_bounds[y] & y_array < y_bounds[y+1])], na.rm = T)
            }else{
              print("error, choose: func = 'median','mean', 'sd' or 'IQR'")
            }
          }
        }
      }
    }
    if(func != "dom_var"){
      newList <- list("val_per_box" = val_per_box,
                      "points_per_box" = points_per_box)
    }else{
      newList <- list("dom_var_1" = dom_var_1,
                      "dom_var_2" = dom_var_2,
                      "sum_dom_var_1" = sum_dom_var_1,
                      "sum_dom_var_2" = sum_dom_var_2,
                      "points_per_box" = points_per_box)
    }
  }else{
    print("error, check dimensions of x_array, y_array and col_array")
  }
}

# MLR (for MLR, stack the response variable [,,1] and all the explanatory variables in one array --> col_array)
calc_boxes_MLR <- function(x_array, # array carrying the values on the x-axis
                           y_array, # array carrying the values on the y-axis
                           col_array, # array carrying the values on the color scale
                           x_bounds, # from first to last tick on x-axis
                           y_bounds, # from first to last tick on y-axis
                           min_points # the minimum amount of points that should be present within the box in order to calculate val_per_box
){
  most_sign_var_per_box <- second_most_sign_var_per_box <- 
    p.most_sign_var_per_box <- p.second_most_sign_var_per_box <- 
    points_per_box <- 
    array(NaN,c(length(x_bounds)-1,length(y_bounds)-1)) # because x_bounds and y_bounds contain the outer ticks, the boxes in between them is -1
  if(length(dim(x_array)) == length(dim(y_array))){ # check if the dimensions of the arguments are the same
    for(x in 1:(length(x_bounds)-1)){
      for(y in 1:(length(y_bounds)-1)){
        lm.summ_per_box <-
          lm_per_box <- list()
        # calculate the number of points used (points_per_box) to calculate val_per_box
        points_per_box[x,y] <- length(col_array[,,1][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                             y_array > y_bounds[y] & y_array < y_bounds[y+1])])
        if(points_per_box[x,y] > min_points){
          count <- 0
          lm_per_box <- lm(c(col_array[,,1][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                    y_array > y_bounds[y] & y_array < y_bounds[y+1])]) ~ 
                             c(col_array[,,2][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                      y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                             c(col_array[,,3][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                      y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                             c(col_array[,,4][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                      y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                             c(col_array[,,5][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                      y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                             c(col_array[,,6][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                      y_array > y_bounds[y] & y_array < y_bounds[y+1])]))
          lm.summ_per_box <- summary(lm_per_box)
          # most_sign_var_per_box[x,y] <- which(unname(lm.summ_per_box$coefficients[2:6,4]) == min(unname(lm.summ_per_box$coefficients[2:6,4])))
          most_sign_var_per_box[x,y] <- which(rank(unname(lm.summ_per_box$coefficients[2:6,4])) == 1)
          second_most_sign_var_per_box[x,y] <- which(rank(unname(lm.summ_per_box$coefficients[2:6,4])) == 2)
          p.most_sign_var_per_box[x,y] <- lm.summ_per_box$coefficients[most_sign_var_per_box[x,y]+1,4] # +1 because we don't include the intercept as an explanatory variable
          p.second_most_sign_var_per_box[x,y] <- lm.summ_per_box$coefficients[second_most_sign_var_per_box[x,y]+1,4] # +1 because we don't include the intercept as an explanatory variable
        }
      }
    }
    newList <- list("most_sign_var_per_box" = most_sign_var_per_box,
                    "second_most_sign_var_per_box" = second_most_sign_var_per_box,
                    "p.most_sign_var_per_box" = p.most_sign_var_per_box,
                    "p.second_most_sign_var_per_box" = p.second_most_sign_var_per_box,
                    "points_per_box" = points_per_box)
  }else{
    print("error, check dimensions of x_array and y_array")
  }
}

# MLR (for MLR, stack the response variable [,,1] and all the explanatory variables in one array --> col_array)
calc_boxes_MLR_cmip6 <- function(x_array, # array carrying the values on the x-axis
                           y_array, # array carrying the values on the y-axis
                           col_array, # array carrying the values on the color scale
                           x_bounds, # from first to last tick on x-axis
                           y_bounds, # from first to last tick on y-axis
                           min_points, # the minimum amount of points that should be present within the box in order to calculate val_per_box
                           min_models # the minimum amount of models that have to have values in a climate box
){
  print(paste0("Warning: make sure that col_array has in the first 11 array slices the independent variable and the other 5*11",
               " are the explanatory variables"))
  most_sign_var_per_box <-
    p.most_sign_var_per_box <-
    points_per_box <- 
    array(NaN,c(length(x_bounds)-1,length(y_bounds)-1)) # because x_bounds and y_bounds contain the outer ticks, the boxes in between them is -1
  if(length(dim(x_array)) == length(dim(y_array))){ # check if the dimensions of the arguments are the same
    for(x in 1:(length(x_bounds)-1)){
      for(y in 1:(length(y_bounds)-1)){
        lm.summ_per_box <-
          lm_per_box <- list()
        # calculate the number of points used (points_per_box) to calculate val_per_box
        points_per_box[x,y] <- length(col_array[,,1][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                             y_array > y_bounds[y] & y_array < y_bounds[y+1])])
        if(points_per_box[x,y] > min_points){
          count <- 0
          for(i in 1:11){ # loop over all available models
            if(length(col_array[,,i][which(x_array[,,i] > x_bounds[x] & x_array[,,i] < x_bounds[x+1] &
                                           y_array[,,i] > y_bounds[y] & y_array[,,i] < y_bounds[y+1])]) > 0){
              count <- count + 1
            }
          }
          if(count >= min_models){
            lm_per_box <- lm(c(col_array[,,1:11][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                         y_array > y_bounds[y] & y_array < y_bounds[y+1])]) ~ 
                               c(col_array[,,12:22][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                            y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                               c(col_array[,,23:33][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                            y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                               c(col_array[,,34:44][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                            y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                               c(col_array[,,45:55][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                            y_array > y_bounds[y] & y_array < y_bounds[y+1])]) +
                               c(col_array[,,56:66][which(x_array > x_bounds[x] & x_array < x_bounds[x+1] &
                                                            y_array > y_bounds[y] & y_array < y_bounds[y+1])]))
            lm.summ_per_box <- summary(lm_per_box)
            most_sign_var_per_box[x,y] <- which(unname(lm.summ_per_box$coefficients[2:6,4]) == min(unname(lm.summ_per_box$coefficients[2:6,4])))
            p.most_sign_var_per_box[x,y] <- lm.summ_per_box$coefficients[most_sign_var_per_box[x,y]+1,4] # +1 because we don't include the intercept as an explanatory variable
          }
        }
      }
    }
    newList <- list("most_sign_var_per_box" = most_sign_var_per_box,
                    "p.most_sign_var_per_box" = p.most_sign_var_per_box,
                    "points_per_box" = points_per_box)
  }else{
    print("error, check dimensions of x_array and y_array")
  }
}


