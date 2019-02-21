par(family="HiraKakuProN-W3")  # ヒラギノ角ゴシックProN W

library(ggplot2)
library(ggsci)
library(scales)
library(reshape2)
library("trackeR")
library("plyr")

#library(tidyverse)

# 勾配（100ｍ進んで10ｍ登ると10％）のグルーピング
split_slope_c <- c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# 富士山・須走ルート
Fujisan_01 <- readGPX(file = "GPX/20180829_Fujisan_elevation_correction.gpx", timezone = "UTC")
# type列を結合
df_type_Fuji <- data.frame(
  type = rep('Fuji', length(Fujisan_01$time))
)
Fujisan_01 <- cbind(Fujisan_01, df_type_Fuji)
# 欠損値（NA）を0に変換
#Fujisan_01[is.na(Fujisan_01)] <- 0
str(Fujisan_01)

# 比良山地・武奈ヶ岳
Hira_01 <- readGPX(file = "GPX/20181111_Bunagatake_elevation_correction.gpx", timezone = "UTC")
# type列を結合
df_type_Hira <- data.frame(
  type = rep('Hira', length(Hira_01$time))
)
Hira_01 <- cbind(Hira_01, df_type_Hira)
# 欠損値（NA）を0に変換
#Fujisan_01[is.na(Fujisan_01)] <- 0
str(Hira_01)

## 関数の定義 start ############################

# GPXを読み取ったデータを引数とし、入力用のデータフレームを作成して返す
make_InputDataFrame <- function(df1) {
  df2 <- data.frame(
    type = df1$type,
    time = df1$time,
    latitude = df1$latitude,
    longitude = df1$longitude,
    altitude = df1$altitude,
    distance = df1$distance
  )
  return(df2)
}

# diff_time <- c()
# diff_ele <- c()
# diff_dis <- c()
# # 水平速度
# speed_horizontal <- c()
# # 垂直速度
# speed_vertical <- c()
# # 勾配（100ｍ進んで10ｍ登ると10％）
# slope <- c()
# # 累積上昇高度（Total ascent altitude）
# total_ascent_altitude <- c()
# # 累積下降高度（Total descent altitude）
# total_descent_altitude <- c()

# 入力用のデータフレームから、diff_time, diff_dis, diff_ele, speed_h, speed_v, slope 
# を計算し、データフレームを作成して返す
make_ResultDataFrame <- function(df1) {
  elapsed_time <- 0
  total_ascent_altitude <- 0
  total_descent_altitude <- 0
  startAltitude <- 0
  result <- c()
  i <- 1
  for (time in df1$time) {
    #unclass(time)
    
    diff_time <- 0
    diff_dis <- 0
    diff_ele <- 0
    speed_h <- 0
    speed_v <- 0
    slope <- 0
    
    relativeAltitude <- 0
    targetDF <- df1[i,]
    lat <- targetDF$latitude
    lon <- targetDF$longitude
    ele <- targetDF$ele
    dis <- targetDF$distance
    type <- targetDF$type
    
    if (i == 1) {
      startAltitude <- ele
    }
    
    if (i > 1) {
      # ひとつ前
      preDF <- df1[(i - 1), ]
      pre_time <- preDF$time
      pre_dis <- preDF$dis
      pre_ele <- preDF$ele
      diff_time <- unclass(time) - unclass(pre_time)
      #diff_time <- time - pre_time
      diff_dis <- dis - pre_dis
      diff_ele <- ele - pre_ele
      speed_h <- (diff_dis / 1000) / diff_time * 3600
      speed_v <- (diff_ele / 1000) / diff_time * 3600
      slope <- diff_ele / diff_dis * 100
      relativeAltitude <- ele - startAltitude
      elapsed_time <- elapsed_time + diff_time
      
      if (diff_ele >= 0) {
        total_ascent_altitude <- total_ascent_altitude + diff_ele
      } else {
        total_descent_altitude <- total_descent_altitude + diff_ele
      }
    }
    
    # # 結果を出力
    # cat("i = ", i, 
    #     "/ time = ", time, 
    #     "/ lat = ", lat, 
    #     "/ lon = ", lon, 
    #     "/ ele = ", ele, 
    #     "/ dis = ", dis, 
    #     "/ speed_h = ", speed_h, 
    #     "/ speed_v = ", speed_v, 
    #     "/ slope = ", slope, 
    #     "/ diff_time = ", diff_time, 
    #     "/ diff_dis = ", diff_dis, 
    #     "/ diff_ele = ", diff_ele, 
    #     "/ total_ascent_altitude = ", total_ascent_altitude, 
    #     "/ total_descent_altitude = ", total_descent_altitude, 
    #     "\n") 
    
    df <- data.frame (
      type,
      time,
      lat,
      lon,
      ele,
      dis,
      speed_h,
      speed_v,
      slope,
      diff_time,
      diff_dis,
      diff_ele,
      total_ascent_altitude,
      total_descent_altitude,
      relativeAltitude,
      elapsed_time
    )
    
    result <- rbind(result, df)
    
    i <- i + 1
  }
  
  return(result)
}

# 勾配とグルーピングごとの平均垂直速度のデータフレームを返す
make_AVV_ResultDataFrame_bySplitSlope <- function(df1) {
  
  # df1 の type をtypeValueに保持しておく 
  element_type <- df1[1,]
  typeValue <- element_type$type

  df_Fujisan_lt_minus90 <- c()
  df_Fujisan_lt_minus80 <- c()
  df_Fujisan_lt_minus70 <- c()
  df_Fujisan_lt_minus60 <- c()
  df_Fujisan_lt_minus50 <- c()
  df_Fujisan_lt_minus40 <- c()
  df_Fujisan_lt_minus30 <- c()
  df_Fujisan_lt_minus20 <- c()
  df_Fujisan_lt_minus10 <- c()
  df_Fujisan_lt_0 <- c()
  df_Fujisan_lt_10 <- c()
  df_Fujisan_lt_20 <- c()
  df_Fujisan_lt_30 <- c()
  df_Fujisan_lt_40 <- c()
  df_Fujisan_lt_50 <- c()
  df_Fujisan_lt_60 <- c()
  df_Fujisan_lt_70 <- c()
  df_Fujisan_lt_80 <- c()
  df_Fujisan_lt_90 <- c()
  df_Fujisan_lt_100 <- c()
  
  i <- 1
  for (slope in df1$slope) {
    #print(slope)
    
    element = df1[i,]
    #element$slope
    #print(element)
    
    if (slope < -90) {
      #cat("i = ", i, " / slope = ", slope)
      df_Fujisan_lt_minus90 <- rbind(df_Fujisan_lt_minus90, element)
    } else if (slope < -80) {
      df_Fujisan_lt_minus80 <- rbind(df_Fujisan_lt_minus80, element)
    } else if (slope < -70) {
      df_Fujisan_lt_minus70 <- rbind(df_Fujisan_lt_minus70, element)
    } else if (slope < -60) {
      df_Fujisan_lt_minus60 <- rbind(df_Fujisan_lt_minus60, element)
    } else if (slope < -50) {
      df_Fujisan_lt_minus50 <- rbind(df_Fujisan_lt_minus50, element)
    } else if (slope < -40) {
      df_Fujisan_lt_minus40 <- rbind(df_Fujisan_lt_minus40, element)
    } else if (slope < -30) {
      df_Fujisan_lt_minus30 <- rbind(df_Fujisan_lt_minus30, element)
    } else if (slope < -20) {
      df_Fujisan_lt_minus20 <- rbind(df_Fujisan_lt_minus20, element)
    } else if (slope < -10) {
      df_Fujisan_lt_minus10 <- rbind(df_Fujisan_lt_minus10, element)
    } else if (slope < 0) {
      df_Fujisan_lt_0 <- rbind(df_Fujisan_lt_0, element)
    } else if (slope < 10) {
      df_Fujisan_lt_10 <- rbind(df_Fujisan_lt_10, element)
    } else if (slope < 20) {
      df_Fujisan_lt_20 <- rbind(df_Fujisan_lt_20, element)
    } else if (slope < 30) {
      df_Fujisan_lt_30 <- rbind(df_Fujisan_lt_30, element)
    } else if (slope < 40) {
      df_Fujisan_lt_40 <- rbind(df_Fujisan_lt_40, element)
    } else if (slope < 50) {
      df_Fujisan_lt_50 <- rbind(df_Fujisan_lt_50, element)
    } else if (slope < 60) {
      df_Fujisan_lt_60 <- rbind(df_Fujisan_lt_60, element)
    } else if (slope < 70) {
      df_Fujisan_lt_70 <- rbind(df_Fujisan_lt_70, element)
    } else if (slope < 80) {
      df_Fujisan_lt_80 <- rbind(df_Fujisan_lt_80, element)
    } else if (slope < 90) {
      df_Fujisan_lt_90 <- rbind(df_Fujisan_lt_90, element)
    } else {
      df_Fujisan_lt_100 <- rbind(df_Fujisan_lt_100, element)
    }
    
    i <- i + 1
  }
  
  # 平均垂直速度（average vertical velocity）
  # 空の場合、平均速度を0にする
  c_Fujisan_AVV <- c(
    ifelse(length(df_Fujisan_lt_minus90) > 0, mean(df_Fujisan_lt_minus90$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus80) > 0, mean(df_Fujisan_lt_minus80$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus70) > 0, mean(df_Fujisan_lt_minus70$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus60) > 0, mean(df_Fujisan_lt_minus60$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus50) > 0, mean(df_Fujisan_lt_minus50$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus40) > 0, mean(df_Fujisan_lt_minus40$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus30) > 0, mean(df_Fujisan_lt_minus30$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus20) > 0, mean(df_Fujisan_lt_minus20$speed_v), 0),
    ifelse(length(df_Fujisan_lt_minus10) > 0, mean(df_Fujisan_lt_minus10$speed_v), 0),
    ifelse(length(df_Fujisan_lt_0) > 0, mean(df_Fujisan_lt_0$speed_v), 0),
    ifelse(length(df_Fujisan_lt_10) > 0, mean(df_Fujisan_lt_10$speed_v), 0),
    ifelse(length(df_Fujisan_lt_20) > 0, mean(df_Fujisan_lt_20$speed_v), 0),
    ifelse(length(df_Fujisan_lt_30) > 0, mean(df_Fujisan_lt_30$speed_v), 0),
    ifelse(length(df_Fujisan_lt_40) > 0, mean(df_Fujisan_lt_40$speed_v), 0),
    ifelse(length(df_Fujisan_lt_50) > 0, mean(df_Fujisan_lt_50$speed_v), 0),
    ifelse(length(df_Fujisan_lt_60) > 0, mean(df_Fujisan_lt_60$speed_v), 0),
    ifelse(length(df_Fujisan_lt_70) > 0, mean(df_Fujisan_lt_70$speed_v), 0),
    ifelse(length(df_Fujisan_lt_80) > 0, mean(df_Fujisan_lt_80$speed_v), 0),
    ifelse(length(df_Fujisan_lt_90) > 0, mean(df_Fujisan_lt_90$speed_v), 0),
    ifelse(length(df_Fujisan_lt_100) > 0, mean(df_Fujisan_lt_100$speed_v), 0)
  )
  
  df_Fujisan_AVV <- data.frame(
    split_slope_c,
    c_Fujisan_AVV
  )
  
  # type列を作成して結合
  df_type <- data.frame(
    type = rep(typeValue, length(df_Fujisan_AVV$split_slope_c))
  )
  df_Fujisan_AVV <- cbind(df_Fujisan_AVV, df_type)

  return(df_Fujisan_AVV)
}

# 勾配とグルーピングごとの平均水平速度のデータフレームを返す
make_AHV_ResultDataFrame_bySplitSlope <- function(df1) {
  
  # df1 の type をtypeValueに保持しておく 
  element_type <- df1[1,]
  typeValue <- element_type$type
  
  df_Fujisan_lt_minus90 <- c()
  df_Fujisan_lt_minus80 <- c()
  df_Fujisan_lt_minus70 <- c()
  df_Fujisan_lt_minus60 <- c()
  df_Fujisan_lt_minus50 <- c()
  df_Fujisan_lt_minus40 <- c()
  df_Fujisan_lt_minus30 <- c()
  df_Fujisan_lt_minus20 <- c()
  df_Fujisan_lt_minus10 <- c()
  df_Fujisan_lt_0 <- c()
  df_Fujisan_lt_10 <- c()
  df_Fujisan_lt_20 <- c()
  df_Fujisan_lt_30 <- c()
  df_Fujisan_lt_40 <- c()
  df_Fujisan_lt_50 <- c()
  df_Fujisan_lt_60 <- c()
  df_Fujisan_lt_70 <- c()
  df_Fujisan_lt_80 <- c()
  df_Fujisan_lt_90 <- c()
  df_Fujisan_lt_100 <- c()
  
  i <- 1
  for (slope in df1$slope) {
    #print(slope)
    
    element = df1[i,]
    #element$slope
    #print(element)
    
    if (slope < -90) {
      #cat("i = ", i, " / slope = ", slope)
      df_Fujisan_lt_minus90 <- rbind(df_Fujisan_lt_minus90, element)
    } else if (slope < -80) {
      df_Fujisan_lt_minus80 <- rbind(df_Fujisan_lt_minus80, element)
    } else if (slope < -70) {
      df_Fujisan_lt_minus70 <- rbind(df_Fujisan_lt_minus70, element)
    } else if (slope < -60) {
      df_Fujisan_lt_minus60 <- rbind(df_Fujisan_lt_minus60, element)
    } else if (slope < -50) {
      df_Fujisan_lt_minus50 <- rbind(df_Fujisan_lt_minus50, element)
    } else if (slope < -40) {
      df_Fujisan_lt_minus40 <- rbind(df_Fujisan_lt_minus40, element)
    } else if (slope < -30) {
      df_Fujisan_lt_minus30 <- rbind(df_Fujisan_lt_minus30, element)
    } else if (slope < -20) {
      df_Fujisan_lt_minus20 <- rbind(df_Fujisan_lt_minus20, element)
    } else if (slope < -10) {
      df_Fujisan_lt_minus10 <- rbind(df_Fujisan_lt_minus10, element)
    } else if (slope < 0) {
      df_Fujisan_lt_0 <- rbind(df_Fujisan_lt_0, element)
    } else if (slope < 10) {
      df_Fujisan_lt_10 <- rbind(df_Fujisan_lt_10, element)
    } else if (slope < 20) {
      df_Fujisan_lt_20 <- rbind(df_Fujisan_lt_20, element)
    } else if (slope < 30) {
      df_Fujisan_lt_30 <- rbind(df_Fujisan_lt_30, element)
    } else if (slope < 40) {
      df_Fujisan_lt_40 <- rbind(df_Fujisan_lt_40, element)
    } else if (slope < 50) {
      df_Fujisan_lt_50 <- rbind(df_Fujisan_lt_50, element)
    } else if (slope < 60) {
      df_Fujisan_lt_60 <- rbind(df_Fujisan_lt_60, element)
    } else if (slope < 70) {
      df_Fujisan_lt_70 <- rbind(df_Fujisan_lt_70, element)
    } else if (slope < 80) {
      df_Fujisan_lt_80 <- rbind(df_Fujisan_lt_80, element)
    } else if (slope < 90) {
      df_Fujisan_lt_90 <- rbind(df_Fujisan_lt_90, element)
    } else {
      df_Fujisan_lt_100 <- rbind(df_Fujisan_lt_100, element)
    }
    
    i <- i + 1
  }
  
  # 平均水平速度（average horizontal velocity）
  # 空の場合、平均速度を0にする
  c_Fujisan_AHV <- c(
    ifelse(length(df_Fujisan_lt_minus90) > 0, mean(df_Fujisan_lt_minus90$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus80) > 0, mean(df_Fujisan_lt_minus80$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus70) > 0, mean(df_Fujisan_lt_minus70$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus60) > 0, mean(df_Fujisan_lt_minus60$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus50) > 0, mean(df_Fujisan_lt_minus50$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus40) > 0, mean(df_Fujisan_lt_minus40$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus30) > 0, mean(df_Fujisan_lt_minus30$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus20) > 0, mean(df_Fujisan_lt_minus20$speed_h), 0),
    ifelse(length(df_Fujisan_lt_minus10) > 0, mean(df_Fujisan_lt_minus10$speed_h), 0),
    ifelse(length(df_Fujisan_lt_0) > 0, mean(df_Fujisan_lt_0$speed_h), 0),
    ifelse(length(df_Fujisan_lt_10) > 0, mean(df_Fujisan_lt_10$speed_h), 0),
    ifelse(length(df_Fujisan_lt_20) > 0, mean(df_Fujisan_lt_20$speed_h), 0),
    ifelse(length(df_Fujisan_lt_30) > 0, mean(df_Fujisan_lt_30$speed_h), 0),
    ifelse(length(df_Fujisan_lt_40) > 0, mean(df_Fujisan_lt_40$speed_h), 0),
    ifelse(length(df_Fujisan_lt_50) > 0, mean(df_Fujisan_lt_50$speed_h), 0),
    ifelse(length(df_Fujisan_lt_60) > 0, mean(df_Fujisan_lt_60$speed_h), 0),
    ifelse(length(df_Fujisan_lt_70) > 0, mean(df_Fujisan_lt_70$speed_h), 0),
    ifelse(length(df_Fujisan_lt_80) > 0, mean(df_Fujisan_lt_80$speed_h), 0),
    ifelse(length(df_Fujisan_lt_90) > 0, mean(df_Fujisan_lt_90$speed_h), 0),
    ifelse(length(df_Fujisan_lt_100) > 0, mean(df_Fujisan_lt_100$speed_h), 0)
  )
  
  df_Fujisan_AHV <- data.frame(
    split_slope_c,
    c_Fujisan_AHV
  )
  
  # type列を作成して結合
  df_type <- data.frame(
    type = rep(typeValue, length(df_Fujisan_AHV$split_slope_c))
  )
  df_Fujisan_AHV <- cbind(df_Fujisan_AHV, df_type)
  
  return(df_Fujisan_AHV)
}

# 勾配とグルーピングごとの経過時間合計のデータフレームを返す
make_TotalTime_ResultDataFrame_bySplitSlope <- function(df1) {
  
  # df1 の type をtypeValueに保持しておく 
  element_type <- df1[1,]
  typeValue <- element_type$type
  
  df_Fujisan_lt_minus90 <- c()
  df_Fujisan_lt_minus80 <- c()
  df_Fujisan_lt_minus70 <- c()
  df_Fujisan_lt_minus60 <- c()
  df_Fujisan_lt_minus50 <- c()
  df_Fujisan_lt_minus40 <- c()
  df_Fujisan_lt_minus30 <- c()
  df_Fujisan_lt_minus20 <- c()
  df_Fujisan_lt_minus10 <- c()
  df_Fujisan_lt_0 <- c()
  df_Fujisan_lt_10 <- c()
  df_Fujisan_lt_20 <- c()
  df_Fujisan_lt_30 <- c()
  df_Fujisan_lt_40 <- c()
  df_Fujisan_lt_50 <- c()
  df_Fujisan_lt_60 <- c()
  df_Fujisan_lt_70 <- c()
  df_Fujisan_lt_80 <- c()
  df_Fujisan_lt_90 <- c()
  df_Fujisan_lt_100 <- c()
  
  i <- 1
  for (slope in df1$slope) {
    #print(slope)
    
    element = df1[i,]
    #element$slope
    #print(element)
    
    # 地点間の経過時間が180秒を超えるものについては、休憩とみなして除外する
    if (element$diff_time <= 180) {
      if (slope < -90) {
        #cat("i = ", i, " / slope = ", slope)
        df_Fujisan_lt_minus90 <- rbind(df_Fujisan_lt_minus90, element)
      } else if (slope < -80) {
        df_Fujisan_lt_minus80 <- rbind(df_Fujisan_lt_minus80, element)
      } else if (slope < -70) {
        df_Fujisan_lt_minus70 <- rbind(df_Fujisan_lt_minus70, element)
      } else if (slope < -60) {
        df_Fujisan_lt_minus60 <- rbind(df_Fujisan_lt_minus60, element)
      } else if (slope < -50) {
        df_Fujisan_lt_minus50 <- rbind(df_Fujisan_lt_minus50, element)
      } else if (slope < -40) {
        df_Fujisan_lt_minus40 <- rbind(df_Fujisan_lt_minus40, element)
      } else if (slope < -30) {
        df_Fujisan_lt_minus30 <- rbind(df_Fujisan_lt_minus30, element)
      } else if (slope < -20) {
        df_Fujisan_lt_minus20 <- rbind(df_Fujisan_lt_minus20, element)
      } else if (slope < -10) {
        df_Fujisan_lt_minus10 <- rbind(df_Fujisan_lt_minus10, element)
      } else if (slope < 0) {
        df_Fujisan_lt_0 <- rbind(df_Fujisan_lt_0, element)
      } else if (slope < 10) {
        df_Fujisan_lt_10 <- rbind(df_Fujisan_lt_10, element)
      } else if (slope < 20) {
        df_Fujisan_lt_20 <- rbind(df_Fujisan_lt_20, element)
      } else if (slope < 30) {
        df_Fujisan_lt_30 <- rbind(df_Fujisan_lt_30, element)
      } else if (slope < 40) {
        df_Fujisan_lt_40 <- rbind(df_Fujisan_lt_40, element)
      } else if (slope < 50) {
        df_Fujisan_lt_50 <- rbind(df_Fujisan_lt_50, element)
      } else if (slope < 60) {
        df_Fujisan_lt_60 <- rbind(df_Fujisan_lt_60, element)
      } else if (slope < 70) {
        df_Fujisan_lt_70 <- rbind(df_Fujisan_lt_70, element)
      } else if (slope < 80) {
        df_Fujisan_lt_80 <- rbind(df_Fujisan_lt_80, element)
      } else if (slope < 90) {
        df_Fujisan_lt_90 <- rbind(df_Fujisan_lt_90, element)
      } else {
        df_Fujisan_lt_100 <- rbind(df_Fujisan_lt_100, element)
      }
    }

    i <- i + 1
  }
  
  # 平均水平速度（average horizontal velocity）
  # 空の場合、平均速度を0にする
  c_Fujisan_TimeBySplitSlope <- c(
    ifelse(length(df_Fujisan_lt_minus90) > 0, sum(df_Fujisan_lt_minus90$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus80) > 0, sum(df_Fujisan_lt_minus80$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus70) > 0, sum(df_Fujisan_lt_minus70$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus60) > 0, sum(df_Fujisan_lt_minus60$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus50) > 0, sum(df_Fujisan_lt_minus50$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus40) > 0, sum(df_Fujisan_lt_minus40$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus30) > 0, sum(df_Fujisan_lt_minus30$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus20) > 0, sum(df_Fujisan_lt_minus20$diff_time), 0),
    ifelse(length(df_Fujisan_lt_minus10) > 0, sum(df_Fujisan_lt_minus10$diff_time), 0),
    ifelse(length(df_Fujisan_lt_0) > 0, sum(df_Fujisan_lt_0$diff_time), 0),
    ifelse(length(df_Fujisan_lt_10) > 0, sum(df_Fujisan_lt_10$diff_time), 0),
    ifelse(length(df_Fujisan_lt_20) > 0, sum(df_Fujisan_lt_20$diff_time), 0),
    ifelse(length(df_Fujisan_lt_30) > 0, sum(df_Fujisan_lt_30$diff_time), 0),
    ifelse(length(df_Fujisan_lt_40) > 0, sum(df_Fujisan_lt_40$diff_time), 0),
    ifelse(length(df_Fujisan_lt_50) > 0, sum(df_Fujisan_lt_50$diff_time), 0),
    ifelse(length(df_Fujisan_lt_60) > 0, sum(df_Fujisan_lt_60$diff_time), 0),
    ifelse(length(df_Fujisan_lt_70) > 0, sum(df_Fujisan_lt_70$diff_time), 0),
    ifelse(length(df_Fujisan_lt_80) > 0, sum(df_Fujisan_lt_80$diff_time), 0),
    ifelse(length(df_Fujisan_lt_90) > 0, sum(df_Fujisan_lt_90$diff_time), 0),
    ifelse(length(df_Fujisan_lt_100) > 0, sum(df_Fujisan_lt_100$diff_time), 0)
  )
  
  df_Fujisan_TimeBySplitSlope <- data.frame(
    split_slope_c,
    c_Fujisan_TimeBySplitSlope
  )
  
  # type列を作成して結合
  df_type <- data.frame(
    type = rep(typeValue, length(df_Fujisan_TimeBySplitSlope$split_slope_c))
  )
  df_Fujisan_TimeBySplitSlope <- cbind(df_Fujisan_TimeBySplitSlope, df_type)
  
  return(df_Fujisan_TimeBySplitSlope)
}

# 勾配とグルーピングごとのデータ分布のデータフレームを返す
make_TotalDataDistribution_ResultDataFrame_bySplitSlope <- function(df1) {
  
  # df1 の type をtypeValueに保持しておく 
  element_type <- df1[1,]
  typeValue <- element_type$type
  
  df_Fujisan_lt_minus90 <- c()
  df_Fujisan_lt_minus80 <- c()
  df_Fujisan_lt_minus70 <- c()
  df_Fujisan_lt_minus60 <- c()
  df_Fujisan_lt_minus50 <- c()
  df_Fujisan_lt_minus40 <- c()
  df_Fujisan_lt_minus30 <- c()
  df_Fujisan_lt_minus20 <- c()
  df_Fujisan_lt_minus10 <- c()
  df_Fujisan_lt_0 <- c()
  df_Fujisan_lt_10 <- c()
  df_Fujisan_lt_20 <- c()
  df_Fujisan_lt_30 <- c()
  df_Fujisan_lt_40 <- c()
  df_Fujisan_lt_50 <- c()
  df_Fujisan_lt_60 <- c()
  df_Fujisan_lt_70 <- c()
  df_Fujisan_lt_80 <- c()
  df_Fujisan_lt_90 <- c()
  df_Fujisan_lt_100 <- c()
  
  i <- 1
  for (slope in df1$slope) {
    #print(slope)
    
    element = df1[i,]
    #element$slope
    #print(element)
    
    if (slope < -90) {
      #cat("i = ", i, " / slope = ", slope)
      df_Fujisan_lt_minus90 <- rbind(df_Fujisan_lt_minus90, element)
    } else if (slope < -80) {
      df_Fujisan_lt_minus80 <- rbind(df_Fujisan_lt_minus80, element)
    } else if (slope < -70) {
      df_Fujisan_lt_minus70 <- rbind(df_Fujisan_lt_minus70, element)
    } else if (slope < -60) {
      df_Fujisan_lt_minus60 <- rbind(df_Fujisan_lt_minus60, element)
    } else if (slope < -50) {
      df_Fujisan_lt_minus50 <- rbind(df_Fujisan_lt_minus50, element)
    } else if (slope < -40) {
      df_Fujisan_lt_minus40 <- rbind(df_Fujisan_lt_minus40, element)
    } else if (slope < -30) {
      df_Fujisan_lt_minus30 <- rbind(df_Fujisan_lt_minus30, element)
    } else if (slope < -20) {
      df_Fujisan_lt_minus20 <- rbind(df_Fujisan_lt_minus20, element)
    } else if (slope < -10) {
      df_Fujisan_lt_minus10 <- rbind(df_Fujisan_lt_minus10, element)
    } else if (slope < 0) {
      df_Fujisan_lt_0 <- rbind(df_Fujisan_lt_0, element)
    } else if (slope < 10) {
      df_Fujisan_lt_10 <- rbind(df_Fujisan_lt_10, element)
    } else if (slope < 20) {
      df_Fujisan_lt_20 <- rbind(df_Fujisan_lt_20, element)
    } else if (slope < 30) {
      df_Fujisan_lt_30 <- rbind(df_Fujisan_lt_30, element)
    } else if (slope < 40) {
      df_Fujisan_lt_40 <- rbind(df_Fujisan_lt_40, element)
    } else if (slope < 50) {
      df_Fujisan_lt_50 <- rbind(df_Fujisan_lt_50, element)
    } else if (slope < 60) {
      df_Fujisan_lt_60 <- rbind(df_Fujisan_lt_60, element)
    } else if (slope < 70) {
      df_Fujisan_lt_70 <- rbind(df_Fujisan_lt_70, element)
    } else if (slope < 80) {
      df_Fujisan_lt_80 <- rbind(df_Fujisan_lt_80, element)
    } else if (slope < 90) {
      df_Fujisan_lt_90 <- rbind(df_Fujisan_lt_90, element)
    } else {
      df_Fujisan_lt_100 <- rbind(df_Fujisan_lt_100, element)
    }
    
    i <- i + 1
  }
  
  # データの個数を調べる
  # library("plyr")の、empty()を使って、DataFrameのNULLチェックをし、NULLの場合は、0。
  # NULLでない場合は、nrow()で行数（データの個数）を代入
  # 個数0の場合、0にする
  c_Fujisan_DataLengthBySplitSlope <- c(
    ifelse(empty(df_Fujisan_lt_minus90) == FALSE, nrow(df_Fujisan_lt_minus90), 0),
    ifelse(empty(df_Fujisan_lt_minus80) == FALSE, nrow(df_Fujisan_lt_minus80), 0),
    ifelse(empty(df_Fujisan_lt_minus70) == FALSE, nrow(df_Fujisan_lt_minus70), 0),
    ifelse(empty(df_Fujisan_lt_minus60) == FALSE, nrow(df_Fujisan_lt_minus60), 0),
    ifelse(empty(df_Fujisan_lt_minus50) == FALSE, nrow(df_Fujisan_lt_minus50), 0),
    ifelse(empty(df_Fujisan_lt_minus40) == FALSE, nrow(df_Fujisan_lt_minus40), 0),
    ifelse(empty(df_Fujisan_lt_minus30) == FALSE, nrow(df_Fujisan_lt_minus30), 0),
    ifelse(empty(df_Fujisan_lt_minus20) == FALSE, nrow(df_Fujisan_lt_minus20), 0),
    ifelse(empty(df_Fujisan_lt_minus10) == FALSE, nrow(df_Fujisan_lt_minus10), 0),
    ifelse(empty(df_Fujisan_lt_0) == FALSE, nrow(df_Fujisan_lt_0), 0),
    ifelse(empty(df_Fujisan_lt_10) == FALSE, nrow(df_Fujisan_lt_10), 0),
    ifelse(empty(df_Fujisan_lt_20) == FALSE, nrow(df_Fujisan_lt_20), 0),
    ifelse(empty(df_Fujisan_lt_30) == FALSE, nrow(df_Fujisan_lt_30), 0),
    ifelse(empty(df_Fujisan_lt_40) == FALSE, nrow(df_Fujisan_lt_40), 0),
    ifelse(empty(df_Fujisan_lt_50) == FALSE, nrow(df_Fujisan_lt_50), 0),
    ifelse(empty(df_Fujisan_lt_60) == FALSE, nrow(df_Fujisan_lt_60), 0),
    ifelse(empty(df_Fujisan_lt_70) == FALSE, nrow(df_Fujisan_lt_70), 0),
    ifelse(empty(df_Fujisan_lt_80) == FALSE, nrow(df_Fujisan_lt_80), 0),
    ifelse(empty(df_Fujisan_lt_90) == FALSE, nrow(df_Fujisan_lt_90), 0),
    ifelse(empty(df_Fujisan_lt_100) == FALSE, nrow(df_Fujisan_lt_100), 0)
  )

  df_Fujisan_DataLengthBySplitSlope <- data.frame(
    split_slope_c,
    c_Fujisan_DataLengthBySplitSlope
  )

  # type列を作成して結合
  df_type <- data.frame(
    type = rep(typeValue, length(df_Fujisan_DataLengthBySplitSlope$split_slope_c))
  )
  df_Fujisan_DataLengthBySplitSlope <- cbind(df_Fujisan_DataLengthBySplitSlope, df_type)
  
  # データの割合ratioを求める
  #all_data_count <- nrow(df_Fujisan_DataLengthBySplitSlope)
  all_data_count <- sum(c_Fujisan_DataLengthBySplitSlope)
  #cat("all_data_count = ", all_data_count)
  #cat("all_data_count2 = ", all_data_count2)
  
  ratio <- c(
    ifelse(empty(df_Fujisan_lt_minus90) == FALSE, (nrow(df_Fujisan_lt_minus90) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus80) == FALSE, (nrow(df_Fujisan_lt_minus80) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus70) == FALSE, (nrow(df_Fujisan_lt_minus70) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus60) == FALSE, (nrow(df_Fujisan_lt_minus60) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus50) == FALSE, (nrow(df_Fujisan_lt_minus50) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus40) == FALSE, (nrow(df_Fujisan_lt_minus40) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus30) == FALSE, (nrow(df_Fujisan_lt_minus30) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus20) == FALSE, (nrow(df_Fujisan_lt_minus20) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_minus10) == FALSE, (nrow(df_Fujisan_lt_minus10) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_0) == FALSE, (nrow(df_Fujisan_lt_0) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_10) == FALSE, (nrow(df_Fujisan_lt_10) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_20) == FALSE, (nrow(df_Fujisan_lt_20) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_30) == FALSE, (nrow(df_Fujisan_lt_30) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_40) == FALSE, (nrow(df_Fujisan_lt_40) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_50) == FALSE, (nrow(df_Fujisan_lt_50) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_60) == FALSE, (nrow(df_Fujisan_lt_60) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_70) == FALSE, (nrow(df_Fujisan_lt_70) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_80) == FALSE, (nrow(df_Fujisan_lt_80) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_90) == FALSE, (nrow(df_Fujisan_lt_90) / all_data_count), 0),
    ifelse(empty(df_Fujisan_lt_100) == FALSE, (nrow(df_Fujisan_lt_100) / all_data_count), 0)
  )
  
  # ratio列を結合
  df_Fujisan_DataLengthBySplitSlope <- cbind(df_Fujisan_DataLengthBySplitSlope, ratio)
  
  return(df_Fujisan_DataLengthBySplitSlope)
}

# 勾配と垂直速度の散布図のためのデータフレームを返す
make_DVV_ResultDataFrame_bySplitSlope <- function(df1) {
  # type, speed_v, slope を取り出してdfを作成
  df <- data.frame(
    type = df1$type,
    speed_v = df1$speed_v,
    slope = df1$slope
  )
  return(df)
}

# 勾配と水平速度の散布図のためのデータフレームを返す
make_DHV_ResultDataFrame_bySplitSlope <- function(df1) {
  # type, speed_h, slope を取り出してdfを作成
  df <- data.frame(
    type = df1$type,
    speed_h = df1$speed_h,
    slope = df1$slope
  )
  return(df)
}

## 関数の定義 end ############################


# GPXを読み取ったデータから、入力用のデータフレームを作成
df_20180829_Fujisan <- make_InputDataFrame(Fujisan_01)
df_20181111_Hira <- make_InputDataFrame(Hira_01)

# 入力用のデータフレームから、diff_time, diff_dis, diff_ele, speed_h, speed_v, slope 
# を計算して追加したデータフレームを作成
result_df_Fujisan <- make_ResultDataFrame(df_20180829_Fujisan)

str(result_df_Fujisan)

result_df_Hira <- make_ResultDataFrame(df_20181111_Hira)
str(result_df_Hira)
# 富士山と比良のデータフレームを結合してresult_df_allを作成
result_df_all <- rbind(result_df_Fujisan, result_df_Hira)

# 勾配データのグルーピングファクタ
# 富士山のデータのみ
split_slope_Fujisan <- cut(result_df_Fujisan$slope, 
                           breaks = split_slope_c,
                           right = TRUE)
table(split_slope_Fujisan)

# 武奈ヶ岳のデータのみ
split_slope_Hira <- cut(result_df_Hira$slope, 
                           breaks = split_slope_c,
                           right = TRUE)
table(split_slope_Hira)

# すべてのデータ
split_slope_all <- cut(result_df_all$slope, 
                           breaks = split_slope_c,
                           right = TRUE)
table(split_slope_all)

# test <- make_SlopeGrouping_DataFrame(result_df_Fujisan)

# 勾配とグルーピングごとの平均垂直速度
df_Fujisan_AVV <- make_AVV_ResultDataFrame_bySplitSlope(result_df_Fujisan)
df_Hira_AVV <- make_AVV_ResultDataFrame_bySplitSlope(result_df_Hira)
result_df_AVV_all <- rbind(df_Fujisan_AVV, df_Hira_AVV)

# 勾配とグルーピングごとの平均水平速度
df_Fujisan_AHV <- make_AHV_ResultDataFrame_bySplitSlope(result_df_Fujisan)
df_Hira_AHV <- make_AHV_ResultDataFrame_bySplitSlope(result_df_Hira)
result_df_AHV_all <- rbind(df_Fujisan_AHV, df_Hira_AHV)

# 勾配とグルーピングごとの経過時間合計
df_Fujisan_TotalTimeBySplitSlope <- make_TotalTime_ResultDataFrame_bySplitSlope(result_df_Fujisan)
df_Hira_TotalTimeBySplitSlope <- make_TotalTime_ResultDataFrame_bySplitSlope(result_df_Hira)
result_df_TotalTime_all <- rbind(df_Fujisan_TotalTimeBySplitSlope, df_Hira_TotalTimeBySplitSlope)

# 勾配とグルーピングごとのデータ数
df_Fujisan_TotalDataDistributionBySplitSlope <- make_TotalDataDistribution_ResultDataFrame_bySplitSlope(result_df_Fujisan)
df_Hira_TotalDataDistributionBySplitSlope <- make_TotalDataDistribution_ResultDataFrame_bySplitSlope(result_df_Hira)
result_df_TotalDataDistribution_all <- rbind(df_Fujisan_TotalDataDistributionBySplitSlope, df_Hira_TotalDataDistributionBySplitSlope)

# 勾配と水平速度の分布
df_Fujisan_DHV <- make_DHV_ResultDataFrame_bySplitSlope(result_df_Fujisan)
df_Hira_DHV <- make_DHV_ResultDataFrame_bySplitSlope(result_df_Hira)
result_df_DHV_all <- rbind(df_Fujisan_DHV, df_Hira_DHV)

# 勾配と垂直速度の分布
df_Fujisan_DVV <- make_DVV_ResultDataFrame_bySplitSlope(result_df_Fujisan)
df_Hira_DVV <- make_DVV_ResultDataFrame_bySplitSlope(result_df_Hira)
result_df_DVV_all <- rbind(df_Fujisan_DVV, df_Hira_DVV)


# 経過時間における次のポイント（水平距離約10m）の移動に180秒以上かかったケースが占める割合
make_BreakTimeRatio <- function(df1) {
  # df1 の type をtypeValueに保持しておく 
  element_type <- df1[1,]
  typeValue <- element_type$type
  
  totalTime <- sum(df1$diff_time)
  breakTime <- 0
  i <- 1
  for (diff_time in df1$diff_time) {
    #element = df1[i,]
    
    if (diff_time >= 180) {
      breakTime <- breakTime + diff_time
    }
    i <- i + 1
  }
  df_timeRatio <- data.frame(
    breakTime,
    totalTime
  )
  
  # type列を作成して結合
  df_type <- data.frame(
    type = rep(typeValue, length(df_timeRatio$breakTime))
  )
  df_timeRatio <- cbind(df_timeRatio, df_type)
  
  return(df_timeRatio)
}

df_Fuji_breakTimeRatio <- make_BreakTimeRatio(result_df_Fujisan)
df_Hira_breakTimeRatio <- make_BreakTimeRatio(result_df_Hira)
result_df_breakTimeRatio_all <- rbind(df_Fuji_breakTimeRatio, df_Hira_breakTimeRatio)

################################################
# ggplot2でグラフ描画
################################################
ggplot()+theme_set(theme_gray(base_size = 14, base_family="HiraKakuProN-W3"))
  
Fuji_ActiveTime <- round((((df_Fuji_breakTimeRatio$totalTime - df_Fuji_breakTimeRatio$breakTime) / df_Fuji_breakTimeRatio$totalTime) * 100), 2)
Fuji_InactiveTime <- 100 - Fuji_ActiveTime
Hira_ActiveTime <- round((((df_Hira_breakTimeRatio$totalTime - df_Hira_breakTimeRatio$breakTime) / df_Hira_breakTimeRatio$totalTime) * 100), 2)
Hira_InactiveTime <- 100 - Hira_ActiveTime

df_breakTimeRatio <- data.frame(
  cell   = c("富士山", "富士山", "武奈ヶ岳", "武奈ヶ岳"),
#  sample <- c("A 富士山：行動時間",
#              "B 富士山：非行動時間",
#              "C 武奈ヶ岳：行動時間",
#              "D 武奈ヶ岳：非行動時間"
#  ),
  Activity <- c("A：行動時間",
                "B：非行動時間",
                "A：行動時間",
                "B：非行動時間"
),
  weight = c(Fuji_ActiveTime,
             Fuji_InactiveTime, 
             Hira_ActiveTime,
             Hira_InactiveTime))

#p <- ggplot(dt,aes(x=Group, y=Weight, fill=Treat))
#p + geom_bar(stat="identity")


g <- ggplot(df_breakTimeRatio, aes(x = cell, 
                                   y = weight, 
                                   fill = Activity))
g <- g + geom_bar(stat = "identity")
#g <- g + scale_y_continuous(labels = percent)
#g <- g + scale_fill_nejm()
g <- g + labs(title = "行動時間と非行動時間の割合", fill = "") + xlab("") + ylab("割合（%）") 
g <- g + geom_text(aes(label = weight), size = 6, hjust = 0.5, vjust = 3, position = "stack") 
plot(g)

# 勾配の分布：棒グラフ（比較）
# x, y軸にそれぞれ値を指定した棒グラフの作成
g <- ggplot(result_df_TotalDataDistribution_all, aes(x = result_df_TotalDataDistribution_all$split_slope_c, 
                                                     y = (result_df_TotalDataDistribution_all$ratio * 100),
                                                     fill = result_df_TotalDataDistribution_all$type))
g <- g + geom_bar(stat="identity", 
                  width = 6, 
                  position = position_dodge(width = 6))

g <- g + labs(title = "勾配とデータ分布の割合", fill = "") + xlab("勾配（%）") + ylab("全体に占める割合（%）") 
plot(g)

# x:勾配, y:平均垂直速度：棒グラフ（比較）
g <- ggplot(result_df_AVV_all, aes(x = result_df_AVV_all$split_slope_c, 
                                   y = result_df_AVV_all$c_Fujisan_AVV,
                                   fill = result_df_AVV_all$type)) + 
  geom_bar(stat = "identity", 
           width = 6, 
           position = position_dodge(width = 6)) + 
  ylim(min(result_df_AVV_all$c_Fujisan_AVV), max(result_df_AVV_all$c_Fujisan_AVV)) + 
  xlab("勾配率（%）") + 
  ylab("平均垂直速度（km/h）") +
  labs(title = "勾配と平均垂直速度", fill = "")
plot(g)

# x:勾配, y:平均水平速度：棒グラフ（比較）
g <- ggplot(result_df_AHV_all, aes(x = result_df_AHV_all$split_slope_c, 
                                         y = result_df_AHV_all$c_Fujisan_AHV,
                                         fill = result_df_AHV_all$type)) + 
  geom_bar(stat = "identity", 
           width = 6, 
           position = position_dodge(width = 6)) + 
  ylim(0, max(result_df_AHV_all$c_Fujisan_AHV)) + 
  xlab("勾配率（%）") + 
  ylab("平均水平速度（km/h）") +
  labs(title = "勾配と平均水平速度", fill = "")
# g <- g + scale_x_discrete(limits = c("trt2","ctrl"), breaks = c("trt2","ctrl"))
plot(g)

# x:勾配, y:経過時間：棒グラフ
g <- ggplot(result_df_TotalTime_all, aes(x = result_df_TotalTime_all$split_slope_c, 
                                         y = result_df_TotalTime_all$c_Fujisan_TimeBySplitSlope,
                                         fill = result_df_TotalTime_all$type)) + 
  geom_bar(stat = "identity", 
                    width = 6, 
                    position = position_dodge(width = 6)) + 
  ylim(0, max(result_df_TotalTime_all$c_Fujisan_TimeBySplitSlope)) + 
  xlab("勾配率（%）") + 
  ylab("経過時間（秒）") +
  labs(title = "勾配と経過時間（180秒以上の停止状態を除外）", fill = "")
plot(g)

# x:水平移動距離, y:標高：折れ線グラフ（比較）
g <- ggplot(data = result_df_all, aes(x = result_df_all$dis, 
                                      y = result_df_all$ele, 
                                      colour = result_df_all$type)) +
  geom_line(size = 1) + 
  # geom_point(aes(y = result_df_all$ele, 
  #                colour = result_df_all$type)) +
  xlim(0, max(result_df_all$dis)) +
  ylim(0, max(result_df_all$ele)) +
  xlab("水平移動距離（m）") +
  ylab("標高（m）") +
  labs(title = "水平移動距離と標高", color = "")
plot(g)

# x:経過時間, y:標高：折れ線グラフ（比較）
g <- ggplot(data = result_df_all, aes(x = result_df_all$elapsed_time, 
                                      y = result_df_all$ele, 
                                      colour = result_df_all$type)) +
  geom_line(size = 1) + 
  # geom_point(aes(y = result_df_all$ele, 
  #                colour = result_df_all$type)) +
  xlim(0, max(result_df_all$elapsed_time)) +
  ylim(0, max(result_df_all$ele)) +
  xlab("経過時間（秒）") +
  ylab("標高（m）") +
  labs(title = "経過時間と標高", color = "")
plot(g)

# x:水平移動距離, y:相対高度：折れ線グラフ（比較）
g <- ggplot(data = result_df_all, aes(x = result_df_all$dis, 
                                      y = result_df_all$relativeAltitude, 
                                      colour = result_df_all$type)) +
  geom_line(size = 1) + 
  # geom_point(aes(y = result_df_all$ele, 
  #                colour = result_df_all$type)) +
  xlim(0, max(result_df_all$dis)) +
  ylim(0, max(result_df_all$relativeAltitude)) +
  xlab("水平移動距離（m）") +
  ylab("相対高度（m）") +
  labs(title = "水平移動距離と相対高度", color = "")
plot(g)

# x:経過時間, y:相対高度：折れ線グラフ（比較）
g <- ggplot(data = result_df_all, aes(x = result_df_all$elapsed_time, 
                                      y = result_df_all$relativeAltitude, 
                                      colour = result_df_all$type)) +
  geom_line(size = 1) + 
  # geom_point(aes(y = result_df_all$ele, 
  #                colour = result_df_all$type)) +
  xlim(0, max(result_df_all$elapsed_time)) +
  ylim(0, max(result_df_all$relativeAltitude)) +
  xlab("経過時間（秒）") +
  ylab("相対高度（m）") +
  labs(title = "経過時間と相対高度", color = "")
plot(g)

# x:水平移動距離, y:累積上昇高度：折れ線グラフ（比較）
g <- ggplot(data = result_df_all, aes(x = result_df_all$dis, 
                                      y = result_df_all$total_ascent_altitude, 
                                      colour = result_df_all$type)) +
  geom_line(size = 1) + 
  # geom_point(aes(y = result_df_all$ele, 
  #                colour = result_df_all$type)) +
  xlim(0, max(result_df_all$dis)) +
  ylim(0, max(result_df_all$total_ascent_altitude)) +
  xlab("水平移動距離（m）") +
  ylab("累積上昇高度（m）") +
  labs(title = "水平移動距離と累積上昇高度", color = "")
plot(g)
  
# x:水平移動距離, y:累積下降高度：折れ線グラフ（比較）
g <- ggplot(data = result_df_all, aes(x = result_df_all$dis, 
                                      y = result_df_all$total_descent_altitude, 
                                      colour = result_df_all$type)) +
  geom_line(size = 1) + 
  # geom_point(aes(y = result_df_all$ele, 
  #                colour = result_df_all$type)) +
  xlim(0, max(result_df_all$dis)) +
  #ylim(0, max(result_df_all$total_descent_altitude)) +
  xlab("水平移動距離（m）") +
  ylab("累積下降高度（m）") +
  labs(title = "水平移動距離と累積下降高度", color = "")
plot(g)

# x:勾配, y:水平速度：富士山の散布図
g_h1 <- ggplot(df_Fujisan_DHV, aes(x = slope, 
                                   y = speed_h))
g_h1 + geom_point(size = 1.0,
               alpha = 1.0) + 
  xlim(-125, 125) + 
  ylim(0, 10.0) + 
  xlab("勾配率（%）") +
  ylab("水平速度（km/h）") +
  labs(title = "富士山の勾配と水平速度")
#g + scale_colour_brewer(palette = "Set1")
#plot(g)

# x:勾配, y:水平速度：比良の散布図
g_h2 <- ggplot(df_Hira_DHV, aes(x = slope, 
                                 y = speed_h))
g_h2 + geom_point(size = 1.0,
                alpha = 1.0) + 
  xlim(-125, 125) + 
  ylim(0, 10.0) + 
  xlab("勾配率（%）") +
  ylab("水平速度（km/h）") +
  labs(title = "比良の勾配と水平速度")
#g + scale_colour_brewer(palette = "Set1")
#plot(g)


# # x:勾配, y:水平速度：散布図
# # result_df_DHV_all
# # df_Fujisan_DHV
# g3 <- ggplot(result_df_DHV_all, aes(x = slope, 
#                                     y = speed_h,
#                                     colour = type))
# g3 + geom_point(size = 0.2,
#                 alpha = 0.4) + 
#   ylim(0, 10.0) + 
#   xlab("勾配率（%）") +
#   ylab("水平速度（km/h）") +
#   labs(title = "勾配と水平速度")
# #g + scale_colour_brewer(palette = "Set1")
# #plot(g)


# x:勾配, y:垂直速度：富士山の散布図
g_v1 <- ggplot(df_Fujisan_DVV, aes(x = slope, 
                                   y = speed_v))
g_v1 + geom_point(size = 1.0,
                  alpha = 1.0) + 
  xlim(-125, 125) + 
  ylim(-5.0, 5.0) + 
  xlab("勾配率（%）") +
  ylab("垂直速度（km/h）") +
  labs(title = "富士山の勾配と垂直速度")

# x:勾配, y:垂直速度：比良の散布図
g_v2 <- ggplot(df_Hira_DVV, aes(x = slope, 
                                y = speed_v))
g_v2 + geom_point(size = 1.0,
                  alpha = 1.0) + 
  xlim(-125, 125) + 
  ylim(-5.0, 5.0) + 
  xlab("勾配率（%）") +
  ylab("垂直速度（km/h）") +
  labs(title = "比良の勾配と垂直速度")

