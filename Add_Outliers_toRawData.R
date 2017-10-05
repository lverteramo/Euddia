# This script adds outliers to each sensor's data
# Sensors' data are saved in 'DataRaw'

# We define outliers as observations more than 3 std away from the mean

# estimate mean and standard deviation for each sensor data for the whole sample period.

sizeD= dim(DataRaw)[1]
m1= mean(DataRaw[1:sizeD, 1])
m2= mean(DataRaw[1:sizeD, 2])
m3= mean(DataRaw[1:sizeD, 3])

sd1= sd(DataRaw[1:sizeD, 1]) 
sd2= sd(DataRaw[1:sizeD, 2]) 
sd3= sd(DataRaw[1:sizeD, 3]) 

# positive outliers
outl1= m1+3*sd1
outl2= m1+3*sd2
outl3= m1+3*sd3

# Negative outliers. If 3*sd > mean, then set the outlier to a small number close to 0. 

if (m1<3*sd1){
  outlneg1= 5 # choose 5 to be the negative outlier
  }else { outlneg1= m1-3*sd1
}

if (m2<3*sd2){
  outlneg2= 5 # choose 5 to be the negative outlier
  }else { outlneg2= m2-3*sd2
}

if (m3<3*sd3){
  outlneg3= 5 # choose 5 to be the negative outlier
}else { outlneg3= m3-3*sd3
}