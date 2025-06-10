vec3 <- seq(3, 27, by = 3)  # 3 6 9 12 15 18 21 24 27
vec3


vec3[c(2, 5, 7)]  # 6 15 21

vec3[length(vec3) - 1]  # 24

vec3[-(length(vec3) - 1)]  # 3 6 9 12 15 18 21 27

vec3[-6] # 3 6 9 12 15 21 24 27

vec3[100] # NA

vec3[-c(1, length(vec3))]  # 6 9 12 15 18 21 24

vec3[vec3 > 4 & vec3 < 10]  # 6 9

vec3[vec3 < 4 | vec3 > 10]  # 3 12 15 18 21 24 27
