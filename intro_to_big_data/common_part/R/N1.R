# p <- seq(7, 4, by = -1)
p <- 7 : 4  # 7 6 5 4
p

# q <- seq(0, 3, by = 1)
q <- 0 : 3  # 0 1 2 3
q


sum_of_vectors <- p + q  # 7 7 7 7
sum_of_vectors


first_sub_of_vectors <- p - q  # 7 5 3 1
first_sub_of_vectors

second_sub_of_vectors <- q - p  # -7 -5 -3 -1
second_sub_of_vectors


prod_of_vectors <- p * q  # 0 6 10 12
prod_of_vectors


first_div_of_vectors <- p / q  # Inf 6.0 2.5 1.3
first_div_of_vectors

second_div_of_vectors <- q / p  # 0.00 0.17 0.40 0.75
second_div_of_vectors


first_pow_of_vectors <- p ^ q  # 1 6 25 64
first_pow_of_vectors

second_pow_of_vectors <- q ^ p  # 0 1 32 81
second_pow_of_vectors
