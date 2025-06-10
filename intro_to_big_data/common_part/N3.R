first_seq <- 1 / (1 : 50 * 2 : 51)
first_seq
sum(first_seq)

second_seq <- 1 / (2 ^ (0 : 20))
second_seq
sum(second_seq)

third_seq <- seq(1, 28, by = 3) / (3 ^ (0 : 9))
third_seq
sum(third_seq)

third_seq[third_seq > 0.5]
