numbers <- 2 : 99
numbers

devisors <- 2 : 9
devisors

is_divisible <- function(number, divisors)
{
  return (any(number %% divisors == 0))
}

count <- sum(sapply(numbers, is_divisible, devisors))
count
