check_lengths <- function(x, y, z, w) {
  len <- c(length(x), length(y), length(z), length(w))
  max_len <- max(len)

  # 각 벡터의 길이가 최대 길이이거나 1인지 확인
  all(len == 1 | len == max_len)
}

# 사용 예시
a <- 1:5
b <- 5
c <- 1:5
d <- 10

check_lengths(a, b, c, d)
#> [1] TRUE  (b의 길이가 1, 나머지는 길이가 5)

# 길이가 모두 다르고, 1도 아닌 경우
e <- 1
f <- 1
g <- 1:5
h <- 1:5
check_lengths(e, f, g, h)
