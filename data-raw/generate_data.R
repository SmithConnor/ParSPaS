set.seed(2021)
n = 100
p = 10
k = 1:p
beta = c(1, 2, -0.5, base::rep(x = 0,
                               times = p - 3))
x = base::matrix(data = stats::rnorm(n = n*p),
                 ncol = p)
colnames(x) = base::paste0("X", 1:p)
y = stats::rbinom(n = n,
                  size = 1,
                  prob = 1/(1+base::exp(-x %*% beta)))

exampleData = base::data.frame(x, y)
usethis::use_data(exampleData)
