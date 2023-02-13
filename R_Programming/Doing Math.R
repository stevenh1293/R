A <- matrix(1:100, nrow=10)  
B <- matrix(1:1000, nrow=10)

det(A)
det(B)

solve(A)
solve(B)

C <- matrix(sample(1:100), nrow = 10)
det(C)
inv_C <- solve(C)
inv_C
