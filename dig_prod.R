digit_prod <- function(n) {
    if (n == 0)   {
        return(0)
    }
    
    else {
        n_prod = 1
        n_dig = log10(10*n)
        for (count_dig in 1:n_dig) {
            n_prod = n_prod*(n %% 10)
            n = floor(n/10)
            }
        return(n_prod)
    }
}

draw_prod <- function(x_num) {
    x = 0:x_num
    y = sapply(X = x, FUN = digit_prod)
    
    plot(x, y, type = 'n', xlim = c(0, x_num), xaxt = 'n', yaxt = 'n')
    lines(x, y, col = '#004C99')
    axis(1, at = 0:10 * x_num/10, cex.axis = 1.8)
    axis(2, cex.axis = 1.5)
}

par(mfrow=c(4,1), mar = c(2.25, 2.5, 0.1, 0.25))
draw_prod(100)
draw_prod(1000)
draw_prod(10000)
draw_prod(100000)