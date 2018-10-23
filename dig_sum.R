digit_sum <- function(n) {
    if (n == 0)   {
        return(0)
    }
    
    else {
        n_sum = 0
        n_dig = log10(10*n)
        for (count_dig in 1:n_dig) {
            n_sum = n_sum + (n %% 10)
            n = floor(n/10)
            }
        return(n_sum)
    }
}

draw_sum <- function(x_num) {
    x = 0:x_num
    y = sapply(X = x, FUN = digit_sum)

    plot(x, y, type = 'n', xlim = c(0, x_num), xaxt = 'n', yaxt = 'n')
    lines(x, y, col = '#004C99')
    axis(1, at = 0:10 * x_num/10, cex.axis = 1.8)
    axis(2, cex.axis = 1.5)
}

par(mfrow=c(4,1), mar = c(2.25, 2.5, 0.1, 0.25))
draw_sum(100)
draw_sum(1000)
draw_sum(10000)
draw_sum(100000)