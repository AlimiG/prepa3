library(tidyverse, warn.conflicts = FALSE)

x_axis = seq(-4,4,0.1)
normal_dist <- tibble(density = dnorm(x_axis),x_axis= x_axis)

t_dist <- tibble(density = dt(x_axis,3),x= x_axis)



p <- ggplot(normal_dist, aes(x = x_axis, y = density ))+
  geom_line()

colors = c("red","blue","green")
for (i in 1:10) {
 p <- p+ geom_line(data = tibble(density = dt(x_axis,2^i + 2*i -1),x= x_axis), aes(x = x_axis, y = density), linetype = "dashed", color = "Red")
}
  
p



normal <- tibble(
  x = seq(-4,4,0.1),
  y = dnorm(x)
)

sampling_data <- tibble(data = rnorm(20))

ggplot(data = sampling_data, aes(data))+
  geom_histogram(aes(y = ..density..), bins = 10)+
  geom_line(data = normal, aes(x,y))
