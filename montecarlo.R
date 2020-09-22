library(Rlab)

infection_rates <- c(1:9 * 1e-3, 1:10 * 1e-2)
pop_size <- 1e5
req_tests <- function(n, p){
  pop_size*(1/n + 1 - (1-p)^n)
}

optml_n <- function(p){
  if(p <= .01) {
    root <- optimize(function(n){req_tests(n, p)}, interval = c(0,50))$minimum
  } else {
    root <- optimize(function(n){req_tests(n, p)}, interval = c(0,20))$minimum
  }
  root.ceil <- ceiling(root)
  root.floor <- floor(root) 
  if(req_tests(root.ceil, p) >= req_tests(root.floor, p)){
    root.floor
  } else {
    root.ceil
  }
}

for(p in infection_rates) {
  total_batch_tests <- c()
  total_indiv_tests <- c() 
  for (sim in 1:100)
  {
    indiv_tests <- 0
    batch_tests <- 0
    population <- rbern(pop_size, p)
    actual_p <- sum(population==1)/pop_size
    n <- optml_n(actual_p)
    for(batch in seq(from=1, to=pop_size, by=n)){
      curr_batch <- population[batch:(batch+n-1)]
      batch_tests <- batch_tests + 1
      if(1 %in% curr_batch){
        if (any (is.na (curr_batch))) 
          curr_batch <- curr_batch[! is.na(curr_batch)]
        indiv_tests <- indiv_tests + length(curr_batch)
      }
    }
    total_batch_tests <- append(total_batch_tests, batch_tests)
    total_indiv_tests <- append(total_indiv_tests, indiv_tests)
  }
  cat("Given infection rate:", p, "\n")
  cat("Mean(standard dev.) batch tests:", 
      ceiling(mean(total_batch_tests)), 
      "(", ceiling(sd(total_batch_tests)), ")\n")
  cat("Mean(standard dev.) indiv tests:", 
      ceiling(mean(total_indiv_tests)), 
      "(", ceiling(sd(total_indiv_tests)), ")\n\n")
}