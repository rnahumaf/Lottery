# MEGA SENA LOTTERY
# PICK A SAMPLE
pick <- sample(c(1:60), 6, replace = F)
sorteio <- 2

# STARTING MONEY
dinheiro <- 0 

# NUMBER OF GAMES PLAYED
jogo <- 0

# LOTTERY FIELDS
ganho_quadra <- 538
ganho_quina <- 29000
ganho_sena <- 50e+6
senas <- 0
quinas <- 0
quadras <- 0

# INFINITE LOOP
repeat{
  jogo <- jogo + 1
  dinheiro <- dinheiro - 4.5
  sorteio <- sample(c(1:60), 6, replace = F)
  
  # IS THERE A MINIMUM MATCH?
  if(sum(sorteio %in% pick)>=4){
    if(sum(sorteio %in% pick)==4){
      dinheiro <- dinheiro + ganho_quadra
      quadras <- quadras + 1
      next
    } else {
      if(sum(sorteio %in% pick)==5){
        dinheiro <- dinheiro + ganho_quina
        quinas <- quinas + 1
        next
      } else {
          dinheiro <- dinheiro + ganho_sena
          senas <- senas + 1
          print(dinheiro/jogo)
          next
      }
    }
    
  }
}
