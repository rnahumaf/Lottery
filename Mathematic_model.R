# Probability of winning, based on type of game (aposta) and number of matches required (acerto)
prob <- function(aposta, acerto){
  choose(aposta,acerto)*choose(60-aposta,6-acerto)/choose(60, 6)
}
# Número de prêmios da quadra, para quem acertou a sena
funqua6 <- function(numb){
  ((numb-7)^2+(numb-7))*15/2
}
# Número de prêmios da quadra, para quem acertou a quina
funqua5 <- function(numb){
  ((numb-7)^2+3*(numb-7)+2)*5/2
}
# Número de prêmios da quadra, para quem acertou a quadra
funqua4 <- function(numb){
  (numb-5)*((numb-5)+1)/2
}
# Número de prêmios da quina, para quem acertou a sena
funqui6 <- function(numb){
  6*(numb-6)
}
# Número de prêmios da quina, para quem acertou a quina
funqui5 <- function(numb){
  numb-5
}
valor_aposta <- function(aposta){
  c(4.5, 31.5, 126, 378, 945, 2079, 4158, 7722, 13513.5, 22522.5)[aposta-5]
}
sena_fun <- function(numb = 6, 
                     premio = 50e6,
                     final_5 = F){
  prob_total <<- prob(numb,6) + prob(numb,5) + prob(numb,4)
  if(final_5){
    premio_quina <<- premio/470
  } else {
    premio_quina <<- premio/420
  }
  premio_quadra <- premio_quina*0.0013
  
    (1 - prob_total) * (-valor_aposta(numb))  + # Bilhete

    # Prob sena    
    prob(numb, 6) * (premio + funqui6(numb)*premio_quina + funqua6(numb)*premio_quadra) + # Sena
    
    # Prob quina
    prob(numb, 5) * (funqui5(numb)*premio_quina + funqua5(numb)*premio_quadra) +         # Quina
    
    # Prob quadra
    prob(numb, 4) * (funqua5(numb)*premio_quadra)                               # Quadra
  # Apresenta perda média por aposta (R$)
}
