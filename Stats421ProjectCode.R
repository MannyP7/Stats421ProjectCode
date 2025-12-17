# Emmanuel Pacheco Hernandez
# 30145004 

# Scenario 2(a)
#AI disclosure:
#Used ChatGPT to assist in clarifying instructions, debugging code and checking my methods and calculations. Also used for writing Rmarkdown code from my written work.
#
#finding the argmax of L(theta)
N0  = 496      
n0  = 4       
n1  = 2        


theta_vals = 4:494

lik_vals = choose(N0-theta_vals,n1)/choose(N0-n0,n1)*choose(theta_vals,n0)/choose(N0,n0)

theta_hat4 = theta_vals[which.max(lik_vals)]
theta_hat4

prob_hat = max(lik_vals)
prob_hat

################################################################################
#part b



bern_prob = function(n2) {
  p_hat = (n2 + 4) / (n2 + 6)    
  p_hat^(n2 + 4) * (1 - p_hat)^2  
}

f = function(x) bern_prob(x) - 0.001

root_res = uniroot(f, interval = c(1, 50))

n2_star = root_res$root 
n2_star

n2_ch = ceiling(n2_star)
n2_ch

p_hat = (n2_ch + 4) / (n2_ch + 6)
p_hat

theta_hat5 = N0 * p_hat        
theta_hat5

bern_prob_chosen = bern_prob(n2_ch)
bern_prob_chosen

hyper_prob = function(theta, n2) {
  num = choose(theta, 4) * choose(492 - theta, 2) * choose(theta - 4, n2)
  den = choose(496, 4) * choose(492, 2) * choose(490, n2)
  num/den
}

n2_vals = 0:20

results = data.frame(
  n2 = n2_vals,
  p_hat = (n2_vals + 4)/(n2_vals + 6)
)

results$theta_hat5 = N0 * results$p_hat
results$theta_int  = round(results$theta_hat5)


results$bern_prob = results$p_hat^(results$n2+4)*(1-results$p_hat)^2
results$hyper_prob = mapply(hyper_prob,theta = results$theta_int,n2 = results$n2)

results

plot(results$n2, results$bern_prob, type = "b",xlab = "n2", ylab = "Probability",main = "Bernoulli vs Hypergeometric (Scenario 2b)")
 lines(results$n2, results$hyper_prob, type = "b")
 legend("topright", legend = c("Bernoulli approx", "Hypergeometric exact"),lty = c(1,1), pch = c(1,1))

 
 ################################################################################
 ###part c
 
 
 
 N0 = 496
 n0 = 4
 n1 = 2      
 n2 = 20      

 

 theta_vals_1 = 4:494      
 theta_vals_2 = 24:496     

 
 p_V1_eq0_given_V0_eq4 = function(theta) {
   choose(496 - theta, 2) / choose(492, 2)
 }
 

 
 p_V1_given_V0_V2 = function(theta, v1_target = 0, v2_obs = 20) {
 
   N1 = 492

   m1 = theta - 4
   n1_clean = N1 - m1
   
   
   num_vals = c()
   v_vals   = 0:2
   
   for (v in v_vals) {
    
     p_V1_given_V0 = dhyper(x = v,
                            m = m1,
                            n = n1_clean,
                            k = n1)
     
  
   
     m2 = theta - 4 - v
     n2_clean = (N0 - theta) - (n1 - v) 
    
     p_V2_given_V0_V1 = dhyper(x = v2_obs,
                               m = m2,
                               n = n2_clean,
                               k = n2)
     
     num_vals = c(num_vals, p_V2_given_V0_V1 * p_V1_given_V0)
   }
   
   denom = sum(num_vals)
   
  
   v = v1_target
   
   p_V1_given_V0 = dhyper(x = v,
                          m = m1,
                          n = n1_clean,
                          k = n1)
   m2 = theta - 4 - v
   n2_clean = (N0 - theta) - (n1 - v)
   p_V2_given_V0_V1 = dhyper(x = v2_obs,
                             m = m2,
                             n = n2_clean,
                             k = n2)
   
   num = p_V2_given_V0_V1 * p_V1_given_V0
   
   num / denom
 }
 

 
 alpha = 0.01  
 target =alpha
 
 P_V0_eq4 = function(theta) {
   dhyper(x = 4, m = theta, n = N0 - theta, k = n0)
 }
 
 prob_V0_eq4_vals = P_V0_eq4(theta_vals_1)
 

 thetaL1_candidates = theta_vals_1[prob_V0_eq4_vals >= target]
 thetaL1 = min(thetaL1_candidates)
 
 
 pU1 = p_V1_eq0_given_V0_eq4(thetaL1)
 
 thetaL1
 pU1
 

 
 P_V0_eq4_V2_eq20 = function(theta) {
   p_V0 = P_V0_eq4(theta)  # P(V0=4)
   
   N1 = 492
   m1 = theta - 4
   n1_clean = N1 - m1
   
 
   if (m1 < 0 || n1_clean < 0 || m1 + n1_clean < n1) {
     return(0)
   }
   
   total = 0
   for (v in 0:2) {
    
     p_V1_given_V0 = dhyper(x = v,
                            m = m1,
                            n = n1_clean,
                            k = n1)
    
     m2 = theta - 4 - v
     n2_clean = (N0 - theta) - (n1 - v)
     
    
     if (m2 < 0 || n2_clean < 0 || m2 + n2_clean < n2) {
       next
     }
     
   
     p_V2_given_V0_V1 = dhyper(x = n2,
                               m = m2,
                               n = n2_clean,
                               k = n2)
     
     total = total + p_V0 * p_V1_given_V0 * p_V2_given_V0_V1
   }
   
   total
 }
 prob_V0_eq4_V2_eq20_vals = sapply(theta_vals_2, P_V0_eq4_V2_eq20)
 
 alpha  = 0.01
 target =alpha
 
 thetaL2_candidates = theta_vals_2[prob_V0_eq4_V2_eq20_vals >= target]
 thetaL2 = min(thetaL2_candidates)
 
 pU2 = p_V1_given_V0_V2(thetaL2)
 
 thetaL2
 pU2
 
 #plots to prove decreasing functions of theta
 
 theta_grid1 = theta_vals_1
 
 p1_grid = p_V1_eq0_given_V0_eq4(theta_grid1)
 
 plot(theta_grid1, p1_grid,
      type = "l",
      xlab = expression(theta),
      ylab = expression(P[theta](V[1] == 0 ~ "|" ~ V[0] == 4)),
      main = expression(P[theta](V[1] == 0 ~ "|" ~ V[0] == 4) ~ "vs" ~ theta))

 theta_grid2 = theta_vals_2 
 
 p2_grid = sapply(theta_grid2, function(th) p_V1_given_V0_V2(th, v1_target = 0))
 
 plot(theta_grid2, p2_grid,
      type = "l",
      xlab = expression(theta),
      ylab = expression(P[theta](V[1] == 0 ~ "|" ~ V[0] == 4, V[2] == 20)),
      main = expression(P[theta](V[1] == 0 ~ "|" ~ V[0] == 4, V[2] == 20) ~ "vs" ~ theta))

 

 ##################################################
 set.seed(4212025)
 theta0 = 331
 N0  = 496
 n0  = 4
 n1  = 2
 n2  = 20
 R1 = 1000  
 R2 = 1000
 ################

 alpha = 0.05 
 target = alpha
 #case 1
 
 P_V0_eq4 = function(theta) {
   dhyper(x = 4, m = theta, n = N0 - theta, k = n0)
 }
 
 prob_V0_eq4_vals = P_V0_eq4(theta_vals_1)
 
 
 thetaL1_candidates = theta_vals_1[prob_V0_eq4_vals >= target]
 thetaL1 = min(thetaL1_candidates)
 
 
 tpU1 = p_V1_eq0_given_V0_eq4(thetaL1)
 
 
 
 
 thetaL1
 tpU1
 
 #case2
 
 P_V0_eq4_V2_eq20 = function(theta) {
   p_V0 = P_V0_eq4(theta)  
   
   N1 = 492
   m1 = theta - 4
   n1_clean = N1 - m1
   
  
   if (m1 < 0 || n1_clean < 0 || m1 + n1_clean < n1) {
     return(0)
   }
   
   total = 0
   for (v in 0:2) {
    
     p_V1_given_V0 = dhyper(x = v,
                            m = m1,
                            n = n1_clean,
                            k = n1)
     
  
     m2 = theta - 4 - v
     n2_clean = (N0 - theta) - (n1 - v)
     
    
     if (m2 < 0 || n2_clean < 0 || m2 + n2_clean < n2) {
       next
     }
     
    
     p_V2_given_V0_V1 = dhyper(x = n2,
                               m = m2,
                               n = n2_clean,
                               k = n2)
     
     total = total + p_V0 * p_V1_given_V0 * p_V2_given_V0_V1
   }
   
   total
 }
 prob_V0_eq4_V2_eq20_vals = sapply(theta_vals_2, P_V0_eq4_V2_eq20)

 
 thetaL2_candidates = theta_vals_2[prob_V0_eq4_V2_eq20_vals >= target]
 thetaL2 = min(thetaL2_candidates)
 
 tpU2 = p_V1_given_V0_V2(thetaL2)
 
 thetaL2
 tpU2
 ########################

 
 #case 1:
 N1_true = N0 - n0
 m1_true = theta0 - 4
 n1_clean_true = N1_true - m1_true
 
 EP1_vals = numeric(R2)
 
 for (r in 1:R2) {
   
   v1_sim = rhyper(nn = R1,
                   m  = m1_true,
                   n  = n1_clean_true,
                   k  = n1)
   
   EP1_vals[r] = mean(v1_sim == 0)
 }
 

 ECL1 = sum(EP1_vals <=tpU1)/R2

 #case 2:
 v_vals = 0:2
 
 p_vec = sapply(v_vals, function(v1) {
   p_V1_given_V0_V2(theta0, v1_target = v1, v2_obs = n2)
 })
 p_vec = p_vec / sum(p_vec)   
 p_vec
 
 EP2_vals = numeric(R2)
 
 for (r in 1:R2) {
   
   v1_sim = sample(x = v_vals,
                   size = R1,
                   replace = TRUE,
                   prob = p_vec)
   
   
   EP2_vals[r] = sum(v1_sim == 0)/R1
 }

 
 
 ECL2 = sum(EP2_vals <= tpU2)/R2

 
 ECL1
 ECL2
 
 tpU1
 tpU2
 thetaL2
 thetaL1
 
 
 

