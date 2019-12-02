#SCRIPT PAPER POLLINATION INCENTIVES


library(ggplot2)
library(fmsb)
library(scales)
library(gridExtra)
library(cowplot)
library(grid)

library(reshape2)
library(ggpubr)
library(gtable)



##################################################### ECOLOGICAL FUNCTIONS#####################################################


#carrying capacities bees
k_honeybees = 18900 #k_honeybees = phi (survival rate = 0.63) * dH (density per hive =30000)
k_wildbees = 5000 #see Montoya 2019 for carrying capacity and model of population (negative linear)
max_beehives = 5/7 #max beehive (500 per beekeeper) for 1 ha
damage = 0.5


#FUNCTION BEES
#Bees abundance per ha
# (1) absolute population
Bees = function(x1,x2,x3){
  x3 * k_honeybees + (1 - x1) * k_wildbees *(1 - x1 * x2^damage)} #the wildbees suffer a negative impact of pesticides, see Woodcock 2016

# (2) reduced population / assumption : Bees(0,0,max_beehives) is the maximum of the function
Bees_reduced = function(x1,x2,x3){
  Bees(x1,x2,x3)/Bees(0,0,max_beehives)
}



##################################################### ECONOMIC FUNCTIONS#####################################################


#AGRICULTURAL PRODUCTIONS

#OSR PRODUCTION

max_yield_OSR = 4.6 #Maximum OSR yield observed in Chizé (from data set)

#(1) Partial yield pollinators
alpha1 = 0.35 #Proportion of OSR yield attributed to bees interactions, see Perrot et al. 2018
beta1 = 0.5 #Half-saturation constant of bees (if 0.2, 20% in bee abundance allows to reach the half-maximum of partial yield)

f1_1 = function(x1,x2,x3){
  alpha1 * max_yield_OSR * (Bees_reduced(x1,x2,x3)/(Bees_reduced(x1,x2,x3) + beta1))
}

#(2) Partial yield agrochemicals
alpha2 = 0.35 #Proportion of OSR yield attributed to agrochemicals application, see Fernandez-Cornejo et al. 1998
beta2 = 0.5 #Half-saturation constant of agrochemicals #Gaba 2016

f1_2 = function(x2){
  alpha2 * max_yield_OSR * (x2/(x2+beta2))
}

#(3) Partial yield independent from both agrochemicals and bees

f1_3 = max_yield_OSR * (1 - alpha1 - alpha2)

#OSR PRODUCTION TOTAL
F1 = function(x1,x2,x3){
  x1 * (f1_1(x1,x2,x3) + f1_2(x2) + f1_3)
}

rho_OSR = function(x1,x2,x3){
  f1_1(x1,x2,x3) + f1_2(x2) + f1_3
}

#GRASSLANDS PRODUCTION

f1bis = 7 #Average grasslands yield per ha
phi = 0.1

F1bis = function(x1){   (1 - x1)^phi*f1bis}

#PROFIT FUNCTION - FARMERS

p1 = 480  #OSR output price
om1 = 1400 #marginal cost of land and others for OSR
om2 = 250 #marginal cost of agrochemicals for OSR / from gross margin data in Chizé && Guillermet 2015
p1bis = 100 #output price of grasslands
om1bis = 660 #marginal cost of land for grasslands

#Profit function
pi1 = function(x1,x2,x3){  p1 * F1(x1,x2,x3) + p1bis * F1bis(x1) - x1*om1 - (1-x1)*om1bis - x1*x2*om2} 

#Profit function with tax
pi1_tax = function(x1,x2,x3){  p1 * F1(x1,x2,x3) + p1bis * F1bis(x1) - x1*om1 - (1-x1)*om1bis - x1*x2*om2} - x2 * t1 * x1

#HONEY PRODUCTION

#Beehive yield (per hive)
a_k_0 = 27/4  ; a_k_1 = 27 ; a_k_2 = 315/4 ; a_k_3 = -225/2  #parameters of the beehive yield function (adapted to different references)
#neoclassical function of beehive yield
Y = function(x1){
  y = a_k_0 +  a_k_1 * x1 + a_k_2* x1^2 + a_k_3 * x1^3 
  return(y)
}


gamma = 0.4 #beehives elasticity/  see Vaziritabar, S. ;  Oshidari, S. ;  Aghamirkarimi, A. 2014 elasticity at 0.35 for capital ////
#0.36 to 0.4 in Siebert


#Honey production function / see Siebert 1980
F2 = function(x1,x2,x3){
  y= x3^gamma * (Y(x1)  * (1 - x1 * x2^damage)   )
  return(y)}


p2 = 7 #honey price
om3 = 100 #marginal cost of beehives

#beekeeper's profit function
pi2 = function(x1,x2,x3){ 
  y = p2 * F2(x1,x2,x3) - om3*x3 
  return(y)}

pi2_s1 = function(x1,x2,x3){ 
  y = p2 * F2(x1,x2,x3) - om3*x3 + s1_lim*x3
  return(y)}

pi2_s2 = function(x1,x2,x3){ 
  y = p2 * F2(x1,x2,x3) - om3*x3  + s2_lim*F2(x1,x2,x3)
  return(y)}


#best response function 

x3_max = function(x1,x2){
  ( om3 / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * p2))^(1/(gamma - 1))
}


##########################################ES MAGNITUDE INDICATORS + ECONOMIC INDICATORS ##############################################

#ECOSYSTEM SERVICES INDICATORS
es_1_1 =  6940000 #calorific power of OSR
es_1_2 = 3417000 #calorific power of grasslands
es_1_3 = 3270 #calorific power of honey
z = 0.25 #slope of the log of species richness

#function of ES magnitude
ecosystem_services = function(x1,x2,x3,S1,S2,T1){
  y1 = F1(x1,x2,x3) * es_1_1 + F1bis(x1) * es_1_2  + F2(x1,x2,x3) * es_1_3 #food provision
  y2 = (1 - x1)^z #species abundance (z calibration  Crawley and Harral 2001)
  y3 = Bees(x1,x2,x3)/x1 #pollination (pollinators abundance)
  y4 = 1 - x1 * x2  #water quality
  y5 = (1 - x1) * k_wildbees *(1 - x1 * x2^damage) #wild pollinators
  return(c(y1,y2,y3,y4,y5))}




########################################## FINDING THE TAX-SUBSIDIES LEVELS ########################################


#SUBSIDY 1 : HS
Subs1 = function(S) {
  x3_max_HS = function(x1,x2){
    ( (om3 - S) / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * p2))^(1/(gamma - 1))
  }
  f = function(x){y =  pi1(x[1],x[2],x3_max_HS(x[1],x[2]))
  return(y)}
  
  
  x0 = c(0.1,0.1)
  
  solution = optim(x0, f, control=list(fnscale= -1)  )
  
  area = solution$par[1]
  chem = solution$par[2]
  hives = x3_max_HS(area,chem)
  
  return(c(area,chem,hives))}

#SUBSIDY 2 : PS
Subs2 = function(S) {
  x3_max_PS = function(x1,x2){
    ( om3 / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * ( p2 + S ) ))^(1/(gamma - 1))
  }
  f = function(x){y =  pi1(x[1],x[2],x3_max_PS(x[1],x[2]))
  return(y)}
  
  
  x0 = c(0.1,0.1)
  
  solution = optim(x0, f, control=list(fnscale= -1)  )
  
  area = solution$par[1]
  chem = solution$par[2]
  hives = x3_max_PS(area,chem)
  
  return(c(area,chem,hives))}

#TAXATION : AT
Tax = function(S) {
  f = function(x){ y =  pi1(x[1],x[2],x3_max(x[1],x[2])) - S * x[2]*x[1]
  return(y)}
  
  
  x0 = c(0.1,0.1)
  
  solution = optim(x0,  f, control=list(fnscale= -1) )
  
  area = solution$par[1]
  chem = solution$par[2]
  hives = x3_max(area,chem)
  return(c(area,chem,hives))}


#PUBLIC INTERVENTION
tot_area = 700
#LEVEL OF TAX
t1 = 110

#PUBLIC SPENDING
Budget = 10000  #spending for one beekeeper

lambda_tax = 0.011 ; lambda_s1 = 0.021 ; lambda_s2 = 0.002 #transaction costs levels

#Pub_spend_Transac = function(x1 ,x2 ,x3 ,S1 ,S2 ,T1 ){
#  (- T1 * x2 * x1 * (1 - lambda_tax) + S1 * x3 * (1 + lambda_s1) + S2 * F2(x1,x2,x3) * (1 + lambda_s2) )*tot_area}

Pub_spend = function(x1 ,x2 ,x3 ,S1 ,S2 ,T1 ){
  (- T1 * x2 * x1  + S1 * x3  + S2 * F2(x1,x2,x3) )*tot_area
}

#LEVEL OF HS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs1(s)[1],Subs1(s)[2],Subs1(s)[3],s,0,0) < Budget){s = s + 0.5 }
s1_lim = s ; rm(s)

#LEVEL OF PS FOR FIXED BUDGET
s = 0
while(Pub_spend(Subs2(s)[1],Subs2(s)[2],Subs2(s)[3],0,s,0) < Budget){s = s + 0.01 }
s2_lim = s ; rm(s)

################################################## FIGURE 2 RADAR CHART ####################################


area = c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1])
chem = c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2])
hives = c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3])


Provision = ecosystem_services(area,chem,hives)[1:4]
Biodiversity = ecosystem_services(area,chem,hives)[5:8]
Poll = ecosystem_services(area,chem,hives)[9:12]
Water = ecosystem_services(area,chem,hives)[13:16]
Wild = ecosystem_services(area,chem,hives)[17:20]

I1 = ((Provision - Provision[1])/Provision[1])*100
I2 = ((Poll - Poll[1])/Poll[1])*100
I3 = ((Biodiversity - Biodiversity[1])/Biodiversity[1])*100
I4 = ((Wild - Wild[1])/Wild[1])*100
I5 = ((Water - Water[1])/Water[1])*100


Priv_Wealth = c(pi1(area[1],chem[1],hives[1]) + pi2(area[1],chem[1],hives[1]),
                pi1(area[2],chem[2],hives[2]) + pi2_s1(area[2],chem[2],hives[2]),
                pi1(area[3],chem[3],hives[3]) + pi2_s2(area[3],chem[3],hives[3]),
                pi1_tax(area[4],chem[4],hives[4]) + pi2(area[4],chem[4],hives[4]))

Tot_Wealth = c(pi1(area[1],chem[1],hives[1])*500 + pi2(area[1],chem[1],hives[1])*500 - Pub_spend(area[1],chem[1],hives[1],0,0,0),
               pi1(area[2],chem[2],hives[2])*500 + pi2_s1(area[2],chem[2],hives[2])*500 - Pub_spend(area[2],chem[2],hives[2],s1_lim,0,0),
               pi1(area[3],chem[3],hives[3])*500 + pi2_s2(area[3],chem[3],hives[3])*500 - Pub_spend(area[3],chem[3],hives[3],0,s2_lim,0),
               pi1_tax(area[4],chem[4],hives[4])*500 + pi2(area[4],chem[4],hives[4])*500 - Pub_spend(area[4],chem[4],hives[4],0,0,t1))

I6 = ((Priv_Wealth - Priv_Wealth[1])/Priv_Wealth[1])*100
I7 = ((Tot_Wealth - Tot_Wealth[1])/Tot_Wealth[1])*100


I_MATRIX_t2 = data.frame(cbind(I1,I2,I5,I4,I3,I7,I6))
colnames(I_MATRIX_t2) = c('Food/Feed provision','Pollination','\nWater          \nquality          ','Wildbees\nabundance','Plant species\nrichness','\n          Total\n          Wealth','Stakeholders\'\nWealth')

colors_vect = c("red","chartreuse2","darkgreen","darkblue")
colors_vect_indic = c(rep("darkorange",3),rep("darkolivegreen3",2),rep("darkslategray3",2))

max_sup = NA ; max_inf = NA
j = 1:4
for (i in 1:7){max_sup[i] = floor(max(I_MATRIX_t2[,i])/5 + 1)*5} ; min_inf = floor(min(I_MATRIX_t2)/5 )*5

data =  rbind(max_sup,min_inf,I_MATRIX_t2[j,])


data_modified = rbind(rep(1,7),rep(-0.5,7),rep(NA,7),rep(NA,7),rep(NA,7),rep(NA,7))
for (i in 3:6){
  for (j in 1:7){
    data_modified[i,j] = ifelse(data[i,j] > 0, data[i,j]/max_sup[j], (0.5*data[i,j])/abs(min_inf))}}


dev.off() ; 
#png("/home/jerome/Documents/Thèse GREThA 2018.2021/Paper sustainability JAE/JAE submit/Fig/Fig5a.png",width = 500, height = 500, units = "px")
radarchart2(data.frame(data_modified), #title = TeX('Bundle of ecosystem services'), 
            vlabels = colnames(I_MATRIX_t2),
            vlabcol = colors_vect_indic,
            #c('Water quality','Wildbees','Plant species \n richness','Food provision','Pollination','Private wealth','Total wealth'), 
            #seg = 10 + abs(min_inf/10),
            cglty = 1, cglwd=0.1,
            pcol = c(colors_vect[1],colors_vect[2],colors_vect[3],colors_vect[4]), 
            vlcex =  1, plwd = c(1.5,rep(2,4)), plty=c(3,1,1,1), pty = c(32,rep(21,4)), 
            paxislabels = paste('+',c(max_sup),'%'),
            caxislabels = c(paste(min_inf,'%'),NA,NA,NA,NA,NA),
            axistype=5, axislabcol = 'grey60' , calcex = 1.15, palcex = 1.15)

legend(x=0.9 ,y=-0.6, legend = c('BAU','HS','PS','AT'), title = 'Scenarios', y.intersp = 0.75 ,
       bty = "n", pch=1 , col=c(colors_vect[1:4]) ,  cex=0.9, pt.cex=1)

legend(x=1.3 ,y=-0.5052, legend = c('Ecosystem\nservice','Ecological','Economic'), title = 'Indicators           ', y.intersp = 0.43 ,x.intersp = 0.7,
       bty = "n", pch=16 , col= unique(colors_vect_indic),  cex=0.9, pt.cex=1)

rm(data,data_modified)



############################################## FIGURE 3 PLOT OF COSTS AND REVENUES ######################################## 
farmer_acreage = 70

#functions BAU
R1_BAU = function(x1){p1 * F1(x1,chem[1],x3_max_BAU(x1,chem[1])) + p1bis * F1bis(x1)} ; C1_BAU = function(x1){x1*om1 + (1-x1)*om1bis + x1*chem[1]*om2} 
R2_BAU = function(x3){p2 * F2(area[1],chem[1],x3)} ; C2_BAU = function(x3){om3*x3} ; 
x3_max_BAU = function(x1,x2){( om3 / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * p2))^(1/(gamma - 1))}
#functions HS
R1_HS = function(x1){p1 * F1(x1,chem[2],x3_max_HS(x1,chem[2])) + p1bis * F1bis(x1)} ; C1_HS = function(x1){x1*om1 + (1-x1)*om1bis + x1*chem[2]*om2}
R2_HS = function(x3){p2 * F2(area[2],chem[2],x3) + x3 * s1_lim} ; C2_HS = function(x3){om3  *x3}
x3_max_HS = function(x1,x2){( (om3 - s1_lim) / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * p2))^(1/(gamma - 1))}
#functions PS
R1_PS = function(x1){p1 * F1(x1,chem[3],x3_max_PS(x1,chem[3])) + p1bis * F1bis(x1)} ; C1_PS = function(x1){x1*om1 + (1-x1)*om1bis + x1*chem[3]*om2}
R2_PS = function(x3){(p2 + s2_lim) * F2(area[3],chem[3],x3)} ; C2_PS = function(x3){om3 *x3} ; 
x3_max_PS = function(x1,x2){( om3 / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * (p2 + s2_lim)))^(1/(gamma - 1))}
#functions AT
R1_AT = function(x1){p1 * F1(x1,chem[4],x3_max_AT(x1,chem[4])) + p1bis * F1bis(x1)} ; C1_AT = function(x1){x1*om1 + (1-x1)*om1bis + x1*chem[4]*(om2 + t1)}
R2_AT = function(x3){p2 * F2(area[4],chem[4],x3)} ; C2_AT = function(x3){om3*x3} ;
x3_max_AT = function(x1,x2){( om3 / (gamma * (Y(x1) * (1 - x1 * x2^damage)) * p2))^(1/(gamma - 1))}

#marginal functions farmer
R1_marg_BAU = function(x1) {p1*x1*((alpha1*max_yield_OSR*(-chem[1]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[1]^damage*x1)+(k_honeybees*p2*(1-chem[1]^damage*x1)*
                                                                                                                                (a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*(om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                                ((chem[1]^damage*om3)/(p2*(1-chem[1]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-(om3*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/(p2*(1-chem[1]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(
                                                                                                                                  om3*(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*
                                                                                                                                                      ((k_wildbees*(1-x1)*(1-chem[1]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))-(alpha1*
                                                                                                                                                                                                                                                                                                                                                   max_yield_OSR*(k_wildbees*(1-x1)*(1-chem[1]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))*
                                                                                                                                                                                                                                                                                                                                                   (-chem[1]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[1]^damage*x1)+(k_honeybees*p2*(1-chem[1]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*
                                                                                                                                                                                                                                                                                                                                                                                                                          (om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                                                                                                                                                                                                                                                                                                                          ((chem[1]^damage*om3)/(p2*(1-chem[1]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-(om3*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/(p2*(1-chem[1]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(
                                                                                                                                                                                                                                                                                                                                                                                                                            om3*(gamma-1))))/(((5*k_honeybees)/7+k_wildbees)^2*
                                                                                                                                                                                                                                                                                                                                                                                                                                                (beta1+(k_wildbees*(1-x1)*(1-chem[1]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7))^2))+p1*(
                                                                                                                                                                                                                                                                                                                                                                                                                                                  (-alpha2-alpha1+1)*max_yield_OSR+(chem[1]*alpha2*max_yield_OSR)/(beta2+chem[1])+
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (alpha1*max_yield_OSR*(k_wildbees*(1-x1)*(1-chem[1]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*((k_wildbees*(1-x1)*(1-chem[1]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[1]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))
                                                                                                                                                                                                                                                                                                                                                                                                                                                )-f1bis*p1bis*phi*(1-x1)^(phi-1)}
C1_marg_BAU = function(x1) {chem[1]*om2-om1bis+om1}
R1_marg_HS = function(x1) {p1*x1*((alpha1*max_yield_OSR*(-chem[2]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[2]^damage*x1)+(k_honeybees*p2*(1-chem[2]^damage*x1)*
                                                                                                                               (a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                               ((chem[2]^damage*(om3-s1_lim))/(p2*(1-chem[2]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-((om3-s1_lim)*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/(p2*(1-chem[2]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(
                                                                                                                                 (om3-s1_lim)*(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*
                                                                                                                                                              ((k_wildbees*(1-x1)*(1-chem[2]^damage*x1)+k_honeybees*((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))-(alpha1*
                                                                                                                                                                                                                                                                                                                                                                    max_yield_OSR*(k_wildbees*(1-x1)*(1-chem[2]^damage*x1)+k_honeybees*((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))*
                                                                                                                                                                                                                                                                                                                                                                    (-chem[2]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[2]^damage*x1)+(k_honeybees*p2*(1-chem[2]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*
                                                                                                                                                                                                                                                                                                                                                                                                                                           ((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                                                                                                                                                                                                                                                                                                                                           ((chem[2]^damage*(om3-s1_lim))/(p2*(1-chem[2]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-((om3-s1_lim)*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/(p2*(1-chem[2]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(
                                                                                                                                                                                                                                                                                                                                                                                                                                             (om3-s1_lim)*(gamma-1))))/(((5*k_honeybees)/7+k_wildbees)^2*
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          (beta1+(k_wildbees*(1-x1)*(1-chem[2]^damage*x1)+k_honeybees*((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7))^2))+p1*(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (-alpha2-alpha1+1)*max_yield_OSR+(chem[2]*alpha2*max_yield_OSR)/(beta2+chem[2])+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (alpha1*max_yield_OSR*(k_wildbees*(1-x1)*(1-chem[2]^damage*x1)+k_honeybees*((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*((k_wildbees*(1-x1)*(1-chem[2]^damage*x1)+k_honeybees*((om3-s1_lim)/(p2*(1-chem[2]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                          )-f1bis*p1bis*phi*(1-x1)^(phi-1)}
C1_marg_HS = function(x1) {chem[2]*om2-om1bis+om1+4}
R1_marg_PS = function(x1) {p1*x1*((alpha1*max_yield_OSR*(-chem[3]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[3]^damage*x1)+(k_honeybees*(p2+s2_lim)*
                                                                                                                               (1-chem[3]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                               ((chem[3]^damage*om3)/((s2_lim+p2)*(1-chem[3]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-
                                                                                                                                  (om3*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(om3*(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*
                                                                                                                                                                                                                                                                                        ((k_wildbees*(1-x1)*(1-chem[3]^damage*x1)+k_honeybees*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))-(
                                                                                                                                                                                                                                                                                          alpha1*max_yield_OSR*
                                                                                                                                                                                                                                                                                            (k_wildbees*(1-x1)*(1-chem[3]^damage*x1)+k_honeybees*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))*(-
                                                                                                                                                                                                                                                                                                                                                                                                                                                          chem[3]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[3]^damage*x1)+(k_honeybees*(p2+s2_lim)*(1-chem[3]^damage*x1)*
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 (chem[3]^damage*om3)/((s2_lim+p2)*(1-chem[3]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (om3*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(om3*(gamma-1))))/(((5*k_honeybees)/7+k_wildbees)^2*
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (beta1+(k_wildbees*(1-x1)*(1-chem[3]^damage*x1)+k_honeybees*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7))^2))+p1*(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           (-alpha2-alpha1+1)*max_yield_OSR+(chem[3]*alpha2*max_yield_OSR)/(beta2+chem[3])+(alpha1*max_yield_OSR*
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              (k_wildbees*(1-x1)*(1-chem[3]^damage*x1)+k_honeybees*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))))/(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                (k_wildbees+(5*k_honeybees)/7)*
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ((k_wildbees*(1-x1)*(1-chem[3]^damage*x1)+k_honeybees*(om3/((s2_lim+p2)*(1-chem[3]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1)))-f1bis*
    p1bis*phi*(1-x1)^(phi-1)}
C1_marg_PS = function(x1) {chem[3]*om2-om1bis+om1} 
R1_marg_AT = function(x1) {p1*x1*((alpha1*max_yield_OSR*(-chem[4]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[4]^damage*x1)+(k_honeybees*p2*(1-chem[4]^damage*x1)*
                                                                                                                               (a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*(om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                               ((chem[4]^damage*om3)/(p2*(1-chem[4]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-(om3*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/(p2*(1-chem[4]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(
                                                                                                                                 om3*(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*
                                                                                                                                                     ((k_wildbees*(1-x1)*(1-chem[4]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))-(alpha1*
                                                                                                                                                                                                                                                                                                                                                  max_yield_OSR*(k_wildbees*(1-x1)*(1-chem[4]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))*
                                                                                                                                                                                                                                                                                                                                                  (-chem[4]^damage*k_wildbees*(1-x1)-k_wildbees*(1-chem[4]^damage*x1)+(k_honeybees*p2*(1-chem[4]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)*
                                                                                                                                                                                                                                                                                                                                                                                                                         (om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))*
                                                                                                                                                                                                                                                                                                                                                                                                                         ((chem[4]^damage*om3)/(p2*(1-chem[4]^damage*x1)^2*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma)-(om3*(a_k_1+2*a_k_2*x1+3*a_k_3*x1^2))/(p2*(1-chem[4]^damage*x1)*(a_k_0+a_k_1*x1+a_k_2*x1^2+a_k_3*x1^3)^2*gamma))*gamma)/(
                                                                                                                                                                                                                                                                                                                                                                                                                           om3*(gamma-1))))/(((5*k_honeybees)/7+k_wildbees)^2*
                                                                                                                                                                                                                                                                                                                                                                                                                                               (beta1+(k_wildbees*(1-x1)*(1-chem[4]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7))^2))+p1*(
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (-alpha2-alpha1+1)*max_yield_OSR+(chem[4]*alpha2*max_yield_OSR)/(beta2+chem[4])+
                                                                                                                                                                                                                                                                                                                                                                                                                                                   (alpha1*max_yield_OSR*(k_wildbees*(1-x1)*(1-chem[4]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1))))/((k_wildbees+(5*k_honeybees)/7)*((k_wildbees*(1-x1)*(1-chem[4]^damage*x1)+k_honeybees*(om3/(p2*(1-chem[4]^damage*x1)*(a_k_3*x1^3+a_k_2*x1^2+a_k_1*x1+a_k_0)*gamma))^(1/(gamma-1)))/(k_wildbees+(5*k_honeybees)/7)+beta1))
                                                                                                                                                                                                                                                                                                                                                                                                                                               )-f1bis*p1bis*phi*(1-x1)^(phi-1)}
C1_marg_AT = function(x1) {chem[4]*(om2+t1)-om1bis+om1-12}

#marginal functions beekeeper
R2_marg_BAU = function(x3) {(1-area[1]*chem[1]^damage)*(area[1]^3*a_k_3+area[1]^2*a_k_2+area[1]*a_k_1+a_k_0)*p2*x3^(gamma-1)*gamma}
C2_marg_BAU = function(x3) {om3-1.5}
R2_marg_HS = function(x3) {(1-area[2]*chem[2]^damage)*(area[2]^3*a_k_3+area[2]^2*a_k_2+area[2]*a_k_1+a_k_0)*p2*x3^(gamma-1)*gamma + s1_lim}
C2_marg_HS = function(x3) {om3}
R2_marg_PS = function(x3) {(1-area[3]*chem[3]^damage)*(area[3]^3*a_k_3+area[3]^2*a_k_2+area[3]*a_k_1+a_k_0)*(s2_lim+p2)*x3^(gamma-1)*gamma}
C2_marg_PS = function(x3) {om3} 
R2_marg_AT = function(x3) {(1-area[4]*chem[4]^damage)*(area[4]^3*a_k_3+area[4]^2*a_k_2+area[4]*a_k_1+a_k_0)*p2*x3^(gamma-1)*gamma}
C2_marg_AT = function(x3) {om3}

###
###
###

colors.gains.loss = c('forestgreen','darkred')

#PLOTS OF GAINS AND LOSSES FOR FARMER
M = data.frame(x1 = seq(0,1.1,0.01))
M = data.frame(x1 = M$x1,
               x1.BAU = seq(0,area[1],length.out = length(M$x1)),
               x1.gain.HS = seq(area[1],area[2],length.out = length(M$x1)),
               x1.gain.PS = seq(area[1],area[3],length.out = length(M$x1)))



M = data.frame(x1 = M$x1, x1.BAU = M$x1.BAU, x1.gain.HS = M$x1.gain.HS, x1.gain.PS = M$x1.gain.PS,
               R1.marg.BAU = sapply(M$x1, FUN = R1_marg_BAU),
               C1.marg.BAU = sapply(M$x1, FUN = C1_marg_BAU),
               R1.marg.HS = sapply(M$x1, FUN = R1_marg_HS),
               C1.marg.HS = sapply(M$x1, FUN = C1_marg_HS),
               R1.marg.PS = sapply(M$x1, FUN = R1_marg_PS),
               C1.marg.PS = sapply(M$x1, FUN = C1_marg_PS),
               R1.marg.AT = sapply(M$x1, FUN = R1_marg_AT),
               C1.marg.AT = sapply(M$x1, FUN = C1_marg_AT),
               
               R1.BAU.marg.A1 = sapply(M$x1.BAU, FUN = R1_marg_BAU),
               R1.HS.marg.A1 = sapply(M$x1.BAU, FUN = R1_marg_HS),
               R1.PS.marg.A1 = sapply(M$x1.BAU, FUN = R1_marg_PS),
               R1.AT.marg.A1 = sapply(M$x1.BAU, FUN = R1_marg_AT),
               
               R1.HS.marg.A2 = sapply(M$x1.gain.HS, FUN = R1_marg_HS),
               C1.HS.marg.A2 = sapply(M$x1.gain.HS, FUN = C1_marg_HS),
               R1.PS.marg.A2 = sapply(M$x1.gain.PS, FUN = R1_marg_PS),
               C1.PS.marg.A2 = sapply(M$x1.gain.PS, FUN = C1_marg_PS))


M_farm_HS = data.frame(x1 = M$x1,
                      marg_rev_BAU = M$R1.marg.BAU,
                      marg_rev = M$R1.marg.HS,
                      marg_cost_BAU = M$C1.marg.BAU,
                      marg_cost = M$C1.marg.HS,
                      x1_BAU = M$x1.BAU,
                      x1_gain = M$x1.gain.HS,
                      cost_area_A2 = M$C1.HS.marg.A2,
                      rev_area_A2 = M$R1.HS.marg.A2,
                      revBAU_area_A1 = M$R1.BAU.marg.A1,
                      rev_area_A1 = M$R1.HS.marg.A1)

M_farm_PS = data.frame(x1 = M$x1,
                       marg_rev_BAU = M$R1.marg.BAU,
                       marg_rev = M$R1.marg.PS,
                       marg_cost_BAU = M$C1.marg.BAU,
                       marg_cost = M$C1.marg.PS,
                       x1_BAU = M$x1.BAU,
                       x1_gain = M$x1.gain.PS,
                       cost_area_A2 = M$C1.PS.marg.A2,
                       rev_area_A2 = M$R1.PS.marg.A2,
                       revBAU_area_A1 = M$R1.BAU.marg.A1,
                       rev_area_A1 = M$R1.PS.marg.A1)

M_farm_AT = data.frame(x1 = M$x1,
                       marg_rev_BAU = M$R1.marg.BAU,
                       marg_rev = M$R1.marg.AT,
                       marg_cost_BAU = M$C1.marg.BAU,
                       marg_cost = M$C1.marg.AT,
                       x1_BAU = M$x1.BAU,
                       revBAU_area_A1 = M$R1.BAU.marg.A1,
                       rev_area_A1 = M$R1.AT.marg.A1)



plot_farmer_HS = ggplot(M_farm_HS, aes(x = x1)) +
  geom_vline(xintercept = c(0,0.25,0.5,area[1],area[2],0.75), color = 'grey80', size = 0.25) + theme_classic() +
  coord_cartesian(xlim = c(0,0.75), ylim=c(650,1400)) +
  geom_line(aes(y = marg_rev, colour = 'HS' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_rev_BAU, colour = 'BAU' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_cost, colour = 'HS' , linetype = 'Marginal costs')) +
  geom_line(aes(y = marg_cost_BAU, colour = 'BAU' , linetype = 'Marginal costs')) +
  geom_ribbon(aes(x = x1_gain, ymin = cost_area_A2, ymax = rev_area_A2, fill = "Gains with\nstrategy change"), alpha = .9)  +
  geom_ribbon(aes(x = x1_BAU, ymin = rev_area_A1, ymax = revBAU_area_A1, fill = "Gains without\nstrategy change"), alpha = .8) + 
  scale_color_manual("Scenarios" , values = colors_vect[1:2], limits = c("BAU","HS")) + 
  scale_fill_manual('Gains',values = c('darkolivegreen2','darkolivegreen3')) + scale_linetype('') +
  labs(title = 'Indirect effects\n\n',x = 'OSR proportion' , y= expression(paste('Euros.',ha^{-1})),  
       colour = "Scenarios" , linetype = "Economic functions") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, vjust = 0.8, color=c(1,colors_vect[1],colors_vect[2],1)),
        axis.title.x = element_text(size = 10.5, vjust = 4),
        axis.title.y = element_text(size = 10.5, vjust = -18),
        plot.title = element_text(size = 12, hjust = -0.8, face = 'italic'),
        panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_x_continuous(labels = percent(c(0,area[1],area[2],0.75)), breaks = round(c(0,area[1]-0.02,area[2]+0.02,0.75),3))



plot_farmer_PS = ggplot(M_farm_PS, aes(x = x1)) +
  geom_vline(xintercept = c(0,0.25,0.5,area[1],area[3],0.75), color = 'grey80', size = 0.25) + theme_classic() +
  coord_cartesian(xlim = c(0,0.75), ylim=c(650,1400)) +
  geom_line(aes(y = marg_rev, colour = 'PS' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_rev_BAU, colour = 'BAU' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_cost, colour = 'PS' , linetype = 'Marginal costs')) +
  geom_line(aes(y = marg_cost_BAU, colour = 'BAU' , linetype = 'Marginal costs')) +
  geom_ribbon(aes(x = x1_gain, ymin = cost_area_A2, ymax = rev_area_A2, fill = "Gains with\nstrategy change"), alpha = .9)  +
  geom_ribbon(aes(x = x1_BAU, ymin = rev_area_A1, ymax = revBAU_area_A1, fill = "Gains without\nstrategy change"), alpha = .8) + 
  scale_color_manual("Scenarios" , values = c(colors_vect[1],colors_vect[3]), limits = c("BAU","PS")) + 
  scale_fill_manual('Gains',values = c('darkolivegreen2','darkolivegreen3')) + scale_linetype('') +
  labs(x = 'OSR proportion' , y = expression(paste('Euros.',ha^{-1})),  
       colour = "Scenarios" , linetype = "Economic functions") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, vjust = 0.8, color=c(1,colors_vect[1],colors_vect[3],1)),
        axis.title.x = element_text(size = 10.5, vjust = 4),
        axis.title.y = element_text(size = 10.5, vjust = -18),
        panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_x_continuous(labels = percent(c(0,area[1],area[3],0.75)), breaks = round(c(0,area[1]-0.02,area[3]+0.03,0.75),3))


plot_farmer_AT = ggplot(M_farm_AT, aes(x = x1)) +
  geom_vline(xintercept = c(0,0.25,0.5,area[1],area[4],0.75), color = 'grey80', size = 0.25) + theme_classic() +
  coord_cartesian(xlim = c(0,0.75), ylim=c(650,1400)) +
  geom_line(aes(y = marg_rev, colour = 'AT' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_rev_BAU, colour = 'BAU' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_cost, colour = 'AT' , linetype = 'Marginal costs')) +
  geom_line(aes(y = marg_cost_BAU, colour = 'BAU' , linetype = 'Marginal costs')) +
  geom_ribbon(aes(x = x1_BAU, ymin = revBAU_area_A1 , ymax = rev_area_A1, fill = "Loss"), alpha = .9) + 
  scale_color_manual("Scenarios" , values = c(colors_vect[1],colors_vect[4]), limits = c("BAU","AT")) + 
  scale_fill_manual('Gains',values = c('firebrick3')) + scale_linetype('') +
  labs(x = 'OSR proportion' , y= expression(paste('Euros.',ha^{-1})),  
       colour = "Scenarios" , linetype = "Economic functions") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, vjust = 0.8, color=c(1,colors_vect[1],colors_vect[4],1)),
        axis.title.x = element_text(size = 10.5, vjust = 4),
        axis.title.y = element_text(size = 10.5, vjust = -18),
        panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_x_continuous(labels = percent(c(0,area[4],area[1],0.75)), breaks = round(c(0,area[4]-0.03,area[1]+0.03,0.75),3))



#PLOTS OF GAINS AND LOSSES FOR BEEKEEPER
M = data.frame(x3 = seq(0,5/7,0.02))
M = data.frame(x3 = M$x3,
               x3.BAU = seq(0,hives[1],length.out = length(M$x3)),
               x3.gain.HS = seq(hives[1],hives[2],length.out = length(M$x3)),
               x3.gain.PS = seq(hives[1],hives[3],length.out = length(M$x3)),
               x3.gain.AT = seq(hives[1],hives[4],length.out = length(M$x3)))



M = data.frame(x3 = M$x3, x3.BAU = M$x3.BAU, x3.gain.HS = M$x3.gain.HS, x3.gain.PS = M$x3.gain.PS, x3.gain.AT = M$x3.gain.AT,
               R2.marg.BAU = sapply(M$x3, FUN = R2_marg_BAU),
               C2.marg.BAU = sapply(M$x3, FUN = C2_marg_BAU),
               R2.marg.HS = sapply(M$x3, FUN = R2_marg_HS),
               C2.marg.HS = sapply(M$x3, FUN = C2_marg_HS),
               R2.marg.PS = sapply(M$x3, FUN = R2_marg_PS),
               C2.marg.PS = sapply(M$x3, FUN = C2_marg_PS),
               R2.marg.AT = sapply(M$x3, FUN = R2_marg_AT),
               C2.marg.AT = sapply(M$x3, FUN = C2_marg_AT),
               
               C2.BAU.marg.A1 = sapply(M$x3.BAU, FUN = C2_marg_BAU),
               C2.HS.marg.A1 = sapply(M$x3.BAU, FUN = C2_marg_HS),
               R2.BAU.marg.A1 = sapply(M$x3.BAU, FUN = R2_marg_BAU),
               R2.HS.marg.A1 = sapply(M$x3.BAU, FUN = R2_marg_HS),
               R2.PS.marg.A1 = sapply(M$x3.BAU, FUN = R2_marg_PS),
               R2.AT.marg.A1 = sapply(M$x3.BAU, FUN = R2_marg_AT),
               
               R2.HS.marg.A2 = sapply(M$x3.gain.HS, FUN = R2_marg_HS),
               C2.HS.marg.A2 = sapply(M$x3.gain.HS, FUN = C2_marg_HS),
               
               R2.PS.marg.A2 = sapply(M$x3.gain.PS, FUN = R2_marg_PS),
               C2.PS.marg.A2 = sapply(M$x3.gain.PS, FUN = C2_marg_PS),
               
               R2.AT.marg.A2 = sapply(M$x3.gain.AT, FUN = R2_marg_AT),
               C2.AT.marg.A2 = sapply(M$x3.gain.AT, FUN = C2_marg_AT))


M_bee_HS = data.frame(hives = M$x3,
                    marg_rev_BAU = M$R2.marg.BAU,
                    marg_rev = M$R2.marg.HS,
                    marg_cost_BAU = M$C2.marg.BAU,
                    marg_cost = M$C2.marg.HS,
                    x3_BAU = M$x3.BAU,
                    x3_gain = M$x3.gain.HS,
                    cost_area_A2 = M$C2.HS.marg.A2,
                    rev_area_A2 = M$R2.HS.marg.A2,
                    revBAU_area_A1 = M$R2.BAU.marg.A1,
                    rev_area_A1 = M$R2.HS.marg.A1)


M_bee_PS = data.frame(hives = M$x3,
                     marg_rev_BAU = M$R2.marg.BAU,
                     marg_rev = M$R2.marg.PS,
                     marg_cost_BAU = M$C2.marg.BAU,
                     marg_cost = M$C2.marg.PS,
                     x3_BAU = M$x3.BAU,
                     x3_gain = M$x3.gain.PS,
                     cost_area_A2 = M$C2.PS.marg.A2,
                     rev_area_A2 = M$R2.PS.marg.A2,
                     revBAU_area_A1 = M$R2.BAU.marg.A1,
                     rev_area_A1 = M$R2.PS.marg.A1)


M_bee_AT = data.frame(hives = M$x3,
                     marg_rev_BAU = M$R2.marg.BAU,
                     marg_rev = M$R2.marg.AT,
                     marg_cost_BAU = M$C2.marg.BAU,
                     marg_cost = M$C2.marg.AT,
                     x3_BAU = M$x3.BAU,
                     x3_gain = M$x3.gain.AT,
                     cost_area_A2 = M$C2.AT.marg.A2,
                     rev_area_A2 = M$R2.AT.marg.A2,
                     revBAU_area_A1 = M$R2.BAU.marg.A1,
                     rev_area_A1 = M$R2.AT.marg.A1)



plot_beek_HS = ggplot(M_bee_HS, aes(x = hives)) +
  geom_vline(xintercept = c(0,hives[1],hives[2],5/7), color = 'grey80', size = 0.25) + theme_classic() +
  coord_cartesian(xlim = c(0,5/7), ylim=c(50,250)) +
  geom_line(aes(y = marg_rev, colour = 'HS' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_rev_BAU, colour = 'BAU' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_cost, colour = 'HS' , linetype = 'Marginal costs')) +
  geom_line(aes(y = marg_cost_BAU, colour = 'BAU' , linetype = 'Marginal costs')) +
  geom_line(aes(y=0, colour = 'PS')) + geom_line(aes(y=0, colour = 'AT')) +
  geom_ribbon(aes(x = x3_gain, ymin = cost_area_A2, ymax = rev_area_A2, fill = "Gains with\nstrategy change"), alpha = .9)  +
  geom_ribbon(aes(x = x3_BAU, ymin = rev_area_A1, ymax = revBAU_area_A1, fill = "Gains without\nstrategy change"), alpha = .8) + 
  geom_ribbon(aes(x = 0, ymin = 0, ymax = 0, fill = "Loss"), alpha = .8) + 
  scale_color_manual('Scenarios' , values = colors_vect[1:4], limits = c('BAU','HS','PS','AT')) + 
  scale_fill_manual(values = c('darkolivegreen2','darkolivegreen3','firebrick3')) + 
  labs(title = 'Direct effects\n\n' , x = 'Beehives' , y= expression(paste('Euros.',Beehive^{-1})),  
       colour = "Scenarios" , linetype = "Economic functions", fill = 'Gains/Losses') +
  theme(legend.position = "right", axis.text.x = element_text(size = 8, angle = 45, vjust = 0.8, color=c(1,colors_vect[1],colors_vect[2],1)),
        axis.title.x = element_text(size = 10.5, vjust = 4),
        axis.title.y = element_text(size = 10.5, vjust = -18),
        plot.title = element_text(size = 12, hjust = -0.8, face = 'italic'), 
        panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_x_continuous(labels = round(c(0,hives[1]*700,hives[2]*700,500),0), breaks = round(c(0,hives[1],hives[2],5/7),3)) 


plot_beek_PS =  ggplot(M_bee_PS, aes(x = hives)) +
  geom_vline(xintercept = c(0,hives[1],hives[3],5/7), color = 'grey80', size = 0.25) + theme_classic() +
  coord_cartesian(xlim = c(0,5/7), ylim=c(50,250)) +
  geom_line(aes(y = marg_rev, colour = 'PS' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_rev_BAU, colour = 'BAU' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_cost, colour = 'PS' , linetype = 'Marginal costs')) +
  geom_line(aes(y = marg_cost_BAU, colour = 'BAU' , linetype = 'Marginal costs')) +
  geom_ribbon(aes(x = x3_gain, ymin = cost_area_A2, ymax = rev_area_A2, fill = "Gains with\nstrategy change"), alpha = .9)  +
  geom_ribbon(aes(x = x3_BAU, ymin = rev_area_A1, ymax = revBAU_area_A1, fill = "Gains without\nstrategy change"), alpha = .8) + 
  scale_color_manual("Scenarios" , values = c(colors_vect[1],colors_vect[3]), limits = c("BAU","PS")) + 
  scale_fill_manual('Gains',values = c('darkolivegreen2','darkolivegreen3')) + scale_linetype('') +
  labs(x = 'Beehives' , y= expression(paste('Euros.',Beehive^{-1})),  
       colour = "Scenarios" , linetype = "Economic functions") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, vjust = 0.8, color=c(1,colors_vect[1],colors_vect[3],1)),
        axis.title.x = element_text(size = 10.5, vjust = 4),
        axis.title.y = element_text(size = 10.5, vjust = -18),
        panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_x_continuous(labels = round(c(0,hives[1]*700,hives[3]*700,500),0), breaks = round(c(0,hives[1],hives[3],5/7),3))


plot_beek_AT = ggplot(M_bee_AT, aes(x = hives)) +
  geom_vline(xintercept = c(0,hives[1],hives[4],5/7), color = 'grey80', size = 0.25) + theme_classic() +
  coord_cartesian(xlim = c(0,5/7), ylim=c(50,250)) +
  geom_line(aes(y = marg_rev, colour = 'AT' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_rev_BAU, colour = 'BAU' , linetype = 'Marginal revenues')) +
  geom_line(aes(y = marg_cost, colour = 'AT' , linetype = 'Marginal costs')) +
  geom_line(aes(y = marg_cost_BAU, colour = 'BAU' , linetype = 'Marginal costs')) +
  geom_ribbon(aes(x = x3_gain, ymin = cost_area_A2, ymax = rev_area_A2, fill = "Gains with\nstrategy change"), alpha = .9)  +
  geom_ribbon(aes(x = x3_BAU, ymin = rev_area_A1, ymax = revBAU_area_A1, fill = "Gains without\nstrategy change"), alpha = .8) + 
  scale_color_manual("Scenarios" , values = c(colors_vect[1],colors_vect[4]), limits = c("BAU","AT")) + 
  scale_fill_manual('Gains',values = c('darkolivegreen2','darkolivegreen3')) + scale_linetype('') +
  labs(x = 'Beehives' , y= expression(paste('Euros.',Beehive^{-1})),  
       colour = "Scenarios" , linetype = "Economic functions") +
  theme(legend.position = "none", axis.text.x = element_text(size = 8, angle = 45, vjust = 0.8, color=c(1,colors_vect[1],colors_vect[4],1)),
        axis.title.x = element_text(size = 10.5, vjust = 4),
        axis.title.y = element_text(size = 10.5, vjust = -18),
        panel.grid.major.x  = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.x = element_blank()) + 
  scale_x_continuous(labels = round(c(0,hives[1]*700,hives[4]*700,500),0), breaks = round(c(0,hives[1],hives[4],5/7),3))






M1 = data.frame(values = c(integrate(f = function(x1){R1_marg_HS(x1)}, lower = 0, upper = area[1])$value - integrate(f = function(x1){R1_marg_BAU(x1)}, lower = 0, upper = area[1])$value,
                          (integrate(f = Vectorize(function(x1){R1_marg_HS(x1)}), lower = area[1], upper = area[2])$value - integrate(f = Vectorize(function(x1){C1_marg_HS(x1)}), lower = area[1], upper = area[2])$value),
                          (integrate(f = Vectorize(function(x1){C1_marg_BAU(x1)}), lower = 0, upper = area[1])$value - integrate(f = Vectorize(function(x1){C1_marg_HS(x1)}), lower = 0, upper = area[1])$value ),
                          integrate(f = function(x3){R2_marg_HS(x3)}, lower = 0, upper = hives[1])$value - integrate(f = function(x3){R2_marg_BAU(x3)}, lower = 0, upper = hives[1])$value ,
                          (integrate(f = Vectorize(function(x3){R2_marg_HS(x3)}), lower = hives[1], upper = hives[2])$value - integrate(f = Vectorize(function(x3){C2_marg_HS(x3)}), lower = hives[1], upper = hives[2])$value ),
                          - s1_lim*hives[2]) * tot_area,
               gains_losses = c('A1','A1','A2','A1','A2','A3'), user = c(rep('Agriculture',3),rep('Beekeeping',2),'Policy maker'),scenario = rep('HS',6))
#A1 = aire déplacement vertical de la courbe (gain) #A2 = aire déplacement horizontal de l'équilibre (gain) #A3 = aire policy maker (loss)


M2 = data.frame(values = c(integrate(f = function(x1){R1_marg_PS(x1)}, lower = 0, upper = area[1])$value - integrate(f = function(x1){R1_marg_BAU(x1)}, lower = 0, upper = area[1])$value,
                          (integrate(f = Vectorize(function(x1){R1_marg_PS(x1)}), lower = area[1], upper = area[3])$value - integrate(f = Vectorize(function(x1){C1_marg_PS(x1)}), lower = area[1], upper = area[3])$value),
                          (integrate(f = Vectorize(function(x1){C1_marg_BAU(x1)}), lower = 0, upper = area[1])$value - integrate(f = Vectorize(function(x1){C1_marg_PS(x1)}), lower = 0, upper = area[1])$value ),
                          integrate(f = function(x3){R2_marg_PS(x3)}, lower = 0, upper = hives[1])$value - integrate(f = function(x3){R2_marg_BAU(x3)}, lower = 0, upper = hives[1])$value ,
                          (integrate(f = Vectorize(function(x3){R2_marg_PS(x3)}), lower = hives[1], upper = hives[3])$value - integrate(f = Vectorize(function(x3){C2_marg_PS(x3)}), lower = hives[1], upper = hives[3])$value ),
                          - s1_lim*hives[2])*tot_area,
               gains_losses = c('A1','A1','A2','A1','A2','A3'), user = c(rep('Agriculture',3),rep('Beekeeping',2),'Policy maker'),scenario = rep('PS',6))



M3 = data.frame(values = c(integrate(f = function(x1){R1_marg_AT(x1)}, lower = 0, upper = area[1])$value - integrate(f = function(x1){R1_marg_BAU(x1)}, lower = 0, upper = area[1])$value,
                          integrate(f = function(x3){R2_marg_AT(x3)}, lower = 0, upper = hives[1])$value - integrate(f = function(x3){R2_marg_BAU(x3)}, lower = 0, upper = hives[1])$value ,
                          (integrate(f = Vectorize(function(x3){R2_marg_AT(x3)}), lower = hives[1], upper = hives[4])$value - integrate(f = Vectorize(function(x3){C2_marg_AT(x3)}), lower = hives[1], upper = hives[4])$value ),
                          t1*area[4]*chem[4])*tot_area,
               gains_losses = c('A1bis','A1','A2','A3bis'), user = c('Agriculture',rep('Beekeeping',2),'Policy maker'),scenario = rep('AT',4))

#A1bis = aire déplacement vertical de la courbe (loss) ; #A3bis = aire policy maker (gain)

Mtot = rbind(M1,M2,M3)


G1 = ggplot(data=M1, aes(x=user, y=values, fill = gains_losses ,width = 0.25))  + theme_bw() + 
  geom_bar(stat="identity") + coord_flip(ylim = c(-30,70)*tot_area) + scale_fill_manual(values = c('darkolivegreen2', #A1
                                                                                                   'darkolivegreen3', #A2
                                                                                                   'firebrick3', #A1bis
                                                                                                   'firebrick3', #A3
                                                                                                   'darkolivegreen2' #A3bis
                                                                                                   ), labels = c('Gains without\nstrategy change',
                                                                                                                 'Gains with\nstrategy change',
                                                                                                                 'Loss')) + 
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits=c('Policy maker', 'Agriculture', 'Beekeeping'), 
                   labels = c('Policy maker', 'Agriculture\n(ten farmers)     ', 'Beekeeping\n(one beekeeper)     ')) +
  labs(title = 'HS scenario\n', x = '', y = expression(paste(Delta,'Wealth','(Euros)')), fill = 'Gains/Losses') + 
  theme(legend.position = 'none', axis.title.x = element_text(size = 10.5, vjust = 4), axis.text.x = element_text(size = 8),
        #axis.text.y = element_text(size = 10, angle = 45), 
        strip.text.x = element_blank(),
        axis.text.y = element_text(color = 'black', angle = 40, size = 9),
        plot.title = element_text(size = 13, face = 'plain', hjust = 0) ) 


G2 = ggplot(data=M2, aes(x=user, y=values, fill = gains_losses ,width = 0.25))  + theme_bw() + 
  geom_bar(stat="identity") + coord_flip(ylim = c(-30,70)*tot_area) + scale_fill_manual(values = c('darkolivegreen2', #A1
                                                                                                   'darkolivegreen3', #A2
                                                                                                   'firebrick3', #A1bis
                                                                                                   'firebrick3', #A3
                                                                                                   'darkolivegreen2' #A3bis
  )) + geom_hline(yintercept = 0) +
  scale_x_discrete(limits=c('Policy maker', 'Agriculture', 'Beekeeping'), 
                   labels = c('Policy maker', 'Agriculture\n(ten farmers)     ', 'Beekeeping\n(one beekeeper)     ')) +
  labs(title = 'PS scenario\n', x = '', y = expression(paste(Delta,'Wealth','(Euros)'))) + 
  theme(legend.position = 'none', axis.title.x = element_text(size = 10.5, vjust = 4), axis.text.x = element_text(size = 8),
        #axis.text.x = element_blank(),
        #axis.text.y = element_text(size = 10, angle = 45), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(color = 'black', angle = 40, size = 9),
        plot.title = element_text(size = 13, face = 'plain', hjust = 0) ) 


G3 = ggplot(data=M3, aes(x=user, y=values, fill = gains_losses ,width = 0.25))  + theme_bw() + 
  geom_bar(stat="identity") + coord_flip(ylim = c(-30,70)*tot_area) + scale_fill_manual(values = c('darkolivegreen2', #A1
                                                                                                   'firebrick3', #A2
                                                                                                   'darkolivegreen3', #A1bis
                                                                                                   'darkolivegreen2', #A3
                                                                                                   'darkolivegreen3' #A3bis
  )) + geom_hline(yintercept = 0) +
  scale_x_discrete(limits=c('Policy maker', 'Agriculture', 'Beekeeping'), 
                   labels = c('Policy maker', 'Agriculture\n(ten farmers)     ', 'Beekeeping\n(one beekeeper)     ')) +
  labs(title = 'AT scenario\n', x = '', y = expression(paste(Delta,'Wealth','(Euros)'))) + 
  theme(legend.position = 'none', axis.title.x = element_text(size = 10.5, vjust = 4), axis.text.x = element_text(size = 8),
        #axis.text.x = element_blank(),
        #axis.text.y = element_text(size = 10, angle = 45), 
        strip.text.x = element_blank(),
        axis.text.y = element_blank(),
        #axis.text.y = element_text(color = 'black', angle = 40, size = 9),
        plot.title = element_text(size = 13, face = 'plain', hjust = 0) ) 



ggarrange(G1,G2,G3,
          plot_beek_HS,plot_beek_PS,plot_farmer_AT,
          plot_farmer_HS,plot_farmer_PS,plot_beek_AT,  
          ncol = 3, nrow = 3, align = 'hv', legend = 'none',
          labels = c('(a)','(b)','(c)','(d)','(e)','(f)','(g)','(h)','(i)'), label.x = 0.25, label.y = c(rep(0.88,3),rep(0.92,6)), font.label = list(size = 12))



ggdraw() +
  draw_plot(plot_wealthes, 0, 0.66, 1, 0.33) +
  draw_plot(plot_beek_HS, 0, 0.33, 0.33, .33) +
  draw_plot(plot_beek_PS,  0.33, 0.33, 0.33, .33) +
  draw_plot(plot_farmer_AT, 0.66, 0.33, 0.33, .33) +
  draw_plot(plot_farmer_HS, 0, 0, 0.33, .33) +
  draw_plot(plot_farmer_PS, 0.33, 0, 0.33, .33) +
  draw_plot(plot_beek_AT, 0.66, 0, 0.33, .33) +
  draw_plot_label(c('(a)','(b)','(c)','(d)','(e)','(f)','(g)','(h)','(i)'), 
                  c(0.3, 0.62, 0.94,0.3, 0.62, 0.94,0.3, 0.62, 0.94), c(0.945, 0.945, 0.945,0.66,0.66,0.66,0.33,0.33,0.33),
                  size = 10.5)
  


direct_impacts = ggarrange(plot_beek_HS,plot_beek_PS,plot_farmer_AT, ncol = 3, labels = c('(d)','(e)','(f)'))

ggarrange(plot_wealthes,direct_impacts, nrow = 2, widths = c(1,0.7))


plot_grid(plot_beek_HS,plot_beek_PS,plot_farmer_AT)



ggarrange(plot.gains.HS, plot.gains.PS, plot.gains.AT, 
          plot2.HS, plot2.PS, plot2.AT,
          plot1.HS, plot1.PS, plot1.AT,
          ncol=3, nrow=3, common.legend = FALSE, legend="none")




####################################################### FIGURE 4 CAUSAL CHAIN #########################

#MARGINAL PRODUCTIVITY OF FARMER
prod_marg_farmer_x1 = function(x1,x2,x3){
  p1*((-alpha2-alpha1+1)*max_yield_OSR+(alpha2*x2*max_yield_OSR)/(x2+beta2)+
        (alpha1*(k_wildbees*(1-x1)*(1-x1*x2^damage)+k_honeybees*x3)*max_yield_OSR)/((k_wildbees+(5*k_honeybees)/7)*((k_wildbees*(1-x1)*(1-x1*x2^damage)+k_honeybees*x3)/(k_wildbees+(5*k_honeybees)/7)+beta1)))+p1*x1*(
          (alpha1*(-k_wildbees*(1-x1)*x2^damage-k_wildbees*(1-x1*x2^damage))*max_yield_OSR)/((k_wildbees+(5*k_honeybees)/7)*((k_wildbees*(1-x1)*(1-x1*x2^damage)+k_honeybees*x3)/(k_wildbees+(5*k_honeybees)/7)+beta1))-
            (alpha1*(-k_wildbees*(1-x1)*x2^damage-k_wildbees*(1-x1*x2^damage))*(k_wildbees*(1-x1)*(1-x1*x2^damage)+k_honeybees*x3)*max_yield_OSR)/(((5*k_honeybees)/7+k_wildbees)^2*(beta1+(k_wildbees*(1-x1)*(1-x1*x2^damage)+k_honeybees*x3)/(k_wildbees+(5*k_honeybees)/7))^2))-f1bis*p1bis*phi*
    (1-x1)^(phi-1)}

#MARGINAL PRODUCTIVITY OF BEEKEEPER
prod_marg_beekeeper_x3 = function(x1,x2,x3){(Y(x1))*(1-x1*x2^damage)*x3^(gamma-1)*gamma}


#PLOTS
M = data.frame(values = c(p2, p2 + s1_lim *(1/(F2(area[2],chem[2],hives[2])/hives[2])), p2 + s2_lim,
                          hives[1:3]))

M = data.frame(diff_values = c((M$values[2:3] - M$values[1])/M$values[1],
                               (M$values[5:6] - M$values[4])/M$values[4]),
               scenario = c('HS','PS'),
               variable = c(rep('rentability',2),rep('hives',2)))

M$scenario = factor(M$scenario,levels = c('HS','PS'))
M$variable = factor(M$variable,levels = c('rentability','hives'))


plot_1_1 = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.5) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-40,80)) +
  scale_x_discrete(name = '', labels = c('Honey kg \nrentability','Beehives number')) +
  
  scale_fill_manual(values = colors_vect[2:3], name = '') + 
  ggtitle('(a)') + theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
plot_1_1






M = data.frame(values = c(Bees(area[1],chem[1],hives[1])/area[1],
                          Bees(area[2],chem[2],hives[2])/area[2],
                          Bees(area[3],chem[3],hives[3])/area[3]))

M = data.frame(diff_values = c((M$values[2:3] - M$values[1])/M$values[1]), 
               scenario = c('HS','PS'),
               variable = rep('pollination',2))

M$scenario = factor(M$scenario,levels = c('HS','PS'))
M$variable = factor(M$variable,levels = c('pollination'))


plot_1_2 = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.25) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-40,80)) +
  scale_x_discrete(name = '', labels = c('Pollination level')) +
  
  scale_fill_manual(values = colors_vect[2:3], name = '') + 
  ggtitle('(b)') + theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
plot_1_2





M = data.frame(values = c(prod_marg_farmer_x1(area,chem,hives)[1:3],
                          chem[1:3],area[1:3]))

M = data.frame(diff_values = c((M$values[2:3] - M$values[1])/M$values[1],
                               (M$values[5:6] - M$values[4])/M$values[4],
                               (M$values[8:9] - M$values[7])/M$values[7]),
               scenario = c('HS','PS'),
               variable = c(rep('marginal productivity',2),rep('chem',2),rep('area',2)))

M$scenario = factor(M$scenario,levels = c('HS','PS'))
M$variable = factor(M$variable,levels = c('marginal productivity','chem','area'))


plot_1_3 = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.7) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-10,20)) +
  scale_x_discrete(name = '', labels = c('OSR hectare \nmarginal productivity','Agrochemicals \nrate','OSR area')) +
  
  scale_fill_manual(values = colors_vect[2:3], name = '') + 
  ggtitle('(c)') + theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
plot_1_3




M = data.frame(values = c(p1,p1 - t1*(1/(F1(area[4],chem[4],hives[4])/area[4])),chem[1],chem[4],area[1],area[4]))

M = data.frame(diff_values = c((M$values[2] - M$values[1])/M$values[1], 
                               (M$values[4] - M$values[3])/M$values[3],
                               (M$values[6] - M$values[5])/M$values[5]),
               scenario = c('AT'),
               variable = c('rentability','chem','area'))

M$scenario = factor(M$scenario,levels = c('AT'))
M$variable = factor(M$variable,levels = c('rentability','chem','area'))


plot_2_1 = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.25) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-30,20)) +
  scale_x_discrete(name = '', labels = c('OSR ton \nrentability','Agrochemicals rate','OSR area')) +
  
  scale_fill_manual(values = colors_vect[4], name = '') + 
  ggtitle('(d)') + theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
plot_2_1




M = data.frame(values = c(Bees(area[1],chem[1],hives[1])/area[1],
                          Bees(area[4],chem[4],hives[4])/area[4]))

M = data.frame(diff_values = c((M$values - M$values[1])/M$values[1]), 
               scenario = c('AT'),
               variable = 'pollination')

M$scenario = factor(M$scenario,levels = c('AT'))
M$variable = factor(M$variable,levels = c('pollination'))


plot_2_2 = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.125) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-30,20)) +
  scale_x_discrete(name = '', labels = c('Pollination level')) +
  
  scale_fill_manual(values = colors_vect[4], name = '') + 
  ggtitle('(e)') + theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
plot_2_2



M = data.frame(values = c(prod_marg_beekeeper_x3(area[1],chem[1],hives[1]),prod_marg_beekeeper_x3(area[4],chem[4],hives[1]),
                          hives[1],hives[4]))

M = data.frame(diff_values = c((M$values[2] - M$values[1])/M$values[1], 
                               (M$values[4] - M$values[3])/M$values[3]),
               scenario = c('AT'),
               variable = c('marginal productivity','hives'))

M$scenario = factor(M$scenario,levels = c('AT'))
M$variable = factor(M$variable,levels = c('marginal productivity','hives'))


plot_2_3 = ggplot(data= M , aes(x = variable , y = diff_values*100 , fill = scenario, width = 0.25) ) +
  geom_hline(yintercept = 0) +
  geom_bar( stat = 'identity' , position=position_dodge(), color="black") + theme_minimal()  + 
  scale_y_continuous(name = 'Variation from BAU (%)', limits = c(-30,20)) +
  scale_x_discrete(name = '', labels = c('Beehive \nmarginal productivity','Beehives number')) +
  
  scale_fill_manual(values = colors_vect[4], name = '') + 
  ggtitle('(f)') + theme(legend.position = 'none', plot.title = element_text(size = 12.5), axis.text = element_text(size = 11)) 
plot_2_3



ggarrange(plot_1_1, plot_1_2, plot_1_3, 
          plot_2_1, plot_2_2, plot_2_3,
          ncol=3, nrow=2, common.legend = TRUE, legend="none")

######################################## SENSIBILITY ANALYSIS ON PRODUCTION EFFICIENCY ###############################

a1 = c(0.2,0.5) #analysis  with - and + 50% of the parameter value.
b1 = c(0.25,0.75)
a2 = c(0.2,0.5)
b2 = c(0.25,0.75)

counter = 1 ; 

area = list() ; chem = list() ; hives = list() 
Provision = list() ; Biodiversity = list() ; Poll = list(); Water = list() ; Wild = list() 
Priv_Wealth = list(); Tot_Wealth = list() 
I1 = list() ; I2 = list() ; I3 = list() ; I4 = list() ; I5 = list() ; I6 = list() ; I7 = list() 

for (i1 in 1:2){
  beta2 = b2[i1]
  
  for (i2 in 1:2){
    alpha2 = a2[i2]
    
    for (i3 in 1:2){
      beta1 = b1[i3]
      
      for (i4 in 1:2){
        alpha1 = a1[i4]
        
        area[[counter]] = c(Subs1(0)[1],Subs1(s1_lim)[1],Subs2(s2_lim)[1],Tax(t1)[1])
        chem[[counter]] = c(Subs1(0)[2],Subs1(s1_lim)[2],Subs2(s2_lim)[2],Tax(t1)[2])
        hives[[counter]] = c(Subs1(0)[3],Subs1(s1_lim)[3],Subs2(s2_lim)[3],Tax(t1)[3])
        
        Provision[[counter]] = ecosystem_services(area[[counter]],chem[[counter]],hives[[counter]])[1:4]
        Biodiversity[[counter]] = ecosystem_services(area[[counter]],chem[[counter]],hives[[counter]])[5:8]
        Poll[[counter]] = ecosystem_services(area[[counter]],chem[[counter]],hives[[counter]])[9:12]
        Water[[counter]] = ecosystem_services(area[[counter]],chem[[counter]],hives[[counter]])[13:16]
        Wild[[counter]] = ecosystem_services(area[[counter]],chem[[counter]],hives[[counter]])[17:20]
        
        I1[[counter]] = ((Provision[[counter]] - Provision[[counter]][1])/Provision[[counter]][1])*100
        I2[[counter]] = ((Poll[[counter]] - Poll[[counter]][1])/Poll[[counter]][1])*100
        I3[[counter]] = ((Biodiversity[[counter]] - Biodiversity[[counter]][1])/Biodiversity[[counter]][1])*100
        I4[[counter]] = ((Wild[[counter]] - Wild[[counter]][1])/Wild[[counter]][1])*100
        I5[[counter]] = ((Water[[counter]] - Water[[counter]][1])/Water[[counter]][1])*100
        
        
        Priv_Wealth[[counter]] = c(pi1(area[[counter]][1],chem[[counter]][1],hives[[counter]][1]) + pi2(area[[counter]][1],chem[[counter]][1],hives[[counter]][1]),
                                   pi1(area[[counter]][2],chem[[counter]][2],hives[[counter]][2]) + pi2_s1(area[[counter]][2],chem[[counter]][2],hives[[counter]][2]),
                                   pi1(area[[counter]][3],chem[[counter]][3],hives[[counter]][3]) + pi2_s2(area[[counter]][3],chem[[counter]][3],hives[[counter]][3]),
                                   pi1_tax(area[[counter]][4],chem[[counter]][4],hives[[counter]][4]) + pi2(area[[counter]][4],chem[[counter]][4],hives[[counter]][4]))
        
        Tot_Wealth[[counter]] = c(pi1(area[[counter]][1],chem[[counter]][1],hives[[counter]][1])*500 + pi2(area[[counter]][1],chem[[counter]][1],hives[[counter]][1])*500 - Pub_spend(area[[counter]][1],chem[[counter]][1],hives[[counter]][1],0,0,0),
                                  pi1(area[[counter]][2],chem[[counter]][2],hives[[counter]][2])*500 + pi2_s1(area[[counter]][2],chem[[counter]][2],hives[[counter]][2])*500 - Pub_spend(area[[counter]][2],chem[[counter]][2],hives[[counter]][2],s1_lim,0,0),
                                  pi1(area[[counter]][3],chem[[counter]][3],hives[[counter]][3])*500 + pi2_s2(area[[counter]][3],chem[[counter]][3],hives[[counter]][3])*500 - Pub_spend(area[[counter]][3],chem[[counter]][3],hives[[counter]][3],0,s2_lim,0),
                                  pi1_tax(area[[counter]][4],chem[[counter]][4],hives[[counter]][4])*500 + pi2(area[[counter]][4],chem[[counter]][4],hives[[counter]][4])*500 - Pub_spend(area[[counter]][4],chem[[counter]][4],hives[[counter]][4],0,0,t1))
        
        I6[[counter]] = ((Priv_Wealth[[counter]] - Priv_Wealth[[counter]][1])/Priv_Wealth[[counter]][1])*100
        I7[[counter]] = ((Tot_Wealth[[counter]] - Tot_Wealth[[counter]][1])/Tot_Wealth[[counter]][1])*100
        
        I_MATRIX_t_SA = data.frame(cbind(I1[[counter]],I2[[counter]],I5[[counter]],I4[[counter]],I3[[counter]],I7[[counter]],I6[[counter]]))
        
        colnames(I_MATRIX_t_SA) = c('Food/Feed provision','Pollination','\nWater          \nquality          ','Wildbees\nabundance','Plant species\nrichness','\n          Total\n          Wealth','Actors\' Wealth')
        
        max_sup = NA ; max_inf = NA
        j = 1:4
        for (i in 1:7){max_sup[i] = floor(max(I_MATRIX_t_SA[,i])/5 + 1)*5} ; min_inf = floor(min(I_MATRIX_t_SA)/5 )*5
        
        data =  rbind(max_sup,min_inf,I_MATRIX_t_SA[j,])
        
        data_modified_SA = rbind(rep(1,7),rep(-0.5,7),rep(NA,7),rep(NA,7),rep(NA,7),rep(NA,7))
        for (i in 3:6){
          for (j in 1:7){
            data_modified_SA[i,j] = ifelse(data[i,j] > 0, data[i,j]/max_sup[j], (0.5*data[i,j])/abs(min_inf))}}
        
        
        dev.off() ; 
        #png("/home/jerome/Documents/Thèse GREThA 2018.2021/Paper sustainability JAE/JAE submit/Fig/Fig5a.png",width = 500, height = 500, units = "px")
        radarchart2(data.frame(data_modified_SA), #title = TeX('Bundle of ecosystem services'), 
                    vlabels = '',
                    vlabcol = colors_vect_indic,
                    #c('Water quality','Wildbees','Plant species \n richness','Food provision','Pollination','Private wealth','Total wealth'), 
                    #seg = 10 + abs(min_inf/10),
                    cglty = 1, cglwd=1.5,
                    pcol = c(colors_vect[1],colors_vect[2],colors_vect[3],colors_vect[4]), 
                    vlcex =  1, plwd = c(1.5,rep(8,4)), plty=c(3,1,1,1), pty = c(32,rep(21,4)), 
                    paxislabels = paste('+',c(max_sup),'%'),
                    caxislabels = c(paste(min_inf,'%'),NA,NA,NA,NA,NA),
                    axistype=5, axislabcol = 'grey60' , calcex = 1.15, palcex = 1.15)
        
        
        text(0.9,1.2,paste('alpha1 =',alpha1,'beta1 =',beta1,'\n','alpha2 =', alpha2, 'beta2 =',beta2))
        text(-1.2,1.2,paste('n =',counter))
        dev.print(device = png, file = paste('/home/jerome/Documents/Thèse GREThA 2018.2021/Paper sustainability JAE/JAE_paper_final/Supporting information/Sensibility_analysis/Sensibility_analysis_',counter), width = 750)
        
        counter = counter + 1 ;      rm(data,data_modified)
      }}}} 
alpha1 = 0.35 ; alpha2 = 0.35 ; beta1 = 0.5 ; beta2 = 0.5



