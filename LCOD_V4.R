## These R codes are "Licensed under the Academic Free License version 3.0". ## These R codes are provided for the research paper entitled "Levelized cost of driving for medium and heavy-duty battery electric trucks"
## We refer to this article for further information about the methodology used in the codes
## Please, first, run all the codes by using Ctrl A and Ctrl Entre.
## Running all the following codes might take a while (maybe around 1 h depending on the CPU)
## The figures in the paper will be produced by running the plots according to the name of figures in the paper
## Second, in the command line write the name of plots one by one and use Ctrl Entre to generate plots.
## The name of plots in the order of appearing in the paper are:  
## Fig.2, Fig.3, Fig.A9, Fig.A10, Fig.4, Fig.A11, Fig.A12, Fig.5, Fig.A13, Fig.A14, Fig.A15, Fig.A16, Fig.6, Fig.A17, Fig.A18

###### clean up all objects and graphics ######
rm(list = ls())
graphics.off()

###### set working directory ######
#setwd("Put the working directory here")


###### libraries ######
library("ggplot2")
library("ggsci")
library("cowplot")
library("data.table")
library("ggrepel")
#install.packages("ggrepel")

###### setting up the model ######

###### descriptions ######
BatCha_scenarios <- c("S1","S2","S3","S1+-")






###### Defining functions ######
# function for levelized cost of driving (LCOD) of a CT and BET
LCOD_all <- function(i, GVW, ATRe, R, PE, PD, DVKMT, UR, LUN, OPC){

  ###### Input variables ######
  #i    <- 1                  # Set battery specification scenarios 1:3 for S1 to S3 and set 4 for the S1 with uncertainty
  #GVW  <- 40                 # Gross vehicle weight
  #ATRe <- 10*U[7]            # Annual average ambient temperature in °C
  #R    <- 300*U[40]          # Full charge Operational driving range of a BET in km
  #PE   <- 0.122*U[18]        # Price of electricity per kWh consumption
  #PD   <- 1.57*U[19]         # Price of fuel (diesel) consumption per liter
  #DVKMT<- 500*U[2]           # Daily vehicle kilometers travel in km
  #UR   <- 1                  # Payload utilization rate of a CT
  #LUN  <- 0.06*U[27]         # Load and unload activities (numbers of stops in daily trip destinations) per km  

  # Output of this function are the levelized cost of deriving (LCOD) per km and tkm for battery electirc trucks (BETs) and conventional trucks (CTs)
  
  
  
  
  ####### Model parameters #######
  #U[1:43]                          # A variable to set uncertainties over 43 variables
  

  ####### Parameters with uncertainty ####### 
  N    <- 10*U[1]            # Time frame for TCO analysis in year
  VKMT <- DVKMT*260          # Annual vehicle kilometers travel in km
  d    <- 0.05*U[3]          # Discount rate for NPV in TCO
  RChd <- 0+U[4]#0.0         # Random variable for drive distance of on-road charging deviation in km
  OChd <- 0.66*U[5]          # On-road charging distance ratio based on the proportion of BET range
  ACL  <- 0.85*U[6]          # Adjustment factor for average battery capacity lost during the battery lifetime  
  ETD1 <- 0.025*U[8]         # Energy consumption ratio due to 1 °C change of ambient temperature
  chcpF <- 0.07*U[9]         # Fast charger cost per kW per hour in USD/kW/h
  chcpS <- 0.03*U[9]         # Slow charger cost per kW per hour in USD/kW/h
  ChPS  <- 50                # Slow charger power in kW
  nCh  <- 0.9*U[10]          # Energy efficiency for the charging equipment
  r    <- 0.06*U[11]         # Annual interest rate
  M    <- 5*U[12]            # Total number of loan payments in year
  DPr  <- 0.12*U[13]         # Loan's down payment ratio for purchasing a BET or CT
  VP40 <- 105798*U[14]       # Purchase price of a base vehicle (CT) with 40 t GVW
  icbv <- 0.05*U[15]         # Insurance cost of a base vehicle (CT) with 40 t GVW per km
  RLU  <- 0.5*U[17]          # Random time variable for loading/unloading activity in h
  Rmcr <- 0.6*U[20]          # Maintenance cost ratio of a BET over a CT 
  mcr  <- 1.15e-06*U[21]     # Maintenance cost ratio of a CT per km based on its purchase price
  DCh  <- 30*U[22]           # Driving cost per working hour (time spent on refueling/charging is considered as working hours)
  Kr   <- 0.15*U[23]         # Refurbishment cost factor of battery pack
  Ku   <- 0.15*U[24]         # Used product discount factor for battery pack 
  PA   <- -0.094*U[25]       # Parameters value for effect of ages on residual value of a vehicle
  PM   <- -5e-4*U[26]        # Parameters value for effect of mileage on residual value of a vehicle
  z0     <- 6.04                       # Parameters for Relative powertrain energy consumption ratio E_BETOCT model 
  z1     <- -0.009                     # Parameters for Relative powertrain energy consumption ratio E_BETOCT model
  vFf    <- 80*U[16]                         # Driving speed for free flow drive cycle in Km/h
  vU    <- 20*U[38]                         # Driving speed for urban drive cycle in Km/h
  Ff     <- 0.75*U[39]                       # Fraction of time driving with free flow speed
  ADS    <- (Ff*vFf+(1-Ff)*vU)  # Average driving speed of vehicles in road network in km/h
  GPM    <- 3* U[42]            # Gross profit margin of a trucking company
  OPC    <- 0 + U[43]/100       # Opportunity charging potential (%) during loading/unloading and rest time. The default is 0% but we set a sensitivity analysis up to 100% 
  
  ###### Constant parameters ######
  DDis <- 824                # Density of mixed conventional diesel fuel in kg/m3
  LHV  <- 11.94              # Lower heating value of diesel fuel in kWh/kg
  Eura <- 0.909              # Euro 5/6 standard emission ratio for estimating the diesel consumption volume of a CT
  ATDe <- 15                 # Default ambient temperature in °C
  
  
  ###### Parameters in a range as a function with uncertainty ######
  #VPCT is CT's purchase price in USD based on GVW of a CT in ton
  VPCT <- function(GVW){
    VPCTV <- (43526*log(GVW)-54764)*U[28]
    return(VPCTV)
  }
  

  #E_BETOCT is Relative powertrain energy consumption ratio by CTs compared to BETs based on GVW of a CT in ton 
  
  E_BETOCT <- function(GVW){
    #E_BETOCTV <- 2.5*U[29]
    
    if (GVW >= 40){
      E_BETOCTFf   <- (z0 * exp(vFf * z1)) * (2.5/3.10191)               # With adjustment factor for conservative value in HDTs 
      E_BETOCTU   <- (z0 * exp(vU * z1)) * (2.5/3.10191)               # With adjustment factor for conservative value in HDTs
      E_BETOCTV    <- (E_BETOCTFf*vFf*(1/ADS)*Ff+E_BETOCTU*(1-vFf*(1/ADS)*Ff))
      
    } else if (GVW < 40 & GVW > 8){
      Adj1 <- U[29]*{(2.5-3.5)*(GVW-8)/(40-8)+3.5}                 # Calculated adjustment factor
      
      E_BETOCTFf   <- (z0 * exp(vFf * z1)) * (Adj1/3.10191)               # With adjustment factor for conservative value in MDTs  
      E_BETOCTU   <- (z0 * exp(vU * z1)) * (Adj1/3.10191)               # With adjustment factor for conservative value in MDTs
      E_BETOCTV    <- (E_BETOCTFf*vFf*(1/ADS)*Ff+E_BETOCTU*(1-vFf*(1/ADS)*Ff))
      
      
    } else {
      E_BETOCTFf   <- (z0 * exp(vFf * z1)) * (3.5/3.10191)               # With adjustment factor for conservative value in MDTs  
      E_BETOCTU   <- (z0 * exp(vU * z1)) * (3.5/3.10191)               # With adjustment factor for conservative value in MDTs
      E_BETOCTV    <- (E_BETOCTFf*vFf*(1/ADS)*Ff+E_BETOCTU*(1-vFf*(1/ADS)*Ff))
      
    }
    return(E_BETOCTV*U[29])
  }
  

  # Average diesel fuel consumption for driving cycle in litre per km
  Vd_km <- function(GVW,lc,UR){
    VdU_km     <- 0.057767*((GVW+lc*(UR-1))^0.6672)*Eura                   #diesel fuel consumption for free flow speed
    VdFf_km     <- 0.059463*((GVW+lc*(UR-1))^0.5515)*Eura                   #diesel fuel consumption for urban/saturated speed     
    Vd_kmV      <- VdFf_km*vFf*(1/ADS)*Ff+VdU_km*(1-vFf*(1/ADS)*Ff)        
    return(Vd_kmV*U[30])
  }

  #AF_BETOCT is Adjustment factor for correcting the purchase price difference between a BET and a CT price (battery pack price excluded) based on GVW of a CT in ton
  AF_BETOCT <- function(GVW){
    AF_BETOCTV <- 0.85*U[31]
    

    return(AF_BETOCTV)
  }

  ####### Battery and charger specifications scenarios (S1, S2, and S3) #######
  #bpp        #Battery pack purchase price per kWh
  #GDBP       #Gravimetric density or specific energy of battery pack in Wh/kg
  #CC         #Battery pack lifetime charging cycles
  #ChPF        #Fast charging power for on-road charging activities in kW
  #BUSOC      #Usable state of charge (SOC) for the battery pack
  
  if (i == 1) {
    bpp   <- 300 
    GDBP  <- 125
    CC    <- 3000
    ChPF   <- 200
    BUSOC <- 75
  } else if (i == 2) {
    bpp   <- 200 
    GDBP  <- 250
    CC    <- 4500
    ChPF   <- 450
    BUSOC <- 78
  } else if (i == 3) {
    bpp   <- 100 
    GDBP  <- 400
    CC    <- 6000
    ChPF   <- 1000
    BUSOC <- 95
  } else if (i == 4){
    bpp   <- 300*U[32] 
    GDBP  <- 125*U[33]
    CC    <- 3000*U[34]
    ChPF   <- 200*U[35]
    BUSOC <- 75*U[36]
  }
  
  ###### Additional equations ######
  
  # Iteration to estimate BPC
  BPC    <- 10
  BPCi   <- 1
  dBPC   <- 2
  
  while(dBPC > 1){
    Chd       <- RChd*OChd*R^-1                      # Deviation drive ratio for on-road charging activity
    Vdf_km     <- Vd_km(GVW,0,1)                     # Diesel consumption volume of a full load CT
    ECTf_km    <- DDis*LHV*Vdf_km*0.001              # a full load CT's tank-to-wheel (TTW) energy consumption per km
    ws        <- U[37]*0.00696*(6.46*GVW+115)        # Weight saving of BET vs. CT powertrain (except to battery pack but including fuel tank)
    EATAR     <- 1+(ATDe-ATRe)*ETD1                  # Energy consumption adjustment of electric trucks for the regional ambient temperature change
    dGVW      <- BPC[BPCi]*(GDBP^-1)-ws              # Curb weight difference of a BET and a CT
    Vdf_kmdW   <- Vd_km(GVW+dGVW,0,1)-Vd_km(GVW,0,1) # Equivalent extra diesel consumption volume for DelWBET-CT extra weight for full load CT
    ECTdWf_km  <- DDis*LHV*Vdf_kmdW*0.001            # CT's TTW energy consumption of DelWBET-CT extra weight per km for full load CT
    EBETf_km   <- ((ECTf_km + ECTdWf_km)/
                    E_BETOCT(GVW))*
      EATAR/nCh                                      # BET's tank-to-wheel (TTW) energy consumption per km for full load BET
    BPC1 <- R*EBETf_km /
      (BUSOC*ACL/100)                                # Nominal Battery pack capacity
    BPC <- cbind(BPC,BPC1) 
    dBPC<- BPC[BPCi+1]-BPC[BPCi]
    BPCi <- BPCi+1
  }
  BPC <- BPC[BPCi]
  
  
  lc        <- 0.7087*GVW-3.0451                    # Load capacity for a CT (and BET)
  ECT_km    <- DDis*LHV*Vd_km(GVW,lc,UR)*0.001      # CT's tank-to-wheel (TTW) energy consumption per km
  dGVW      <- BPC*(GDBP^-1)-ws                     # Curb weight difference of a BET and a CT
  Vd_kmdW   <- Vd_km(GVW+dGVW,lc,UR)-Vd_km(GVW,lc,UR)  # Equivalent extra diesel consumption volume for DelWBET-CT extra weight
  
  ECTdW_km  <- DDis*LHV*Vd_kmdW*0.001               # CT's TTW energy consumption of DelWBET-CT extra weight per km
  EBET_km   <- ((ECT_km + ECTdW_km)/
                  E_BETOCT(GVW))*
                  EATAR/nCh                         # BET's tank-to-wheel (TTW) energy consumption per km

  BPP       <- bpp*BPC                              # Battery pack price
  VPBET     <- AF_BETOCT(GVW)*VPCT(GVW)+BPP         # BET's purchase price
  tCh       <- BUSOC*ACL*0.01*BPC*
                          (ChPF^-1)*(nCh)           # Recharging time in h
  CECS_km    <- ChPS*chcpS*tCh/R                    # Slow Charger equipment cost per km for a 50 kW depot charger with 50% occupancy rate#second equation
  CECF_km    <- ChPF*chcpF*tCh/R                    # Fast Charger equipment cost per km #second equation
  LP_BET0  <- (1-DPr)*VPBET                         # Loan principal of a BET's purchase in year 0
  LP_CT0   <- (1-DPr)*VPCT(GVW)                     # Loan principal of a CT's purchase in year 0
  ICBET_km  <- VPBET*(VP40^-1)*icbv                 # Insurance cost of a BET per km
  ICCT_km   <- VPCT(GVW)*(VP40^-1)*icbv             # Insurance cost of a CT per km
  tCTd      <- DVKMT*(ADS^-1)*60                    # Time spent on driving in min
  tChd      <- RChd*(OChd^-1)*(R^-1)*
    DVKMT*(ADS^-1)*60                               # Time spent on deviation drive for on-road charging in min
  
  
  # calculation of energy provided by fast charger and slow charger #
  E_fast    <- EBET_km*DVKMT-BUSOC*ACL*BPC*0.01  
  if (E_fast < 0){
    E_fastR <-0
    E_slowR <-1
  } else {
    E_fastR  <- (EBET_km*DVKMT-BUSOC*ACL*BPC*0.01)/(EBET_km*DVKMT)
    E_slowR  <- (BUSOC*ACL*BPC*0.01)/(EBET_km*DVKMT)
  }
  
  tOCh      <- (E_fastR*EBET_km*DVKMT)*(ChPF^-1)*
    (nCh^-1)*60                                    # Time spent on on-road charging stations by a BET in min
  tLU       <- RLU*LUN*DVKMT*60                    # Time spent on loading and unloading activities in different destinations 
  
  if (tOCh - (45 + tLU)*OPC < 0){
    tOCh2 <-0
  } else {
    tOCh2 <- tOCh - (45 + tLU)*OPC
  }                                                # Extra time spent on charging based on opportunity charging in the 45 min rest time and loading/unloading activities (Overlap in rest time, loading/uloading and charging activities) 
                                                   # OPC is the main variable
                                                  
  
  tBET_km   <- (tCTd + tChd +
                  tOCh2 + tLU)/DVKMT                # Driver's working time per km estimate for a BET in min
  tBETT_km  <- (tCTd + tChd +
                  tOCh2 + tLU + 45)/DVKMT           # Total Driver's working time (with 45 min rest) per km estimate for a BET in min
  tCT_km    <- (tCTd + tLU)/DVKMT                  # Driver's working time per km estimate for a CT in min
  tCTT_km    <- (tCTd + tLU + 45)/DVKMT            # Total Driver's working time (with 45 min rest) per km estimate for a CT in min
  ECBET_km  <- EBET_km*PE                          # BET's electricity cost per km
  ECCT_km   <- Vd_km(GVW,lc,UR)*PD                 # CT's electricity cost per km
  MCCT_km   <- VPCT(GVW)*mcr                       # Maintenance cost of a CT per km
  MCBET_km  <- MCCT_km*Rmcr                        # Maintenance cost of a BET per km
  VKMT_Bat  <- R*CC                                # Lifetime vehicle kilometres travel of battery pack in km
  BRT       <- VKMT_Bat*
    ((VKMT*(1+Chd))^-1)                            # Battery pack replacement time intervals in year
  Kh        <- 0.03*BRT                            # Battery health factor of an old battery pack
  RVBP      <- (1-Kr-Ku)*(1-Kh)*BPP                # Residual value of the old battery pack for battery pack replacement
  BRN       <- ceiling(VKMT*N*(1+Chd)*
                         (VKMT_Bat^-1))-1          # Battery pack replacement numbers
  KhEoL     <- 0.03*(N-BRN*BRT)                    # Battery health factor of an old battery pack at end of life of a BET in year N 
  RVBPEoL   <- (1-Kr-Ku)*(1-KhEoL)*BPP             # Residual value of a battery pack at end of life of a BET in year N
  
  
  LCODCT_km_tempdf   <- NULL
  LCODCT_tkm_tempdf  <- NULL
  LCODBET_km_tempdf  <- NULL
  LCODBET_tkm_tempdf <- NULL
  
  ###### Cost equations ######
  ###### CT ######
  DPCT      <- DPr*VPCT(GVW)                       # Vehicle purchase price's down payment (DP) in CT
  LP_CT     <- LP_CT0
  LPR_CT    <- 0
  LIP_CT    <- 0
  for(i1 in 1:M){
    LPR_CT[i1+1]   <- LP_CT[i1]*r*((1+r)^(M-i1+1)
                                   /(((1+r)^(M-i1+1))-1)-1)        # Loan principal repayment (LPR) in CT in year i
    LP_CT[i1+1]    <- LP_CT[i1]-LPR_CT[i1+1]       # Loan principal in next year
    LIP_CT[i1+1]   <- LP_CT[i1]*r                  # Loan interest paid (LIP) in CT in year i
    
  }
  ICCT     <- ICCT_km*VKMT                         # Insurance cost (IC) in CT
  ECCT     <- ECCT_km*VKMT                         # Energy cost (EC) in CT
  MCCT     <- MCCT_km*VKMT                         # Maintenance cost (MC) in CT
  DCCT     <- DCh*tCT_km*VKMT/60                   # Driver cost (DC) in CT
  RVCT     <- VPCT(GVW)*exp(PA*N+PM*N*VKMT*(1.60934^-1)*
                              0.001)               # Residual value of a CT at its end of life
  
  ###### BET ######
  DPBET     <- DPr*VPBET                           # Vehicle purchase price's down payment (DP) in BET
  CEC       <- (E_fastR*CECF_km
  + E_slowR*CECS_km)*VKMT*N                        # Charger equipment cost (CEC) #second version
  LP_BET    <- LP_BET0
  LPR_BET   <- 0
  LIP_BET   <- 0
  for(i1 in 1:M){
    LPR_BET[i1+1]  <- LP_BET[i1]*r*((1+r)^(M-i1+1)
                                    /(((1+r)^(M-i1+1))-1)-1)        # Loan principal repayment (LPR) in BET in year i
    LP_BET[i1+1]   <- LP_BET[i1]-LPR_BET[i1+1]     # Loan principal in next year
    LIP_BET[i1+1]  <- LP_BET[i1]*r                 # Loan interest paid (LIP) in BET in year i
    
  }
  ICBET     <- {ICBET_km*VKMT*
                tBET_km*(tCT_km^-1)}               # Insurance cost (IC) in BET
  ECBET     <- ECBET_km*VKMT*(1+Chd)               # Electricity cost in BET
  MCBET     <- MCBET_km*VKMT*(1+Chd)               # Maintenance cost (MC) in BET
  DCBET     <- DCh*tBET_km*VKMT*(1+Chd)/60         # Driver cost (DC) in BET
  BRC       <- {BPP-RVBP}                          # Battery replacement cost (BRC)
  RVBETexcBP <- VPCT(GVW)*exp(PA*N+PM*N*VKMT*
                                (1+Chd)*(1.60934^-1)*
                                0.001)             # Residual value of a BET excluded battery pack
  RVBET     <- {RVBETexcBP+RVBPEoL}                # Residual value (RV) of a BET at its end of life
  
  ###### Main TCO  equations ######
  ###### CT ######
  CCT              <- data.frame(matrix(0, nrow = N+1, ncol = 8))
  names(CCT)       <- c("DP","LPR","LIP","IC","EC","MC","DC","RV")
  # Defining cash flow dataframe with columns for each cost elements
  # and rows for year 0 to N
  
  CCT[1,1]         <- DPCT                         # Cash flow in year 0
  CCT[2:(M+1),2]   <- LPR_CT[2:(M+1)]
  CCT[2:(M+1),3]   <- LIP_CT[2:(M+1)]
  CCT[2:(M+1),4]   <- ICCT
  CCT[2:(M+1),5]   <- ECCT
  CCT[2:(M+1),6]   <- MCCT
  CCT[2:(M+1),7]   <- DCCT                         # Cash flow in year 1 to M
  
  CCT[(M+2):N,4]   <- ICCT
  CCT[(M+2):N,5]   <- ECCT
  CCT[(M+2):N,6]   <- MCCT
  CCT[(M+2):N,7]   <- DCCT                         # Cash flow in year M+1 to N-1
  CCT[N+1,8]       <- -RVCT                        # Cash flow in year N
  # TCO for different cost elements of a BET
  TCOCT              <- data.frame(matrix(0, nrow = 1, ncol = 8))
  names(TCOCT)       <- c("DP","LPR","LIP","IC","EC","MC","DC","RV")
  # Defining one row cash flow dataframe with columns for each cost elements
  for(j1 in 1:8){
    for(i1 in 0:N){
      TCOCT0        <- CCT[i1+1,j1]/((1+d)^i1)
      TCOCT[1,j1]   <- TCOCT[1,j1]+TCOCT0
    }
  }
  # Levelized cost of driving (LCOD) of individual cost j per km 
  LCODCT_km           <- data.frame(matrix(0, nrow = 1, ncol = 8))
  names(LCODCT_km)    <- c("DP","LPR","LIP","IC","EC","MC","DC","RV")
  # Defining one row LCOD dataframe with columns for each cost elements
  for(j1 in 1:8){
    LCODCT_km1          <- 0
    for(i1 in 1:N){
      LCODCT_km0        <- VKMT*(1+Chd)/((1+d)^i1)
      LCODCT_km1        <- LCODCT_km1+LCODCT_km0
      LCODCT_km[1,j1]   <- TCOCT[1,j1]/LCODCT_km1
    }
  }
  # Levelized cost of driving (LCOD) of individual cost j per tkm
  LCODCT_tkm           <- data.frame(matrix(0, nrow = 1, ncol = 8))
  names(LCODCT_tkm)    <- c("DP","LPR","LIP","IC","EC","MC","DC","RV")
  # Defining one row LCOD dataframe with columns for each cost elements
  for(j1 in 1:8){
    LCODCT_tkm1          <- 0
    
    for(i1 in 1:N){
      LCODCT_tkm0        <- VKMT*(1+Chd)*lc*UR/((1+d)^i1)
      LCODCT_tkm1        <- LCODCT_tkm1+LCODCT_tkm0
      LCODCT_tkm[1,j1]   <- TCOCT[1,j1]/LCODCT_tkm1
    }
  }
  ###### BET ######
  
  #Calculating cost element for profit lost in BETs
  TWHCT    <- tCT_km*DVKMT*260*N/60                 # Total working hours of a CT in a N-year life span
  HCDCT    <- sum(TCOCT)/TWHCT                      # Hourly cost of driving of a CT in USD
  HGPCT    <- HCDCT*GPM/(100-GPM)                   # Hourly Gross profit per working hour for a CT in USD/h 
  PLBET    <- HGPCT*(tBET_km-tCT_km)*DVKMT*260      # Annual profit lost (PL) due to time spent on charging activities 
  
  #TCO of BET
  CBET              <- data.frame(matrix(0, nrow = N+1, ncol = 11))
  names(CBET)       <- c("DP","CEC","LPR","LIP","IC","EC","MC","DC","BRC","RV", "PL")
  
  
  # Defining cash flow dataframe with columns for each cost elements
  # and rows for year 0 to N
  
  CBET[1,1]         <- DPBET
  CBET[1,2]         <- CEC                          # Cash flow in year 0
  
  CBET[2:(M+1),3]   <- LPR_BET[2:(M+1)]
  CBET[2:(M+1),4]   <- LIP_BET[2:(M+1)]
  CBET[2:(M+1),5]   <- ICBET
  CBET[2:(M+1),6]   <- ECBET
  CBET[2:(M+1),7]   <- MCBET
  CBET[2:(M+1),8]   <- DCBET                 
  CBET[2:(M+1),11]  <- PLBET                         # Cash flow in year 1 to M
  
  CBET[(M+2):N,5]   <- ICBET
  CBET[(M+2):N,6]   <- ECBET
  CBET[(M+2):N,7]   <- MCBET
  CBET[(M+2):N,8]   <- DCBET                 
  CBET[(M+2):N,11]  <- PLBET                         # Cash flow in year M+1 to N-1
  
  CBET[N+1,10]      <- -RVBET                
  CBET[N+1,11]      <- PLBET                         # Cash flow in year N
  
  if (BRT <= N){
    BRTY              <- ceiling(seq(BRT, N, by=BRT))
    CBET[BRTY+1,9]    <- BRC                         # Cash flow for battery replacement BRN times with BRT intervals in years between 1 to N
  }
  # TCO for different cost elements of a BET
  TCOBET              <- data.frame(matrix(0, nrow = 1, ncol = 11))
  names(TCOBET)       <- c("DP","CEC","LPR","LIP","IC","EC","MC","DC","BRC","RV","PL")
  # Defining one row cash flow dataframe with columns for each cost elements
  for(j1 in 1:11){
    for(i1 in 0:N){
      TCOBET0       <- CBET[i1+1,j1]/((1+d)^i1)
      TCOBET[1,j1]  <- TCOBET[1,j1]+TCOBET0
    }
  }
  # Levelized cost of driving (LCOD) of individual cost j per km 
  LCODBET_km          <- data.frame(matrix(0, nrow = 1, ncol = 11))
  names(LCODBET_km)   <- c("DP","CEC","LPR","LIP","IC","EC","MC","DC","BRC","RV","PL")
  # Defining one row LCOD dataframe with columns for each cost elements
  for(j1 in 1:11){
    LCODBET_km1         <- 0
    for(i1 in 1:N){
      LCODBET_km0       <- VKMT*(1+Chd)/((1+d)^i1)
      LCODBET_km1       <- LCODBET_km1+LCODBET_km0
      LCODBET_km[1,j1]  <- TCOBET[1,j1]/LCODBET_km1
    }
  }
  # Levelized cost of driving (LCOD) of individual cost j per tkm
  LCODBET_tkm          <- data.frame(matrix(0, nrow = 1, ncol = 11))
  names(LCODBET_tkm)   <- c("DP","CEC","LPR","LIP","IC","EC","MC","DC","BRC","RV","PL")
  # Defining one row LCOD dataframe with columns for each cost elements
  for(j1 in 1:11){
    LCODBET_tkm1         <- 0
    for(i1 in 1:N){
      LCODBET_tkm0       <- VKMT*(1+Chd)*lc*UR/((1+d)^i1)
      LCODBET_tkm1       <- LCODBET_tkm1+LCODBET_tkm0
      LCODBET_tkm[1,j1]  <- TCOBET[1,j1]/LCODBET_tkm1
    }
  }
  

  LCODCT_km_tempdf    <- data.frame(GVW = GVW, BatChaSs = BatCha_scenarios[i], ATRe, BETRange= R, PElectricity= PE, PDFuel= PD, DVTkm= DVKMT ,PCUrate= UR,LCODCT_km,TTime_km=tCTT_km,dGVW=dGVW,lc=lc,BPC=BPC)
  LCODCT_tkm_tempdf   <- data.frame(GVW = GVW, BatChaSs = BatCha_scenarios[i], ATRe, BETRange= R, PElectricity= PE, PDFuel= PD, DVTkm= DVKMT ,PCUrate= UR,LCODCT_tkm,TTime_km=tCTT_km,dGVW=dGVW,lc=lc,BPC=BPC)
  LCODBET_km_tempdf   <- data.frame(GVW = GVW, BatChaSs = BatCha_scenarios[i], ATRe, BETRange= R, PElectricity= PE, PDFuel= PD, DVTkm= DVKMT ,PCUrate= UR,LCODBET_km,TTime_km=tBETT_km,dGVW=dGVW,lc=lc,BPC=BPC)
  LCODBET_tkm_tempdf  <- data.frame(GVW = GVW, BatChaSs = BatCha_scenarios[i], ATRe, BETRange= R, PElectricity= PE, PDFuel= PD, DVTkm= DVKMT ,PCUrate= UR,LCODBET_tkm,TTime_km=tBETT_km,dGVW=dGVW,lc=lc,BPC=BPC)
  
  
  
  return(list(CT_km = LCODCT_km_tempdf,CT_tkm = LCODCT_tkm_tempdf,BET_km = LCODBET_km_tempdf,BET_tkm = LCODBET_tkm_tempdf))
  
}


###### Figure 2 ###### 
###### creating dataframe for Figure 2 ######

# Set ranges for Fig 2

U       <- rep(1,43)                               #Uncertainty over variables
U[4]    <- 0
U[43]   <- 0
i_R     <- 4                                      # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq (10,80,by=5)                       #seq (10,80,by=10), 4:80
ATRe_R  <- 10                                     #seq (-20,15,by=1), -20:15
R_R     <- 300                                    #seq (100,1000,by=50)
PE_R    <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD_R    <- 1.57                                   #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT_R <- 500                                    #seq (300,1000,by=50)
UR_R    <- 1                                      #seq(0.5,1,by=0.05)
LUN_R   <- 0.06                                   #seq(0.01,0.1,by=0.01)

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)

# scenarios for fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

BatScenario_name <-c("ST-BBS", "MT-BBS", "LT-BBS")
# Values for fast charger in S2 --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)

rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.2.data <- NULL

SChid <- 0

for (Si in 1:3){
  U[32] <- Ss[Si,1]
  U[33] <- Ss[Si,2]
  U[34] <- Ss[Si,3]
  U[36] <- Ss[Si,4]
  for(S in S_R){
    U[35] <- S
    SChid <- SChid + 1
    for (GVW in GVW_R){
      for (ATRe in ATRe_R){
        for (R in R_R){
          for (PE in PE_R){
            for (PD in PD_R){
              for (DVKMT in DVKMT_R){
                for (UR in UR_R){
                  for(LUN in LUN_R){
                    for (i in i_R){
                      CT_km   <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_km
                      CT_tkm  <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_tkm
                      BET_km  <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_km
                      BET_tkm <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_tkm
                      
                      temp.data  <- data.frame(BatScenario=paste(BatScenario_name[Si],sep= ""),CharScenario=paste(S*200, " kW ChPF", sep = ""), GVW = BET_km$GVW, BatChaSs = Scenarios_name[SChid], km = rowSums(BET_km[9:19])-rowSums(CT_km[9:16]), tkm = rowSums(BET_tkm[9:19])-rowSums(CT_tkm[9:16]))
                      Fig.2.data <- rbind(Fig.2.data, temp.data)
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
  }
  
  
}

Fig.2.data$CharScenario[Fig.2.data$CharScenario=="1000 kW ChPF"] <- "1 MW ChPF"


###### Plotting Figure 2 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
)

Fig.2A <- ggplot(data = Fig.2.data, aes(x = GVW, y = km, color = BatScenario, linetype = CharScenario)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="top") + 
  labs(color="Battery pack specification:") +
  labs(linetype="Charger specification:") + 
  labs(tag = "A)") + 
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  labs(y = "BET to CT LCOD difference in USD per km") +
  scale_x_continuous(expand = c(0, 0),  breaks = scales::pretty_breaks(n = 10)) + 
  scale_color_npg()


Fig.2B <- ggplot(data = Fig.2.data, aes(x = GVW, y = tkm, color = BatScenario, linetype = CharScenario)) +
  comFiglayer +  
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="top") + 
  labs(color="Battery pack specification:") +
  labs(linetype="Charger specification:") + 
  labs(tag = "B)") + 
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  labs(y = "BET to CT LCOD difference in USD per tkm") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

legend_Fig.2 <- get_legend(Fig.2A)
Fig.2_row <- plot_grid(Fig.2A + theme(legend.position="none"),
                        Fig.2B + theme(legend.position="none"))
Fig.2 <- plot_grid(Fig.2_row, legend_Fig.2, ncol = 1, rel_heights = c(1, 0.1))




###### Figure 3 ###### 
###### creating dataframe for Figure 3 ######

# Set ranges for Fig 3

U       <- rep(1,43)                              #Uncertainty over variables
U[4]    <-0
U[43]    <-0
i       <- 4                                       # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq (10,80,by=5)                      #seq (10,80,by=10), 10:80
ATRe    <- 10                                     #seq (-20,15,by=1), default 10, -20:15
R       <- 300                                    #seq (100,1000,by=50), default 300
PE      <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD      <- 1.57                                   #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT   <- 500                                    #seq (300,1000,by=50)
UR      <- 1                                      #seq(0.5,1,by=0.05)
LUN     <- 0.06                                   #seq(0.01,0.1,by=0.01), default 0.06

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)

# scenarios for fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

# Values for fast charger in Ss --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)

rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.3.data <- NULL

SChid <- 0

for (Si in 1:3){
  U[32] <- Ss[Si,1]
  U[33] <- Ss[Si,2]
  U[34] <- Ss[Si,3]
  U[36] <- Ss[Si,4]
  for(S in S_R){
    U[35] <- S
    SChid <- SChid + 1
    for (GVW in GVW_R){
      CT_km_temp   <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_km, Scenarios=Scenarios_name[SChid])
      CT_tkm_temp  <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_tkm, Scenarios=Scenarios_name[SChid])
      BET_km_temp  <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_km, Scenarios=Scenarios_name[SChid])
      BET_tkm_temp <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_tkm, Scenarios=Scenarios_name[SChid])
      
      CT_km    <- rbind (CT_km, CT_km_temp)
      CT_tkm   <- rbind (CT_tkm, CT_tkm_temp)
      BET_km   <- rbind (BET_km, BET_km_temp)
      BET_tkm  <- rbind (BET_tkm, BET_tkm_temp)
    }
  }
}



Cost_names <- c("Vehicle's purchase costs",
                "Charger equipment",
                "Insurance",
                "Fuel/electricity",
                "Maintenance",
                "Driver",
                "Battery replacement",
                "Vehicles's residual value",
                "Profit lost in BET")
Cost_names_BET <- c("1VPC","2CEC","3IC","4EC","5MC","6DC","7BRC","8RV","9SPL")
Cost_names_CT <- c("1VPC","3IC","4EC","5MC","6DC","8RV")

Fig.3.data <- NULL

#For BET
temp.data <- data.frame(GVW = BET_km$GVW, Scenarios = BET_km$Scenarios, Vehicle = "BET", Cost = Cost_names_BET[1], Value= BET_km[,9]+BET_km[,11]+BET_km[,12])
Fig.3.data <- rbind(Fig.3.data, temp.data)
temp.data <- data.frame(GVW = BET_km$GVW, Scenarios = BET_km$Scenarios, Vehicle = "BET", Cost = Cost_names_BET[2], Value= BET_km[,10])
Fig.3.data <- rbind(Fig.3.data, temp.data)


for (i in (13:19)){
  temp.data <- data.frame(GVW = BET_km$GVW, Scenarios = BET_km$Scenarios, Vehicle = "BET", Cost = Cost_names_BET[i-10], Value= BET_km[,i])
  Fig.3.data <- rbind(Fig.3.data, temp.data)
}

#For CT
temp.data <- data.frame(GVW = CT_km$GVW, Scenarios = CT_km$Scenarios, Vehicle = "CT", Cost = Cost_names_CT[1], Value= CT_km[,9]+CT_km[,10]+CT_km[,11])
Fig.3.data <- rbind(Fig.3.data, temp.data)


for (i in (12:16)){
  temp.data <- data.frame(GVW = CT_km$GVW, Scenarios = CT_km$Scenarios, Vehicle = "CT", Cost = Cost_names_CT[i-10], Value= CT_km[,i])
  Fig.3.data <- rbind(Fig.3.data, temp.data)
}
temp.data <- data.frame(GVW = c(40,40), Scenarios = c(Scenarios_name[1],Scenarios_name[1]), Vehicle = c("CT","CT"), Cost = c("2CEC","7BRC"), Value= c(0,0))
Fig.3.data <- rbind(Fig.3.data, temp.data)





###### Plotting Figure 3 ######
theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
)


Fig.3B <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[1])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[1],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9, name = "Cost", labels = Cost_names)

Fig.3C <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[2])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[2],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

Fig.3D <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[3])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[3],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

Fig.3E <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[4])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[4],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  labs(y = "LCOD in USD per km") +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

Fig.3F <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[5])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[5],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

Fig.3G <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[6])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[6],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)


Fig.3H <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[7])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[7],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  labs(y = "LCOD in USD per km") +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

Fig.3I <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[8])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[8],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

Fig.3J <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "BET") & (Fig.3.data$Scenarios == Scenarios_name[9])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[9],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)



Fig.3A <- ggplot() +
  geom_area(data = subset(Fig.3.data, (Fig.3.data$Vehicle == "CT") & (Fig.3.data$Scenarios == Scenarios_name[1])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle("CT") +
  theme(plot.title = element_text(hjust=0.5)) +  
  labs(x = "GVW of CT in tonne") +
  labs(y = "LCOD in USD per km") +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_line(colour = "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-1, 4.0)) + 
  scale_fill_npg(alpha = 0.9)

legend_Fig.3 <- get_legend(Fig.3B)
Fig.3_rows <- plot_grid(Fig.3A + theme(legend.position="none"),
                         Fig.3B + theme(legend.position="none"),
                         Fig.3C + theme(legend.position="none"),
                         Fig.3D + theme(legend.position="none"),
                         NULL,
                         Fig.3E + theme(legend.position="none"),
                         Fig.3F + theme(legend.position="none"),
                         Fig.3G + theme(legend.position="none"),
                         NULL,
                         Fig.3H + theme(legend.position="none"),
                         Fig.3I + theme(legend.position="none"),
                         Fig.3J + theme(legend.position="none"),
                         ncol = 4,
                         nrow = 3,
                         hjust = -2)
Fig.3 <- plot_grid(Fig.3_rows, legend_Fig.3, rel_widths = c(1, 0.25))


###### Figure A9 ###### 
###### creating dataframe for Figure A9 ######

# Set ranges for Fig A9

U       <- rep(1,43)                              #Uncertainty over variables
U[4]    <- 0
U[43]    <- 0
i       <- 4                                       # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq (10,80,by=5)                      #seq (10,80,by=10), 10:80
ATRe    <- 10                                     #seq (-20,15,by=1), default 10, -20:15
R       <- 300                                    #seq (100,1000,by=50), default 300
PE      <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD      <- 1.57                                   #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT   <- 500                                    #seq (300,1000,by=50)
UR      <- 1                                      #seq(0.5,1,by=0.05)
LUN     <- 0.06                                   #seq(0.01,0.1,by=0.01), default 0.06

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)

# scenarios for fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

# Values for fast charger in Ss --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)

rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.A9.data <- NULL

SChid <- 0

for (Si in 1:3){
  U[32] <- Ss[Si,1]
  U[33] <- Ss[Si,2]
  U[34] <- Ss[Si,3]
  U[36] <- Ss[Si,4]
  for(S in S_R){
    U[35] <- S
    SChid <- SChid + 1
    for (GVW in GVW_R){
      CT_km_temp   <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_km, Scenarios=Scenarios_name[SChid])
      CT_tkm_temp  <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_tkm, Scenarios=Scenarios_name[SChid])
      BET_km_temp  <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_km, Scenarios=Scenarios_name[SChid])
      BET_tkm_temp <-  data.frame(LCOD_all(i=4, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_tkm, Scenarios=Scenarios_name[SChid])
      
      CT_km    <- rbind (CT_km, CT_km_temp)
      CT_tkm   <- rbind (CT_tkm, CT_tkm_temp)
      BET_km   <- rbind (BET_km, BET_km_temp)
      BET_tkm  <- rbind (BET_tkm, BET_tkm_temp)
    }
  }
}
CT_km [9:16]  <- 100*CT_km [9:16] / rowSums(CT_km[9:16])
CT_tkm [9:16] <- 100*CT_tkm [9:16] / rowSums(CT_tkm[9:16])
BET_km[9:19]  <- 100*BET_km[9:19] / rowSums(BET_km[9:19])
BET_tkm[9:19]  <- 100*BET_tkm[9:19] / rowSums(BET_tkm[9:19])




Cost_names <- c("Vehicle's purchase costs",
                "Charger equipment",
                "Insurance",
                "Fuel/electricity",
                "Maintenance",
                "Driver",
                "Battery replacement",
                "Vehicles's residual value",
                "Profit lost in BET")
Cost_names_BET <- c("1VPC","2CEC","3IC","4EC","5MC","6DC","7BRC","8RV","9SPL")
Cost_names_CT <- c("1VPC","3IC","4EC","5MC","6DC","8RV")

Fig.A9.data <- NULL

#For BET
temp.data <- data.frame(GVW = BET_km$GVW, Scenarios = BET_km$Scenarios, Vehicle = "BET", Cost = Cost_names_BET[1], Value= BET_km[,9]+BET_km[,11]+BET_km[,12])
Fig.A9.data <- rbind(Fig.A9.data, temp.data)
temp.data <- data.frame(GVW = BET_km$GVW, Scenarios = BET_km$Scenarios, Vehicle = "BET", Cost = Cost_names_BET[2], Value= BET_km[,10])
Fig.A9.data <- rbind(Fig.A9.data, temp.data)


for (i in (13:19)){
  temp.data <- data.frame(GVW = BET_km$GVW, Scenarios = BET_km$Scenarios, Vehicle = "BET", Cost = Cost_names_BET[i-10], Value= BET_km[,i])
  Fig.A9.data <- rbind(Fig.A9.data, temp.data)
}

#For CT
temp.data <- data.frame(GVW = CT_km$GVW, Scenarios = CT_km$Scenarios, Vehicle = "CT", Cost = Cost_names_CT[1], Value= CT_km[,9]+CT_km[,10]+CT_km[,11])
Fig.A9.data <- rbind(Fig.A9.data, temp.data)


for (i in (12:16)){
  temp.data <- data.frame(GVW = CT_km$GVW, Scenarios = CT_km$Scenarios, Vehicle = "CT", Cost = Cost_names_CT[i-10], Value= CT_km[,i])
  Fig.A9.data <- rbind(Fig.A9.data, temp.data)
}
temp.data <- data.frame(GVW = c(40,40), Scenarios = c(Scenarios_name[1],Scenarios_name[1]), Vehicle = c("CT","CT"), Cost = c("2CEC","7BRC"), Value= c(0,0))
Fig.A9.data <- rbind(Fig.A9.data, temp.data)





###### Plotting Figure A9 ######
theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))
)

Fig.A9B <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[1])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[1],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9, name = "Cost", labels = Cost_names)

Fig.A9C <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[2])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[2],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

Fig.A9D <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[3])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[3],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

Fig.A9E <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[4])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[4],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  labs(y = "Cost structure percentage based on LCOD (per km)") +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

Fig.A9F <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[5])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[5],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

Fig.A9G <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[6])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[6],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)


Fig.A9H <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[7])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[7],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  labs(y = "Cost structure percentage based on LCOD (per km)") +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

Fig.A9I <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[8])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[8],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

Fig.A9J <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "BET") & (Fig.A9.data$Scenarios == Scenarios_name[9])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle(paste("BET (",Scenarios_name[9],")", sep = "")) +
  theme(plot.title = element_text(hjust=0.5)) +
  labs(x = "GVW of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  theme(panel.grid.major.y = element_line(colour = "black")) + 
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)



Fig.A9A <- ggplot() +
  geom_area(data = subset(Fig.A9.data, (Fig.A9.data$Vehicle == "CT") & (Fig.A9.data$Scenarios == Scenarios_name[1])),
            aes(x = GVW, y = Value, fill = Cost), 
            stat = "identity") +
  comFiglayer +  
  ggtitle("CT") +
  theme(plot.title = element_text(hjust=0.5)) +  
  labs(x = "GVW of CT in tonne") +
  labs(y = "Cost structure percentage based on LCOD (per km)") +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_line(colour = "black")) +
  scale_x_continuous(expand = c(0, 0), limits = c(10, 80)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-20, 120)) + 
  scale_fill_npg(alpha = 0.9)

legend_Fig.A9 <- get_legend(Fig.A9B)
Fig.A9_rows <- plot_grid(Fig.A9A + theme(legend.position="none"),
                         Fig.A9B + theme(legend.position="none"),
                         Fig.A9C + theme(legend.position="none"),
                         Fig.A9D + theme(legend.position="none"),
                         NULL,
                         Fig.A9E + theme(legend.position="none"),
                         Fig.A9F + theme(legend.position="none"),
                         Fig.A9G + theme(legend.position="none"),
                         NULL,
                         Fig.A9H + theme(legend.position="none"),
                         Fig.A9I + theme(legend.position="none"),
                         Fig.A9J + theme(legend.position="none"),
                         ncol = 4,
                         nrow = 3,
                         hjust = -2)
Fig.A9 <- plot_grid(Fig.A9_rows, legend_Fig.A9, rel_widths = c(1, 0.25))




###### Figure A10 ###### 
###### creating dataframe for Figure A10 ######

# Set ranges for Fig A10

U       <- rep(1,43)           #Uncertainty over variables
U[4]    <- 0
U[43]    <- 0
i_R     <- 4                                      # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq (10,80,by=5)                      #seq (10,80,by=10), 4:80
ATRe_R  <- 10                                     #seq (-20,15,by=1), -20:15
R_R     <- 300                                    #seq (100,1000,by=50)
PE_R    <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD_R    <- 1.57                                   #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT_R <- 500                                    #seq (300,1000,by=50)
UR_R    <- 1                                      #seq(0.5,1,by=0.05)
LUN_R   <- 0.06                                   #seq(0.01,0.1,by=0.01)

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)

# scenarios for fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")
BatScenario_name <-c("ST-BBS", "MT-BBS", "LT-BBS")

# Values for fast charger in S2 --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)

rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.A10.data <- NULL

SChid <- 0

for (Si in 1:3){
  U[32] <- Ss[Si,1]
  U[33] <- Ss[Si,2]
  U[34] <- Ss[Si,3]
  U[36] <- Ss[Si,4]
  for(S in S_R){
    U[35] <- S
    SChid <- SChid + 1
    for (GVW in GVW_R){
      for (ATRe in ATRe_R){
        for (R in R_R){
          for (PE in PE_R){
            for (PD in PD_R){
              for (DVKMT in DVKMT_R){
                for (UR in UR_R){
                  for(LUN in LUN_R){
                    for (i in i_R){
                      CT_km   <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_km
                      CT_tkm  <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$CT_tkm
                      BET_km  <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_km
                      BET_tkm <-  LCOD_all(i=i, GVW=GVW, ATRe=ATRe, R=R, PE=PE, PD=PD, DVKMT=DVKMT, UR=UR, LUN=LUN)$BET_tkm
                      
                      temp.data  <- data.frame(BatScenario=paste(BatScenario_name[Si],sep= ""),CharScenario=paste(S*200, " kW ChPF", sep = ""),GVW = BET_km$GVW, BatChaSs = Scenarios_name[SChid], TimeBETtoCT = (BET_tkm$TTime_km-CT_tkm$TTime_km), TimeBETOCT = (BET_tkm$TTime_km/CT_tkm$TTime_km))
                      Fig.A10.data <- rbind(Fig.A10.data, temp.data)
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    
  }
  
  
}

Fig.A10.data$CharScenario[Fig.A10.data$CharScenario=="1000 kW ChPF"] <- "1 MW ChPF"

###### Plotting Figure A10 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
)

Fig.A10A <- ggplot(data = Fig.A10.data, aes(x = GVW, y = TimeBETtoCT, color = BatScenario, linetype = CharScenario)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="top") + 
  labs(color="Battery pack specification:") +
  labs(linetype="Charger specification:") + 
  labs(tag = "A)") + 
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  labs(y = "BET to CT delivery time difference in min per km") +
  scale_x_continuous(expand = c(0, 0),  breaks = scales::pretty_breaks(n = 10)) + 
  scale_color_npg()


Fig.A10B <- ggplot(data = Fig.A10.data, aes(x = GVW, y = TimeBETOCT, color = BatScenario, linetype = CharScenario)) +
  comFiglayer +  
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="top") + 
  labs(color="Battery pack specification:") +
  labs(linetype="Charger specification:") + 
  labs(tag = "B)") + 
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  labs(y = "BET over CT delivery time ratio") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(1,1.18),breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

legend_Fig.A10 <- get_legend(Fig.A10A)
Fig.A10_row <- plot_grid(Fig.A10A + theme(legend.position="none"),
                         Fig.A10B + theme(legend.position="none"))
Fig.A10 <- plot_grid(Fig.A10_row, legend_Fig.A10, ncol = 1, rel_heights = c(1, 0.1))


###### Figure 4 ###### 
###### creating dataframe for Figure 4 ######


###### Variation in relative cost a 40 t BET to CT because of increae of different parameters ###### 

Umax <- c(7/5,4/3,4/3,0.5,4/3,1.059,1.5,4/3,4/3,1.037,4/3,7/5,rep(4/3,28),1,4/3,100)     # Upper range ratio for variables
Umin <- c(3/5,2/3,2/3,0,2/3,1-0.059,0.5,1/5,2/3,1-0.037,2/3,3/5,rep(2/3,28),0.5,2/3,0)   # Lower range ratio for variables

#default values for variables
Variables_def40t <- c(10,500,0.05,0,0.66,0.85,10,0.025,0.07,0.9,0.06,5,0.12,105798,0.05,80,0.5,0.122,1.57,0.6,1.15e-06,30,0.15,0.15,-0.094,-5e-4,0.06,105798,2.5,0.615,0.85,300,125,3000,200,75,2.599,20,0.75,300,1,3,0)
#Units of values
Variables_units  <- c("year","km","","km","","","°C","","USD/kW/h","","","year","","USD","USD/km","km/h","h","USD/kWh","USD/litre","","1/km","USD/h","","","","","1/km","USD","","litre/km","","USD/kWh","Wh/kg","","kW","%","t","km/h","","km","","%","%")
Variables_UpR    <- Umax*Variables_def40t               # Upper range for variables
Variables_UpR[4] <- Umax[4]
Variables_UpR[43] <- Umax[43]
Variables_LoR    <- Umin*Variables_def40t               # Lower range for variables
Variables_LoR[4] <- Umin[4]
Variables_LoR[43] <- Umin[43]
Variables_UpR    <- round(Variables_UpR,2)
Variables_UpR[26]    <- round({Umax*Variables_def40t}[26],7)
Variables_UpR[21]    <- round({Umax*Variables_def40t}[21],7)
Variables_UpR[8]    <- round({Umax*Variables_def40t}[8],3)
Variables_LoR <- round(Variables_LoR,2)
Variables_LoR[26]    <- round({Umin*Variables_def40t}[26],7)
Variables_LoR[21]    <- round({Umin*Variables_def40t}[21],7)
Variables_LoR[8]    <- round({Umin*Variables_def40t}[8],3)

###### Variabes in main function #####
# U[2], U[7], U[16], U[18], U[19], U[27] in the main function

Variable_Names <- c("Time frame for TCO analysis",
                    "Daily vehicle kilometers travel",
                    "Discount rate for TCO calculation",
                    "Drive distance of route deviation for on-road charging",
                    "On-road charging accessibility ratio in routes",
                    "Adjustment factor for average battery capacity lost",
                    "Annual average ambient temperature",
                    "Energy consumption ratio for ambient temperature change",
                    "Charger cost per kW per hour",
                    "Energy efficiency for the charging equipment",
                    "Annual interest rate for purchasing a vehicle",
                    "Total number (years) of loan payments for purchasing a vehicle",
                    "Vehicle purchase price-down payment",
                    "Base vehicle purchase price",
                    "Base vehicle insurance cost",
                    "Driving speed (free flow)",
                    "Time variable for loading/unloading activity",
                    "Price of electricity",
                    "Price of fuel (diesel)",
                    "Maintenance cost ratio of BET over CT",
                    "Maintenance cost ratio of CT based on its purchase price",
                    "Driving cost per working hour",
                    "Refurbishment cost factor for battery pack",
                    "Used product discount factor for battery pack",
                    "Parameter's value for age impact on RV of a vehicle",
                    "Parameter's value for mileage impact on RV of a vehicle",
                    "Number of stops for load and unload activities per km",
                    "CT's purchase price",
                    "Relative powertrain energy consumption ratio of BET over CT",
                    "Diesel consumption volume per km",
                    "Adjustment factor for purchase price of BET over CT (battery pack excluded)",
                    "Battery pack price",
                    "Gravimetric density or specific energy of battery pack",
                    "Lifetime charging cycle of battery pack",
                    "On-road charging power",
                    "Usable state of charge (SOC) of battery pack",
                    "Weight saving of BET vs. CT powertrain",
                    "Driving speed (Saturated/Urban)",
                    "Fraction of driving time with free flow speed",
                    "Operational driving range:",
                    "Payload utilization rate",
                    "GPM for the profit lost due to the time spent on charging activities",
                    "Opportunity charging potential during loading/unloading and rest time")

Variable_Names_Fig4 <- paste(Variable_Names," (Def= ",Variables_def40t,", UpR= ",Variables_UpR,", LoR= ",Variables_LoR,") ",Variables_units,sep="")

# Set ranges for Fig 4

U    <- rep(1,43)                                 #Uncertainty over variables
U[4] <- 0
U[43]    <- 0
#i_R     <- 1                                     # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- 40                                     #seq (10,80,by=10), 10:80
ATRe_R  <- 10                                     #seq (-20,15,by=1), default 10, -20:15
R_R     <- 300                                    #seq (100,1000,by=50), default 300
PE_R    <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD_R    <- 1.57                                   #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT_R <- 500                                    #seq (300,1000,by=50)
UR_R    <- 1                                      #seq(0.5,1,by=0.05)
LUN_R   <- 0.06                                   #seq(0.01,0.1,by=0.01), default 0.06



#CT_km_temp   <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R, LUN=LUN_R*U[27])$CT_km
#CT_tkm_temp  <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R, LUN=LUN_R*U[27])$CT_tkm
#BET_km_temp  <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R, LUN=LUN_R*U[27])$BET_km
#BET_tkm_temp <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R, LUN=LUN_R*U[27])$BET_tkm

CT_tkm_def  <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$CT_tkm
BET_tkm_def <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$BET_tkm


Fig.4.data <- NULL

for (i in 1:length(Variable_Names_Fig4)){
  U    <- rep(1,43)
  U[4] <- 0
  U[43]    <- 0
  U[i] <- Umax[i]
  CT_tkm_temp  <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$CT_tkm
  BET_tkm_temp <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$BET_tkm
  
  temp.data <- data.frame(Variables = Variable_Names_Fig4[i],
                          tkm_variation = ((rowSums(BET_tkm_temp[9:19]) - rowSums(CT_tkm_temp[9:16])) - (rowSums(BET_tkm_def[9:19]) - rowSums(CT_tkm_def[9:16])))/rowSums(CT_tkm_temp[9:16])
  )
  Fig.4.data <- rbind(Fig.4.data, temp.data)
  
}
for (i in 1:length(Variable_Names_Fig4)){
  U    <- rep(1,43)
  U[4] <- 0
  U[43]    <- 0
  U[i] <- Umin[i]
  CT_tkm_temp  <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$CT_tkm
  BET_tkm_temp <-  LCOD_all(i=4, GVW=GVW_R, ATRe=ATRe_R*U[7], R=R_R*U[40], PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$BET_tkm
  
  temp.data <- data.frame(Variables = Variable_Names_Fig4[i],
                          tkm_variation = (((rowSums(BET_tkm_temp[9:19]) - rowSums(CT_tkm_temp[9:16])) - (rowSums(BET_tkm_def[9:19]) - rowSums(CT_tkm_def[9:16]))))/rowSums(CT_tkm_temp[9:16])
  )
  Fig.4.data <- rbind(Fig.4.data, temp.data)
  
}

###### Plotting Figure 4 ######
theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))
)

index_change <-NULL
index_change[1:43]  <- "Upper range (UpR) change"
index_change[44:86] <- "Lower range (LoR) change"

Fig.4 <- ggplot(data = Fig.4.data, aes(x = Variables, y = tkm_variation*100)) +
  comFiglayer +  
  geom_bar(stat="identity", aes(fill=index_change), width=.5) +
  geom_hline(yintercept = 0) +
  theme(legend.position="top") +
  labs(y = "Relative BET to CT LCOD (per tkm) change in % of default CT LCOD (per tkm)") +
  labs(x = "Variables") +
  scale_y_continuous(expand = c(0, 0), limits = c(min(Fig.4.data$tkm_variation*100), max(Fig.4.data$tkm_variation*100)), breaks = scales::pretty_breaks(n = 15)) +
  coord_flip() + 
  scale_fill_npg(name="")


###### Figure A11 ###### 
###### creating dataframe for Figure A11 ######

###### set random seed to Monte Carlo ######
set.seed(1)

# Set ranges for Fig A11

#i_R     <- 1                                     # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq (10,80,by=5)                      #seq (10,80,by=10), 10:80
ATRe_R  <- 10                                     #seq (-20,15,by=1), default 10, -20:15
R_R     <- 300                                    #seq (100,1000,by=50), default 300
PE_R    <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD_R    <- 1.57                                   #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT_R <- 500                                    #seq (300,1000,by=50)
UR_R    <- 1                                      #seq(0.5,1,by=0.05)
LUN_R   <- 0.06                                   #seq(0.01,0.1,by=0.01), default 0.06

rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km      <- NULL
CT_tkm     <- NULL
BET_km     <- NULL
BET_tkm    <- NULL
Fig.A11.data <- NULL

for (i in 1:1000){
  ###### Uncertainty ranges in Monte Carlo analysis #######
  U <- sample(c(1-0.4, 1+0.4), replace = TRUE, size = 1)
  U[2:3] <- sample(c(1-0.33, 1+0.33), replace = TRUE, size = 2)
  U[4] <- sample(c(0, 0.5), replace = TRUE, size = 1)
  U[5]<- sample(c(1-0.33, 1+0.33), replace = TRUE, size = 1)
  U[6]<- sample(c(1-0.059, 1+0.059), replace = TRUE, size = 1)
  U[7]<- sample(c(1-0.5, 1+0.5), replace = TRUE, size = 1)
  U[8]<- sample(c(1-0.8, 1+0.33), replace = TRUE, size = 1)
  U[9]<- sample(c(1-0.33, 1+0.33), replace = TRUE, size = 1)
  U[10]<- sample(c(1-0.037, 1+0.037), replace = TRUE, size = 1)
  U[11]<- sample(c(1-0.33, 1+0.33), replace = TRUE, size = 1)
  U[12]<- sample(c(1-0.4, 1+0.4), replace = TRUE, size = 1)
  U[13:40]<- sample(c(1-0.33, 1+0.33), replace = TRUE, size = 28)
  U[41]<- sample(c(0.5, 1), replace = TRUE, size = 1)
  U[42]<- sample(c(1-0.33, 1+0.33), replace = TRUE, size = 1)
  U[43] <- sample(c(0, 100), replace = TRUE, size = 1)
  
  
  for (GVW in GVW_R){
    CT_km_temp   <-  LCOD_all(i=4, GVW=GVW, ATRe=ATRe_R*U[7], R=R_R, PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$CT_km
    CT_tkm_temp  <-  LCOD_all(i=4, GVW=GVW, ATRe=ATRe_R*U[7], R=R_R, PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$CT_tkm
    BET_km_temp  <-  LCOD_all(i=4, GVW=GVW, ATRe=ATRe_R*U[7], R=R_R, PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$BET_km
    BET_tkm_temp <-  LCOD_all(i=4, GVW=GVW, ATRe=ATRe_R*U[7], R=R_R, PE=PE_R*U[18], PD=PD_R*U[19], DVKMT=DVKMT_R*U[2], UR=UR_R*U[41], LUN=LUN_R*U[27])$BET_tkm
    
    CT_km    <- rbind (CT_km, CT_km_temp)
    CT_tkm   <- rbind (CT_tkm, CT_tkm_temp)
    BET_km   <- rbind (BET_km, BET_km_temp)
    BET_tkm  <- rbind (BET_tkm, BET_tkm_temp)
    
  }
  
  temp.data <- data.frame(
    iteration = toString(i), 
    GVW = BET_km$GVW, 
    km = rowSums(BET_km[9:19])-rowSums(CT_km[9:16]), 
    tkm = rowSums(BET_tkm[9:19])-rowSums(CT_tkm[9:16])
  )
  Fig.A11.data <- rbind(Fig.A11.data, temp.data)
  CT_km      <- NULL
  CT_tkm     <- NULL
  BET_km     <- NULL
  BET_tkm    <- NULL
  
}



###### Plotting Figure A11 ######
theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
)

Fig.A11 <- ggplot(data = Fig.A11.data, aes(x = GVW, y=tkm, group = GVW)) + 
  geom_boxplot(outlier.shape = NA) +
  comFiglayer +  
  labs(x = "GVW of CT in tonne") +
  labs(y = "BET to CT LCOD difference in USD per tkm") +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.3, 0.5))

#saveRDS(Fig.A11.data, file = "Fig.A11.data.RDS")
#Fig.A11.data <- readRDS("Fig.A11.data.RDS")

###### Plotting Figure A12 ######
theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14))
)

Fig.A12 <- ggplot(data = Fig.A11.data, aes(x = GVW, y=km, group = GVW)) + 
  geom_boxplot(outlier.shape = NA) +
  comFiglayer +  
  labs(x = "GVW of CT in tonne") +
  labs(y = "BET to CT LCOD difference in USD per km") +
  scale_y_continuous(expand = c(0, 0), limits = c(-2, 5))


###### Figure 5 ###### 
###### creating dataframe for Figure 5 ######
# Fig 5 is for showing optimum range setting for different trip classifications in S1-S3 with different charging power

# Set ranges for Fig 5

# U       <- rep(1,43)                              #Uncertainty over variables
i       <- 4                                      # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq(10,80,by=5)                       #seq (10,80,by=10), 4:80
ATRe_R  <- seq(-15,15,by=15)                       #seq (-20,15,by=1), -20:15
R_R     <- seq(100, 900, by=25)                   #seq (100,1000,by=50)
R_def   <- 300
#PE_R    <- seq(0.122*7,0.122/5, by=-(0.122/5))   #0.122*seq(0.122/2,0.122*7, 0.05)
#PD_R    <- seq(1.57/2,1.57*14, by=(1.57/3))      #1.57*seq(1.57/2,1.57*7, 0.05)
PE      <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD      <- 1.57                                    #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT   <- 500                                    #seq (300,1000,by=50)
UR      <- 1                                      #seq(0.5,1,by=0.05)
LUN     <- 0.06                                   #seq(0.01,0.1,by=0.01)

# scenarios for Battery tech and fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)


# Values for fast charger in Ss --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)




U_R     <-NULL
#Define U for Urban trips
U_tem     <- rep(1,43)
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 300/300                          #Operational driving range
U_tem[2]        <- 200/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 10/10                            #Time frame for TCO analysis
U_tem[16]       <- 60/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.5/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.1/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 10/30                            #Time variable for loading/unloading activity in min
U_R <- rbind(U_R,U_tem)

#Define U for short-haul trips
U_tem     <- rep(1,43) 
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 500/300                          #Operational driving range
U_tem[2]        <- 400/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 8/10                            #Time frame for TCO analysis
U_tem[16]       <- 80/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.8/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.01/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

#Define U for long-haul trips
U_tem     <- rep(1,43)  
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 700/300                          #Operational driving range
U_tem[2]        <- 800/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 4/10                            #Time frame for TCO analysis
U_tem[16]       <- 80/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.95/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.002/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 4/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

Tripclass_names <- c("Urban", "Short-haul","Long-haul")


rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.5.data <- NULL

for (Ui in 1:3){
  SChid <- 0
  U <- U_R[Ui,]
  
  for (Si in 1:3){
    
    U[32] <- Ss[Si,1]
    U[33] <- Ss[Si,2]
    U[34] <- Ss[Si,3]
    U[36] <- Ss[Si,4]
    
    for (S in S_R){
      U[35] <- S
      SChid <- SChid + 1
      for (GVW in GVW_R){
        for(ATRe in ATRe_R){
          
          CT_tkm_temp  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[1], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
          BET_tkm_temp <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[1], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
          tkm_temp1     <- rowSums(BET_tkm_temp[9:19])-rowSums(CT_tkm_temp[9:16])
          
          for(R in R_R){
            CT_tkm_temp  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
            BET_tkm_temp <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
            tkm_temp2    <- rowSums(BET_tkm_temp[9:19])-rowSums(CT_tkm_temp[9:16])
            BPC          <- BET_tkm_temp$BPC
            if (tkm_temp2 <= tkm_temp1){
              
              tkm_temp1  <- tkm_temp2
              
              CT_tkm_def  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_def*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
              BET_tkm_def <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_def*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
              tkm_def     <- rowSums(BET_tkm_def[9:19])-rowSums(CT_tkm_def[9:16])
              
              temp.data  <- data.frame(GVW = GVW, BatChaSs = Scenarios_name[SChid], Tripclass = Tripclass_names[Ui], ATRe = ATRe, R = R, tkm = tkm_temp2, ReLCOD = (tkm_temp2 - tkm_def)/rowSums(CT_tkm_def[9:16]), BPC = BPC, dGVW = BET_tkm_temp$dGVW, lcChange= BET_tkm_temp$dGVW/BET_tkm_temp$lc)
              
            }
            
          }
          Fig.5.data <- rbind(Fig.5.data, temp.data)
          
        }
      }
      
      
    }
  }  
  
  
}
Fig.5.data$ATRe <- paste(Fig.5.data$ATRe," °C", sep="")


###### Plotting Figure 5 ######

GVW_Label   <- seq(10,80,by=10)                       #seq (10,80,by=10), 4:80


theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)




Fig.5.S1 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.05,0.2), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S2 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.05,0.1), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.5.S3 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.05,0.05), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S4 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "BET to CT LCOD difference in USD per tkm") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.06,0.08), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S5 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.06,0.08), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S6 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(-0.06,0.04), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S7 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.07,0.02), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S8 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.08,0.02), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.5.S9 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = tkm, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.08,0.02), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.5 <- get_legend(Fig.5.S1)
Fig.5_row <- plot_grid(Fig.5.S1 + theme(legend.position="none"),
                       Fig.5.S2 + theme(legend.position="none"),
                       Fig.5.S3 + theme(legend.position="none"),
                       Fig.5.S4 + theme(legend.position="none"),
                       Fig.5.S5 + theme(legend.position="none"),
                       Fig.5.S6 + theme(legend.position="none"),
                       Fig.5.S7 + theme(legend.position="none"),
                       Fig.5.S8 + theme(legend.position="none"),
                       Fig.5.S9 + theme(legend.position="none"),
                       ncol = 3,
                       nrow = 3,
                       hjust = -2)
Fig.5 <- plot_grid(Fig.5_row,legend_Fig.5, ncol = 1, rel_heights = c(1, 0.1))


#saveRDS(Fig.5.data, file = "Fig.5.data.RDS")
#Fig.5.data <- readRDS("Fig.5.data.RDS")

###### Plotting Figure A13 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16))
)




Fig.A13.S1 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S2 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.A13.S3 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S4 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Optimum driving ranges (ORs) in km") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S5 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S6 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S7 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900),  breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S8 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A13.S9 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = R, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,900), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.A13 <- get_legend(Fig.A13.S1)
Fig.A13_row <- plot_grid(Fig.A13.S1 + theme(legend.position="none"),
                         Fig.A13.S2 + theme(legend.position="none"),
                         Fig.A13.S3 + theme(legend.position="none"),
                         Fig.A13.S4 + theme(legend.position="none"),
                         Fig.A13.S5 + theme(legend.position="none"),
                         Fig.A13.S6 + theme(legend.position="none"),
                         Fig.A13.S7 + theme(legend.position="none"),
                         Fig.A13.S8 + theme(legend.position="none"),
                         Fig.A13.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.A13 <- plot_grid(Fig.A13_row,legend_Fig.A13, ncol = 1, rel_heights = c(1, 0.1))

###### Plotting Figure A14 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)



Fig.A14.S1 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,9000), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S2 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,9000), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.A14.S3 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,9000), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S4 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Battery pack capacities in kWh") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,7000), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S5 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,7000), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S6 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(0,7000), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S7 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,5500),  breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S8 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,5500), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A14.S9 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = BPC, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0,5500), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.A14 <- get_legend(Fig.A14.S1)
Fig.A14_row <- plot_grid(Fig.A14.S1 + theme(legend.position="none"),
                         Fig.A14.S2 + theme(legend.position="none"),
                         Fig.A14.S3 + theme(legend.position="none"),
                         Fig.A14.S4 + theme(legend.position="none"),
                         Fig.A14.S5 + theme(legend.position="none"),
                         Fig.A14.S6 + theme(legend.position="none"),
                         Fig.A14.S7 + theme(legend.position="none"),
                         Fig.A14.S8 + theme(legend.position="none"),
                         Fig.A14.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.A14 <- plot_grid(Fig.A14_row,legend_Fig.A14, ncol = 1, rel_heights = c(1, 0.1))


###### Plotting Figure A15 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)



Fig.A15.S1 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,70), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S2 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,70), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.A15.S3 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,70), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S4 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "BET to CT Curb weight change in tonne") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,25), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S5 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,25), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S6 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(-5,25), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S7 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,10),  breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S8 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,10), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A15.S9 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = dGVW, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(position = position_dodge(width = 2), size = 1) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-5,10), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.A15 <- get_legend(Fig.A15.S1)
Fig.A15_row <- plot_grid(Fig.A15.S1 + theme(legend.position="none"),
                         Fig.A15.S2 + theme(legend.position="none"),
                         Fig.A15.S3 + theme(legend.position="none"),
                         Fig.A15.S4 + theme(legend.position="none"),
                         Fig.A15.S5 + theme(legend.position="none"),
                         Fig.A15.S6 + theme(legend.position="none"),
                         Fig.A15.S7 + theme(legend.position="none"),
                         Fig.A15.S8 + theme(legend.position="none"),
                         Fig.A15.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.A15 <- plot_grid(Fig.A15_row,legend_Fig.A15, ncol = 1, rel_heights = c(1, 0.1))



###### Plotting Figure A16 ######

GVW_Label   <- seq(10,80,by=10)                       #seq (10,80,by=10), 4:80

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)



Fig.A16.S1 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[1] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-200,30), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S2 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[2] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-25,5), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.A16.S3 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[3] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-25,5), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S4 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[4] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Relative BET to CT LCOD (per tkm) change in %") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-250,30), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S5 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[5] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-70,5), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S6 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[6] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(-12,2), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S7 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[7] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-250,30), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S8 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[8] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-100,7), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A16.S9 <- ggplot(data = subset(Fig.5.data, Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )), aes(x = GVW, y = ReLCOD*100, color = Tripclass, linetype = ATRe)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.5.data, Fig.5.data$GVW %in% GVW_Label & Fig.5.data$BatChaSs == Scenarios_name[9] & (Fig.5.data$ATRe == '-15 °C' | Fig.5.data$ATRe == '0 °C' |Fig.5.data$ATRe == '15 °C' )),aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +      
  labs(linetype="Average ambient temperature:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-40,5), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.A16 <- get_legend(Fig.A16.S1)
Fig.A16_row <- plot_grid(Fig.A16.S1 + theme(legend.position="none"),
                         Fig.A16.S2 + theme(legend.position="none"),
                         Fig.A16.S3 + theme(legend.position="none"),
                         Fig.A16.S4 + theme(legend.position="none"),
                         Fig.A16.S5 + theme(legend.position="none"),
                         Fig.A16.S6 + theme(legend.position="none"),
                         Fig.A16.S7 + theme(legend.position="none"),
                         Fig.A16.S8 + theme(legend.position="none"),
                         Fig.A16.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.A16 <- plot_grid(Fig.A16_row,legend_Fig.A16, ncol = 1, rel_heights = c(1, 0.1))

###### Figure 6 ###### 
###### creating dataframe for Figure 6 ######
# Fig 6 is for showing optimum range setting for different trip classifications in S1-S3 with different charging power

# Set ranges for Fig 6

# U       <- rep(1,43)                           #Uncertainty over variables
i       <- 4                                      # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq(10,80,by=5)                        #seq (10,80,by=10), 4:80
ATRe    <- 15                                     #seq (-20,15,by=1), -20:15
OPC_R   <- c(0,100)                             # Scenarios for Opportunity charging (OPC) 
R_R     <- seq(100, 900, by=25)                   #seq (100,1000,by=50)
R_def   <- 300
#PE_R    <- seq(0.122*7,0.122/5, by=-(0.122/5))   #0.122*seq(0.122/2,0.122*7, 0.05)
#PD_R    <- seq(1.57/2,1.57*14, by=(1.57/3))      #1.57*seq(1.57/2,1.57*7, 0.05)
PE      <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD      <- 1.57                                    #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT   <- 500                                    #seq (300,1000,by=50)
UR      <- 1                                      #seq(0.5,1,by=0.05)
LUN     <- 0.06                                   #seq(0.01,0.1,by=0.01)

# scenarios for Battery tech and fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)


# Values for fast charger in Ss --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)




U_R     <-NULL
#Define U for Urban trips
U_tem     <- rep(1,43)
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 300/300                          #Operational driving range
U_tem[2]        <- 200/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 10/10                            #Time frame for TCO analysis
U_tem[16]       <- 60/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.5/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.1/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 10/30                            #Time variable for loading/unloading activity in min
U_R <- rbind(U_R,U_tem)

#Define U for short-haul trips
U_tem     <- rep(1,43) 
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 500/300                          #Operational driving range
U_tem[2]        <- 400/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 8/10                            #Time frame for TCO analysis
U_tem[16]       <- 80/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.8/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.01/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

#Define U for long-haul trips
U_tem     <- rep(1,43)  
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 700/300                          #Operational driving range
U_tem[2]        <- 800/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 4/10                            #Time frame for TCO analysis
U_tem[16]       <- 80/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.95/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.002/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 4/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

Tripclass_names <- c("Urban", "Short-haul","Long-haul")


rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.6.data <- NULL

for (Ui in 1:3){
  SChid <- 0
  U <- U_R[Ui,]
  
  for (Si in 1:3){
    
    U[32] <- Ss[Si,1]
    U[33] <- Ss[Si,2]
    U[34] <- Ss[Si,3]
    U[36] <- Ss[Si,4]
    
    for (S in S_R){
      U[35] <- S
      SChid <- SChid + 1
      for (GVW in GVW_R){
        for(OPC in OPC_R){
          
          U[43] <- OPC
          CT_tkm_temp  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[1], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
          BET_tkm_temp <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[1], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
          tkm_temp1     <- rowSums(BET_tkm_temp[9:19])-rowSums(CT_tkm_temp[9:16])
          
          for(R in R_R){
            CT_tkm_temp  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
            BET_tkm_temp <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
            tkm_temp2     <- rowSums(BET_tkm_temp[9:19])-rowSums(CT_tkm_temp[9:16])
            
            if (tkm_temp2 <= tkm_temp1){
              
              tkm_temp1  <- tkm_temp2
              
              CT_tkm_def  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_def*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
              BET_tkm_def <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_def*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
              tkm_def     <- rowSums(BET_tkm_def[9:19])-rowSums(CT_tkm_def[9:16])
              
              temp.data  <- data.frame(GVW = GVW, BatChaSs = Scenarios_name[SChid], Tripclass = Tripclass_names[Ui], OPC = OPC, R = R, tkm = tkm_temp2, ReLCOD = (tkm_temp2 - tkm_def)/rowSums(CT_tkm_def[9:16]), BPC = BET_tkm_def$BPC)
              
            }
            
          }
          Fig.6.data <- rbind(Fig.6.data, temp.data)
          
        }
      }
      
      
    }
  }  
  
  
}
Fig.6.data$OPC <- paste(Fig.6.data$OPC, " %", sep="")

###### Plotting Figure 6 ######


GVW_Label   <- seq(10,80,by=10)                       #seq (10,80,by=10), 4:80

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)


Fig.6.S1 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[1] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[1] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.05,0.05), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S2 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[2] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[2] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.05,0.05), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.6.S3 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[3] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[3] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.05,0.05), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S4 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[4] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[4] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "BET to CT LCOD difference in USD per tkm") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.06,0.02), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S5 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[5] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[5] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.06,0.02), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S6 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[6] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[6] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(-0.06,0.02), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S7 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[7] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[7] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.08,0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S8 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[8] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[8] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.08,0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.6.S9 <- ggplot(data = subset(Fig.6.data, Fig.6.data$BatChaSs == Scenarios_name[9] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(x = GVW, y = tkm, color = Tripclass, linetype = OPC)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_point() +
  geom_text_repel(data = subset(Fig.6.data, Fig.6.data$GVW %in% GVW_Label & Fig.6.data$BatChaSs == Scenarios_name[9] & (Fig.6.data$OPC == "0 %" | Fig.6.data$OPC == "100 %" )), aes(label = R), hjust = 0.5,  vjust = -1) +
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +   
  labs(linetype="Opportunity charging potential (OPC):") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(-0.08,0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.6 <- get_legend(Fig.6.S1)
Fig.6_row <- plot_grid(Fig.6.S1 + theme(legend.position="none"),
                         Fig.6.S2 + theme(legend.position="none"),
                         Fig.6.S3 + theme(legend.position="none"),
                         Fig.6.S4 + theme(legend.position="none"),
                         Fig.6.S5 + theme(legend.position="none"),
                         Fig.6.S6 + theme(legend.position="none"),
                         Fig.6.S7 + theme(legend.position="none"),
                         Fig.6.S8 + theme(legend.position="none"),
                         Fig.6.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.6 <- plot_grid(Fig.6_row,legend_Fig.6, ncol = 1, rel_heights = c(1, 0.1))

#saveRDS(Fig.6.data, file = "Fig.6.data.RDS")
#Fig.6.data <- readRDS("Fig.6.data.RDS")



###### Figure A17 ###### 
###### creating dataframe for Figure A17 ######

# Set ranges for Fig A17

# U       <- rep(1,43)                              #Uncertainty over variables
i       <- 4                                      # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq(10,80,by=5)                       #seq (10,80,by=10), 4:80
ATRe  <-15 #seq(-15,15,by=15)                       #seq (-20,15,by=1), -20:15
#R_R     <- c(100,200,400)                         #seq (100,1000,by=50)
#PE_R    <- seq(0.122*7,0.122/5, by=-(0.122/5))   #0.122*seq(0.122/2,0.122*7, 0.05)
#PD_R    <- seq(1.57/2,1.57*14, by=(1.57/3))      #1.57*seq(1.57/2,1.57*7, 0.05)
PE      <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD      <- 1.57                                    #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT   <- 500                                    #seq (300,1000,by=50)
UR      <- 1                                      #seq(0.5,1,by=0.05)
LUN     <- 0.06                                   #seq(0.01,0.1,by=0.01)

# scenarios for Battery tech and fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

Scenarios_name2 <-rbind(c("25% DVKMT", "50% DVKMT", "75% DVKMT"),c("25% DVKMT", "50% DVKMT", "75% DVKMT"),c("25% DVKMT", "50% DVKMT", "75% DVKMT"))

R_R <-rbind(c(50,100,150),c(100,200,300),c(200,400,600))

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)


# Values for fast charger in Ss --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)




U_R     <-NULL
#Define U for Urban trips
U_tem     <- rep(1,43)
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 300/300                          #Operational driving range
U_tem[2]        <- 200/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 10/10                            #Time frame for TCO analysis
U_tem[16]       <- 60/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.5/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.1/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 10/30                            #Time variable for loading/unloading activity in min
U_R <- rbind(U_R,U_tem)

#Define U for short-haul trips
U_tem     <- rep(1,43) 
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 500/300                          #Operational driving range
U_tem[2]        <- 400/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 8/10                            #Time frame for TCO analysis
U_tem[16]       <- 100/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.8/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.01/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

#Define U for long-haul trips
U_tem     <- rep(1,43)  
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 700/300                          #Operational driving range
U_tem[2]        <- 800/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 4/10                            #Time frame for TCO analysis
U_tem[16]       <- 100/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.95/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.002/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 4/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

Tripclass_names <- c("Urban", "Short-haul","Long-haul")


rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.A17.data <- NULL

for (Ui in 1:3){
  SChid <- 0
  U <- U_R[Ui,]
  
  for (Si in 1:3){
    
    U[32] <- Ss[Si,1]
    U[33] <- Ss[Si,2]
    U[34] <- Ss[Si,3]
    U[36] <- Ss[Si,4]
    
    for (S in S_R){
      U[35] <- S
      SChid <- SChid + 1
      
      for(Ri in 1:3){
        
        for (GVW in GVW_R){
          CT_tkm_temp  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[Ui,Ri]*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
          BET_tkm_temp <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[Ui,Ri]*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
          
          temp.data  <- data.frame(GVW = BET_tkm_temp$GVW, BatChaSs = Scenarios_name[SChid], Scenarios = Scenarios_name2[Ui,Ri], Tripclass = Tripclass_names[Ui], TimeBETtoCT = (BET_tkm_temp$TTime_km-CT_tkm_temp$TTime_km), TimeBETOCT = (BET_tkm_temp$TTime_km/CT_tkm_temp$TTime_km))
          Fig.A17.data <- rbind(Fig.A17.data, temp.data)
        }
        
      }
      
      
    }
  }  
  
  
}


###### Plotting Figure A17 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)

Fig.A17.S1 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[1]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") +
  labs(color="Operational trip profile:") +      
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S2 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[2]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.A17.S3 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[3]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S4 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[4]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "BET over CT delivery time ratio") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S5 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[5]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S6 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[6]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S7 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[7]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S8 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[8]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A17.S9 <- ggplot(data = subset(Fig.A17.data, Fig.A17.data$BatChaSs == Scenarios_name[9]) , aes(x = GVW, y = TimeBETOCT, color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), limit= c(0.95,2.4), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.A17 <- get_legend(Fig.A17.S1)
Fig.A17_row <- plot_grid(Fig.A17.S1 + theme(legend.position="none"),
                         Fig.A17.S2 + theme(legend.position="none"),
                         Fig.A17.S3 + theme(legend.position="none"),
                         Fig.A17.S4 + theme(legend.position="none"),
                         Fig.A17.S5 + theme(legend.position="none"),
                         Fig.A17.S6 + theme(legend.position="none"),
                         Fig.A17.S7 + theme(legend.position="none"),
                         Fig.A17.S8 + theme(legend.position="none"),
                         Fig.A17.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.A17 <- plot_grid(Fig.A17_row,legend_Fig.A17, ncol = 1, rel_heights = c(1, 0.1))



###### Figure A18 ###### 
###### creating dataframe for Figure A18 ######

# Set ranges for Fig A18

# U       <- rep(1,43)                              #Uncertainty over variables
i       <- 4                                      # Scenario 1 to 3 for different battery tech and scenario 4 for uncertainty in the scenario 1
GVW_R   <- seq(10,80,by=5)                       #seq (10,80,by=10), 4:80
ATRe_R  <- seq(-15,15,by=15)                       #seq (-20,15,by=1), -20:15
#R_R     <- c(100,200,400)                         #seq (100,1000,by=50)
#PE_R    <- seq(0.122*7,0.122/5, by=-(0.122/5))   #0.122*seq(0.122/2,0.122*7, 0.05)
#PD_R    <- seq(1.57/2,1.57*14, by=(1.57/3))      #1.57*seq(1.57/2,1.57*7, 0.05)
PE      <- 0.122                                  #0.122*seq(0.122/2,0.122*7, 0.05)
PD      <- 1.57                                    #1.57*seq(1.57/2,1.57*7, 0.05)
DVKMT   <- 500                                    #seq (300,1000,by=50)
UR      <- 1                                      #seq(0.5,1,by=0.05)
LUN     <- 0.06                                   #seq(0.01,0.1,by=0.01)

# scenarios for Battery tech and fast chargers
Scenarios_name <- c("ST-BBS & 200 kW ChPF", "ST-BBS & 450 kW ChPF", "ST-BBS & 1 MW ChPF","MT-BBS & 200 kW ChPF", "MT-BBS & 450 kW ChPF", "MT-BBS & 1 MW ChPF","LT-BBS & 200 kW ChPF", "LT-BBS & 450 kW ChPF", "LT-BBS & 1 MW ChPF")

Scenarios_name2 <-rbind(c("25% DVKMT", "50% DVKMT", "75% DVKMT"),c("25% DVKMT", "50% DVKMT", "75% DVKMT"),c("25% DVKMT", "50% DVKMT", "75% DVKMT"))

R_R <-rbind(c(50,100,150),c(100,200,300),c(200,400,600))

#Settings for battery specification in S1
S1 <- c(300/300, 125/125, 3000/3000, 75/75)

#U[32] <- 300/300
#U[33] <- 125/125
#U[34] <- 3000/3000
#U[36] <- 75/75

#Settings for battery specification in S2
S2 <- c(200/300, 250/125, 4500/3000, 78/75)

#Settings for battery specification in S3
S3 <- c(100/300, 400/125, 6000/3000, 95/75)

Ss <- rbind(S1, S2, S3)


# Values for fast charger in Ss --> via U[35]
S_R <- c(200/200, 450/200, 1000/200)




U_R     <-NULL
#Define U for Urban trips
U_tem     <- rep(1,43)
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 300/300                          #Operational driving range
U_tem[2]        <- 200/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 10/10                            #Time frame for TCO analysis
U_tem[16]       <- 60/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.5/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.1/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 10/30                            #Time variable for loading/unloading activity in min
U_R <- rbind(U_R,U_tem)

#Define U for short-haul trips
U_tem     <- rep(1,43) 
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 500/300                          #Operational driving range
U_tem[2]        <- 400/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 8/10                            #Time frame for TCO analysis
U_tem[16]       <- 100/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.8/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.01/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 5/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

#Define U for long-haul trips
U_tem     <- rep(1,43)  
U_tem[4]    <- 0
U_tem[43]    <- 0

#U_tem[40]       <- 700/300                          #Operational driving range
U_tem[2]        <- 800/500                          #Daily vehicle kilometers travel
U_tem[1]        <- 4/10                            #Time frame for TCO analysis
U_tem[16]       <- 100/80                            #Driving speed for free flow drive cycle
U_tem[39]       <- 0.95/0.75                         #Fraction of driving time with free flow speed
U_tem[27]       <- 0.002/0.06                         #Number of stops for load and unload activities per km
U_tem[12]       <- 4/5                              # Total number of loan payments in year
U_tem[17]       <- 30/30                            #Random Time variable for loading/unloading activity in min

U_R <- rbind(U_R,U_tem)

Tripclass_names <- c("Urban", "Short-haul","Long-haul")


rm(CT_km,CT_tkm,BET_km,BET_tkm)
CT_km  <- NULL
CT_tkm <- NULL
BET_km  <- NULL
BET_tkm  <- NULL
Fig.A18.data <- NULL

for (Ui in 1:3){
  SChid <- 0
  U <- U_R[Ui,]
  
  for (Si in 1:3){
    
    U[32] <- Ss[Si,1]
    U[33] <- Ss[Si,2]
    U[34] <- Ss[Si,3]
    U[36] <- Ss[Si,4]
    
    for (S in S_R){
      U[35] <- S
      SChid <- SChid + 1
      
      for(Ri in 1:3){
        
        for (GVW in GVW_R){
          CT_tkm_temp  <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[Ui,Ri]*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$CT_tkm
          BET_tkm_temp <- LCOD_all(i=i, GVW=GVW, ATRe=ATRe*U[7], R=R_R[Ui,Ri]*U[40], PE=PE*U[18], PD=PD*U[19], DVKMT=DVKMT*U[2], UR=UR*U[41], LUN=LUN*U[27])$BET_tkm
          
          temp.data  <- data.frame(GVW = BET_tkm_temp$GVW, BatChaSs = Scenarios_name[SChid], Scenarios = Scenarios_name2[Ui,Ri], Tripclass = Tripclass_names[Ui], dGVW = BET_tkm_temp$dGVW, lcChange= BET_tkm_temp$dGVW/BET_tkm_temp$lc)
          Fig.A18.data <- rbind(Fig.A18.data, temp.data)
        }
        
      }
      
      
    }
  }  
  
  
}


###### Plotting Figure A18 ######

theme_set(theme_classic())

comFiglayer = list(
  theme(panel.background = element_rect(fill = "white", colour = "grey50")),
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))
)

Fig.A18.S1 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[1]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[1]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S2 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[2]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[2]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()


Fig.A18.S3 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[3]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[3]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S4 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[4]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[4]) +
  theme(axis.title.x = element_blank()) +
  labs(y = "BET to CT Curb weight change in tonne") +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S5 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[5]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[5]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S6 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[6]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[6]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S7 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[7]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[7]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S8 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[8]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[8]) +
  labs(x = "Gross Vehicle Weight of CT in tonne") +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()

Fig.A18.S9 <- ggplot(data = subset(Fig.A18.data, Fig.A18.data$BatChaSs == Scenarios_name[9]) , aes(x = GVW, y = dGVW, , color = Tripclass, linetype = Scenarios)) +
  comFiglayer +
  geom_line(size = 1.25) + 
  geom_hline(yintercept = 0) + 
  theme(legend.position="bottom") + 
  labs(color="Operational trip profile:") +         
  labs(linetype="Operational driving range:") + 
  ggtitle(Scenarios_name[9]) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  scale_x_continuous(expand = c(0, 0), limit= c(5,85), breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 10)) +
  scale_color_npg()






legend_Fig.A18 <- get_legend(Fig.A18.S1)
Fig.A18_row <- plot_grid(Fig.A18.S1 + theme(legend.position="none"),
                         Fig.A18.S2 + theme(legend.position="none"),
                         Fig.A18.S3 + theme(legend.position="none"),
                         Fig.A18.S4 + theme(legend.position="none"),
                         Fig.A18.S5 + theme(legend.position="none"),
                         Fig.A18.S6 + theme(legend.position="none"),
                         Fig.A18.S7 + theme(legend.position="none"),
                         Fig.A18.S8 + theme(legend.position="none"),
                         Fig.A18.S9 + theme(legend.position="none"),
                         ncol = 3,
                         nrow = 3,
                         hjust = -2)
Fig.A18 <- plot_grid(Fig.A18_row,legend_Fig.A18, ncol = 1, rel_heights = c(1, 0.1))






