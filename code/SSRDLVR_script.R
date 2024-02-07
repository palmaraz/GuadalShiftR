if (!require(librarian)) install.packages('librarian')

librarian::shelf(tidyverse,readODS,runjags,coda,ggmcmc,bayesplot,dygraphs,qgraph,
                 imputeTS,ggridges,viridis,bayestestR,mvtnorm,ggbreak,psych,data.table,reshape2,patchwork)
# seed = 3125796
seed = 827545
set.seed(seed)

runjags.options(inits.warning=F,
                rng.warning=F,
                blockignore.warning=F,
                blockcombine.warning=F,
                nodata.warning=F,
                silent.jags=F,
                silent.runjags=F)
testjags()

source("R/functions.R")

Count_data = read_ods("././data-raw/data_raw.ods", sheet = "CountsFlood")

scal_Fact=1000

# December data
DecTS = Count_data[-1,] %>%
  dplyr::filter(Month == 12) %>%
  dplyr::select(Year,
                Anas_acuta,
                Anas_clypeata,
                Anas_crecca,
                Anas_penelope,
                Anas_platyrhynchos,
                Anas_strepera,
                Anser_anser,
                Aythya_ferina,
                Netta_rufina,
                Tadorna_tadorna)

# January data
JanTS = Count_data[-1,] %>%
  dplyr::filter(Month == 1) %>%
  dplyr::select(Year,
                Anas_acuta,
                Anas_clypeata,
                Anas_crecca,
                Anas_penelope,
                Anas_platyrhynchos,
                Anas_strepera,
                Anser_anser,
                Aythya_ferina,
                Netta_rufina,
                Tadorna_tadorna)

WaterfowlData = Count_data[-nrow(Count_data),] %>%
  dplyr::mutate(time=rep(1:36, each=2)) %>%
  dplyr::select(time,
                Anas_acuta,
                Anas_clypeata,
                Anas_crecca,
                Anas_penelope,
                Anas_platyrhynchos,
                Anas_strepera,
                Anser_anser,
                Aythya_ferina,
                Netta_rufina,
                Tadorna_tadorna) %>%
  reshape2::melt(., id=c("time")) %>%
  dplyr::mutate(obs=value,
                ts=as.numeric(variable),
                time=time) %>%
  dplyr::select(obs,ts,time)

Names = c("Pintail","Shoveler","Common teal","Eurasian wigeon","Mallard",
          "Gadwall","Greylag goose","Common pochard","Red-crested pochard","Shelduck")

flood = read_ods("././data-raw/guadalshift.ods")$Flood.exp

# Pre-Pinatubo ####

NYears = 14
est.k = WaterfowlData %>% dplyr::filter(time < NYears) %>% group_by(ts) %>% summarise_all(mean, na.rm=T)

NSpecies = 10

data_list_LVR_regime_SSVS = list(
  NSpecies = NSpecies,
  NYears = NYears,
  n1 = as.matrix((log(DecTS[1:NYears,2:11]/scal_Fact + 1))),
  n2 = as.matrix((log(JanTS[1:NYears,2:11]/scal_Fact + 1))),
  flood = scale(Env_data[1:NYears,'Flood'])[,1],
  est.k = est.k$obs/scal_Fact)

# data_full_to_imp = as.matrix(data_list_LVR_regime_SSVS$n1)
#
# for(i in 1:ncol(data_full_to_imp)){
#
#   if (any(is.na(data_full_to_imp[,i]))) {
#     temp_imp = pmax(imputeTS::na_kalman(data_full_to_imp[,i],
#                                         model = arima(data_full_to_imp[,i], order=c(1,0,0))$model,
#                                         smooth = TRUE),0)
#     data_full_to_imp[,i] <- temp_imp
#
#   }
#
#   else data_full_to_imp[,i] <- data_full_to_imp[,i]
#
# }
#
# data_list_LVR_regime_SSVS$n1 = data_full_to_imp
#
# data_full_to_imp = as.matrix(data_list_LVR_regime_SSVS$n2)
#
# for(i in 1:ncol(data_full_to_imp)){
#
#   if (any(is.na(data_full_to_imp[,i]))) {
#     temp_imp = pmax(imputeTS::na_kalman(data_full_to_imp[,i],
#                                         model = arima(data_full_to_imp[,i], order=c(1,0,0))$model,
#                                         smooth = TRUE),0)
#     data_full_to_imp[,i] <- temp_imp
#
#   }
#
#   else data_full_to_imp[,i] <- data_full_to_imp[,i]
#
# }
#
# data_list_LVR_regime_SSVS$n2 = data_full_to_imp

r_mean=c(0.3642208,0.4838519,0.2205646,0.2493296,0.5639658,0.2970468,1.1095769,0.4966023,0.3705176,0.3716657)
b_mean=c(-0.8544,-1.05,0.6868,0.03144,0.3442,-0.06122,-0.3627,-1.763,-3.054,-1.235)

inits_LVR_regime_SSVS=function(){
  list(
    prob_inter=rbeta(1,2,8),
    active_alpha=replicate(NSpecies,rnorm(NSpecies,0,0.1)) + diag(NA, NSpecies),
    # alpha=replicate(NSpecies,rnorm(NSpecies,0,0.1)) + diag(NA, NSpecies),
    g.alpha=replicate(NSpecies,rbinom(NSpecies,1,0.5)) + diag(NA, NSpecies),
    Obs.prec.mat=diag(0.1,NSpecies),
    Sys.prec.mat=diag(0.1,NSpecies),
    k=pmax(rnorm(NSpecies, est.k$obs/scal_Fact, 1), 0.1),
    # r=pmax(rnorm(NSpecies, r_mean, 0.1), 0.1),
    b=rnorm(NSpecies, b_mean,0.1),
    gamma=rnorm(NSpecies, b_mean, 0.1),
    .RNG.seed=seed)
}

parameters_LVR_regime_SSVS = c("prob_inter","g.alpha","alpha","k","r","b","gamma",
                               "Corr.sigma","Corr.tau",
                               "state","n1_ppc","n2_ppc")

# Run model ####
LVR_regime_SSVS_analysis_PrePinatubo = run.jags(
  data=data_list_LVR_regime_SSVS,
  inits=inits_LVR_regime_SSVS,
  monitor=parameters_LVR_regime_SSVS,
  model=read.jagsfile("code/LVRSS_regime_flood_ifelse.JAGS"),
  n.chains = 3,
  adapt = 5000,
  burnin = 5000,
  sample = 1000,
  thin = 1,
  method='parallel',
  modules = 'glm',
  # keep.jags.files=file.path(paste("analyses/LVRSS/LVRSS_regime_results/","mcmc_run_pre",sep = "")),
  summarise=TRUE)

save(LVR_regime_SSVS_analysis_PrePinatubo, file="analyses/LVRSS/LVRSS_regime_results/JAGS_object_LVR_regime_SSVSanalysis_PrePinatubo.RData")

post_vars = as.data.frame(combine.mcmc(as.mcmc.list(LVR_regime_SSVS_analysis_PrePinatubo)))
nloops = nrow(post_vars)

Feasibility_domain = dplyr::select(post_vars, starts_with(c("alpha","r","k")))

ggs_object = ggs(as.mcmc.list(LVR_regime_SSVS_analysis_PrePinatubo))

ggmcmc(ggs_object,family="prob_inter",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_prob_inter_pre.pdf",
       param_page = 5, width=6, height=6)

ggmcmc(ggs_object,family="b",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_b_pre.pdf",
       param_page = 10, width=6, height=6)

ggmcmc(ggs_object,family="alpha",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_alpha_pre.pdf",
       param_page = 10, width=6, height=12)

ggmcmc(ggs_object,family="k",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_k_pre.pdf",
       param_page = 10, width=6, height=12)

ggmcmc(ggs_object,family="r",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_r_pre.pdf",
       param_page = 10, width=6, height=12)

alpha = dplyr::select(post_vars, starts_with("alpha"))

# alpha[,c("alpha[1,1]","alpha[2,2]","alpha[3,3]","alpha[4,4]","alpha[5,5]",
#          "alpha[6,6]","alpha[7,7]","alpha[8,8]","alpha[9,9]","alpha[10,10]")]=NA

# alpha and p_i matrices ####
alpha_mean_matrix = matrix(colMeans(alpha), ncol=NSpecies, nrow=NSpecies)

alpha_point_mean = as.data.frame(cbind(rownames(as.data.frame(colMeans(alpha))),
                                       as.data.frame(colMeans(alpha))))
colnames(alpha_point_mean) = c("variable","value")
plot=ggplot(reshape2::melt(alpha), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  geom_vline(xintercept=0) +
  # geom_point(data = alpha_point_mean, col="green") +
  scale_x_continuous(limits = c(-0.5,1.2)) +
  scale_y_discrete() +
  scale_fill_viridis(name = expression(alpha[ij]), option = "C") +
  labs(x="Interaction strength") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
plot
ggsave(file="analyses/LVRSS/figs/alpha_density_PrePinatubo.pdf", plot=plot,
       width=6.5, height=NSpecies^1.2, limitsize = FALSE)

# Posterior mean of interaction probability hyperprior: ####
# prob_interaction_mean = mean(dplyr::select(post_vars,starts_with("prob_inter"))[[1]])
# prob_interaction_mean = 0.3
(prob_interaction = map_estimate(dplyr::select(post_vars,starts_with("prob_inter"))))
prob_interaction_mean = prob_interaction$MAP_Estimate
(prob_interaction_HDI = bayestestR::hdi(dplyr::select(post_vars,starts_with("prob_inter")), ci = 0.9))

# Assemble values for g.alpha's:
g_alpha_matrix = dplyr::select(post_vars, starts_with("g.alpha"))
g_alpha_mean = matrix(colMeans(g_alpha_matrix), ncol=NSpecies, nrow=NSpecies)
g_alpha_matrix[,c("g.alpha[1,1]","g.alpha[2,2]","g.alpha[3,3]","g.alpha[4,4]","g.alpha[5,5]",
                  "g.alpha[6,6]","g.alpha[7,7]","g.alpha[8,8]","g.alpha[9,9]","g.alpha[10,10]")]=NA
g_alpha_point_mean = as.data.frame(cbind(rownames(as.data.frame(colMeans(g_alpha_matrix))),
                                         as.data.frame(colMeans(g_alpha_matrix))))
colnames(g_alpha_point_mean) = c("variable","value")

g_alpha_matrix2=g_alpha_matrix
g_alpha_point_mean2=g_alpha_point_mean

names(g_alpha_matrix2) <- gsub(x = names(g_alpha_matrix2), pattern = "g.alpha", replacement = "p_i")
rownames(g_alpha_point_mean2) <- gsub(x = rownames(g_alpha_point_mean2), pattern = "g.alpha", replacement = "p_i")
g_alpha_point_mean2$variable <- gsub(x = g_alpha_point_mean2$variable, pattern = "g.alpha", replacement = "p_i")

plot=ggplot(reshape2::melt(g_alpha_matrix2), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  # geom_density_ridges_gradient(stat = "binline", bins = 250, scale = 0.95, draw_baseline = FALSE) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  # geom_vline(xintercept=0, lty="dotdash") +
  geom_vline(aes(xintercept=prob_interaction$MAP_Estimate,col="Average")) +
  # geom_vline(aes(xintercept=prob_interaction$MAP_Estimate,col="Intra")) +
  scale_color_manual(name = "Probability of\n interaction", values = c(Average = "red", Intra = "blue")) +
  geom_vline(xintercept=1, lty="dotdash") +
  geom_point(data = g_alpha_point_mean2, col="red") +
  scale_x_continuous() +
  scale_y_discrete() +
  scale_fill_viridis(name = expression(italic(p)[i]), option = "C") +
  labs(x="Posterior probability of interaction") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
plot
ggsave(file="analyses/LVRSS/figs/p_i_density_PrePinatubo.pdf", plot=plot,
       width=6.5, height=NSpecies^1.2, limitsize = FALSE)


g_alpha_matrix = dplyr::select(post_vars, starts_with("g.alpha"))
g_alpha_mean = matrix(colMeans(g_alpha_matrix), ncol=NSpecies, nrow=NSpecies)

Bayes_factors = matrix(sapply(g_alpha_mean, function(x){
  2*log(x/(1-x)*(1-prob_interaction$MAP_Estimate)/prob_interaction$MAP_Estimate)}), ncol=NSpecies, nrow=NSpecies)

# Bayes factors ####
# For the intra- coefficients!!:
# diag(Bayes_factors) = sapply(diag(g_alpha_mean), function(x){
#   2*log(x/(1-x)*(1-prob_interaction_Intra_mean)/prob_interaction_Intra_mean)})
diag(Bayes_factors) = 0

# Replace values when Bayes factor is very close to 0 or 1
Bayes_factors[Bayes_factors == Inf] = 2*log(5e+50)
Bayes_factors[Bayes_factors < 0] = 0

Bayes_factors_Intra = mean(diag(Bayes_factors))
Bayes_factors_Intra_SD = sd(diag(Bayes_factors))

Bayes_factors_Inter = mean(Bayes_factors + diag(NA, NSpecies), na.rm=TRUE)
Bayes_factors_Inter_SD = sd(Bayes_factors + diag(NA, NSpecies), na.rm=TRUE)

# Plot matrix of Bayes factor in Kass & Raftery scale (1995):
gradient_color = colorRampPalette(c('white', 'darkorange', 'firebrick'))
plot=ggplot(reshape2::melt(t(Bayes_factors)), aes(Var1, Var2, fill=value)) +
  geom_raster() +
  scale_fill_gradient(name="Bayes factor in\nKass & Raftery scale", low = gradient_color(1), high = gradient_color(250)) +
  scale_x_continuous(breaks=rep(1:NSpecies, 1), position = "top") +
  scale_y_reverse(breaks=rep(1:NSpecies, 1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x="Functional type", y = "Functional type")
plot
ggsave(file="analyses/LVRSS/figs/bayes_factors_PrePinatubo.pdf",plot=plot, width=7, height=5)

# Environmental effects ####

gamma = dplyr::select(post_vars, starts_with("gamma"))
colnames(gamma) = Names

gamma_PrePinatubo = ggplot(reshape2::melt(gamma), aes(x=value, y=reorder(variable, desc(variable)),  fill= after_stat(x))) +
  geom_density_ridges_gradient(stat = "binline", bins = 100, scale = 1.5, draw_baseline = FALSE) +
  geom_vline(xintercept=0) +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  scale_y_discrete(expression(N)) +
  scale_fill_viridis(name = "Posterior\nvalue", option = "magma") +
  labs(x="Posterior value",
       title = "Effect of flooding extension") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=18))
gamma_PrePinatubo
ggsave(file="analyses/LVRSS/figs/Flooding_impact_PrePinatubo.pdf", plot=gamma_PrePinatubo,
       width=7.5, height=5.5, limitsize = FALSE)

# Stability measures ####
alpha = dplyr::select(post_vars, starts_with("alpha"))
alpha_mean = matrix(colMeans(alpha),NSpecies,NSpecies)
k_vec = dplyr::select(post_vars, starts_with("k"))
k_mean = as.numeric(colMeans(k_vec))
r_vec = dplyr::select(post_vars, starts_with("r"))[,1:NSpecies]
r_mean = as.numeric(colMeans(r_vec))

(EqAbund = solve(alpha_mean) %*% k_mean)
# (jacobi2 = numDeriv::jacobian(function(n) n + r_mean*(1 - (alpha_mean %*% exp(n))/k_mean), EqAbund, "complex"))
(jacobi2 = numDeriv::jacobian(function(N) N*exp(r_mean*(1 - (alpha_mean %*% N)/k_mean)), EqAbund, "complex"))
(max_eig2=max(Mod(eigen(jacobi2)$values)))

nloops = nrow(post_vars)

# Create the objects to be populated:
DynDimension = c()
Entropy = c()
NormDepart = c()
det_A = c()
Sth_rootDetA = c()
DynStab=c()
DynStab_cont=c()
Resilience=c()
Slow_Eigenv = c()
Slow_Eig_Elas = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
DynStab_Elas = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
OmegaL1_mcmc=c()
Logofet_mcmc=c()
Stoch_invar_mcmc=c()
WC_Reactivity_mcmc=c()

# Guy Bunin, Ecological communities with Lotka-Volterra dynamics, Phys. Rev. E 95, 042414 (2017)
alpha_mean=c()
alpha_var=c()
alpha_corr=c()

Centrality_alpha=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
Centrality_jacob=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
Centrality_nNEM=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)

nNEM_jacobi=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
jacobi_mat = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
Eigenvals = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
# VERY IMPORTANT: Stability in the continuous-time counterpart of the discrete-time Jacobian
# Discrete-continuous model conversion (Shieh, L.S., Wang, H., Yates, R.E. 1980 10.1016/0307-904X(80)90177-8)
# Package 'expm', function logm, converting from continuous to discrete and back!!
# See Higham, Nicholas J. 2008. Functions of Matrices. Theory and Computation, page 39
# Eigenvals_cont = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
NEM_jacobi_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
nNEM_jacobi_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
NEM_alpha_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
nNEM_alpha_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
EqAbund = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)

initial_time = proc.time()

pb = txtProgressBar(min = 0, max = nloops, initial = 0, char = "*", width = 60)

for (j in 1:nloops){

  # Interaction matrix
  alpha_mat = matrix(as.matrix(as.numeric(alpha[j,])), nrow = NSpecies, ncol = NSpecies)

  # Guy Bunin, Ecological communities with Lotka-Volterra dynamics, Phys. Rev. E 95, 042414 (2017)
  alpha_mat_diagNA = alpha_mat; diag(alpha_mat_diagNA)=NA
  alpha_mean[j] = mean(na.omit(as.vector(alpha_mat_diagNA)))
  alpha_var[j] = var(na.omit(as.vector(alpha_mat_diagNA)))/(1 - mean(na.omit(as.vector(alpha_mat_diagNA))))
  alpha_corr[j] = cor(na.omit(as.vector(alpha_mat_diagNA)), na.omit(as.vector(t(alpha_mat_diagNA))))

  k_vector = as.vector(as.numeric(k_vec[j,]))
  r_vector = as.vector(as.numeric(r_vec[j,]))
  EqAbund[j, ] = as.vector(solve(alpha_mat) %*% k_vector)

  if(all(EqAbund[j, ] >= 0)) {

    # Jacobian (community) matrix:
    # Three ways of building the Jacobian:
    # jacobian = diag(NSpecies) - ((r_vector/k_vector) * alpha_mat * EqAbund[j, ])
    # jacobian = numDeriv::jacobian(function(n) n + r_vector*(1 - (alpha_mat %*% exp(n))/k_vector), EqAbund[j, ], "complex")
    jacobian = numDeriv::jacobian(function(N) N*exp(r_vector*(1 - (alpha_mat %*% N)/k_vector)), EqAbund[j, ], "complex")
    # jacobian = numDeriv::jacobian(function(n) n + r_vector*(1 - (alpha_mat %*% exp(n))/k_vector), log(EqAbund[j, ]), "complex")


    # DynamicDimensionality
    # David Gilljam (2016) Structure and Stability of Ecological Networks The role of dynamic dimensionality and species variability in resource use. PhD

    # In a system which ‘deterministically’ acts to recover along both dimensions (directions)
    # at similar rates, the risk of species going extinct due to destabilizing environmental
    # fluctuations is likely to be lower compared to a system which recover along one
    # dimension (direction) very quickly and along the other very slowly. The former system
    # could be characterised as having a higher dynamic dimensionality (Roughgarden 1998, p.339.
    # Dynamic dimensionality can be quantified as the inverse participation
    # ratio (inverse Simpson index) of the eigenvalues (real parts) of the Jacobian
    # (community) matrix of the system (see Paper II for details). A high DD means that the
    # real-part of the eigenvalues are of similar magnitude and that the system therefore
    # approaches the equilibrium from all directions at a similar rate (Fig. 11). On the other
    # hand, when DD is low one (or a few) of the eigenvalues has a large magnitude
    # compared to the others and the deterministic forces pulling the system towards
    # equilibrium is therefore weak in many directions compared to the stochastic forces
    # pushing the system away from the equilibrium. As a consequence the risk of crossing
    # extinction thresholds increases, and hence persistence and time to first extinction
    # decreases, as DD decreases.
    DynDim = DynamicDimensionality(A=alpha_mat, J=jacobian, SVD_Entropy = TRUE)
    DynDimension[j] = DynDim[["DynDim"]]
    Entropy[j] = DynDim[["Entropy"]]

    jacobi_mat[j,] = as.vector(jacobian)

    # Robust coexistence, see "Theory-Based Ecology A: Darwinian approach", TBox 9.3, p. 184, Barabas et al 2015 doi:10.1111/ele.12350
    # Is simply the determinant of alpha matrix (!!, not the Jacobi matrix):
    # "if det(a)  is not zero but close to it, then the components of alpha become
    # large and the equilibrium values, calculated from eq. (9.19), depend very sensitively on ri.
    # This is the formal basis of the phenomenon observed in Figure 9.2 that small values
    # "...of the determinant spoil the robustness of the coexistence":
    # that is, we have concluded that the condition for robust coexistence is a sufficient difference in regulation,
    # expressed by det a not being too close to zero":
    det_A[j] = det(alpha_mat)
    Sth_rootDetA[j] = exp(mean(log(Mod(eigen(alpha_mat,only.values = TRUE)$values))))

    # Departure from normality:
    # Calculate the degree of departure from normality of a square matrix, Barabas & Allesina (2015),
    # Supp. Mat, p. 15, Predicting global community properties from uncertain estimates of interaction strengths.
    # Departures from normality < 1 means the spectra has low sensitivity to perturbations
    NormDepart[j] = depn(alpha_mat)

    Eigenvals[j,] = matrix(eigen(jacobian, only.values = TRUE)$values, nrow=1)

    Resilience[j] = 1/max(abs(Re(Eigenvals[j,])))

    DynStab[j] = max(Mod(Eigenvals[j,]))

    # Snyder (2005) What makes ecological systems reactive? 10.1016/j.tpb.2010.03.004
    # WC_Reactivity_mcmc[j] = max(Mod(eigen(t(alpha_mat)%*%alpha_mat)$values)) - 1
    WC_Reactivity_mcmc[j] = log(max(Mod(eigen(t(jacobian)%*%jacobian, only.values = TRUE)$values)))

    OmegaL1_mcmc[j] = Omega(alpha_mat)

    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%*%diag(NSpecies) - alpha_mat%*%alpha_mat),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%*%diag(NSpecies) - alpha_mat%*%alpha_mat),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%x%diag(NSpecies) - alpha_mat%x%alpha_mat),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%x%diag(NSpecies) - alpha_mat%x%alpha_mat),"2"))^-1)

    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%*%diag(NSpecies) - jacobian%*%jacobian),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%*%diag(NSpecies) - jacobian%*%jacobian),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%x%diag(NSpecies) - jacobian%x%jacobian),"2"))^-1)
    Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%x%diag(NSpecies) - jacobian%x%jacobian),"2"))^-1)

    for (k in 1:NSpecies){

      Logofet_mcmc[j] = abs(det(alpha_mat))/prod(sum(abs(alpha_mat[k,])))

    }
    # VERY IMPORTANT: Stability in the continuous-time counterpart of the discrete-time Jacobian
    # Discrete-continuous model conversion (Shieh, L.S., Wang, H., Yates, R.E. 1980 10.1016/0307-904X(80)90177-8)
    # Package 'expm', function logm, converting from continuous to discrete and back!!
    # See Higham, Nicholas J. 2008. Functions of Matrices. Theory and Computation, page 39
    # Eigenvals_cont[j,] = matrix(eigen(logm(jacobian,"Eigen"))$values, nrow=1)

    # VERY IMPORTANT: Stability in the continuous-time counterpart of the discrete-time Jacobian
    # Discrete-continuous model conversion (Shieh, L.S., Wang, H., Yates, R.E. 1980 10.1016/0307-904X(80)90177-8)
    # Package 'expm', function logm, converting from continuous to discrete and back!!
    # See Higham, Nicholas J. 2008. Functions of Matrices. Theory and Computation, page 39
    # DynStab_cont[j] = max(Re(eigen(logm(jacobian,"Eigen"))$values))

    # Idea from Hanski & Ovaskainen 2000, The metapopulation capacity of a fragmented landscape,
    # Relat. contribution of species i to the leading eigenvalue, see
    # Solé & Bascompte 2006 Self-organization in complex ecosystems, bottom page 200
    # See also Arnoldi et al The inherent multidimensionality of temporal variability 2019 10.1111/ele.13345

    for(i in 1:NSpecies) {
      DynStab_Elas[j,i] =
        max(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,1]^2)[i]/
        sum(max(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,1]^2))
    }

    Slow_Eigenv[j] = min(Mod(eigen(jacobian)$values)) # slowest eigenvalue, see Cenci & Saavedra () Nonlinear structural stability
    # and Arnoldi et al The inherent multidimensionality of temporal variability 2019 10.1111/ele.13345

    for(i in 1:NSpecies) {
      Slow_Eig_Elas[j,i] =
        min(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,NSpecies]^2)[i]/
        sum(min(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,NSpecies]^2))
    }

    for(i in 1:NSpecies){

      Centrality_alpha[j,i] = sum(abs(alpha_mat[i,])) + sum(abs(alpha_mat[,i])) - 1
      Centrality_jacob[j,i] = sum(abs(jacobian[i,])) + sum(abs(jacobian[,i]))
      Centrality_nNEM[j,i] = sum(abs(nNEM_jacobi[i,])) + sum(abs(nNEM_jacobi[,i])) - abs(nNEM_jacobi[i,i])

    }
  }

  setTxtProgressBar(pb,j)
  final_time = proc.time() - initial_time
  if (j == nloops*0.25) cat(" 25% completed in", final_time[3], "sec!\n")
  final_time = proc.time() - initial_time
  if (j == nloops*0.5) cat(" 50% completed in", final_time[3], "sec!\n")
  final_time = proc.time() - initial_time
  if (j == nloops*0.75) cat(" 75% completed in", final_time[3], "sec!\n")
  final_time = proc.time() - initial_time
  if (j == nloops) cat(" 100% completed in", final_time[3], "sec!\n")

}

# Plot stability measures ####
stability_measures_PrePinatubo = as.data.frame(cbind(
  NormDepart,
  # det_A,
  Sth_rootDetA,
  DynDimension,
  Entropy,
  # Sth_rootDetA,
  Resilience,
  # DynStab,
  WC_Reactivity_mcmc,
  # Logofet_mcmc,
  # OmegaL1_mcmc,
  Stoch_invar_mcmc))
# colnames(stability_measures_PrePinatubo) = c("Departure from normality","Robustness of coexistence","Sth_rootDetA","Resilience","Dynamic stability",
#                                  "Worst-case reactivity", "Logofet's equilibriumness","Structural stability,"Stochastic invariability")
colnames(stability_measures_PrePinatubo) = c("Departure from normality","Robustness of coexistence","Dynamic dimensionality","Entropy",
                                             "Resilience","Reactivity","Structural stability")

stability_measures_PrePinatubo = stability_measures_PrePinatubo[complete.cases(stability_measures_PrePinatubo),]

save(LVR_regime_SSVS_analysis_PrePinatubo, stability_measures_PrePinatubo,
     file="analyses/LVRSS/LVRSS_regime_results/JAGS_object_LVR_regime_SSVSanalysis_PrePinatubo.RData")


# (MAP_stability = map_estimate(stability_measures_PrePinatubo))
(HDInterval_stability = bayestestR::hdi(as.mcmc(stability_measures_PrePinatubo), credMass = 0.9, allowSplit = TRUE))
map_estimate(subset(stability_measures_PrePinatubo$`Departure from normality`, stability_measures_PrePinatubo$`Departure from normality`< 1))
p_direction(stability_measures_PrePinatubo$Reactivity)

plot_stab = ggplot(reshape2::melt(stability_measures_PrePinatubo), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  geom_density_ridges_gradient(stat = "binline", bins = 250, scale = 0.95, draw_baseline = FALSE) +
  # geom_density_ridges_gradient(scale = 1.1, rel_min_height = 0.01) +
  geom_vline(xintercept=0, lty="dotdash") +
  geom_vline(xintercept=1, col="red") +
  scale_x_break(c(2.6, 9.4), scales = 0.65) +
  scale_fill_viridis(name = "Value", option = "magma") +
  scale_color_manual(name = element_blank()) +
  labs(x="Stability measures",
       title = "Metrics of matrix stability") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=18))
plot_stab
ggsave(file="analyses/LVRSS/figs/Stability_measures_PrePinatubo.pdf", plot=plot_stab, onefile=FALSE,
       width=7.5, height=6.5, limitsize = FALSE)

pdf("analyses/LVRSS/figs/Correlation_matrix_PrePinatubo.pdf",width=5.5, height=5.5,paper='special')
pairs.panels(stability_measures_PrePinatubo[,c("Departure from normality","Entropy","Reactivity","Structural stability")],
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()

# Plot unit circle, prob. of dynamic stability ####

Eigenvals = Eigenvals[complete.cases(Eigenvals),]

ReEigenvals = reshape2::melt(Re(Eigenvals), value.name="Real_part")
ImEigenvals = reshape2::melt(Im(Eigenvals), value.name="Imaginary_part")
Eigenvals_toplot = as.data.frame(cbind(ReEigenvals[,c("Real_part")],ImEigenvals[,c("Imaginary_part")]))
colnames(Eigenvals_toplot) = c("Real_part","Imaginary_part")
Eigenvals_toplot$Modulus = sqrt((Eigenvals_toplot$Real_part)^2 + (Eigenvals_toplot$Imaginary_part)^2)

(EmpProbDynStab = sum(Eigenvals_toplot$Modulus < 1)/length(Eigenvals_toplot$Modulus))

th = seq(-pi, pi, len = 100)
z = exp((0+1i) * th)
UnitCircle=as.data.frame(cbind(Re(z),Im(z)))
Origin=data.frame(x=0,y=0)

unitcircle_plot_PrePinatubo = ggplot(data=UnitCircle,aes(Re(z),Im(z))) +
  geom_path() +
  geom_vline(xintercept = 0,lty="dotted") +
  geom_hline(yintercept = 0,lty="dotted") +
  geom_point(data=Eigenvals_toplot,aes(x=Real_part,y=Imaginary_part),size=0.5,col="red") +
  # stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white") +  # Add density contour
  scale_x_continuous(limits = c(-1.5,1.5)) +
  scale_y_continuous(limits = c(-1.5,1.5)) +
  labs(x="Real", y = "Imaginary") +
  theme(
    axis.line.x=element_line(linewidth=0.5,colour="Black"),
    axis.line.y=element_line(linewidth=0.5,colour="Black"),
    axis.text=element_text(size=18,colour="Black"),
    axis.title=element_text(size=18,colour="Black"),
    plot.title = element_text(size=15),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA)) +
  ggtitle(paste0("Resilience: ", round(map_estimate(Resilience), 3),
                 " (", round(bayestestR::hdi(Resilience, credMass = 0.9, allowSplit = TRUE)[["CI_low"]], 3),",",
                 round(bayestestR::hdi(Resilience, credMass = 0.9, allowSplit = TRUE)[["CI_high"]],3), ") MAP, 90% HDI", sep=""),
          paste0("Probability of dynamic stability: ", round(EmpProbDynStab, 4),"\n", sep=""))
unitcircle_plot_PrePinatubo
ggsave("analyses/LVRSS/figs/Probability_of_stability_PrePinatubo.pdf", unitcircle_plot_PrePinatubo, height = 6.5, width = 6)

# Plot feasibility ####

(EqAbund_mean = colMeans(EqAbund,na.rm=T))
(EqAbund_sd = colSd(EqAbund))

EquilAbund = as.data.frame(EqAbund)
colnames(EquilAbund) = c("Pintail","Shoveler","Common teal","Eurasian wigeon","Mallard",
                         "Gadwall","Greylag goose","Common pochard","Red-crested pochard","Shelduck")

# Probability of species extinction
ProbSpExtinct=matrix(NA,1,NSpecies)
for(g in 1:NSpecies){
  ProbSpExtinct[,g] = length(which(apply(as.data.frame(EquilAbund[,g]), 1, function(row) any(row <= 0))))/nrow(EquilAbund)
}

(EmpProbFeas = (dim(EquilAbund)[1] - length(which(apply(EquilAbund, 1, function(row) any(row < 0)))))/dim(EquilAbund)[1])

k_posterior = as.data.frame(map_estimate(k_vec))
rownames(k_posterior) = c("Pintail","Shoveler","Common teal","Eurasian wigeon","Mallard",
                          "Gadwall","Greylag goose","Common pochard","Red-crested pochard","Shelduck")

k_posterior = as.data.frame(cbind(rownames(k_posterior),as.data.frame(k_posterior$MAP_Estimate)))
colnames(k_posterior) = c("variable","value")

plot_feas_PrePinatubo = ggplot(reshape2::melt(EquilAbund), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  geom_density_ridges_gradient(stat = "binline", bins = 100, scale = 1.5, draw_baseline = FALSE) +
  # geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  geom_vline(xintercept=0) +
  scale_x_continuous(limits = c(-20,100)) +
  scale_y_discrete(expression(N)) +
  scale_fill_viridis(name = expression("Equilibrium \nabundance,"~italic(N)^"*"), option = "viridis") +
  geom_point(data = k_posterior, aes(col="k_posterior"), size=2) +
  scale_color_manual(name = element_blank(),
                     values = c(k_posterior = "brown"),
                     labels = "Carrying capacity") +
  # labs(x=expression('Posterior biomass, ' ~ mu * g * C * L^-1),
  labs(x="Abundance (n. ind x1000)",
       title = paste("Probability of feasibility: ",round(EmpProbFeas,3))) +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=18))
plot_feas_PrePinatubo
ggsave(file="analyses/LVRSS/figs/Prob_of_feasibility_PrePinatubo.pdf", plot=plot_feas_PrePinatubo,
       width=7.5, height=5.5, limitsize = FALSE)

# Plot interaction networks ####
pdf("analyses/LVRSS/figs/alpha_spring_PrePinatubo.pdf",width=9.5, height=6.5)
plot=qgraph(t(alpha_mean_matrix),layout="spring",
            diag=T,
            edge.labels=FALSE,
            edge.label.cex=2,
            edge.label.bg=TRUE,
            posCol="red",
            negCol="blue",
            # vsize=as.matrix(node.size),
            # title="Normalized Net Effects Matrix, alpha",
            # title.cex = 1.5,
            # groups = plankton_group,
            nodeNames = Names,
            palette = "colorblind")
title(main = "Interaction matrix", line = 3, cex.main = 1.5)
dev.off()

jacobian_Mean = matrix(colMeans(jacobi_mat),NSpecies,NSpecies)
pdf("analyses/LVRSS/figs/Community_matrix_PrePinatubo.pdf",width=9.5, height=6.5)
plot=qgraph(t(jacobian_Mean),layout="spring",
            edge.labels=FALSE,
            edge.label.cex=2,
            edge.label.bg=TRUE,
            posCol="red",
            negCol="blue",
            # vsize=as.matrix(node.size),
            # title="Normalized Net Effects Matrix, alpha",
            # title.cex = 1.5,
            # groups = plankton_group,
            nodeNames = Names,
            palette = "colorblind")
title(main = "Community matrix", line = 3, cex.main = 1.5)
dev.off()

# Model probabilities ####
# (check: https://r-nimble.org/nimbleExamples/RJMCMC_example.html)!!

modprob = as.data.table((g_alpha_matrix != 0) + 0)
colnames(modprob) = gsub(",", "", colnames(g_alpha_matrix))
ModProbRes = modprob[,.N, by=names(modprob)]
ModProbRes = ModProbRes[order(N, decreasing = TRUE)]
ModProbRes = ModProbRes[, ModProb := N/dim(g_alpha_matrix)[1]]
MostProbModel = matrix(unlist(matrix(ModProbRes[1,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)
SecondMostProbModel = matrix(unlist(matrix(ModProbRes[2,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)
ThirdMostProbModel = matrix(unlist(matrix(ModProbRes[3,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)

Model_list_ordered = list()
for(i in 1:nrow(ModProbRes)) Model_list_ordered[[i]] = matrix(unlist(matrix(ModProbRes[i,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)

pdf("analyses/LVRSS/figs/RJModel8NW_PrePinatubo.pdf",height=2,width=4)
par(mfrow=c(2,4),mai=c(1,1,1,1))
for(i in 1:8){
  qgraph(matrix(unlist(t(matrix(ModProbRes[i,-c("N","ModProb")],ncol=NSpecies))),ncol=NSpecies),
         layout='circle',
         edge.color="azure4",
         directed=T,
         asize=5,
         diag=T,
         esize=2,
         edge.width=2,
         # groups = plankton_group,
         palette = "colorblind",
         nodeNames = Names,
         legend = FALSE,
         # vsize=2*as.matrix(node.size),
         title=paste("p =",round(ModProbRes[i,"ModProb"],2)),title.cex=0.8)
}
dev.off()

# Post-Pinatubo ####

FirstYear = 21
FinalYear = max(WaterfowlData$time)
NYears = FinalYear-FirstYear

est.k = WaterfowlData %>% dplyr::filter(time > FirstYear) %>% group_by(ts) %>% summarise_all(mean, na.rm=T)

NSpecies = 10
# NYears = nrow(DecTS)

data_list_LVR_regime_SSVS = list(
  NSpecies = NSpecies,
  NYears = NYears,
  n1 = as.matrix((log(DecTS[FirstYear:FinalYear,2:11]/scal_Fact + 1))),
  n2 = as.matrix((log(JanTS[FirstYear:FinalYear,2:11]/scal_Fact + 1))),
  flood = scale(flood[FirstYear:FinalYear])[,1],
  est.k = est.k$obs/scal_Fact)

r_mean=c(0.6431664,1.2565923,0.7310434,1.0069860,1.2221060,0.7143320,1.7506734,0.3759912,0.2801735,0.3212522)
b_mean=c(-0.8544,-1.05,0.6868,0.03144,0.3442,-0.06122,-0.3627,-1.763,-3.054,-1.235)

inits_LVR_regime_SSVS=function(){
  list(
    prob_inter=rbeta(1,2,8),
    active_alpha=replicate(NSpecies,rnorm(NSpecies,0,0.1)) + diag(NA, NSpecies),
    # alpha=replicate(NSpecies,rnorm(NSpecies,0,0.1)) + diag(NA, NSpecies),
    g.alpha=replicate(NSpecies,rbinom(NSpecies,1,0.5)) + diag(NA, NSpecies),
    Obs.prec.mat=diag(0.1,NSpecies),
    Sys.prec.mat=diag(0.1,NSpecies),
    k=pmax(rnorm(NSpecies, est.k$obs/scal_Fact, 1), 1),
    # r=pmax(rnorm(NSpecies, r_mean, 0.1), 0.1),
    b=rnorm(NSpecies, b_mean,0.1),
    gamma=rnorm(NSpecies, b_mean, 0.1),
    .RNG.seed=seed)
}

parameters_LVR_regime_SSVS = c("prob_inter","g.alpha","alpha","k","r","b","gamma",
                               "Corr.sigma","Corr.tau",
                               "state","n1_ppc","n2_ppc")

# Run model ####
LVR_regime_SSVS_analysis_PostPinatubo = run.jags(
  data=data_list_LVR_regime_SSVS,
  inits=inits_LVR_regime_SSVS,
  monitor=parameters_LVR_regime_SSVS,
  model=read.jagsfile("R/LVRSS_regime_flood_ifelse.R"),
  n.chains = 3,
  adapt = 5000,
  burnin = 5000,
  sample = 1000,
  thin = 3,
  method='parallel',
  modules = 'glm',
  keep.jags.files=file.path(paste("analyses/LVRSS/LVRSS_regime_results/","mcmc_run_post",sep = "")),
  summarise=TRUE)

save(LVR_regime_SSVS_analysis_PostPinatubo, file="analyses/LVRSS/LVRSS_regime_results/JAGS_object_LVR_regime_SSVSanalysis_PostPinatubo.RData")

post_vars = as.data.frame(combine.mcmc(as.mcmc.list(LVR_regime_SSVS_analysis_PostPinatubo)))
Feasibility_domain = dplyr::select(post_vars, starts_with(c("alpha","r","k")))

ggs_object = ggs(as.mcmc.list(LVR_regime_SSVS_analysis_PostPinatubo))

ggmcmc(ggs_object,family="prob_inter",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_prob_inter_post.pdf",
       param_page = 1, width=6, height=6)

ggmcmc(ggs_object,family="b",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_b_post.pdf",
       param_page = 10, width=6, height=6)

ggmcmc(ggs_object,family="alpha",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_alpha_post.pdf",
       param_page = 10, width=6, height=12)

ggmcmc(ggs_object,family="k",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_k_post.pdf",
       param_page = 10, width=6, height=12)

ggmcmc(ggs_object,family="r",
       file = "analyses/LVRSS/LVRSS_regime_results/MCMC_diagnostics_r_post.pdf",
       param_page = 10, width=6, height=12)

alpha = dplyr::select(post_vars, starts_with("alpha"))

# alpha[,c("alpha[1,1]","alpha[2,2]","alpha[3,3]","alpha[4,4]","alpha[5,5]",
#          "alpha[6,6]","alpha[7,7]","alpha[8,8]","alpha[9,9]","alpha[10,10]")]=NA

# alpha and p_i matrices ####
alpha_mean_matrix = matrix(colMeans(alpha), ncol=NSpecies, nrow=NSpecies)

alpha_point_mean = as.data.frame(cbind(rownames(as.data.frame(colMeans(alpha))),
                                       as.data.frame(colMeans(alpha))))
colnames(alpha_point_mean) = c("variable","value")
plot=ggplot(reshape2::melt(alpha), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  geom_vline(xintercept=0) +
  # geom_point(data = alpha_point_mean, col="green") +
  scale_x_continuous(limits = c(-0.5,1.2)) +
  scale_y_discrete() +
  scale_fill_viridis(name = expression(alpha[ij]), option = "C") +
  labs(x="Interaction strength") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
plot
ggsave(file="analyses/LVRSS/figs/alpha_density_PostPinatubo.pdf", plot=plot,
       width=6.5, height=NSpecies^1.2, limitsize = FALSE)

# Posterior mean of interaction probability hyperprior: ####
# prob_interaction_mean = mean(dplyr::select(post_vars,starts_with("prob_inter"))[[1]])
# prob_interaction_mean = 0.3
(prob_interaction = map_estimate(dplyr::select(post_vars,starts_with("prob_inter"))))
prob_interaction_mean = prob_interaction$MAP_Estimate
(prob_interaction_HDI = bayestestR::hdi(dplyr::select(post_vars,starts_with("prob_inter")), ci = 0.9))

# Assemble values for g.alpha's:
g_alpha_matrix = dplyr::select(post_vars, starts_with("g.alpha"))
g_alpha_mean = matrix(colMeans(g_alpha_matrix), ncol=NSpecies, nrow=NSpecies)
g_alpha_matrix[,c("g.alpha[1,1]","g.alpha[2,2]","g.alpha[3,3]","g.alpha[4,4]","g.alpha[5,5]",
                  "g.alpha[6,6]","g.alpha[7,7]","g.alpha[8,8]","g.alpha[9,9]","g.alpha[10,10]")]=NA
g_alpha_point_mean = as.data.frame(cbind(rownames(as.data.frame(colMeans(g_alpha_matrix))),
                                         as.data.frame(colMeans(g_alpha_matrix))))
colnames(g_alpha_point_mean) = c("variable","value")

g_alpha_matrix2=g_alpha_matrix
g_alpha_point_mean2=g_alpha_point_mean

names(g_alpha_matrix2) <- gsub(x = names(g_alpha_matrix2), pattern = "g.alpha", replacement = "p_i")
rownames(g_alpha_point_mean2) <- gsub(x = rownames(g_alpha_point_mean2), pattern = "g.alpha", replacement = "p_i")
g_alpha_point_mean2$variable <- gsub(x = g_alpha_point_mean2$variable, pattern = "g.alpha", replacement = "p_i")

plot=ggplot(reshape2::melt(g_alpha_matrix2), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  # geom_density_ridges_gradient(stat = "binline", bins = 250, scale = 0.95, draw_baseline = FALSE) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  # geom_vline(xintercept=0, lty="dotdash") +
  geom_vline(aes(xintercept=prob_interaction$MAP_Estimate,col="Average")) +
  # geom_vline(aes(xintercept=prob_interaction$MAP_Estimate,col="Intra")) +
  scale_color_manual(name = "Probability of\n interaction", values = c(Average = "red", Intra = "blue")) +
  geom_vline(xintercept=1, lty="dotdash") +
  geom_point(data = g_alpha_point_mean2, col="red") +
  scale_x_continuous() +
  scale_y_discrete() +
  scale_fill_viridis(name = expression(italic(p)[i]), option = "C") +
  labs(x="Posterior probability of interaction") +
  theme_ridges(font_size = 13, grid = TRUE) + theme(axis.title.y = element_blank())
plot
ggsave(file="analyses/LVRSS/figs/p_i_density_PostPinatubo.pdf", plot=plot,
       width=6.5, height=NSpecies^1.2, limitsize = FALSE)


g_alpha_matrix = dplyr::select(post_vars, starts_with("g.alpha"))
g_alpha_mean = matrix(colMeans(g_alpha_matrix), ncol=NSpecies, nrow=NSpecies)

Bayes_factors = matrix(sapply(g_alpha_mean, function(x){
  2*log(x/(1-x)*(1-prob_interaction$MAP_Estimate)/prob_interaction$MAP_Estimate)}), ncol=NSpecies, nrow=NSpecies)

# Bayes factors ####
# For the intra- coefficients!!:
# diag(Bayes_factors) = sapply(diag(g_alpha_mean), function(x){
#   2*log(x/(1-x)*(1-prob_interaction_Intra_mean)/prob_interaction_Intra_mean)})
diag(Bayes_factors) = 0

# Replace values when Bayes factor is very close to 0 or 1
Bayes_factors[Bayes_factors == Inf] = 2*log(5e+50)
Bayes_factors[Bayes_factors < 0] = 0

Bayes_factors_Intra = mean(diag(Bayes_factors))
Bayes_factors_Intra_SD = sd(diag(Bayes_factors))

Bayes_factors_Inter = mean(Bayes_factors + diag(NA, NSpecies), na.rm=TRUE)
Bayes_factors_Inter_SD = sd(Bayes_factors + diag(NA, NSpecies), na.rm=TRUE)

# Plot matrix of Bayes factor in Kass & Raftery scale (1995):
gradient_color = colorRampPalette(c('white', 'darkorange', 'firebrick'))
plot=ggplot(reshape2::melt(t(Bayes_factors)), aes(Var1, Var2, fill=value)) +
  geom_raster() +
  scale_fill_gradient(name="Bayes factor in\nKass & Raftery scale", low = gradient_color(1), high = gradient_color(250)) +
  scale_x_continuous(breaks=rep(1:NSpecies, 1), position = "top") +
  scale_y_reverse(breaks=rep(1:NSpecies, 1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x="Functional type", y = "Functional type")
plot
ggsave(file="analyses/LVRSS/figs/bayes_factors_PostPinatubo.pdf",plot=plot, width=7, height=5)

# Environmental effects ####

gamma = dplyr::select(post_vars, starts_with("gamma"))
colnames(gamma) = Names

gamma_PostPinatubo = ggplot(reshape2::melt(gamma), aes(x=value, y=reorder(variable, desc(variable)),  fill= after_stat(x))) +
  geom_density_ridges_gradient(stat = "binline", bins = 100, scale = 1.5, draw_baseline = FALSE) +
  geom_vline(xintercept=0) +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  scale_y_discrete(expression(N)) +
  scale_fill_viridis(name = "Posterior\nvalue", option = "magma") +
  labs(x="Posterior value",
       title = "Effect of flooding extension") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=18))
gamma_PostPinatubo
ggsave(file="analyses/LVRSS/figs/Flooding_impact_PostPinatubo.pdf", plot=gamma_PostPinatubo,
       width=7.5, height=5.5, limitsize = FALSE)

# Stability measures ####
alpha = dplyr::select(post_vars, starts_with("alpha"))
alpha_mean = matrix(colMeans(alpha),NSpecies,NSpecies)
k_vec = dplyr::select(post_vars, starts_with("k"))
k_mean = as.numeric(colMeans(k_vec))
r_vec = dplyr::select(post_vars, starts_with("r"))[,1:NSpecies]
r_mean = as.numeric(colMeans(r_vec))

(EqAbund = solve(alpha_mean) %*% k_mean)
# (jacobi2 = numDeriv::jacobian(function(n) n + r_mean*(1 - (alpha_mean %*% exp(n))/k_mean), EqAbund, "complex"))
(jacobi2 = numDeriv::jacobian(function(N) N*exp(r_mean*(1 - (alpha_mean %*% N)/k_mean)), EqAbund, "complex"))
(max_eig2=max(Mod(eigen(jacobi2)$values)))

nloops = nrow(post_vars)

# Create the objects to be populated:
DynDimension = c()
Entropy = c()
NormDepart = c()
det_A = c()
Sth_rootDetA = c()
DynStab=c()
DynStab_cont=c()
Resilience=c()
Slow_Eigenv = c()
Slow_Eig_Elas = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
DynStab_Elas = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
OmegaL1_mcmc=c()
Logofet_mcmc=c()
Stoch_invar_mcmc=c()
WC_Reactivity_mcmc=c()

# Guy Bunin, Ecological communities with Lotka-Volterra dynamics, Phys. Rev. E 95, 042414 (2017)
alpha_mean=c()
alpha_var=c()
alpha_corr=c()

Centrality_alpha=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
Centrality_jacob=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
Centrality_nNEM=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)

nNEM_jacobi=matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
jacobi_mat = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
Eigenvals = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
# VERY IMPORTANT: Stability in the continuous-time counterpart of the discrete-time Jacobian
# Discrete-continuous model conversion (Shieh, L.S., Wang, H., Yates, R.E. 1980 10.1016/0307-904X(80)90177-8)
# Package 'expm', function logm, converting from continuous to discrete and back!!
# See Higham, Nicholas J. 2008. Functions of Matrices. Theory and Computation, page 39
# Eigenvals_cont = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)
NEM_jacobi_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
nNEM_jacobi_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
NEM_alpha_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
nNEM_alpha_mcmc = matrix(rep(NA,(NSpecies^2)*nloops),ncol=NSpecies^2,nrow=nloops)
EqAbund = matrix(rep(NA,NSpecies*nloops),ncol=NSpecies,nrow=nloops)

initial_time = proc.time()

pb = txtProgressBar(min = 0, max = nloops, initial = 0, char = "*", width = 60)

for (j in 1:nloops){

  # Interaction matrix
  alpha_mat = matrix(as.matrix(as.numeric(alpha[j,])), nrow = NSpecies, ncol = NSpecies)

  # Guy Bunin, Ecological communities with Lotka-Volterra dynamics, Phys. Rev. E 95, 042414 (2017)
  alpha_mat_diagNA = alpha_mat; diag(alpha_mat_diagNA)=NA
  alpha_mean[j] = mean(na.omit(as.vector(alpha_mat_diagNA)))
  alpha_var[j] = var(na.omit(as.vector(alpha_mat_diagNA)))/(1 - mean(na.omit(as.vector(alpha_mat_diagNA))))
  alpha_corr[j] = cor(na.omit(as.vector(alpha_mat_diagNA)), na.omit(as.vector(t(alpha_mat_diagNA))))

  k_vector = as.vector(as.numeric(k_vec[j,]))
  r_vector = as.vector(as.numeric(r_vec[j,]))
  EqAbund[j, ] = as.vector(solve(alpha_mat) %*% k_vector)

  if(all(EqAbund[j, ] >= 0)) {

    # Jacobian (community) matrix:
    # Three ways of building the Jacobian:
    # jacobian = diag(NSpecies) - ((r_vector/k_vector) * alpha_mat * EqAbund[j, ])
    # jacobian = numDeriv::jacobian(function(n) n + r_vector*(1 - (alpha_mat %*% exp(n))/k_vector), EqAbund[j, ], "complex")
    jacobian = numDeriv::jacobian(function(N) N*exp(r_vector*(1 - (alpha_mat %*% N)/k_vector)), EqAbund[j, ], "complex")
    # jacobian = numDeriv::jacobian(function(n) n + r_vector*(1 - (alpha_mat %*% exp(n))/k_vector), log(EqAbund[j, ]), "complex")


    # DynamicDimensionality
    # David Gilljam (2016) Structure and Stability of Ecological Networks The role of dynamic dimensionality and species variability in resource use. PhD

    # In a system which ‘deterministically’ acts to recover along both dimensions (directions)
    # at similar rates, the risk of species going extinct due to destabilizing environmental
    # fluctuations is likely to be lower compared to a system which recover along one
    # dimension (direction) very quickly and along the other very slowly. The former system
    # could be characterised as having a higher dynamic dimensionality (Roughgarden 1998, p.339.
    # Dynamic dimensionality can be quantified as the inverse participation
    # ratio (inverse Simpson index) of the eigenvalues (real parts) of the Jacobian
    # (community) matrix of the system (see Paper II for details). A high DD means that the
    # real-part of the eigenvalues are of similar magnitude and that the system therefore
    # approaches the equilibrium from all directions at a similar rate (Fig. 11). On the other
    # hand, when DD is low one (or a few) of the eigenvalues has a large magnitude
    # compared to the others and the deterministic forces pulling the system towards
    # equilibrium is therefore weak in many directions compared to the stochastic forces
    # pushing the system away from the equilibrium. As a consequence the risk of crossing
    # extinction thresholds increases, and hence persistence and time to first extinction
    # decreases, as DD decreases.
    DynDim = DynamicDimensionality(A=alpha_mat, J=jacobian, SVD_Entropy = TRUE)
    DynDimension[j] = DynDim[["DynDim"]]
    Entropy[j] = DynDim[["Entropy"]]

    jacobi_mat[j,] = as.vector(jacobian)

    # Robust coexistence, see "Theory-Based Ecology A: Darwinian approach", TBox 9.3, p. 184, Barabas et al 2015 doi:10.1111/ele.12350
    # Is simply the determinant of alpha matrix (!!, not the Jacobi matrix):
    # "if det(a)  is not zero but close to it, then the components of alpha become
    # large and the equilibrium values, calculated from eq. (9.19), depend very sensitively on ri.
    # This is the formal basis of the phenomenon observed in Figure 9.2 that small values
    # "...of the determinant spoil the robustness of the coexistence":
    # that is, we have concluded that the condition for robust coexistence is a sufficient difference in regulation,
    # expressed by det a not being too close to zero":
    det_A[j] = det(alpha_mat)
    Sth_rootDetA[j] = exp(mean(log(Mod(eigen(alpha_mat,only.values = TRUE)$values))))

    # Departure from normality:
    # Calculate the degree of departure from normality of a square matrix, Barabas & Allesina (2015),
    # Supp. Mat, p. 15, Predicting global community properties from uncertain estimates of interaction strengths.
    # Departures from normality < 1 means the spectra has low sensitivity to perturbations
    NormDepart[j] = depn(alpha_mat)

    Eigenvals[j,] = matrix(eigen(jacobian, only.values = TRUE)$values, nrow=1)

    Resilience[j] = 1/max(abs(Re(Eigenvals[j,])))

    DynStab[j] = max(Mod(Eigenvals[j,]))

    # Snyder (2005) What makes ecological systems reactive? 10.1016/j.tpb.2010.03.004
    # WC_Reactivity_mcmc[j] = max(Mod(eigen(t(alpha_mat)%*%alpha_mat)$values)) - 1
    WC_Reactivity_mcmc[j] = log(max(Mod(eigen(t(jacobian)%*%jacobian, only.values = TRUE)$values)))

    OmegaL1_mcmc[j] = Omega(alpha_mat)

    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%*%diag(NSpecies) - alpha_mat%*%alpha_mat),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%*%diag(NSpecies) - alpha_mat%*%alpha_mat),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%x%diag(NSpecies) - alpha_mat%x%alpha_mat),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%x%diag(NSpecies) - alpha_mat%x%alpha_mat),"2"))^-1)

    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%*%diag(NSpecies) - jacobian%*%jacobian),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%*%diag(NSpecies) - jacobian%*%jacobian),"2"))^-1)
    # Stoch_invar_mcmc[j] = -log(1-(norm(solve(diag(NSpecies)%x%diag(NSpecies) - jacobian%x%jacobian),"2"))^-1)
    Stoch_invar_mcmc[j] = -log(1-(norm(MASS::ginv(diag(NSpecies)%x%diag(NSpecies) - jacobian%x%jacobian),"2"))^-1)

    for (k in 1:NSpecies){

      Logofet_mcmc[j] = abs(det(alpha_mat))/prod(sum(abs(alpha_mat[k,])))

    }
    # VERY IMPORTANT: Stability in the continuous-time counterpart of the discrete-time Jacobian
    # Discrete-continuous model conversion (Shieh, L.S., Wang, H., Yates, R.E. 1980 10.1016/0307-904X(80)90177-8)
    # Package 'expm', function logm, converting from continuous to discrete and back!!
    # See Higham, Nicholas J. 2008. Functions of Matrices. Theory and Computation, page 39
    # Eigenvals_cont[j,] = matrix(eigen(logm(jacobian,"Eigen"))$values, nrow=1)

    # VERY IMPORTANT: Stability in the continuous-time counterpart of the discrete-time Jacobian
    # Discrete-continuous model conversion (Shieh, L.S., Wang, H., Yates, R.E. 1980 10.1016/0307-904X(80)90177-8)
    # Package 'expm', function logm, converting from continuous to discrete and back!!
    # See Higham, Nicholas J. 2008. Functions of Matrices. Theory and Computation, page 39
    # DynStab_cont[j] = max(Re(eigen(logm(jacobian,"Eigen"))$values))

    # Idea from Hanski & Ovaskainen 2000, The metapopulation capacity of a fragmented landscape,
    # Relat. contribution of species i to the leading eigenvalue, see
    # Solé & Bascompte 2006 Self-organization in complex ecosystems, bottom page 200
    # See also Arnoldi et al The inherent multidimensionality of temporal variability 2019 10.1111/ele.13345

    for(i in 1:NSpecies) {
      DynStab_Elas[j,i] =
        max(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,1]^2)[i]/
        sum(max(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,1]^2))
    }

    Slow_Eigenv[j] = min(Mod(eigen(jacobian)$values)) # slowest eigenvalue, see Cenci & Saavedra () Nonlinear structural stability
    # and Arnoldi et al The inherent multidimensionality of temporal variability 2019 10.1111/ele.13345

    for(i in 1:NSpecies) {
      Slow_Eig_Elas[j,i] =
        min(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,NSpecies]^2)[i]/
        sum(min(Mod(eigen(jacobian)$values))*Mod(eigen(jacobian)$vectors[,NSpecies]^2))
    }

    for(i in 1:NSpecies){

      Centrality_alpha[j,i] = sum(abs(alpha_mat[i,])) + sum(abs(alpha_mat[,i])) - 1
      Centrality_jacob[j,i] = sum(abs(jacobian[i,])) + sum(abs(jacobian[,i]))
      Centrality_nNEM[j,i] = sum(abs(nNEM_jacobi[i,])) + sum(abs(nNEM_jacobi[,i])) - abs(nNEM_jacobi[i,i])

    }
  }

  setTxtProgressBar(pb,j)
  final_time = proc.time() - initial_time
  if (j == nloops*0.25) cat(" 25% completed in", final_time[3], "sec!\n")
  final_time = proc.time() - initial_time
  if (j == nloops*0.5) cat(" 50% completed in", final_time[3], "sec!\n")
  final_time = proc.time() - initial_time
  if (j == nloops*0.75) cat(" 75% completed in", final_time[3], "sec!\n")
  final_time = proc.time() - initial_time
  if (j == nloops) cat(" 100% completed in", final_time[3], "sec!\n")

}

# Plot stability measures ####
stability_measures_PostPinatubo = as.data.frame(cbind(
  NormDepart,
  # det_A,
  Sth_rootDetA,
  DynDimension,
  Entropy,
  # Sth_rootDetA,
  Resilience,
  # DynStab,
  WC_Reactivity_mcmc,
  # Logofet_mcmc,
  # OmegaL1_mcmc,
  Stoch_invar_mcmc))
# colnames(stability_measures_PostPinatubo) = c("Departure from normality","Robustness of coexistence","Sth_rootDetA","Resilience","Dynamic stability",
#                                  "Worst-case reactivity", "Logofet's equilibriumness","Structural stability,"Stochastic invariability")
colnames(stability_measures_PostPinatubo) = c("Departure from normality","Robustness of coexistence","Dynamic dimensionality","Entropy",
                                              "Resilience","Reactivity","Structural stability")

stability_measures_PostPinatubo = stability_measures_PostPinatubo[complete.cases(stability_measures_PostPinatubo),]

save(LVR_regime_SSVS_analysis_PostPinatubo, stability_measures_PostPinatubo,
     file="analyses/LVRSS/LVRSS_regime_results/JAGS_object_LVR_regime_SSVSanalysis_PostPinatubo.RData")

# (MAP_stability = map_estimate(stability_measures_PostPinatubo))
(HDInterval_stability = bayestestR::hdi(as.mcmc(stability_measures_PostPinatubo), credMass = 0.9, allowSplit = TRUE))
map_estimate(subset(stability_measures_PostPinatubo$`Departure from normality`, stability_measures_PostPinatubo$`Departure from normality`< 1))
p_direction(stability_measures_PostPinatubo$Reactivity)

plot_stab = ggplot(reshape2::melt(stability_measures_PostPinatubo), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  geom_density_ridges_gradient(stat = "binline", bins = 250, scale = 0.95, draw_baseline = FALSE) +
  # geom_density_ridges_gradient(scale = 1.1, rel_min_height = 0.01) +
  geom_vline(xintercept=0, lty="dotdash") +
  geom_vline(xintercept=1, col="red") +
  scale_x_break(c(2.6, 9.4), scales = 0.65) +
  scale_fill_viridis(name = "Value", option = "magma") +
  scale_color_manual(name = element_blank()) +
  labs(x="Stability measures",
       title = "Metrics of matrix stability") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=18))
plot_stab
ggsave(file="analyses/LVRSS/figs/Stability_measures_PostPinatubo.pdf", plot=plot_stab, onefile=FALSE,
       width=7.5, height=6.5, limitsize = FALSE)

pdf("analyses/LVRSS/figs/Correlation_matrix_PostPinatubo.pdf",width=5.5, height=5.5,paper='special')
pairs.panels(stability_measures_PostPinatubo[,c("Departure from normality","Entropy","Reactivity","Structural stability")],
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
dev.off()

# Plot unit circle, prob. of dynamic stability ####

Eigenvals = Eigenvals[complete.cases(Eigenvals),]

ReEigenvals = reshape2::melt(Re(Eigenvals), value.name="Real_part")
ImEigenvals = reshape2::melt(Im(Eigenvals), value.name="Imaginary_part")
Eigenvals_toplot = as.data.frame(cbind(ReEigenvals[,c("Real_part")],ImEigenvals[,c("Imaginary_part")]))
colnames(Eigenvals_toplot) = c("Real_part","Imaginary_part")
Eigenvals_toplot$Modulus = sqrt((Eigenvals_toplot$Real_part)^2 + (Eigenvals_toplot$Imaginary_part)^2)

(EmpProbDynStab = sum(Eigenvals_toplot$Modulus < 1)/length(Eigenvals_toplot$Modulus))

th = seq(-pi, pi, len = 100)
z = exp((0+1i) * th)
UnitCircle=as.data.frame(cbind(Re(z),Im(z)))
Origin=data.frame(x=0,y=0)

unitcircle_plot_PostPinatubo = ggplot(data=UnitCircle,aes(Re(z),Im(z))) +
  geom_path() +
  geom_vline(xintercept = 0,lty="dotted") +
  geom_hline(yintercept = 0,lty="dotted") +
  geom_point(data=Eigenvals_toplot,aes(x=Real_part,y=Imaginary_part),size=0.5,col="red") +
  scale_x_continuous(limits = c(-1.5,1.5)) +
  scale_y_continuous(limits = c(-1.5,1.5)) +
  labs(x="Real", y = "Imaginary") +
  theme(
    axis.line.x=element_line(linewidth=0.5,colour="Black"),
    axis.line.y=element_line(linewidth=0.5,colour="Black"),
    axis.text=element_text(size=18,colour="Black"),
    axis.title=element_text(size=18,colour="Black"),
    plot.title = element_text(size=15),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.key = element_blank(),
    panel.grid = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = "transparent", colour = NA)) +
  ggtitle(paste0("Resilience: ", round(map_estimate(Resilience), 3),
                 " (", round(bayestestR::hdi(Resilience, credMass = 0.9, allowSplit = TRUE)[["CI_low"]], 3),",",
                 round(bayestestR::hdi(Resilience, credMass = 0.9, allowSplit = TRUE)[["CI_high"]],3), ") MAP, 90% HDI", sep=""),
          paste0("Probability of dynamic stability: ", round(EmpProbDynStab, 4),"\n", sep=""))
unitcircle_plot_PostPinatubo
ggsave("analyses/LVRSS/figs/Probability_of_stability_PostPinatubo.pdf", unitcircle_plot_PostPinatubo, height = 6.5, width = 6)

# Plot feasibility ####

(EqAbund_mean = colMeans(EqAbund,na.rm=T))
(EqAbund_sd = colSd(EqAbund))

EquilAbund = as.data.frame(EqAbund)
colnames(EquilAbund) = c("Pintail","Shoveler","Common teal","Eurasian wigeon","Mallard",
                         "Gadwall","Greylag goose","Common pochard","Red-crested pochard","Shelduck")

# Probability of species extinction
ProbSpExtinct=matrix(NA,1,NSpecies)
for(g in 1:NSpecies){
  ProbSpExtinct[,g] = length(which(apply(as.data.frame(EquilAbund[,g]), 1, function(row) any(row <= 0))))/nrow(EquilAbund)
}

(EmpProbFeas = (dim(EquilAbund)[1] - length(which(apply(EquilAbund, 1, function(row) any(row < 0)))))/dim(EquilAbund)[1])

k_posterior = as.data.frame(map_estimate(k_vec))
rownames(k_posterior) = c("Pintail","Shoveler","Common teal","Eurasian wigeon","Mallard",
                          "Gadwall","Greylag goose","Common pochard","Red-crested pochard","Shelduck")

k_posterior = as.data.frame(cbind(rownames(k_posterior),as.data.frame(k_posterior$MAP_Estimate)))
colnames(k_posterior) = c("variable","value")

plot_feas_PostPinatubo = ggplot(reshape2::melt(EquilAbund), aes(x=value, y=reorder(variable, desc(variable)), fill = after_stat(x))) +
  geom_density_ridges_gradient(stat = "binline", bins = 100, scale = 1.5, draw_baseline = FALSE) +
  # geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  geom_vline(xintercept=0) +
  scale_x_continuous(limits = c(-20,100)) +
  scale_y_discrete(expression(N)) +
  scale_fill_viridis(name = expression("Equilibrium \nabundance,"~italic(N)^"*"), option = "viridis") +
  geom_point(data = k_posterior, aes(col="k_posterior"), size=2) +
  scale_color_manual(name = element_blank(),
                     values = c(k_posterior = "brown"),
                     labels = "Carrying capacity") +
  # labs(x=expression('Posterior biomass, ' ~ mu * g * C * L^-1),
  labs(x="Abundance (n. ind x1000)",
       title = paste("Probability of feasibility: ",round(EmpProbFeas,3))) +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size=18))
plot_feas_PostPinatubo
ggsave(file="analyses/LVRSS/figs/Prob_of_feasibility_PostPinatubo.pdf", plot=plot_feas_PostPinatubo,
       width=7.5, height=5.5, limitsize = FALSE)

# Plot interaction networks ####
pdf("analyses/LVRSS/figs/alpha_spring_PostPinatubo.pdf",width=9.5, height=6.5)
plot=qgraph(t(alpha_mean_matrix),layout="spring",
            diag=T,
            edge.labels=FALSE,
            edge.label.cex=2,
            edge.label.bg=TRUE,
            posCol="red",
            negCol="blue",
            # vsize=as.matrix(node.size),
            # title="Normalized Net Effects Matrix, alpha",
            # title.cex = 1.5,
            # groups = plankton_group,
            nodeNames = Names,
            palette = "colorblind")
title(main = "Interaction matrix", line = 3, cex.main = 1.5)
dev.off()

jacobian_Mean = matrix(colMeans(jacobi_mat),NSpecies,NSpecies)
pdf("analyses/LVRSS/figs/Community_matrix_PostPinatubo.pdf",width=9.5, height=6.5)
plot=qgraph(t(jacobian_Mean),layout="spring",
            edge.labels=FALSE,
            edge.label.cex=2,
            edge.label.bg=TRUE,
            posCol="red",
            negCol="blue",
            # vsize=as.matrix(node.size),
            # title="Normalized Net Effects Matrix, alpha",
            # title.cex = 1.5,
            # groups = plankton_group,
            nodeNames = Names,
            palette = "colorblind")
title(main = "Community matrix", line = 3, cex.main = 1.5)
dev.off()

# Model probabilities ####
# (check: https://r-nimble.org/nimbleExamples/RJMCMC_example.html)!!

modprob = as.data.table((g_alpha_matrix != 0) + 0)
colnames(modprob) = gsub(",", "", colnames(g_alpha_matrix))
ModProbRes = modprob[,.N, by=names(modprob)]
ModProbRes = ModProbRes[order(N, decreasing = TRUE)]
ModProbRes = ModProbRes[, ModProb := N/dim(g_alpha_matrix)[1]]
MostProbModel = matrix(unlist(matrix(ModProbRes[1,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)
SecondMostProbModel = matrix(unlist(matrix(ModProbRes[2,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)
ThirdMostProbModel = matrix(unlist(matrix(ModProbRes[3,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)

Model_list_ordered = list()
for(i in 1:nrow(ModProbRes)) Model_list_ordered[[i]] = matrix(unlist(matrix(ModProbRes[i,-c("N","ModProb")],ncol=NSpecies)),ncol=NSpecies)

pdf("analyses/LVRSS/figs/RJModel8NW_PostPinatubo.pdf",height=2,width=4)
par(mfrow=c(2,4),mai=c(1,1,1,1))
for(i in 1:8){
  qgraph(matrix(unlist(t(matrix(ModProbRes[i,-c("N","ModProb")],ncol=NSpecies))),ncol=NSpecies),
         layout='circle',
         edge.color="azure4",
         directed=T,
         asize=5,
         diag=T,
         esize=2,
         edge.width=2,
         # groups = plankton_group,
         palette = "colorblind",
         nodeNames = Names,
         legend = FALSE,
         # vsize=2*as.matrix(node.size),
         title=paste("p =",round(ModProbRes[i,"ModProb"],2)),title.cex=0.8)
}
dev.off()

save(LVR_regime_SSVS_analysis_PrePinatubo,
     LVR_regime_SSVS_analysis_PostPinatubo,
     stability_measures_PrePinatubo,
     stability_measures_PostPinatubo,
     file="analyses/LVRSS/LVRSS_regime_results/JAGS_object_LVR_regime_SSVS_analysis.RData")

save.image("analyses/LVRSS/LVRSS_regime_results/Results.RData")

# END ####
