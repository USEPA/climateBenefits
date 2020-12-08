% lake_ghg_MC_simulation.m
% This script performs uncertainty analysis on the estimates of GHG
% emsissions from lakes with different trophic status by calling the 
% function fghg_confint 
%
% Inputs
%   b_mean_ch4: kx1 vector of means for parameters of ch4 emissions function
%   b_cov_ch4:  kxk covariance matrix for ch4 parameters 
%   ch4_offset: scalar offset used for emmisions data
%
%   b_mean_co2: kx1 vector of means for parameters of co2 emissions function
%   b_cov_co2:  kxk covariance matrix for co2 parameters 
%   co2_offset: scalar offset used for emmisions data
%
%   b_mean_n2o: kx1 vector of means for parameters of n2o emissions function
%   b_cov_n2o:  kxk covariance matrix for n2o parameters
%   n2o_offset: scalar offset used for emmisions data
% 
%   chla_base: Nx1 vecor of baseline chl_a measurments for all N lakes
%   tp_base:    Nx1 vecor of baseline TP measurements for all N lakes 
%   chla_tmdl: Nx1 vecor of post-TMDL chl_a measurments for all N lakes
%   tp_tmdl:    Nx1 vecor of post-TMDL TP measurements for all N lakes 
%   size:       Nx1 vector of lake sizes 
%
% Outputs 
%   total_ch4: scalar mean estimate of total incremental ch4 emissions
%   total_co2: scalar mean estimate of total incremental co2 emissions
%   total_n2o: scalar mean estimate of total incremental n2o emissions
%
%   lower_ch4: lower 2.5th percentile of total incremental ch4 emissions
%   lower_co2: lower 2.5th percentile of total incremental co2 emissions
%   lower_n2o: lower 2.5th percentile of total incremental n2o emissions
%
%   upper_ch4: upper 2.5th percentile of total incremental ch4 emissions
%   upper_co2: upper 2.5th percentile of total incremental co2 emissions
%   upper_n2o: upper 2.5th percentile of total incremental n2o emissions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load('confint_data.mat')

% Simulation Parameters 
  S = 10000;            % number of draws from beta distribution for simulation  
  N = length(lake_id);  % number of lakes in data 
  interval = 90;        % Width of confidence interval
 
% Gas coversions
  ch4ratio = 16/12;
  co2ratio = 44/12;
  n2oratio = 44/14;
  
% Offsets 
 ch4_offset = 1;
 co2_offset = 43;
 n2o_offset = 0.25;
    
% Convert milligrams to micrograms
  chla_base = chla_base*1000;   chla_tmdl = chla_tmdl*1000; 
  tp_base   = tp_base*1000;     tp_tmdl   = tp_tmdl*1000;
  
% Parameter distributions 
  b_mean_ch4 = [0.9396; 0.7779];
  b_cov_ch4  = [0.01493794 -0.01222670;
               -0.01222670  0.01397636];   
  
  b_mean_co2 = [2.447393 -0.033986 0.079762 -0.072419];
  b_cov_co2  = [0.00012521640	0.00002967531	-0.00009667993	-0.00001745237;
                0.00002967531	0.00011188930	-0.00001759816	-0.00008644185;
                -0.00009667993	-0.00001759816	0.00008806085	0.00001334851;
                -0.00001745237	-0.00008644185	0.00001334851	0.00007906991]; 

  b_mean_n2o = [-0.50460 0.02975 0.10437];
  b_cov_n2o  = [0.00019672930	-0.00001767890	-0.00018537130;
                -0.00001767890	0.00011161680	-0.00007469625;
                -0.00018537130	-0.00007469625	0.00089991870]; % 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Call fghg_confint function for each GHG

%%%%%%%%%%%%%%%
% CH4
X1_ch4 = [ones(N,1),log10(chla_base)];
X2_ch4 = [ones(N,1),log10(chla_tmdl)];
[total_ch4,lower_ch4,upper_ch4] = fghg_confint(b_mean_ch4,b_cov_ch4,ch4_offset,ch4ratio,X1_ch4,X2_ch4,interval,S,size);
[total_ch4,lower_ch4,upper_ch4]*245
%%%%%%%%%%%%%%%
% C02
X1_co2 = [ones(N,1),log10(size),log10(tp_base),log10(size).*log10(tp_base)];
X2_co2 = [ones(N,1),log10(size),log10(tp_tmdl),log10(size).*log10(tp_tmdl)];
[total_co2,lower_co2,upper_co2] = fghg_confint(b_mean_co2,b_cov_co2,co2_offset,co2ratio,X1_co2,X2_co2,interval,S,size);
[total_co2,lower_co2,upper_co2]*245
%%%%%%%%%%%%%%%
% C02
X1_n2o = [ones(N,1),log10(size),log10(chla_base)];
X2_n2o = [ones(N,1),log10(size),log10(chla_tmdl)];
[total_n2o,lower_n2o,upper_n2o] = fghg_confint(b_mean_n2o,b_cov_n2o,n2o_offset,n2oratio,X1_n2o,X2_n2o,interval,S,size);
[total_n2o,lower_n2o,upper_n2o]*245




