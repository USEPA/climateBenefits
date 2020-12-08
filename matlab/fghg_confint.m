function [ mean_emissions,lower_bound,upper_bound ] = fghg_confint( b_mean,b_cov,offset,gasratio,X1,X2,interval,S,size )
% This function performs a monte carlo (Krinskey-Robb) simulation to
% generate confidence intervals for incremental GHG emissions from lakes
% based on a set of baseline and post-policy input for trophic status 
% 
% Inputs
%   b_mean: kx1 vector of means for regression coefficients
%   b_cov:  kxk full covariance matrix for estimated coefficients
%   X1:     Nxk data matrix containing basline values for variables
%           SHOULD CONTAIN VECTOR OF ONES IN FIRST COLUMN FOR INTERCEPT
%   X2:     Nxk data matrix containing post-policy values for variables
%           SHOULD CONTAIN VECTOR OF ONES IN FIRST COLUMN FOR INTERCEPT
%   interval: Scalar specifying the width of the confidence interval (e.g. 90, 95, etc.)  
%   S:      Number of random draws to simulate distribution of coefficients
%   size:   Nx1 vector of lakes sizes in square km
%
% Outputs
%   mean_emissions: mean value for total incremental emissions
%   lower_bound, upper_bound: bounds on confidence interval for total incremental emissions

%%%%%%%%%%%%%%%%%%%%%%%%
% Draw values from coefficient distribution - kxS matrix 
  b_dist = mvnrnd(b_mean,b_cov,S);

% Generate distribution of emission rates from each lake - NxS matrix for each scenario
  emissionrate1_dist = (10.^(X1*b_dist') - offset)*gasratio;     % RECOVERING LEVEL VALUES FROM LOG10 TRANSFORMED DATA
  emissionrate2_dist = (10.^(X2*b_dist') - offset)*gasratio;     % OFFSET: SCALAR USED TO AVOID LOGGING NEGATIVE VALUES IN ESTIMATION
  mean(emissionrate1_dist(1,:))
% Convert rates to total emissions in tonnes
  size = size*ones(1,S); 
  lake_emissions1_dist = emissionrate1_dist.*(size.*10^6)*10^-9; 
  lake_emissions2_dist = emissionrate2_dist.*(size.*10^6)*10^-9;

% Take difference to find distribution for incremental emissions for each lake - NxS matrix
  incr_emissions_dist = lake_emissions2_dist-lake_emissions1_dist;

% Sum across lakes to find total incremental emissions - 1xS row vector
  total_emissions_dist = sum(incr_emissions_dist,1);

% Produce mean and confidence interval bounds for output
  lower_p = (100-interval)/2; upper_p = 100 - lower_p;
  mean_emissions = mean(total_emissions_dist);
  lower_bound = prctile(total_emissions_dist,lower_p);
  upper_bound = prctile(total_emissions_dist,upper_p);

end

