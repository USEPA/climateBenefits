% data_mangage
  clear all, clc

% Read in data  
  data = xlsread('data');
  
% Drop rows with missing obs
  [row,col] = find(isnan(data));
  data(row,:) = [];
 
 % Define variables 
  lake_id    = data(:,1);
  chla_base  = data(:,2);
  chla_tmdl  = data(:,3);
  tp_base    = data(:,4);
  tp_tmdl    = data(:,5); 
  size       = data(:,6);
  sizextp_base = size.*tp_base;
  sizextp_tmdl = size.*tp_tmdl;
  
 save('confint_data')
  
   

 