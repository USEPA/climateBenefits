load confint_data
b = [0.9396; 0.7779];
x2 = chla_base(1,1)*1000;
x2 = log10(x2);
x = [1;x2];

y = (10^(b'*x)-1)*(16/12)