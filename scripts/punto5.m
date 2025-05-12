%% 01 Limpiamos Entorno  f
clc;
clear all;

%% 02 Cargamos datos
ds = readtable("stores\\VAR.xlsx", "UseExcel", false);

%% 03 Transformaciones de la serie
ds.Properties.VariableNames = {'fecha', 'ingresos','brent'};
ds = sortrows(ds, 'fecha');

ds.lnIngresos = log(ds.ingresos);
ds.lnBrent = log(ds.brent);

ds.diffLnIng = [NaN(1,1); diff(ds.lnIngresos)];
ds.diffLnBrent = [NaN(1,1); diff(ds.lnBrent)];

logIng = log(ds.ingresos);

logBrent = log(ds.brent);


%% 04 Pruebas de raiz unitaria
[ADFTblIng,regLnIng] = adftest(ds, ...
                          DataVariable="lnIngresos");

[ADFTblBrent,regLnBrent] = adftest(ds, ...
                             DataVariable="lnBrent");

disp("ADF Log Ingresos")
disp(ADFTblIng)

disp("ADF Log Brent")
disp(ADFTblBrent)

[ADFTblDiffIng,regDiffLnIng] = adftest(ds);

[ADFTblDiffBrent,regDiffLnBrent] = adftest(ds);

disp("ADF Diff Log Ingresos")
disp(ADFTblDiffIng)

disp("ADF Diff Log Brent")
disp(ADFTblDiffBrent)

%% 04 Estimacion VAR

nombres = {'diffLnIng','diffLnBrent'};
ds_var = ds(:, {'fecha','diffLnIng', 'diffLnBrent'});
ds_var = rmmissing(ds_var);

rezagos = [2,4,8,12];

vacio = nan(height(rezagos'), 1);

tblAic = table(rezagos', vacio, 'VariableNames', {'p', 'AIC'});
tblBic = table(rezagos', vacio, 'VariableNames', {'p', 'BIC'});

for i = 1:length(rezagos)
    p = rezagos(i);
    
    var = varm(2, p);
    var.SeriesNames = nombres;
    [EstMdl, EstSE, LogL, E] = estimate(var, ds_var{:,2:3});

    R=summarize(EstMdl);
    np = R.NumEstimatedParameters;

    [aic, bic]= aicbic(LogL, np, 253);
    tblAic.AIC(tblAic.p == p) = aic;
    tblBic.BIC(tblBic.p == p) = bic;
end

[~, idxMinAic] = min(tblAic.AIC);

pAic = tblAic.p(idxMinAic);

[~, idxMinBic] = min(tblBic.BIC);

pBic = tblBic.p(idxMinBic);

%% 05 Estimacion Mejores Modelo
var = varm(2, pBic);
[EstMdl, EstSE, LogL, E] = estimate(var, ds_var{:,2:3});

%% 05 Causalidad de Granger
[hg,summary] = gctest(EstMdl);

disp("Granger")
disp(summary)

%% 06 Impulso Respuesta

[imp, lower, upper] = irf(EstMdl, ...
    "NumObs",   20, ...      % ahora devuelve 21 pasos (0,…,20)
    "NumPaths", 1000 ...
);


%% 07 Descomposicion de Varianza

%% 08 Descomposicion de Varianza historica