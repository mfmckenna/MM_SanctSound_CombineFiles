
ncfile = "D:\RESEARCH\SanctSound\data\CI04\SanctSound_CI04_02_OL_1h.nc";
r = ncinfo(ncfile);
ncdisp(ncfile)
myvar = ncread(r.Variables.Name,'time') ;

r.Variables(2).FillValue