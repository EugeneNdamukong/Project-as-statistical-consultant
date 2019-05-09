/* A Look at the data*/
proc print data=work.happy;
run;

title "Happiness data";
data happy_cluster;
set work.happy(keep=Economy Family Health Freedom Trust Generosity);
run;

proc print data=happy_cluster;
run;

/* Data exploration*/
title "Principal component analysis";
proc princomp data=happy_cluster out=scores;
proc print data=scores;
run;

title "Biplot";
proc prinqual data=happy n=3 mdpref mdpref=5;
transform identity(Economy Family Health Freedom Trust Generosity );
id country;
run;

/*Hierachical clustering*/
title "Single linkage";
proc cluster data=happy_cluster method=single rsq nonorm noeigen out=singleout;
proc sort data=singleout;
by _ncl_;

data singleout;
set singleout;
single=_rsq_;

data singleout2;
set singleout;
single2=_sprsq_;

title "Complete linkage";
proc cluster data=happy_cluster method=complete rsq nonorm noeigen out=completeout;
proc sort data=completeout;
by _ncl_;

data completeout;
set completeout;
complete=_rsq_;

data completeout2;
set completeout;
complete2=_sprsq_;

title "centroid linkage";
proc cluster data=happy_cluster method=centroid rsq nonorm noeigen out=centroidout; 
proc sort data=centroidout;
by _ncl_;

data centroidout;
set centroidout;
centroid=_rsq_;

data centroidout2;
set centroidout;
centroid2=_sprsq_;

title "Ward method";
proc cluster data=happy_cluster method=ward rsq nonorm noeigen out=wardout;
proc sort data=wardout nodupkey;
by _ncl_;

data wardout;
set wardout;
ward=_rsq_;
run;

data wardout2;
set wardout;
ward2=_sprsq_;
proc print data=wardout;
run;

title "Combining clustering outputs";
data outputs;
merge singleout completeout centroidout wardout singleout2 completeout2 centroidout2 wardout2;
by _ncl_;

title "R-square plot";
symbol1 i=join v=s l=15 c=black;
symbol2 i=join v=p l=10 c=blue;
symbol3 i=join v=c l=2 c=red;
symbol4 i=join v=w l=1 c=green;

proc gplot data=outputs;
plot  single*_ncl_=1 complete*_ncl_=2 centroid*_ncl_=3 ward*_ncl_=4 single2*_ncl_=1 complete2*_ncl_=2 centroid2*_ncl_=3 ward2*_ncl_=4/overlay legend;
run;

title "New Ward results with variable";
proc tree data=wardout nclusters=6 out=outtreeward;
copy Economy Family Health Freedom Trust Generosity ;
run;
proc sort data= outtreeward;
by cluster;
proc means data=outtreeward;
by cluster;
output out=meanout mean=Economy Family Health Freedom Trust Generosity;
run;

/*Non-hierachical clustering*/
title "K-means clustering with 6 centres";
data initials;
set meanout;
if cluster<7;

proc fastclust data=happy_cluster seed=initials maxclusters=6 out=new maxiter=20;
var Economy Family Health Freedom Trust Generosity;

proc sort data=new;
by cluster;
proc print data=new;
by cluster;
var Economy Family Health Freedom Trust Generosity;
run;

