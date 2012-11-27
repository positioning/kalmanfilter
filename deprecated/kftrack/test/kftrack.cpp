  #include <fstream.h>
  #include <math.h>
  #include <fvar.hpp>
  #include <adstring.hpp>
  #include "yrmonday.h"
  #include "trace.h"
  #include <strstream>
  using std::ostrstream;    
  #undef REPORT
  #define REPORT(object) report << #object " = " << object << endl;
  #define MREPORT(object) report << #object ": \n" << object << endl;
  // function prototypes
  adstring make_banner();
  double azimuth(const double& y, const double& x);
  dvariable azimuth(const dvariable& y, const dvariable& x);
  dvariable gc_dist(const dvector& y1, const dvar_vector y2);
  int previous_solstice(const int y0, const int m0, const int d0);
  
  // global variables
  const double mpg = 60.0; // Nautical miles per degree
  const double rmpg = 1.0/mpg;
  const double mpi180 = M_PI/180.0;
  const double two_pi = 2.0*M_PI;
  const double eps = 1e-8;  //small number to avoid divide by 0
  year_month_day solstice;
  year_month_day_vector track_dates;
  ofstream clogf("kftrack.log");
  int u_phase = -1;
  int v_phase = -1;
  int D_phase = -1;
  int bx_phase = -1;
  int by_phase = -1;
  int vx_phase = -1;
  int vy_phase = -1;
  int cos_phase = -1;
  int a0_phase = -1;
  int b0_phase = -1;
  int dev_phase = -1;
  int vscale_phase = -1;
  int t;
  adstring copyright("\n  (c) 2001 John Sibert\n"\
          "  Pelagic Fisheries Research Program, University of Hawaii\n");
  adstring banner = make_banner();
 
#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <kftrack.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
cout << banner << endl;
clogf << banner << endl;
  npoint.allocate("npoint");
TRACE(npoint)
  release_point.allocate("release_point");
TRACE(release_point)
  recap_point.allocate("recap_point");
TRACE(recap_point)
 N = 2;
 m = 2;
TTRACE(N,m)
  u_active.allocate("u_active");
  v_active.allocate("v_active");
  D_active.allocate("D_active");
  bx_active.allocate("bx_active");
  by_active.allocate("by_active");
  vx_active.allocate("vx_active");
  vy_active.allocate("vy_active");
  a0_active.allocate("a0_active");
  b0_active.allocate("b0_active");
  vscale_active.allocate("vscale_active");
  init_u.allocate("init_u");
  init_v.allocate("init_v");
  init_D.allocate("init_D");
  init_bx.allocate("init_bx");
  init_by.allocate("init_by");
  init_vx.allocate("init_vx");
  init_vy.allocate("init_vy");
  init_a0.allocate("init_a0");
  init_b0.allocate("init_b0");
  init_vscale.allocate("init_vscale");
  cos_errors.allocate("cos_errors");
  dev_errors.allocate("dev_errors");
  vy_dev_penalty_wt.allocate("vy_dev_penalty_wt");
  specified.allocate("specified");
  Y.allocate(1,npoint,1,N);
  y_deg.allocate(1,npoint,1,N);
  P0.allocate(1,m,1,m);
  solar_date.allocate(1,npoint);
  vy_t.allocate(1,npoint);
  dat_mat.allocate(1,npoint,1,5+3*specified,"dat_mat");
 P0.initialize(); 
   gsolstice = previous_solstice((int)dat_mat(1,3),(int)dat_mat(1,2),(int)dat_mat(1,1));
   year_month_day ts(gsolstice);
   solstice = ts;
   TRACE(solstice)
   nphase = 0;
   if (u_active || v_active || D_active)
   {
     nphase ++;
     if (u_active)
       u_phase = nphase;
     if (v_active)
       v_phase = nphase;
     if (D_active)
       D_phase = nphase;
   } 
   if (vx_active || vy_active || bx_active || by_active || vscale_active)
   {
     nphase ++;
     if (bx_active)
       bx_phase = nphase;
     if (by_active)
       by_phase = nphase;
     if (vx_active)
       vx_phase = nphase;
     if (vy_active)
       vy_phase = nphase;
     if (specified){
       vx_phase = -1; 
       vy_phase = -1; 
       if (vscale_active) 
         vscale_phase = nphase;
     }
   }
   if (cos_errors)
   {
     nphase ++;
     cos_phase = nphase;
     if(a0_active)a0_phase=cos_phase;
     if(b0_active)b0_phase=cos_phase;
   }
   if (dev_errors)
   {
     nphase ++;
     dev_phase = nphase;
   }
   TTRACE(u_active,u_phase)
   TTRACE(v_active,v_phase)
   TTRACE(D_active,D_phase)
   TTRACE(bx_active,bx_phase)
   TTRACE(by_active,by_phase)
   TTRACE(vx_active,vx_phase)
   TTRACE(vy_active,vy_phase)
   TTRACE(cos_errors,cos_phase)
   TTRACE(dev_errors,dev_phase)
   TTRACE(vscale_active,vscale_phase)
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  a.allocate(1,npoint,1,m,"a");
  #ifndef NO_AD_INITIALIZE
    a.initialize();
  #endif
  a1.allocate(1,npoint,1,m,"a1");
  #ifndef NO_AD_INITIALIZE
    a1.initialize();
  #endif
  aSmooth.allocate(1,npoint,1,m,"aSmooth");
  #ifndef NO_AD_INITIALIZE
    aSmooth.initialize();
  #endif
  T.allocate(1,m,1,m,"T");
  #ifndef NO_AD_INITIALIZE
    T.initialize();
  #endif
  TT.allocate(1,m,1,m,"TT");
  #ifndef NO_AD_INITIALIZE
    TT.initialize();
  #endif
  d.allocate(1,N,"d");
  #ifndef NO_AD_INITIALIZE
    d.initialize();
  #endif
  c.allocate(1,npoint,1,m,"c");
  #ifndef NO_AD_INITIALIZE
    c.initialize();
  #endif
  Q.allocate(1,m,1,m,"Q");
  #ifndef NO_AD_INITIALIZE
    Q.initialize();
  #endif
  H.allocate(1,N,1,N,"H");
  #ifndef NO_AD_INITIALIZE
    H.initialize();
  #endif
  v.allocate(1,npoint,1,N,"v");
  #ifndef NO_AD_INITIALIZE
    v.initialize();
  #endif
  P.allocate(1,npoint,1,m,1,m,"P");
  #ifndef NO_AD_INITIALIZE
    P.initialize();
  #endif
  P1.allocate(1,npoint,1,m,1,m,"P1");
  #ifndef NO_AD_INITIALIZE
    P1.initialize();
  #endif
  PSmooth.allocate(1,npoint,1,m,1,m,"PSmooth");
  #ifndef NO_AD_INITIALIZE
    PSmooth.initialize();
  #endif
  PSmoothTrans.allocate(1,npoint,1,m,1,m,"PSmoothTrans");
  #ifndef NO_AD_INITIALIZE
    PSmoothTrans.initialize();
  #endif
  PStar.allocate(1,npoint,1,m,1,m,"PStar");
  #ifndef NO_AD_INITIALIZE
    PStar.initialize();
  #endif
  ySmooth.allocate(1,npoint,1,N,"ySmooth");
  #ifndef NO_AD_INITIALIZE
    ySmooth.initialize();
  #endif
  next_y.allocate(1,N,"next_y");
  #ifndef NO_AD_INITIALIZE
    next_y.initialize();
  #endif
  uu.allocate(-50.0,50.0,u_phase,"uu");
  vv.allocate(-50.0,50.0,v_phase,"vv");
  D.allocate(0.0,5000.0,D_phase,"D");
  vx.allocate(0.0,50.0,vx_phase,"vx");
  vy.allocate(0.0,50.0,vy_phase,"vy");
  bx.allocate(-50.0,50.0,bx_phase,"bx");
  by.allocate(-50.0,50.0,by_phase,"by");
  a0.allocate(0.0,50.0,a0_phase,"a0");
  b0.allocate(-80.0,80.0,b0_phase,"b0");
  vscale.allocate(0.0,100.0,vscale_phase,"vscale");
  vy_dev.allocate(2,npoint,-500.0,500.0,dev_phase,"vy_dev");
  Z.allocate(1,N,1,m,"Z");
  #ifndef NO_AD_INITIALIZE
    Z.initialize();
  #endif
  f.allocate("f");
  kalman_like.allocate("kalman_like");
  #ifndef NO_AD_INITIALIZE
  kalman_like.initialize();
  #endif
  recap_err.allocate("recap_err");
  #ifndef NO_AD_INITIALIZE
  recap_err.initialize();
  #endif
  gc_recap_err.allocate("gc_recap_err");
  #ifndef NO_AD_INITIALIZE
  gc_recap_err.initialize();
  #endif
  e1.allocate("e1");
  #ifndef NO_AD_INITIALIZE
  e1.initialize();
  #endif
  e2.allocate("e2");
  #ifndef NO_AD_INITIALIZE
  e2.initialize();
  #endif
  e3.allocate("e3");
  #ifndef NO_AD_INITIALIZE
  e3.initialize();
  #endif
  e4.allocate("e4");
  #ifndef NO_AD_INITIALIZE
  e4.initialize();
  #endif
  sduu.allocate("sduu");
  sdvv.allocate("sdvv");
  sdD.allocate("sdD");
  sdbx.allocate("sdbx");
  sdby.allocate("sdby");
  sdvx.allocate("sdvx");
  sdvy.allocate("sdvy");
  vxy.allocate("vxy");
  hdg.allocate("hdg");
  spd.allocate("spd");
   uu = init_u;
   vv = init_v;
   D  = init_D;
   bx = init_bx;
   by = init_by;
   vx = init_vx;
   vy = init_vy;
   if (D < eps) 
     D = eps;
   TTRACE(init_D,D)
   if (vx < eps) 
     vx = eps;
   TTRACE(init_vx,vx)
   if (vy < eps) 
     vy = eps;
   TTRACE(init_vy,vy)
   if (cos_phase < 0){
     a0 = eps;
     b0 = 0.0;
   }else{
     a0 = init_a0;
     b0 = init_b0;
   }
   vscale = init_vscale;
   track_dates.allocate(1,npoint);
   d.initialize();
   Z.initialize();
   T.initialize();
   T(1,1)=1; T(2,2)=1;
   TT=trans(T);
  
   dvector yLong = column(dat_mat,4);
   TTRACE(min(yLong),max(yLong))
   start_long = min(yLong)+0.5*(max(yLong)-min(yLong));
   //start_long = dat_mat(1,4);
   TRACE(start_long)
   clogf << "\nInput data:" << endl;
   clogf << "index        date sday      long       lat         x         y"
         << endl;
   for (int i = 1; i <= npoint; i++)
   {
     // convert month day year to date (year_month_day object)
     track_dates(i).set((int)dat_mat(i,3),(int)dat_mat(i,2),(int)dat_mat(i,1));
     // convert date to days from solstice
     solar_date(i) = track_dates(i).get_gregorian() - gsolstice + 1;
     // set up estimated geographic posision from tag data
     y_deg(i,1) = dat_mat(i,4);
     y_deg(i,2) = dat_mat(i,5);
     // shift origin longitude
     Y(i,1) = y_deg(i,1) - start_long;
     Y(i,2) = y_deg(i,2);
     dvar_vector Yi=zInv(Y(i));
     clogf << setw(5) << i << "  " << track_dates(i) 
           << setw(5) << solar_date(i)
           << setw(10) << Y(i,1) << setw(10) << Y(i,2)
           << setw(10) << Yi(1) << setw(10) << Yi(2)
           << endl;
   }
   clogf << "\nFinished LOCAL_CALCS in PARAMETER_SECTION.\n" << endl;
   cout << "  Finished LOCAL_CALCS in PARAMETER_SECTION.\n" << endl;
}

void model_parameters::userfunction(void)
{
  setup_d();
  if(!specified)setup_H();
  f+=kalman_filter();
  vxy = sqrt(vx*vx+vy*vy);
  sduu = uu;
  sdvv = vv;
  sdD = D;
  sdbx = bx;
  sdby = by;
  sdvx = vx;
  sdvy = vy;
  spd = sqrt(uu*uu+vv*vv+eps);
  hdg = azimuth(vv,uu);
}

void model_parameters::setup_d(void)
{
  d(1) = bx;
  d(2) = by;
}

void model_parameters::setup_Q(void)
{
  Q.initialize();
  for (int i=1;i<=m;i++)
    Q(i,i) = 2.0*D*dt;
}

void model_parameters::setup_H(void)
{
  H.initialize();
  H(1,1) = vx*vx; 
  H(2,2) = vy*vy; 
}

dvar_vector model_parameters::varA(dvar_matrix Y)
{
  dvar_vector A1(1,npoint), A2(1,npoint);
  dvar_vector tmp(1,2);
  for(int i=1; i<=npoint; ++i){
    tmp=zInv(Y(i));
    A1(i)=tmp(1); A2(i)=tmp(2);
  }
  tmp(1)=pow(std_dev(A1),2); tmp(2)=pow(std_dev(A2),2);
  return(tmp);
									
}

dvar_vector model_parameters::z(dvar_vector alpha)
{
  dvar_vector tmp(1,N);								 
  tmp(1)=alpha(1)/(mpg*cos(alpha(2)/mpg*mpi180));				 
  tmp(2)=alpha(2)/mpg;								 
  return(tmp);									 
									
}

dvar_vector model_parameters::zInv(dvar_vector y)
{
  dvar_vector tmp(1,m);								 	
  tmp(1)=y(1)*mpg*cos(y(2)*mpi180);						 
  tmp(2)=y(2)*mpg;								 
  return(tmp);									 
									
}

dvar_matrix model_parameters::ZHatFun(dvar_vector a)
{
  dvar_matrix tmp(1,N,1,N);							 
  dvariable ex1=mpg*cos(a(2)/mpg*mpi180);					 
  tmp(1,1)=1.0/ex1;								 
  tmp(1,2)=a(1)*sin(a(2)/mpg*mpi180)/(ex1*ex1)*mpi180;				 
  tmp(2,1)=0;									 
  tmp(2,2)=1.0/mpg;								 
  return(tmp);									 
}

dvariable model_parameters::kalman_filter(void)
{
  dvar3_array F(1,npoint,1,N,1,N);
  dvar3_array Finv(1,npoint,1,N,1,N);
  dvar_matrix Ptemp(1,m,1,m);
  a(1,1) = mpg*(Y(1,1))*cos(mpi180*Y(1,2));
  a(1,2) = mpg*Y(1,2);
  if(!release_point){
    dvar_vector tmp(1,2); tmp=varA(Y);
    P0(1,1)=value(tmp(1)); P0(2,2)=value(tmp(2));
  }
  P(1)=P0;
  // This is the Kalman filter recursion. The objects tmp1
  // and tmp2 hold common calculations to optimize a bit
  for (t=2;t<=npoint;t++)
  {
    dt = (int)solar_date(t) - (int)solar_date(t-1);
    setup_Q();
    c(t,1)=uu*dt;
    c(t,2)=vv*dt;
    a1(t)=T*a(t-1)+c(t);
    P1(t)=T*P(t-1)*TT+Q;
    Z=ZHatFun(a1(t));								 
    if((recap_point)&&(t==npoint)){							 
      next_y=z(a1(t));								 
    }else{  								         
      next_y=z(a1(t))+d;							 
    }										 
    v(t)=Y(t)-next_y;
   
    dvar_matrix tmp1=P1(t)*trans(Z); 
 
    if (cos_errors){ 
      int sdx = (int)fmod(solar_date(t),365.25); 
      int bdx = (int)(sdx/182.625) + 1; 
      e1 = cos(two_pi*(pow(-1.0,bdx)*b0+solar_date(t))/365.25); 
      e3 = vy*1.0/sqrt(e1*e1+a0); 
      H(2,2) = e3*e3; 
    } 
    if (active(vy_dev))
    {
      e4 = vy*(exp(vy_dev(t)));
      H(2,2) = e4*e4;
    }
    if(specified){
      H(1,1)=dat_mat(t,6); H(1,2)=dat_mat(t,8); 
      H(2,1)=dat_mat(t,8); H(2,2)=dat_mat(t,7);					 
      H*=vscale; 
    }
    if((recap_point)&&(t==npoint)){							 
      H(1,1)=0; H(1,2)=0; H(2,1)=0; H(2,2)=0;					 
    }									         
    vy_t(t) = sqrt(value(H(2,2)));
    F(t)=Z*tmp1+H;
    Finv(t)=inv(F(t));
    dvar_matrix tmp2= tmp1*Finv(t);
    P(t)=P1(t)-tmp2*Z*P1(t);
    a(t)=a1(t)+tmp2*v(t);
  } // for (t=2;t<=npoint;t++)
  int sgn=0;
  kalman_like = (npoint-1)*log(two_pi);					         	
  										 
  for (t=2;t<=npoint;t++)
  {
    dvariable tkl = v(t)*Finv(t)*v(t);
    kalman_like +=0.5*ln_det(F(t),sgn)+0.5*v(t)*Finv(t)*v(t);   		 
  } // for (t=2;t<=;t++)
  
  dvariable f = kalman_like;							 					 
  if (active(vy_dev))
  {
     f += vy_dev_penalty_wt*norm2(vy_dev);
  }
  return f;  
}

void model_parameters::report()
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  //Smoothing loop                                                          	 
  if(recap_point){								 
    PSmooth(npoint,1,1)=0; PSmooth(npoint,1,2)=0;	 				 
    PSmooth(npoint,2,1)=0; PSmooth(npoint,2,2)=0;					 
  }else{									 
    PSmooth(npoint)=P(npoint);                                                    	 
  }	
  if(recap_point){									 
    aSmooth(npoint)=zInv(Y(npoint));
    ySmooth(npoint)=Y(npoint);   
  }else{
    aSmooth(npoint)=a(npoint);                                                    	 
    ySmooth(npoint)=z(aSmooth(npoint));  //notice without bias term                      	  
  } 
  
  
  for(int i=(npoint-1); i>=1; --i){                                           	 
    PStar(i)=P(i)*inv(P1(i+1));                                          	  
    aSmooth(i)=a(i)+PStar(i)*(aSmooth(i+1)-a(i)-c(i+1));                           	 
    ySmooth(i)=z(aSmooth(i));  //notice without bias term                      	  
    PSmooth(i)=P(i)+PStar(i)*(PSmooth(i+1)-P1(i+1))*trans(PStar(i));     	   
  }                                                                        	 
  for(int i=1; i<=npoint; ++i){PSmoothTrans(i)=ZHatFun(aSmooth(i))*PSmooth(i)*trans(ZHatFun(aSmooth(i)));}
  report << banner << endl;
  REPORT(current_phase())
  char flags[80];
  ostrstream ss(flags,80);
  ss <<active(uu)<<active(vv)<<active(D)<<active(bx)<<active(by)
     <<active(vx)<<active(vy)
     <<cos_errors<<active(vy_dev) << ends;
  REPORT(flags)
  int days_at_liberty = track_dates(npoint)-track_dates(1)+1;
  REPORT(days_at_liberty)
  REPORT(npoint)
  double reporting_rate = (double)npoint/(double)days_at_liberty;
  REPORT(reporting_rate)
  REPORT(npoint)
  int Number_of_parameters =  initial_params::nvarcalc();
  REPORT(Number_of_parameters)
  REPORT(f)
  REPORT(kalman_like)
  REPORT(recap_point)
  if (recap_point)
  {
    //REPORT(recap_point_penalty_weight)
    REPORT(recap_err)
    REPORT(gc_recap_err)
  }
  REPORT(uu)
  REPORT(vv)
  REPORT(D)
  REPORT(bx)
  REPORT(by)
  REPORT(vx)
  REPORT(vy)
  REPORT(a0)
  REPORT(b0)
  REPORT(spd)
  REPORT(hdg)
  REPORT(vxy)
  REPORT(vy_dev_penalty_wt)
  REPORT(norm2(vy_dev))
  REPORT(c)
  MREPORT(Q)
  REPORT(d)
  MREPORT(H)
  if (last_phase())
  { 
    adstring gmt_name("gmt_");
    gmt_name += adstring(flags);
    gmt_name += adstring(".dat");
    REPORT(gmt_name)
    adstring mpt_name("mpt_");
    mpt_name += adstring(flags);
    mpt_name += adstring(".dat");
    REPORT(mpt_name)
    ofstream mpt(mpt_name);
    mpt << "# npoint" << endl;
    mpt << setw(5) << npoint << endl;
    mpt << "#  i       date  dt    j        vy         ax        ay         ox        oy         px        py     smoothX    smoothY   Psmooth11   Psmooth12   Psmooth21   Psmooth22" << endl; 
    dvector PY(1,N);
    for (int i = 1; i <= npoint; i++)
    {
      int dt = 0;
      double vyt = 0.0;
      PY = y_deg(i);
      if (i > 1)
      {
        dt = (int)solar_date(i) - (int)solar_date(i-1);
        vyt = vy_t(i);
        PY = value(z(a(i)));  							 
        PY(1) += start_long;
      }
  
      mpt    << setw(4) << (i-1) << " " << track_dates(i) << setw(4) << dt <<" "
             << setw(5) << solar_date(i) <<" "
             << setw(10) << setprecision(4) << vyt <<" "
             << setw(11) << setprecision(5) << a(i,1) <<" "
             << setw(10) << setprecision(4) << a(i,2) <<" "
             << setw(11) << setprecision(6) << y_deg(i,1) <<" "
             << setw(10) << setprecision(5) << y_deg(i,2) <<" "
             << setw(11) << setprecision(6) << PY(1) <<" "
             << setw(10) << setprecision(5) << PY(2) <<" "
             << setw(12) << setprecision(6) << ySmooth(i,1)+start_long 	<<" "         
             << setw(11) << setprecision(5) << ySmooth(i,2) <<" "			 
             << setw(11) << setprecision(5) << PSmoothTrans(i,1,1) <<" "			 
             << setw(11) << setprecision(5) << PSmoothTrans(i,1,2) <<" "			 
             << setw(11) << setprecision(5) << PSmoothTrans(i,2,1) <<" "			 
             << setw(11) << setprecision(5) << PSmoothTrans(i,2,2) <<" "			 
             << endl;
    }
  
    const int npma = 5;
    const double rmaden = 1.0/(double)npma;
    const int n2 = npma/2  + 1;
    double sumx = 0.0;
    double sumy = 0.0;
    
    dmatrix ZP(1,m,1,m);
    ZP.initialize();
    ofstream gmt(gmt_name);
    // these labels will cause GMT to complain, but shouldn't cause an error
    // they work with R
    gmt << "ox  oy  px  py  mx  my  ex  ey  smoothX  smoothY" << endl;         	 
    for (int i = 1; i <= npoint; i++)
    {
      if (i > 1)
      {
        int dt = (int)solar_date(i) - (int)solar_date(i-1);
        if (dt > 1)
          gmt << ">   >   >   >   >   >   >   >   >   >" << dt <<endl;           
      }
      sumx = 0.0;
      sumy = 0.0;
      if (i < n2)
      {
        sumx = y_deg(i,1);
        sumy = y_deg(i,2);
      }
      else  if (i > (npoint-n2) )
      {
        sumx = y_deg(i,1);
        sumy = y_deg(i,2);
      }
      else
      {
        int n1 = i - n2 + 1;
        int n2 = n1 + npma - 1;
        for (int nn = n1; nn <= n2; nn++)
        {
          sumx += y_deg(nn,1); 
          sumy += y_deg(nn,2); 
        }
        sumx *= rmaden;
        sumy *= rmaden; 
      }
  
      gmt << y_deg(i,1) << "  " << y_deg(i,2);
      if (i==1)
        gmt << "  " << y_deg(i,1) << "  " << y_deg(i,2);
      else
      {
        //gmt << "  " << (pred_y(i,1)+start_long) << "  " << pred_y(i,2);
        //Z(1,1) =  1.0/(mpg*cos(mpi180*a(i,2)*rmpg));					 
        dvector ta = value(z(a(i)));							 
        gmt << "  " << (ta(1)+start_long) << "  " << ta(2);
        dmatrix PP = value(P(i));
        //ZP = sqrt(value(Z)*PP+eps);
        ZP = ((value(Z)*PP+eps)/i);  //!!!!!!!!!!!!!!!!!				 
      }
      gmt << "  " << sumx << "  " << sumy;
      gmt << "  " << ZP(1,1) << "  "  << ZP(2,2);
      //gmt << "  " << (ZP(1,1)/i) << "  "  << (ZP(2,2)/i);
      gmt << "  " << ySmooth(i,1)+start_long << "  "  << ySmooth(i,2);			 
      gmt << endl;
    }
    report << "\nPhase " << current_phase() << " tracks written to files " 
         << mpt_name << " and "
         << gmt_name << endl;
    clogf << "\nPhase " << current_phase() << " tracks written to files " 
         << mpt_name << " and "
         << gmt_name << endl;
    cout << "\nPhase " << current_phase() << " tracks written to files " 
         << mpt_name << " and "
         << gmt_name << endl;
  }
}

void model_parameters::preliminary_calculations(void){
  admaster_slave_variable_interface(*this);
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
  arrmblsize=20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(3000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
 
  //RUNTIME_SECTION
  //convergence_criteria 1e-9, 1e-9, 1e-9, 1e-9
  //maximum_function_evaluations 20, 20, 1000 
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    // so we can stop here
    exit(i);
  }
}
