// KFtrack program by John Sibert <jsibert@soest.hawaii.edu> (2001)
// Modifications by Anders Nielsen <anielsen@dina.kvl.dk> (2002+3)    
//
// This version includes: 
//
// Extended Kalman filter 
// Smoothing  
// minor modification of likelihood function 
// Known recapture position option (but weight is not used anymore)
// first need not to be known 
// error estimates on most probable track 
// Now with SST also ...  
 
GLOBALS_SECTION
  #include <fstream.h>
  #include <math.h>
  #include <fvar.hpp>
  #include <adstring.hpp>
  #include "yrmonday.h"
  #include <strstream>
  using std::ostrstream;
  ofstream checkSST("check.dat");
  #include "trace.h"
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
  int bsst_phase = -1;
  int vx_phase = -1;
  int vy_phase = -1;
  int vsst_phase=-1;
  int cos_phase = -1;
  int a0_phase = -1;
  int b0_phase = -1;
  int dev_phase = -1;
  int t;
  adstring copyright("\n  (c) 2001,2002,2003,2004, and 2005 John Sibert and Anders Nielsen \n"\
          "  Pelagic Fisheries Research Program, University of Hawaii\n");
  adstring banner = make_banner();
  int isOutside=1;
 
DATA_SECTION
  !!cout << banner << endl;
  !!clogf << banner << endl;
  init_int npoint;
  !!TRACE(npoint)
  init_int release_point
  !!TRACE(release_point)
  init_int recap_point
  !!TRACE(recap_point)
  int dt;
  int N;
  int m;
  !! N = 3;
  !! m = 2;
  !!TTRACE(N,m) 
  init_int u_active;
  init_int v_active;
  init_int D_active;
  init_int bx_active;
  init_int by_active;
  init_int bsst_active;
  init_int vx_active;
  init_int vy_active;
  init_int vsst_active;
  init_int a0_active;
  init_int b0_active;

  init_number init_u;
  init_number init_v;
  init_number init_D;
  init_number init_bx;
  init_number init_by;
  init_number init_bsst;  
  init_number init_vx;
  init_number init_vy;
  init_number init_vsst;
  init_number init_a0;
  init_number init_b0;

  init_int cos_errors;
  init_int dev_errors;
  init_number vy_dev_penalty_wt;

  int nphase;
  matrix Y(1,npoint,1,N)
  matrix y_deg(1,npoint,1,N)
  matrix P0(1,m,1,m)
  !! P0.initialize(); 
  vector solar_date(1,npoint);
  vector vy_t(1,npoint);
  int gsolstice;
  init_matrix dat_mat(1,npoint,1,6);

  number start_long;
  //##################################################################SST-part
  !! ad_comm::change_datafile_name("sst.dat");
  init_int noSstTimes; 
  init_int noSstLon; 
  init_int noSstLat;
  init_vector sstTimes(1,noSstTimes);
  init_vector sstLon(1,noSstLon);
  init_vector sstLat(1,noSstLat);
  init_3darray sst(1,noSstTimes,1,noSstLat,1,noSstLon);
  init_3darray sstDx(1,noSstTimes,1,noSstLat,1,noSstLon);
  init_3darray sstDy(1,noSstTimes,1,noSstLat,1,noSstLon);
  //##################################################################END SST-part
 LOCAL_CALCS
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
   if (vx_active || vy_active || vsst_active || bx_active || by_active || bsst_active)
   {
     nphase ++;
     if (bx_active)
       bx_phase = nphase;
     if (by_active)
       by_phase = nphase;
     if (bsst_active)
       bsst_phase = nphase;
     if (vx_active)
       vx_phase = nphase;
     if (vy_active)
       vy_phase = nphase;
     if (vsst_active)
       vsst_phase = nphase;
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

PARAMETER_SECTION
  matrix a(1,npoint,1,m);
  matrix a1(1,npoint,1,m);
  matrix aSmooth(1,npoint,1,m);                                     	
  matrix T(1,m,1,m)
  matrix TT(1,m,1,m)
  vector d(1,N)
  matrix c(1,npoint,1,m)
  matrix Q(1,m,1,m)
  matrix H(1,N,1,N)
  matrix v(1,npoint,1,N);
  3darray P(1,npoint,1,m,1,m);
  3darray P1(1,npoint,1,m,1,m);                                     		
  3darray PSmooth(1,npoint,1,m,1,m);                                		
  3darray PSmoothTrans(1,npoint,1,N,1,N);                                		
  3darray PStar(1,npoint,1,m,1,m); 	                                		
  matrix ySmooth(1,npoint,1,N);								
  vector next_y(1,N);
  init_bounded_number uu(-50.0,50.0,u_phase);
  init_bounded_number vv(-50.0,50.0,v_phase);
  init_bounded_number D(0.0,5000.0,D_phase);
  init_bounded_number vx(0.0,50.0,vx_phase);
  init_bounded_number vy(0.0,50.0,vy_phase);
  init_bounded_number vsst(0.0,50.0,vsst_phase);
  init_bounded_number bx(-50.0,50.0,bx_phase);
  init_bounded_number by(-50.0,50.0,by_phase);
  init_bounded_number bsst(-50.0,50.0,bsst_phase);
  init_bounded_number a0(0.0,50.0,a0_phase); 
  init_bounded_number b0(-80.0,80.0,b0_phase);
  init_bounded_vector vy_dev(2,npoint,-500.0,500.0,dev_phase)
  matrix Z(1,N,1,m)
  objective_function_value f
  number kalman_like;
  number e1;
  number e2;
  number e3;
  number e4;
 
  sdreport_number sduu;
  sdreport_number sdvv;
  sdreport_number sdD;
  sdreport_number sdbx;
  sdreport_number sdby;
  sdreport_number sdbsst;
  sdreport_number sdvx;
  sdreport_number sdvy;
  sdreport_number sdvsst;
  sdreport_number vxy;
  sdreport_number hdg;
  sdreport_number spd;
 LOCAL_CALCS
   uu = init_u;
   vv = init_v;
   D  = init_D;
   bx = init_bx;
   by = init_by;
   bsst = init_bsst;
   vx = init_vx;
   vy = init_vy;
   vsst = init_vsst;
   if (D < eps) 
     D = eps;
   TTRACE(init_D,D)
   if (vx < eps) 
     vx = eps;
   TTRACE(init_vx,vx)
   if (vy < eps) 
     vy = eps;
   TTRACE(init_vy,vy)
   if (vsst < eps) 
     vsst = eps;
   TTRACE(init_vsst,vsst)
   if (cos_phase < 0){
     a0 = eps;
     b0 = 0.0;
   }else{
     a0 = init_a0;
     b0 = init_b0;
   }
   track_dates.allocate(1,npoint);
   d.initialize();
   Z.initialize();
   T.initialize();
   T(1,1)=1; T(2,2)=1;
   TT=trans(T);
  
   dvector yLong = column(dat_mat,4);
   TTRACE(min(yLong),max(yLong))
   start_long = min(yLong)+0.5*(max(yLong)-min(yLong));

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
     Y(i,3) = dat_mat(i,6);
     

     dvar_vector Yi=zInv(Y(i));
     clogf << setw(5) << i << "  " << track_dates(i) 
           << setw(5) << solar_date(i)
           << setw(10) << Y(i,1) << setw(10) << Y(i,2)
           << setw(10) << Yi(1) << setw(10) << Yi(2)
           << endl;
   }
   clogf << "\nFinished LOCAL_CALCS in PARAMETER_SECTION.\n" << endl;
   cout << "  Finished LOCAL_CALCS in PARAMETER_SECTION.\n" << endl;
PROCEDURE_SECTION
  isOutside=0;
  setup_d();
  setup_H();

  f+=kalman_filter();

  if(isOutside==1){
    cout<<"*** Warning: Some track-points are outside the SST-region.\n";
    cout<<"             This is not a problem during optimization, but make sure\n";
    cout<<"             this warning is not present at the final function evaluation\n";
  }

  vxy = sqrt(vx*vx+vy*vy);
  sduu = uu;
  sdvv = vv;
  sdD = D;
  sdbx = bx;
  sdby = by;
  sdbsst = bsst;
  sdvx = vx;
  sdvy = vy;
  sdvsst = vsst;
  spd = sqrt(uu*uu+vv*vv+eps);
  hdg = azimuth(vv,uu);

FUNCTION int getIndexFromVector(dvariable x, dvar_vector vec)
  int i=1; 
  while((vec(i)<x)&&(i<=vec.indexmax()))i++;
  if(x<vec(vec.indexmin())){
    i++;
  //cout<<"WARNING from getIndexFromVector: "<<x<<" not in ("<<min(vec)<<","<<max(vec)<<")\n";  
    isOutside=1;
  }
  if(x>vec(vec.indexmax())){
    i--;
  //cout<<"WARNING from getIndexFromVector: "<<x<<" not in ("<<min(vec)<<","<<max(vec)<<")\n";  
    isOutside=1;
  }
  return(i-1);

FUNCTION int timeindex(dvariable greg)
  int tmp=getIndexFromVector(greg, sstTimes);
  if((sstTimes(tmp)-greg)>(sstTimes(tmp+1)-greg))tmp++;
  return(tmp);

FUNCTION int lonindex(dvariable x)
  return(getIndexFromVector(x, sstLon));

FUNCTION int latindex(dvariable y)
  return(getIndexFromVector(y, sstLat));

FUNCTION dvariable kernel(dvariable w)
  dvariable tmp=w*w*w;
  return((1-tmp)*(1-tmp)*(1-tmp));

FUNCTION dvar_vector getField(dvariable t, dvariable x, dvariable y)
  int it=timeindex(t);  
  int ix=lonindex(x);
  int iy=latindex(y);
  dvariable x1=sstLon(ix), x2=sstLon(ix+1), y1=sstLat(iy), y2=sstLat(iy+1), dx=x2-x1, dy=y2-y1;
  dvariable w11=kernel((x-x1)/dx)*kernel((y-y1)/dy);
  dvariable w12=kernel((x2-x)/dx)*kernel((y-y1)/dy);
  dvariable w21=kernel((x-x1)/dx)*kernel((y2-y)/dy);
  dvariable w22=kernel((x2-x)/dx)*kernel((y2-y)/dy);
  dvariable wdot=w11+w12+w21+w22;
  dvar_vector ret(1,3);
  ret(1)=w11*sst(it,iy,ix)+w12*sst(it,iy,ix+1)+w21*sst(it,iy+1,ix)+w22*sst(it,iy+1,ix+1);  
  ret(2)=w11*sstDx(it,iy,ix)+w12*sstDx(it,iy,ix+1)+w21*sstDx(it,iy+1,ix)+w22*sstDx(it,iy+1,ix+1);  
  ret(3)=w11*sstDy(it,iy,ix)+w12*sstDy(it,iy,ix+1)+w21*sstDy(it,iy+1,ix)+w22*sstDy(it,iy+1,ix+1);  
  return(ret/wdot);

FUNCTION setup_d
  d(1) = bx;
  d(2) = by;
  d(3) = bsst;

FUNCTION setup_Q
  Q.initialize();
  for (int i=1;i<=m;i++)
    Q(i,i) = 2.0*D*dt;

FUNCTION setup_H
  H.initialize();
  H(1,1) = vx*vx; 
  H(2,2) = vy*vy; 
  H(3,3) = vsst*vsst;

FUNCTION dvar_vector varA(dvar_matrix Y)
  dvar_vector A1(1,npoint), A2(1,npoint);
  dvar_vector tmp(1,m);
  for(int i=1; i<=npoint; ++i){
    tmp=zInv(Y(i));
    A1(i)=tmp(1); A2(i)=tmp(2);
  }
  tmp(1)=pow(std_dev(A1),2); tmp(2)=pow(std_dev(A2),2);
  return(tmp);
									
FUNCTION dvar_vector z(dvar_vector alpha, int i)					
  dvar_vector tmp(1,N);								 
  tmp(1)=alpha(1)/(mpg*cos(alpha(2)/mpg*mpi180));				 
  tmp(2)=alpha(2)/mpg;
  dvar_vector tmp2=getField(0.0+track_dates(i).get_gregorian(),tmp(1)+start_long,tmp(2));
  tmp(3)=tmp2(1);
  return(tmp);									 
									
FUNCTION dvar_vector zInv(dvar_vector y)
  dvar_vector tmp(1,m);								 	
  tmp(1)=y(1)*mpg*cos(y(2)*mpi180);						 
  tmp(2)=y(2)*mpg;								 
  return(tmp);									 

									
FUNCTION dvar_matrix ZHatFun(dvar_vector a, int i)
  dvar_matrix tmp(1,N,1,m);							 
  dvariable ex1=mpg*cos(a(2)/mpg*mpi180);				 
  dvariable lon=a(1)/(mpg*cos(a(2)/mpg*mpi180));				 
  dvariable lat=a(2)/mpg;
  //cout<<i<<" "<<track_dates(i)<<" "<<track_dates(i).get_gregorian()<<" "<<0.0+track_dates(i).get_gregorian()<<"\n";
  dvar_vector tmp2=getField(0.0+track_dates(i).get_gregorian(),lon+start_long,lat);			 
  tmp(1,1)=1.0/ex1;								 
  tmp(1,2)=a(1)*sin(a(2)/mpg*mpi180)/(ex1*ex1)*mpi180;				 
  tmp(2,1)=0;									 
  tmp(2,2)=1.0/mpg;
  tmp(3,1)=tmp2(2)*tmp(1,1);
  tmp(3,2)=tmp2(2)*tmp(1,2)+tmp2(3)*tmp(2,2);								 
  return(tmp);									 


FUNCTION dvariable kalman_filter(void)
  dvar3_array F(1,npoint,1,N,1,N);
  dvar3_array Finv(1,npoint,1,N,1,N);
  dvar_matrix Ptemp(1,m,1,m);

  a(1,1) = mpg*(Y(1,1))*cos(mpi180*Y(1,2));
  a(1,2) = mpg*Y(1,2);

  if(!release_point){
    dvar_vector tmp=varA(Y);
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
    Z=ZHatFun(a1(t),t);								 

    if((recap_point)&&(t==npoint)){							 
      next_y=z(a1(t),t);								 
    }else{  								         
      next_y=z(a1(t),t)+d;							 
    }										 

    v(t)=Y(t)-next_y;
    
    

    dvar_matrix tmp1=P1(t)*trans(Z); 
 
    if (cos_errors){ 
      int sdx = (int)fmod(solar_date(t),365.25); 
      int bdx = (int)(sdx/182.625) + 1; 
      e1 = cos(two_pi*(pow(-1.0,bdx)*b0+solar_date(t))/365.25); 
      e3 = vy*1.0/sqrt(e1*e1+a0); 
      H(2,2) = e3*e3; 
    }else{
      H(2,2) = vy*vy;
    } 

    if (active(vy_dev))
    {
      e4 = vy*(exp(vy_dev(t)));
      H(2,2) = e4*e4;
    }

    if(fabs(Y(t,2))>95.0){
      H(2,2)=10000;
    }
    if(Y(t,3)<0.0){
      H(3,3)=10000;
    }else{
      H(3,3)=vsst*vsst;
    }

    if((recap_point)&&(t==npoint)){							 
      H(1,1)=0; H(1,2)=0; H(2,1)=0; H(2,2)=0;					 
    }									         

    vy_t(t) = sqrt(value(H(2,2)));

    F(t)=Z*tmp1+H;
    if(fabs(det(F(t)))<1.0e-3){
      cout<<"*** Warning: Matrix is close to singular diagonal elements added\n";
      F(t)+=identity_matrix(1,N)*1.0e-3;
    }
    Finv(t)=inv(F(t));
    dvar_matrix tmp2= tmp1*Finv(t);
    P(t)=P1(t)-tmp2*Z*P1(t);
    a(t)=a1(t)+tmp2*v(t);
  } // for (t=2;t<=npoint;t++)

  int sgn=0;
  kalman_like = (npoint-1)*0.5*N*log(two_pi); 
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

REPORT_SECTION
  //Smoothing loop                                                          	 
  if(recap_point){								 
    PSmooth(npoint,1,1)=0; PSmooth(npoint,1,2)=0;	 				 
    PSmooth(npoint,2,1)=0; PSmooth(npoint,2,2)=0;					 
  }else{									 
    PSmooth(npoint)=P(npoint);                                                    	 
  }	

  if(recap_point){									 
    aSmooth(npoint)=zInv(Y(npoint));
  }else{
    aSmooth(npoint)=a(npoint);                                                    	 
  } 
  ySmooth(npoint)=z(aSmooth(npoint),npoint);  //notice without bias term                      	  
  

  for(int i=(npoint-1); i>=1; --i){                                           	 
    PStar(i)=P(i)*inv(P1(i+1));                                          	  
    aSmooth(i)=a(i)+PStar(i)*(aSmooth(i+1)-a(i)-c(i+1));                           	 
    ySmooth(i)=z(aSmooth(i),i);  //notice without bias term                      	  
    PSmooth(i)=P(i)+PStar(i)*(PSmooth(i+1)-P1(i+1))*trans(PStar(i));     	   
  }                                                                        	 

  for(int i=1; i<=npoint; ++i){
    PSmoothTrans(i)=ZHatFun(aSmooth(i),i)*PSmooth(i)*trans(ZHatFun(aSmooth(i),i));
  }


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
    mpt << "  i       date  dt    j        vy         ax        ay         ox        oy      oSST       px        py     pSST     smoothX    smoothY   smoothSST   Psmooth11   Psmooth12   Psmooth21   Psmooth22" << endl; 
    dvector PY(1,N);
    for (int i = 1; i <= npoint; i++){
      int dt = 0;
      double vyt = 0.0;
      PY = y_deg(i);
      if (i > 1){
        dt = (int)solar_date(i) - (int)solar_date(i-1);
        vyt = vy_t(i);
        PY = value(z(a(i),i));  							 
        PY(1) += start_long;
      }
  
      mpt<< setw(4) << (i-1) << " " << track_dates(i) << setw(4) << dt <<" "
         << setw(5) << solar_date(i) <<" "
         << setw(10) << setprecision(4) << vyt <<" "
         << setw(11) << setprecision(5) << a(i,1) <<" "
         << setw(10) << setprecision(4) << a(i,2) <<" "
         << setw(11) << setprecision(6) << y_deg(i,1) <<" "
         << setw(10) << setprecision(5) << y_deg(i,2) <<" "
         << setw(10) << setprecision(5) << dat_mat(i,6) <<" "
         << setw(11) << setprecision(6) << PY(1) <<" "
         << setw(10) << setprecision(5) << PY(2) <<" "
         << setw(10) << setprecision(5) << PY(3) <<" "
         << setw(12) << setprecision(6) << ySmooth(i,1)+start_long <<" "         
         << setw(11) << setprecision(5) << ySmooth(i,2) <<" "
         << setw(11) << setprecision(5) << ySmooth(i,3) <<" "     			 
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
    
    dmatrix ZP(1,N,1,m);
    ZP.initialize();
    ofstream gmt(gmt_name);
    // these labels will cause GMT to complain, but shouldn't cause an error
    // they work with R
    gmt << "ox  oy  px  py  mx  my  ex  ey  smoothX  smoothY" << endl;         	 
    for (int i = 1; i <= npoint; i++){
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
        dvector ta = value(z(a(i),i));							 
        gmt << "  " << (ta(1)+start_long) << "  " << ta(2);
        dmatrix PP = value(P(i));
        ZP = ((value(Z)*PP+eps)/i);  //!!!!!!!!!!!!!!!!!				 
      }
      gmt << "  " << sumx << "  " << sumy;
      gmt << "  " << ZP(1,1) << "  "  << ZP(2,2);
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
  checkSST<<sst(1);


TOP_OF_MAIN_SECTION
  arrmblsize=20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(3000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
 
  //RUNTIME_SECTION
  //convergence_criteria 1e-9, 1e-9, 1e-9, 1e-9
  //maximum_function_evaluations 20, 20, 1000 
