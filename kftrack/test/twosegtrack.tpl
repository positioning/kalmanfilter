// KFtrack program by John Sibert <jsibert@soest.hawaii.edu> (2001)
// Minor modifications by Anders Nielsen <anielsen@dina.kvl.dk> (2002)    
//
// This version includes: 
//
// Extended Kalman filter 
// Smoothing  
// minor modification of likelihood function 
// Known recapture position option (but weight is not used anymore)
// one or two segments (under development)
 

GLOBALS_SECTION
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
  
  //  global variables
  const double  mpg = 60.0; // Nautical miles per degree
  const double rmpg = 1.0/mpg;
  const double mpi180 = M_PI/180.0;
  const double two_pi = 2.0*M_PI;
  const double eps = 1e-8;  //small number to avoid divide by 0
  year_month_day solstice;
  year_month_day tau_init_date;							//twosegments modification (anielsen@dina.kvl.dk)
  year_month_day_vector track_dates;
  ofstream clogf("kftrack.log");
  int u_phase = -1;
  int v_phase = -1;
  int D_phase = -1;
  int bx_phase = -1;
  int by_phase = -1;
  int vx_phase = -1;
  int vy_phase = -1;
  int tau_phase = -1;								//twosegments modification (anielsen@dina.kvl.dk)
  int cos_phase = -1;
  int dev_phase = -1;
  int t;
  adstring copyright("\n  (c) 2001 John Sibert\n"\
           "  Pelagic Fisheries Research Program, University of Hawaii\n");
  adstring banner = make_banner();
 
DATA_SECTION
  !!cout << banner << endl;
  !!clogf << banner << endl;
  init_int npoint;
  !!TRACE(npoint)
  init_int recap_point
  !!TRACE(recap_point)
  init_int noOfSegments								//twosegments modification (anielsen@dina.kvl.dk)
  !!TRACE(noOfSegments)								//this line is modified (anielsen@dina.kvl.dk)
  int dt;
  int nt;
  !! nt = npoint;
  !!TRACE(nt)
  int nt_kf_like;
  !! nt_kf_like = npoint  ;//  - recap_point; 					//this line is modified (anielsen@dina.kvl.dk)
  !!TRACE(nt_kf_like)
  int N;
  int m;
  !! N = 2;
  !! m = 2;
  !!TTRACE(N,m)
  init_int u_active;
  init_int v_active;
  init_int D_active;
  init_int bx_active;
  init_int by_active;
  init_int vx_active;
  init_int vy_active;
  init_int tau_active;	 							//twosegments modification (anielsen@dina.kvl.dk)

  !!if(noOfSegments==1){tau_active=0;}
  !!cout<<"tau active"<<tau_active<<"\n\n";

  init_number init_u;
  init_number init_v;
  init_number init_D;
  init_number init_bx;
  init_number init_by;
  init_number init_vx;
  init_number init_vy;

  init_number init_tau;						//twosegments modification (anielsen@dina.kvl.dk)	

  init_int cos_errors;
  init_number init_b0;						//twosegments modification (anielsen@dina.kvl.dk)	
  init_number init_a0;
  init_int dev_errors;
  init_number vy_dev_penalty_wt;
  int nphase;
  matrix Y(1,nt,1,N)
  matrix y_deg(1,nt,1,N)
  matrix P0(1,m,1,m)
  vector solar_date(1,nt);
  vector vy_t(1,nt);
  int gsolstice;
  init_matrix dat_mat(1,nt,1,5);
  !! P0.initialize(); 
  number start_long;

 LOCAL_CALCS
   //solstice.set(syr,smo,sda);
   //gsolstice = solstice.get_gregorian();
   gsolstice = previous_solstice((int)dat_mat(1,3),(int)dat_mat(1,2),(int)dat_mat(1,3));
   year_month_day ts(gsolstice);
   solstice = ts;
   TRACE(solstice)

   nphase = 0;
   if (u_active || v_active)			
   {
     nphase ++;
     if (u_active)
       u_phase = nphase;
     if (v_active)
       v_phase = nphase;
   } 
   if (vx_active || vy_active || bx_active || by_active || cos_errors || dev_errors || D_active ||tau_active)
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
     if (D_active)
       D_phase = nphase;
     if (cos_errors){cos_phase = nphase;}
     if (dev_errors){dev_phase = nphase;}
     if (tau_active){
       nphase++;
       tau_phase = nphase;
     }					//twosegments modification (anielsen@dina.kvl.dk)
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

   //tau_init.set(tau_year,tau_month,tau_day);					//twosegments modification (anielsen@dina.kvl.dk)				

   track_dates.allocate(1,nt);							//twosegments modification (anielsen@dina.kvl.dk)
   for (int i = 1; i <= nt; i++)						//twosegments modification (anielsen@dina.kvl.dk)
   {										//twosegments modification (anielsen@dina.kvl.dk)
     // convert month day year to date (year_month_day object)			//twosegments modification (anielsen@dina.kvl.dk)
     track_dates(i).set((int)dat_mat(i,3),(int)dat_mat(i,2),(int)dat_mat(i,1)); //twosegments modification (anielsen@dina.kvl.dk)

     // convert date to days from solstice					//twosegments modification (anielsen@dina.kvl.dk)
     solar_date(i) = track_dates(i).get_gregorian() - gsolstice + 1;		//twosegments modification (anielsen@dina.kvl.dk)
   }										//twosegments modification (anielsen@dina.kvl.dk)

PARAMETER_SECTION
  matrix a(1,nt,1,m);
  matrix a1(1,nt,1,m);
  matrix aSmooth(1,nt,1,m);                                     		//this line is modified (anielsen@dina.kvl.dk)
  matrix T(1,m,1,m)
  matrix TT(1,m,1,m)
  vector d(1,N)
  matrix c(1,nt,1,m)
  matrix Q(1,m,1,m)
  matrix H(1,N,1,N)
  matrix v(1,nt,1,N);
  3darray P(1,nt,1,m,1,m);
  3darray P1(1,nt,1,m,1,m);                                     		//this line is modified (anielsen@dina.kvl.dk)
  3darray PSmooth(1,nt,1,m,1,m);                                		//this line is modified (anielsen@dina.kvl.dk)
  3darray PStar(1,nt,1,m,1,m); 	                                		//this line is modified (anielsen@dina.kvl.dk)
  matrix ySmooth(1,nt,1,N);							//this line is modified (anielsen@dina.kvl.dk)	
  vector next_y(1,N);

  
  !! double tau_low=(double)solar_date(6)-(double)solar_date(1); 		//twosegments modification (anielsen@dina.kvl.dk)
  !! double tau_high=(double)solar_date(nt-5)-(double)solar_date(1);		//twosegments modification (anielsen@dina.kvl.dk)
  !! cout<<"low high "<<tau_low<<" "<<tau_high<<"\n";				//twosegments modification (anielsen@dina.kvl.dk)
  init_bounded_number tau(tau_low,tau_high,tau_phase);				//twosegments modification (anielsen@dina.kvl.dk)

  init_bounded_vector uu(1,noOfSegments,-50.0,50.0,u_phase);			//twosegments modification (anielsen@dina.kvl.dk)
  init_bounded_vector vv(1,noOfSegments,-50.0,50.0,v_phase);			//twosegments modification (anielsen@dina.kvl.dk)
  init_bounded_vector D(1,noOfSegments,0.0,50000.0,D_phase);			//twosegments modification (anielsen@dina.kvl.dk)

  init_bounded_number vx(0.0,15.0,vx_phase);
  init_bounded_number vy(0.0,15.0,vy_phase);
  init_bounded_number bx(-15.0,15.0,bx_phase);
  init_bounded_number by(-15.0,15.0,by_phase);
  init_bounded_number b0(-50.0,50.0,cos_phase);
  init_bounded_number a0(0,50,cos_phase);
  init_bounded_vector vy_dev(2,nt,-500.0,500.0,dev_phase)
  matrix Z(1,N,1,m)
  objective_function_value f
  number kalman_like;
  number recap_err;
  number gc_recap_err;
  number e1;
  number e2;
  number e3;
  number e4;
 
  sdreport_vector sduu(1,noOfSegments);						//twosegments modification (anielsen@dina.kvl.dk)
  sdreport_vector sdvv(1,noOfSegments);						//twosegments modification (anielsen@dina.kvl.dk)
  sdreport_vector sdD(1,noOfSegments);						//twosegments modification (anielsen@dina.kvl.dk)
  sdreport_number sdbx;
  sdreport_number sdby;
  sdreport_number sdvx;
  sdreport_number sdvy;
  sdreport_number vxy;
  sdreport_vector hdg(1,noOfSegments);						//twosegments modification (anielsen@dina.kvl.dk)			
  sdreport_vector spd(1,noOfSegments);						//twosegments modification (anielsen@dina.kvl.dk)

 LOCAL_CALCS
   uu = init_u;  								//twosegments modification (anielsen@dina.kvl.dk)
   vv = init_v;  								//twosegments modification (anielsen@dina.kvl.dk)
   D  = init_D;  								//twosegments modification (anielsen@dina.kvl.dk)
   bx = init_bx;
   by = init_by;
   vx = init_vx;
   vy = init_vy;
   
   if(noOfSegments==2){ 							//twosegments modification (anielsen@dina.kvl.dk)
     tau=init_tau;								//twosegments modification (anielsen@dina.kvl.dk)
   }else{									//twosegments modification (anielsen@dina.kvl.dk)
     tau=(double)solar_date(nt)-(double)solar_date(1)+1;			//twosegments modification (anielsen@dina.kvl.dk)
   }										//twosegments modification (anielsen@dina.kvl.dk)
   cout<<"tau_init "<<tau<<"\n";						//twosegments modification (anielsen@dina.kvl.dk)
   for(int i=1; i<=noOfSegments; ++i){						//twosegments modification (anielsen@dina.kvl.dk)
     if (D(i) < eps) D(i) = eps;						//twosegments modification (anielsen@dina.kvl.dk)
     TTRACE(init_D,D(i));				        		//twosegments modification (anielsen@dina.kvl.dk)
   }										//twosegments modification (anielsen@dina.kvl.dk)

   if (vx < eps) 
     vx = eps;
   TTRACE(init_vx,vx)
   if (vy < eps) 
     vy = eps;
   TTRACE(init_vy,vy)
   if (cos_phase < 0){
     b0 = 0.0;
     a0 = eps;
  }else{
     b0 = init_b0;
     a0 = init_a0;
  }
   //track_dates.allocate(1,nt);						//twosegments modification (anielsen@dina.kvl.dk)
   d.initialize();
   Z.initialize();
   //Z(2,2) = rmpg;								//this line is modified (anielsen@dina.kvl.dk)	
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
   for (int i = 1; i <= nt; i++)
   {
     // convert month day year to date (year_month_day object)
     //track_dates(i).set((int)dat_mat(i,3),(int)dat_mat(i,2),(int)dat_mat(i,1));//twosegments modification (anielsen@dina.kvl.dk)

     // convert date to days from solstice
     //solar_date(i) = track_dates(i).get_gregorian() - gsolstice + 1;		//twosegments modification (anielsen@dina.kvl.dk)

     // set up estimated geographic posision from tag data
     y_deg(i,1) = dat_mat(i,4);
     y_deg(i,2) = dat_mat(i,5);

     // shift origin longitude
     Y(i,1) = y_deg(i,1) - start_long;
     Y(i,2) = y_deg(i,2);

     // set up Z
     //Z(1,1) =  1.0/(mpg*cos(mpi180*Y(i,2)));					//this line is modified (anielsen@dina.kvl.dk)	
     //dvar_vector Yi = inv(Z)*Y(i);						//this line is modified (anielsen@dina.kvl.dk)	
     dvar_vector Yi=zInv(Y(i));
     //dvar_vector iY = Z*Yi;
     clogf << setw(5) << i << "  " << track_dates(i) 
           << setw(5) << solar_date(i)
           << setw(10) << Y(i,1) << setw(10) << Y(i,2)
           << setw(10) << Yi(1) << setw(10) << Yi(2)
     //    << setw(10) << iY(1) << setw(10) << iY(2)
           << endl;
   }
   clogf << "\nFinished LOCAL_CALCS in PARAMETER_SECTION.\n" << endl;
   cout << "  Finished LOCAL_CALCS in PARAMETER_SECTION.\n" << endl;
PROCEDURE_SECTION



  setup_d();
  setup_H();
  f+=kalman_filter();

    //cout<<uu<<" "<<vv<<" "<<D<<" "<<bx<<" "<<by<<" "<<vx<<" "<<vy<<" "<<b0<<" "<<H(2,2);
    //cout<<" "<<f<<"\n\n";

  vxy = sqrt(vx*vx+vy*vy);
  sduu = uu;
  sdvv = vv;
  sdD = D;
  sdbx = bx;
  sdby = by;
  sdvx = vx;
  sdvy = vy;

  for(int i=1; i<=noOfSegments; ++i){						//twosegments modification (anielsen@dina.kvl.dk) 
    spd(i) = sqrt(uu(i)*uu(i)+vv(i)*vv(i)+eps);					//twosegments modification (anielsen@dina.kvl.dk) 
    hdg(i) = azimuth(vv(i),uu(i));                                            	//twosegments modification (anielsen@dina.kvl.dk) 
  }										//twosegments modification (anielsen@dina.kvl.dk) 
									
										//This function substitutes 'setup_c'
FUNCTION dvar_vector drift(dvariable tau, int dt, int time, dvar_vector u, dvar_vector v)
  dvar_vector tmp(1,m);
  if(noOfSegments==2){
    tmp(1) = swave(tau, u(1), u(2), time, dt, 100.0);
    tmp(2) = swave(tau, v(1), v(2), time, dt, 100.0);
  }else{
    tmp(1)=u(1)*dt;
    tmp(2)=v(1)*dt;
  }
  return(tmp);
 
  //FUNCTION setup_c
  //c(1) = uu*dt;
  //c(2) = vv*dt;

FUNCTION setup_d
  d(1) = bx;
  d(2) = by;

									//twosegments modification (anielsen@dina.kvl.dk)
 									//This function substitutes 'setup_Q'
FUNCTION dvar_matrix noise(dvariable tau, int dt, int time, dvar_vector D)
  dvar_matrix tmp(1,m,1,m);
  tmp.initialize();
  dvariable tmpD;
  if(noOfSegments==2){
    tmpD=swave(tau, D(1), D(2), time, dt, 100.0);
  }else{
    tmpD=dt*D(1);
  }
  for (int i=1;i<=m;i++) tmp(i,i) = 2.0*tmpD;
  return(tmp);

//FUNCTION setup_Q
//  Q.initialize();
//  for (int i=1;i<=m;i++)
//    Q(i,i) = 2.0*D*dt;

FUNCTION setup_H
  H.initialize();
  H(1,1) = vx*vx; 
  H(2,2) = vy*vy; 

										//next line is modified (anielsen@dina.kvl.dk)
FUNCTION dvar_vector z(dvar_vector alpha)					
  dvar_vector tmp(1,N);								//this line is modified (anielsen@dina.kvl.dk)
  tmp(1)=alpha(1)/(mpg*cos(alpha(2)/mpg*mpi180));				//this line is modified (anielsen@dina.kvl.dk)
  tmp(2)=alpha(2)/mpg;								//this line is modified (anielsen@dina.kvl.dk)
  return(tmp);									//this line is modified (anielsen@dina.kvl.dk)

										//next line is modified (anielsen@dina.kvl.dk)
FUNCTION dvar_vector zInv(dvar_vector y)
  dvar_vector tmp(1,m);								//this line is modified (anielsen@dina.kvl.dk)	
  tmp(1)=y(1)*mpg*cos(y(2)*mpi180);						//this line is modified (anielsen@dina.kvl.dk)
  tmp(2)=y(2)*mpg;								//this line is modified (anielsen@dina.kvl.dk)
  return(tmp);									//this line is modified (anielsen@dina.kvl.dk)

										//next line is modified (anielsen@dina.kvl.dk)
FUNCTION dvar_matrix ZHatFun(dvar_vector a)
  dvar_matrix tmp(1,N,1,N);							//this line is modified (anielsen@dina.kvl.dk)
  dvariable ex1=mpg*cos(a(2)/mpg*mpi180);					//this line is modified (anielsen@dina.kvl.dk)
  tmp(1,1)=1.0/ex1;								//this line is modified (anielsen@dina.kvl.dk)
  tmp(1,2)=a(1)*sin(a(2)/mpg*mpi180)/(ex1*ex1)*mpi180;				//this line is modified (anielsen@dina.kvl.dk)
  tmp(2,1)=0;									//this line is modified (anielsen@dina.kvl.dk)
  tmp(2,2)=1.0/mpg;								//this line is modified (anielsen@dina.kvl.dk)
  return(tmp);									//this line is modified (anielsen@dina.kvl.dk)

										//this function computes smooth weighted average. 
//FUNCTION dvariable swave(dvariable tau, dvariable u1, dvariable u2, int time, int dt, dvariable scale)
//  dvariable tmp;
//  //cout<<log(cosh(scale*(time-tau)))<<" "<<log(cosh(scale*(time-dt-tau)))<<" "<<log(cosh(scale*(time-tau)))-log(cosh(scale*(time-dt-tau)))<<"\n";
//  tmp=0.5*(u2-u1)*((1.0/scale)*log(cosh(scale*(time-tau))/cosh(scale*(time-dt-tau)))+dt)+u1*dt;  
//  return(tmp);

FUNCTION dvariable swave(dvariable tau, dvariable u1, dvariable u2, int time, int dt, dvariable scale)
  dvariable tmp;
  tmp=(u2-u1)/(M_PI*scale)*(
        scale*(time-tau)*atan(scale*(time-tau))-0.5*log(scale*scale*(time-tau)*(time-tau)+1)
        -scale*(time-dt-tau)*atan(scale*(time-dt-tau))+0.5*log(scale*scale*(time-dt-tau)*(time-dt-tau)+1)
      )+0.5*dt*(u1+u2);
  //cout<<tmp<<" "<<u1<<" "<<u2<<" "<<dt<<" "<<tau<<" "<<time<<"\n";
  return(tmp);

//FUNCTION dvariable swave(dvariable tau, dvariable u1, dvariable u2, int t1, int t2, dvariable scale)
//  dvariable alpha=(u2-u1)/(t2-t1);  
//  dvariable a = (-alpha)/(scale*scale);
//  dvariable b = alpha/scale-a*scale;
//  dvariable c = 0;
//  dvariable d = u1;
//  dvariable dtau=tau-t1;
//  dvariable tmp;
//  if((tau-t1)<=scale){
//    tmp = a*dtau*dtau*dtau+b*dtau*dtau+c*dtau+d;
//  //  cout<<"case1 "<<tau<<" "<<u1<<" "<< u2<<" "<<t1<<" "<<t2<<" "<<scale<<" "<<a<<" "<<b<<" "<<c<<" "<<d<<" "<<tmp<<"\n";
//  }else{
//    if((t2-tau)<=scale){
//      dvariable tmp2=(t2-tau);
//      tmp = u2-(a*tmp2*tmp2*tmp2+b*tmp2*tmp2+c*tmp2+d-u1);
//  //    cout<<"case2 "<<tau<<" "<<u1<<" "<< u2<<" "<<t1<<" "<<t2<<" "<<scale<<" "<<a<<" "<<b<<" "<<c<<" "<<d<<" "<<tmp<<"\n";
//    }else{
//      tmp = u1+dtau*alpha;
//  //    cout<<"case3 "<<tau<<" "<<u1<<" "<< u2<<" "<<t1<<" "<<t2<<" "<<scale<<" "<<a<<" "<<b<<" "<<c<<" "<<d<<" "<<tmp<<"\n";
//    }
//  }
//  return(u1+u2-tmp);


FUNCTION dvariable kalman_filter(void)
  //dvar3_array P(1,nt,1,m,1,m);
  //dvar3_array P1(1,nt,1,m,1,m);                               		//this line is modified (anielsen@dina.kvl.dk)
  dvar3_array F(1,nt,1,N,1,N);
  dvar3_array Finv(1,nt,1,N,1,N);
  dvar_matrix Ptemp(1,m,1,m);
  //dvar_matrix v(1,nt,1,N);

  a(1,1) = mpg*(Y(1,1))*cos(mpi180*Y(1,2));
  a(1,2) = mpg*Y(1,2);

  P(1)=P0;
  // This is the Kalman filter recursion. The objects tmp1
  // and tmp2 hold common calculations to optimize a bit
  //int t;
  //cout<<"\n\n TAU "<<tau<<"\n";
  for (t=2;t<=nt;t++)
  {
    dt = (int)solar_date(t) - (int)solar_date(t-1);
    //setup_Q();                                                                //twosegments modification (anielsen@dina.kvl.dk)
    Q=noise(tau, dt, (int)solar_date(t)-(int)solar_date(1), D);               	//twosegments modification (anielsen@dina.kvl.dk)
    //setup_c();								//twosegments modification (anielsen@dina.kvl.dk)
    c(t)=drift(tau, dt, (int)solar_date(t)-(int)solar_date(1), uu, vv);   		//twosegments modification (anielsen@dina.kvl.dk)
    a1(t)=T*a(t-1)+c(t);
    P1(t)=T*P(t-1)*TT+Q;
    //Z(1,1) =  1.0/(mpg*cos(mpi180*a1(t,2)*rmpg));				//this line is modified (anielsen@dina.kvl.dk)
    Z=ZHatFun(a1(t));								//this line is modified (anielsen@dina.kvl.dk)

    if((recap_point)&&(t==nt)){							//this line is modified (anielsen@dina.kvl.dk)
      next_y=z(a1(t));								//this line is modified (anielsen@dina.kvl.dk)
    }else{  								        //this line is modified (anielsen@dina.kvl.dk)
      next_y=z(a1(t))+d;							//this line is modified (anielsen@dina.kvl.dk)
    }										//this line is modified (anielsen@dina.kvl.dk)

    v(t)=Y(t)-next_y;
    dvar_matrix tmp1=P1(t)*trans(Z); //TZ;

    if (cos_errors){
      int sdx = (int)fmod(solar_date(t),365.25);
      int bdx = (int)(sdx/182.625) + 1;
      e1 = cos(two_pi*(pow(-1.0,bdx)*b0+solar_date(t))/365.25);
      e3 = vy*1.0/sqrt(e1*e1+a0);
      H(2,2) = e3*e3;
    }

    //if (cos_errors)
    //{
    //  e1 = cos(two_pi*(b0+solar_date(t))/365.25);
    //  e3 = vy*1.0/(e1*e1+eps);

    //  H(2,2) = e3*e3;
    //}

    if (active(vy_dev))
    {
      e4 = vy*(exp(vy_dev(t)));
      H(2,2) = e4*e4;
    }

    if((recap_point)&&(t==nt)){							//this line is modified (anielsen@dina.kvl.dk)
      H(1,1)=0; H(1,2)=0; H(2,1)=0; H(2,2)=0;					//this line is modified (anielsen@dina.kvl.dk)
    }									        //this line is modified (anielsen@dina.kvl.dk)

    vy_t(t) = sqrt(value(H(2,2)));

    F(t)=Z*tmp1+H;
    Finv(t)=inv(F(t));
    dvar_matrix tmp2= tmp1*Finv(t);
    P(t)=P1(t)-tmp2*Z*P1(t);
    a(t)=a1(t)+tmp2*v(t);

  } // for (t=2;t<=nt;t++)

  int sgn=0;
  kalman_like = (nt-1)*log(two_pi);					        //this line is modified (anielsen@dina.kvl.dk)	

  for (t=2;t<=nt_kf_like;t++)
  {
    dvariable tkl = v(t)*Finv(t)*v(t);
    kalman_like +=0.5*ln_det(F(t),sgn)+0.5*v(t)*Finv(t)*v(t);   		//this line is modified (anielsen@dina.kvl.dk)
  } // for (t=2;t<=nt_kf_like;t++)

  //recap_err = 0.0;								//this line is modified (anielsen@dina.kvl.dk)
  //gc_recap_err = 0.0;								//this line is modified (anielsen@dina.kvl.dk)
  //if (recap_point)								//this line is modified (anielsen@dina.kvl.dk)
  //{										//this line is modified (anielsen@dina.kvl.dk)
  //  t = nt;									//this line is modified (anielsen@dina.kvl.dk)	
  //										//this line is modified (anielsen@dina.kvl.dk)	
  //  // Pythagorean distance in degrees					//this line is modified (anielsen@dina.kvl.dk)	
  //  next_y = z(a(t));								//this line is modified (anielsen@dina.kvl.dk)
  //  v(t)=Y(t)-next_y;								//this line is modified (anielsen@dina.kvl.dk)	
  //  recap_err = sqrt(norm2(v(t)));						//this line is modified (anielsen@dina.kvl.dk)	
  //										//this line is modified (anielsen@dina.kvl.dk)	
  //  // great circle distance in Nmi						//this line is modified (anielsen@dina.kvl.dk)	
  //  gc_recap_err = gc_dist(Y(t),next_y);					//this line is modified (anielsen@dina.kvl.dk)	
  //}										//this line is modified (anielsen@dina.kvl.dk)	
   
  //dvariable f = kalman_like 							//this line is modified (anielsen@dina.kvl.dk)	
  //            + recap_point_penalty_weight*recap_err;				//this line is modified (anielsen@dina.kvl.dk)	
  
  dvariable f = kalman_like;							//this line is modified (anielsen@dina.kvl.dk)					 

  if (active(vy_dev))
  {
     f += vy_dev_penalty_wt*norm2(vy_dev);
  }

  return f;  



REPORT_SECTION  

  //Smoothing loop                                                          	//this line is modified (anielsen@dina.kvl.dk)
  if(recap_point){								//this line is modified (anielsen@dina.kvl.dk)
    PSmooth(nt,1,1)=0; PSmooth(nt,1,2)=0;	 				//this line is modified (anielsen@dina.kvl.dk)
    PSmooth(nt,2,1)=0; PSmooth(nt,2,2)=0;					//this line is modified (anielsen@dina.kvl.dk)
  }else{									//this line is modified (anielsen@dina.kvl.dk)
    PSmooth(nt)=P(nt);                                                    	//this line is modified (anielsen@dina.kvl.dk)
  }										//this line is modified (anielsen@dina.kvl.dk)
  aSmooth(nt)=a(nt);                                                    	//this line is modified (anielsen@dina.kvl.dk)
  ySmooth(nt)=z(aSmooth(nt));  //notice without bias term                      	//this line is modified (anielsen@dina.kvl.dk) 
  for(int i=(nt-1); i>=1; --i){                                           	//this line is modified (anielsen@dina.kvl.dk)
    PStar(i)=P(i)*inv(P1(i+1));                                          	//this line is modified (anielsen@dina.kvl.dk) 
    aSmooth(i)=a(i)+PStar(i)*(aSmooth(i+1)-a(i)-c(i+1));                      	//this line is modified (anielsen@dina.kvl.dk)
    ySmooth(i)=z(aSmooth(i));  //notice without bias term                      	//this line is modified (anielsen@dina.kvl.dk) 
    PSmooth(i)=P(i)+PStar(i)*(PSmooth(i+1)-P1(i+1))*trans(PStar(i));     	//this line is modified (anielsen@dina.kvl.dk)  
  }                                                                        	//this line is modified (anielsen@dina.kvl.dk)

  report << banner << endl;
  REPORT(current_phase())
  char flags[80];
  ostrstream ss(flags,80);
  ss <<active(uu)<<active(vv)<<active(D)<<active(bx)<<active(by)
     <<active(vx)<<active(vy)
     <<active(b0)<<active(vy_dev) << ends;
  REPORT(flags)
  int days_at_liberty = track_dates(npoint)-track_dates(1)+1;
  REPORT(days_at_liberty)
  REPORT(npoint)
  double reporting_rate = (double)npoint/(double)days_at_liberty;
  REPORT(reporting_rate)
  REPORT(nt_kf_like)
  int Number_of_parameters =  initial_params::nvarcalc();
  REPORT(Number_of_parameters)
  REPORT(f)
  REPORT(kalman_like)
  REPORT(recap_point)
  //if (recap_point)								//this line is modified (anielsen@dina.kvl.dk)
  //{										//this line is modified (anielsen@dina.kvl.dk)
  //  REPORT(recap_point_penalty_weight)					//this line is modified (anielsen@dina.kvl.dk)
  //  REPORT(recap_err)								//this line is modified (anielsen@dina.kvl.dk)
  //  REPORT(gc_recap_err)							//this line is modified (anielsen@dina.kvl.dk)
  //}										//this line is modified (anielsen@dina.kvl.dk)
  REPORT(uu)
  REPORT(vv)
  REPORT(spd)
  REPORT(hdg)
  REPORT(D)
  REPORT(bx)
  REPORT(by)
  REPORT(vx)
  REPORT(vy)
  REPORT(vxy)
  REPORT(a0)
  REPORT(b0)
  REPORT(vy_dev_penalty_wt)
  REPORT(norm2(vy_dev))
  REPORT(c)
  MREPORT(Q)
  REPORT(d)
  MREPORT(H)
  if(noOfSegments==2){
    dvariable cutDays = tau;				
    REPORT(cutDays);
    //year_month_day cutDate((int)gsolstice+(int)(value(tau)+.5)-1);
    //REPORT(cutDate);
  }

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
    mpt    << "#  i       date  dt    j        vy         ax        ay         ox        oy         px        py     smoothX    smoothY" << endl; //this line is modified (anielsen@dina.kvl.dk)
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
  
        //Z(1,1) =  1.0/(mpg*cos(mpi180*a(i,2)*rmpg));				//this line is modified (anielsen@dina.kvl.dk)
        PY = value(z(a(i)));  							//this line is modified (anielsen@dina.kvl.dk)
        PY(1) += start_long;
      }
  
      mpt    << setw(4) << (i-1) << " " << track_dates(i) << setw(4) << dt <<" "
             << setw(5) << solar_date(i)<<" "
             << setw(10) << setprecision(4) << vyt<<" " 
             << setw(11) << setprecision(5) << a(i,1)<<" "
             << setw(10) << setprecision(4) << a(i,2)<<" "
             << setw(11) << setprecision(6) << y_deg(i,1)<<" "
             << setw(10) << setprecision(5) << y_deg(i,2)<<" "
             << setw(11) << setprecision(6) << PY(1)<<" "
             << setw(10) << setprecision(5) << PY(2)<<" "
             << setw(12) << setprecision(6) << ySmooth(i,1)+start_long<<" "	        //this line is modified (anielsen@dina.kvl.dk)
             << setw(11) << setprecision(5) << ySmooth(i,2)<<" "			//this line is modified (anielsen@dina.kvl.dk)
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
    //clogf << "\nox  oy  px  py  ax  ay  ex  ey" << endl;
    gmt << "ox  oy  px  py  mx  my  ex  ey  smoothX  smoothY" << endl;         	//this line is modified (anielsen@dina.kvl.dk)
    for (int i = 1; i <= npoint; i++)
    {
      if (i > 1)
      {
        int dt = (int)solar_date(i) - (int)solar_date(i-1);
        if (dt > 1)
          gmt << ">   >   >   >   >   >   >   >   >   >" << dt <<endl;          //this line is modified (anielsen@dina.kvl.dk)
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
        //Z(1,1) =  1.0/(mpg*cos(mpi180*a(i,2)*rmpg));					//this line is modified (anielsen@dina.kvl.dk)
        dvector ta = value(z(a(i)));							//this line is modified (anielsen@dina.kvl.dk)
        gmt << "  " << (ta(1)+start_long) << "  " << ta(2);
        dmatrix PP = value(P(i));
        //ZP = sqrt(value(Z)*PP+eps);
        ZP = sqrt((value(Z)*PP+eps)/i);
      }
      gmt << "  " << sumx << "  " << sumy;
      gmt << "  " << ZP(1,1) << "  "  << ZP(2,2);
      //gmt << "  " << (ZP(1,1)/i) << "  "  << (ZP(2,2)/i);
      gmt << "  " << ySmooth(i,1)+start_long << "  "  << ySmooth(i,2);			//this line is modified (anielsen@dina.kvl.dk)
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




TOP_OF_MAIN_SECTION
  arrmblsize=20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(3000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
 
  //RUNTIME_SECTION
  //convergence_criteria 1e-9, 1e-9, 1e-9, 1e-9
  //maximum_function_evaluations 20, 20, 1000 
