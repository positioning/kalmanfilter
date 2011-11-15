GLOBALS_SECTION
  //#include <fstream.h>
  #include <fvar.hpp>
  //#include "newverify.h"
  ofstream clogf("program.log");
  #define TRACE(object) clogf<<"line "<<__LINE__<<", file "<<__FILE__<<", "<<#object " =\n "<<object<<endl<<endl;
  const double PiDiv180 = M_PI/180.0;
  const double RPiDiv180 = 180.0/M_PI;
  
  double deg2rad(double d){
    return d*PiDiv180;
  }

  double rad2deg(double r){
    return RPiDiv180*r;
  }

  dvector deg2rad(dvector d){
    return d*PiDiv180;
  }

  dvector rad2deg(dvector r){
    return RPiDiv180*r;
  }

  dmatrix deg2rad(dmatrix d){
    return d*PiDiv180;
  }

  dmatrix rad2deg(dmatrix r){
    return RPiDiv180*r;
  }

  dvector colSums(dmatrix x){
    int min=x.rowmin();
    int max=x.rowmax();
    dvector ret=x(min);
    for(int i=(min+1); i<=max; ++i)ret+=x(i);
    return ret;
  }

  dvector atan2vec(dvector x, dvector y){
    int min=x.indexmin();
    int max=x.indexmax();
    dvector ret(min,max);
    for(int i=min; i<=max; ++i)ret(i)=atan2(x(i),y(i));
    return ret;
  }

  double myfmod(double x1, double x2)
  {
    double q = x1 / x2, tmp;
    tmp = x1 - floor(q) * x2;
    q = floor(tmp/x2);
    return tmp - q * x2;
  }

  dvector myfmod(const dvector& x1, double x2)
  {
    int min=x1.indexmin();
    int max=x1.indexmax();
    dvector ret(min,max);  
    for(int i=min; i<=max; ++i){
      ret(i)=myfmod(x1(i),x2);
    }
    return ret;
  }


  double siderealUT(double jdu){
     double T=(jdu-2451545.0)/36525; // (12.1)
     double theta0=280.46061837+360.98564736629*(jdu-2451545.0)+0.000387933*T*T-T*T*T/38710000;
     return(myfmod(theta0,360));
  }

  dvector obliquity(double T){
    double c1 = 23.43929;  //(23+(26/60)+(21.448/3600))
    double c2 = -46.8150;
    double c3 = -0.00059;
    double c4 = 0.001813;
    double eps0 = c1 + T*(c2 + T*(c3 + T*c4))/3600; // (22.2)
    double LSun = 288.4665 + 36000.7698*T;
    double LMoon = 218.3165 + 481267.8813*T;
    double Omega = 125.04452 - 1934.136261*T + 0.0020708*T*T + T*T*T/450000;
    Omega=deg2rad(myfmod(Omega,360));
    LSun=deg2rad(myfmod(LSun,360));
    LMoon=deg2rad(myfmod(LMoon,360));
    // these are accurate to 0.5 sec;
    double deltaPsi = (-17.2*sin(Omega) - 1.32*sin(2*LSun) - 0.23*sin(2*LMoon) +0.21*sin(2*Omega))/3600;
    double deltaEps = (9.2*cos(Omega) + 0.57*cos(2*LSun) + 0.1*cos(2*LMoon) - 0.09*cos(2*Omega))/3600;
    double eps = eps0 + deltaEps;
    double epsApp = eps + 0.00256*cos(Omega); //this line corrects to apparent position (p.165)
    
    dvector ret(1,6);
    ret(1)=eps;
    ret(2)=epsApp;
    ret(3)=eps0;
    ret(4)=deltaPsi;
    ret(5)=deltaEps;
    ret(6)=rad2deg(Omega);

    return ret;
  }

    dvector solarCoordinates(double jde){
    double T = (jde-2451545.0)/36525; // (25.1)
    double L0 = 280.46646 + 36000.76983*T + 0.0003032*T*T; // (25.2)
    L0=myfmod(L0,360);
    double M = 357.52911 + T*(35999.05029 - T*0.0001537); // (25.3)
    double e = 0.016708634 - T*(0.000042037 + T*0.0000001267); // (25.4)
    M=deg2rad(myfmod(M,360));
    double C = (1.914602 - T*(0.004817 + T*0.000014))*sin(M) +
               (0.019993 - 0.000101*T)*sin(2*M) +
               0.000289*sin(3*M);
    double STL = L0+C;
    dvector ob=obliquity(T);
    double lambda = STL - 0.00569 - 0.00478*sin(deg2rad(ob(6))); // (25.8)
    lambda=deg2rad(myfmod(lambda,360));
    STL=deg2rad(myfmod(STL,360));
    double eps=deg2rad(myfmod(ob(2),360));
    double tanAlpha = cos(eps)*sin(lambda)/cos(lambda); // (25.6)
    double sinDelta = sin(eps)*sin(lambda);             // (25.7)
    double alpha = 360*atan2(cos(eps)*sin(lambda),cos(lambda))/(2*M_PI);
    double delta = 360*asin(sinDelta)/(2*M_PI);
    double alphaApp = myfmod(alpha,360);
    double deltaApp = delta;

    dvector ret(1,2);
    ret(1)=alphaApp;
    ret(2)=deltaApp;
    return ret;
  }

  dmatrix lunarCoordinates(dvector jde, dvector lpCoef, dvector dCoef, dvector mCoef, dvector mpCoef, dvector fCoef, dmatrix tableA, dmatrix tableB){
    int low=jde.indexmin();
    int hig=jde.indexmax();
    dvector T=(jde-2451545.0)/36525.0; // (22.1)  
    dvector TT=square(T);    

    dmatrix T04(1,5,low,hig);
    for(int i=0; i<=4; ++i){
      T04(i+1)=pow(T,i);
    }
    dvector Lp=myfmod(lpCoef*T04,360.);
    dvector D=myfmod(dCoef*T04,360.);
    dvector M=myfmod(mCoef*T04,360.);
    dvector Mp=myfmod(mpCoef*T04,360.);
    dvector F=myfmod(fCoef*T04,360.);

    dvector A1=myfmod(119.75+131.849*T,360.);
    dvector A2=myfmod(53.09+479264.290*T,360.);
    dvector A3=myfmod(313.45+481266.484*T,360.);
 
    dvector E=1.0-0.002516*T-0.0000074*TT;
    dvector EE=square(E);
 
    dmatrix DMMpF(1,4,low,hig);  
    DMMpF(1)=D;
    DMMpF(2)=M;
    DMMpF(3)=Mp;
    DMMpF(4)=F;

    dmatrix tabA14T(1,4,tableA.rowmin(),tableA.rowmax());
    for(int i=1; i<=4; ++i){
      tabA14T(i)=column(tableA,i);
    }
    dmatrix AArg=trans(tabA14T)*DMMpF;

    dmatrix l(tableA.rowmin(),tableA.rowmax(),low,hig);
    dmatrix r(tableA.rowmin(),tableA.rowmax(),low,hig);
    for(int i=low; i<=hig; ++i){
      dvector tmpA=sin(deg2rad(column(AArg,i)));
      l.colfill(i,elem_prod(column(tableA,5),tmpA));
      r.colfill(i,elem_prod(column(tableA,6),tmpA));
    }
    for(int i=tableA.rowmin(); i<=tableA.rowmax(); ++i){
      if(fabs(fabs(tableA(i,2))-1)<0.001){//1
        l(i)=elem_prod(E,l(i));
        r(i)=elem_prod(E,r(i));
      }
      if(fabs(fabs(tableA(i,2))-2)<0.001){//2
        l(i)=elem_prod(EE,l(i));
        r(i)=elem_prod(EE,r(i));
      }
    }

    dmatrix tabB14T(1,4,tableB.rowmin(),tableB.rowmax());
    for(int i=1; i<=4; ++i){
      tabB14T(i)=column(tableB,i);
    }
    dmatrix BArg=trans(tabB14T)*DMMpF;
    
    dmatrix b(tableB.rowmin(),tableB.rowmax(),low,hig);
    for(int i=low; i<=hig; ++i){
      dvector tmpB=sin(deg2rad(column(BArg,i)));
      b.colfill(i,elem_prod(column(tableB,5),tmpB));
    }
    for(int i=tableB.rowmin(); i<=tableB.rowmax(); ++i){
      if(fabs(fabs(tableB(i,2))-1)<0.001){//1
        b(i)=elem_prod(E,b(i));
      }
      if(fabs(fabs(tableB(i,2))-2)<0.001){//2
        b(i)=elem_prod(EE,b(i));
      }
    }
 
    dvector Sl=colSums(l)+
               3958*sin(deg2rad(A1))+
               1962*sin(deg2rad(Lp-F))+
               318*sin(deg2rad(A2));
    dvector Sr=colSums(r);
    dvector Sb=colSums(b)-
               2235*sin(deg2rad(Lp))+
               382*sin(deg2rad(A3))+
               175*sin(deg2rad(A1-F))+
               175*sin(deg2rad(A1+F))+
               127*sin(deg2rad(Lp-Mp))-
               115*sin(deg2rad(Lp+Mp));
    dvector lambda=Lp+Sl/1000000;
    dvector beta=Sb/1000000;
    dvector Delta=385000.56+Sr/1000;

    dvector ppii=rad2deg(asin(6378.14/Delta));

    dmatrix ob(1,6,low,hig);
    for(int i=low; i<=hig; ++i){
      ob.colfill(i,obliquity(T(i)));
    }
    dvector obEps=ob(1);
    //dvector obEpsApp=ob(2);
    //dvector obEps0=ob(3);
    dvector obDeltaPsi=ob(4);
    //dvector obDeltaEps=ob(5);
    //dvector obOmega=ob(6);

    dvector lambdaApp=lambda+obDeltaPsi;
    dvector eps=obEps;

    dvector alphaApp=rad2deg(
                       atan2vec(
		         elem_prod(sin(deg2rad(lambdaApp)),cos(deg2rad(eps)))-
		         elem_prod(tan(deg2rad(beta)),sin(deg2rad(eps))),
		         cos(deg2rad(lambdaApp))
                       )
		     );

    dvector deltaApp=rad2deg(
                       asin(
                         elem_prod(sin(deg2rad(beta)),cos(deg2rad(eps)))+
	                 elem_prod(elem_prod(cos(deg2rad(beta)),sin(deg2rad(eps))),sin(deg2rad(lambdaApp)))
                       )        
                     );

    dvector i=180.0-D-6.289*sin(deg2rad(Mp))+
              2.100*sin(deg2rad(M))-
              1.274*sin(deg2rad(2*D-Mp))-
              0.658*sin(deg2rad(2*D))-
              0.214*sin(deg2rad(2*Mp))-
              0.110*sin(deg2rad(D));
    dvector k=(1.0+cos(deg2rad(i)))/2;
    dmatrix ret(1,3,low,hig);
    ret(1)=myfmod(alphaApp,360.);
    ret(2)=deltaApp;
    ret(3)=k;
    return ret;  
  }

  int fl(double x){
    int ret;
    if(x<0){
      ret=-(int)fabs(x);
    }else{
      ret=(int)x;
    }
    return ret;
  }

  ivector fl(dvector x){
    int min=x.indexmin();
    int max=x.indexmax();
    ivector ret(min,max);  
    for(int i=min; i<=max; ++i){
      ret(i)=fl(x(i));
    }
    return ret;
  }

  double JDE(double year, double month, double day){
    if(month<=2){
      year-=1;
      month+=12; 
    }
    int A=fl(year/100);
    int B=0;
    if(!((year<1582)||((year==1582)&&(month<10))||((year==1582)&&(month==10)&&(day<=4)))){
      B+=2-A+A/4;
    }
    double JD=(int)(365.25*(year+4716)) + (int)(30.6001*(month+1)) + day + B - 1524.5;
    return JD;
  }

  dvector JDE(dvector year, dvector month, dvector day){
    int min=year.indexmin();
    int max=year.indexmax();
    dvector ret(min,max);  
    for(int i=min; i<=max; ++i){
      ret(i)=JDE(year(i),month(i),day(i));
    }
    return ret;
  }

  int binSearch(double x, const dvector & tab){
    int min=tab.indexmin();
    int max=tab.indexmax();
    int mid, ascend;  
    if((x<tab(min))||(x>=tab(max))){
      cout<<setprecision(10)<<"\nx = "<<x<<" not in table"<<endl;
      cout<<setprecision(10)<<tab(min)<<" "<<tab(max)<<endl;
      exit(1);
    }
    ascend=(tab(max)>tab(min));
    while((max-min)>1){
      mid=(max+min) >> 1; // fast bitwise shift way of computing mid-point 
      if(x>=tab(mid) == ascend){
        min=mid;
      }else{
        max=mid;
      }
    }
    return min;
  }

  double approxfun(double x, const dvector& xTab, const dvector& yTab){
    int idx=binSearch(x,xTab);
    double dx=xTab(idx+1)-xTab(idx);
    double frac=(x-xTab(idx))/dx;
    return yTab(idx)*(1-frac)+yTab(idx+1)*frac;
  }

  double higestDec(const dvector& jdu, dvector& light){
    int i; 
    int res=10; 
    int n[res]; 
    double v[res]; 
    for(i=0; i<res; ++i){
      v[i]=0.0; 
      n[i]=0; 
    }
    int min=jdu.indexmin();
    int max=jdu.indexmax();
    int idx;
    double t; 
    double x;  
    for(i=min; i<=max; ++i){
      t=jdu(i); 
      idx=fl((t-fl(t))*10); 
      n[idx]++; 
      v[idx]+=light(i);
    }
    int which=0;
    double maxval=v[0]/n[0];  
    for(i=1; i<res; ++i){
      if((v[i]/n[i])>maxval){
        maxval=v[i]/n[i];
        which=i; 
      } 
    }
    return(0.1*which);
  }

  dvector approxfun(const dvector x, const dvector& xTab, const dvector& yTab){
    int min=x.indexmin();
    int max=x.indexmax();
    dvector ret(min,max);  
    for(int i=min; i<=max; ++i){
      ret(i)=approxfun(x(i),xTab,yTab);
    }
    return ret;
  }

  dvector JDU(const dvector& jde, const dvector& xTab, const dvector& yTab){
    dvector ret(jde.indexmin(),jde.indexmax());
    ret=jde-approxfun(jde,xTab,yTab)/86400;
    return ret;  
  }

  dmatrix solarEventTimes(const dvector& jdu, dvector& light){
    //first we count
    double highDec=higestDec(jdu, light);
    double lowDec=highDec+.5;
    if(lowDec>1.0){
      lowDec-=1.0; 
    }
    //cout<<"Here it is"<< highDec<<endl; 
    int min=jdu.indexmin();
    int max=jdu.indexmax();
    ivector day=fl(jdu-highDec); //shifts the cut point
    int nday=0;
    for(int i=min+1; i<=max; ++i){
      if((day(i)-day(i-1))>0.5)nday++;
    }
    //determine the cutpoints 
    ivector cutPoints(1,nday);
    nday=0;
    for(int i=min+1; i<=max; ++i){
      if((day(i)-day(i-1))>0.5){
        nday++; 
        cutPoints(nday)=i;
      }
    }
    //define day ranges 
    ivector dayFirst(1,nday-1);    
    ivector dayLast(1,nday-1);
    for(int i=1; i<=(nday-1); ++i){
      dayFirst(i)=cutPoints(i); 
    }
    for(int i=2; i<=nday; ++i){
      dayLast(i-1)=cutPoints(i)-1; 
    }
    nday--;
    for(int i=1; i<=nday; ++i){
      if((dayLast(i)-dayFirst(i)+1)<5.5){
        cout<<"Number of observations in day number "<<i<<" is < 5, which is too few"<<endl;
	cout<<dayFirst(i)<<" "<<dayLast(i)<<endl;
        exit(i);
      }
    }
    // now the big search for two event times per day 
    cout<<"Scanning for solar event times ";
    ivector idxEventTimes(1,2*nday);
    dmatrix ret(1,2*nday,1,2);
    int iBest, jBest, ni, no, d, i, j, lastm2, first, last; 
    double mBest, si, so, thismean, thislight;
    double lastdot=0;
    for(d=1; d<=nday; ++d){
      first=dayFirst(d);
      last=dayLast(d);
      lastm2=last-2;
      mBest=0;
      for(i=first; i<=(last-2); ++i){
        si=sum(light(i+1,last-1));
        so=sum(light(first,i))+light(last);
        ni=(last-1)-(i+1)+1;
        no=(i-first+1)+1;
        thismean=fabs(si/ni-so/no);
        if(fabs(jdu(i)-jdu(last-1))<(1.0/24.0))thismean=0;
        if(fabs(jdu(i)-jdu(first+1))<(1.0/24.0))thismean=0; 
        if(thismean>mBest){
          iBest=i;
          jBest=last-1;
          mBest=thismean;
        }
        for(j=lastm2; j>i; --j){
          thislight=light(j); 
          si-=thislight;
          so+=thislight;
          ni--;
          no++;
          thismean=fabs(si/ni-so/no); 
          if(fabs(jdu(i)-jdu(j))<(1.0/12.0))thismean=0; 
          if(fabs(jdu(i)-jdu(last-1))<(1.0/24.0))thismean=0;
          if(fabs(jdu(i)-jdu(first+1))<(1.0/24.0))thismean=0; 
          if(thismean>mBest){
            iBest=i;
            jBest=j;
            mBest=thismean;
          }
        }
      }
      //cout<<"mBest for day:"<<d<<" "<<first<<" "<<iBest<<" "<<jBest<<" "<<last
      //    <<" "<<jdu(iBest)-floor(jdu(iBest))<<" "<<jdu(jBest)-floor(jdu(jBest))<<" "<<mBest<<endl; 
      idxEventTimes(2*d-1)=iBest;
      idxEventTimes(2*d)=jBest;
      
      // find out which are up and which down 
      si=sum(light(iBest+1,jBest));
      so=sum(light(first,iBest))+sum(light(jBest+1,last));
      ni=jBest-(iBest+1)+1;
      no=(iBest-first+1)+(last-(jBest+1)+1);
      //cout<<d<<" "<<si/ni<<" "<<so/no<<" "<<ni<<" "<<no<<" "<<ni+no<<endl; 
      if(si/ni>so/no){
        ret(2*d-1,2) = 1;
        ret(2*d,2) = -1;
      }else{
        ret(2*d-1,2) = -1;
        ret(2*d,2) = 1;
      }
      if((d-lastdot)>(0.0125*nday)){  // about 80 dots
        lastdot+=0.0125*nday;
        cout<<"."; cout.flush();
      }
    }   

    //11234 11242 11243 11521
    //ofstream bug("../bug.log");
    //for(int i=11234; i<=11521; ++i){
    //  bug<<jdu(i)<<" "<<light(i)<<endl; 
    //}
    //bug.close();
    cout<<" Done."<<endl;
    // organize the output 
    
    for(int i=1; i<=2*nday; ++i){
      ret(i,1)=0.5*(jdu(idxEventTimes(i))+jdu(idxEventTimes(i)+1));
    }
    //cout<<setprecision(10)<<ret<<endl; 
    return ret;
  }

  dmatrix solarEventTimesNoScan(dvector& jdu, dvector& light){
    int min=jdu.indexmin();
    int max=jdu.indexmax();
    int nstep=1;
    for(int i=min+1; i<=max; ++i){
      if(fabs(jdu(i)-jdu(i-1))>(2.0/24.0))nstep++;
    }

    //determine the cutpoints 
    ivector stepFirst(1,nstep);
    ivector stepLast(1,nstep);
    stepFirst(1)=min;
    stepLast(nstep)=max;
    int step=1;
    for(int i=min+1; i<=max; ++i){
      if(fabs(jdu(i)-jdu(i-1))>(2.0/24.0)){
        step++;
        stepFirst(step)=i;
        stepLast(step-1)=i-1;
      }
    }
    dmatrix ret(1,nstep,1,4);
    for(int step=1; step<=nstep; ++step){
      int i1=stepFirst(step);
      int i2=stepLast(step);
      ret(step,1)=0.5*(jdu(i1)+jdu(i2));
      ret(step,2)=sum(jdu(i1,i2)-mean(jdu(i1,i2)))*sum(light(i1,i2)-mean(light(i1,i2)))/
                  sum(square(jdu(i1,i2)-mean(jdu(i1,i2))));
      ret(step,2)/=fabs(ret(step,2));
      ret(step,3)=i1;
      ret(step,4)=i2;
    }
    return ret;
  }

 
  ivector isNear(const dvector& x, const dvector& points, double delta){
    double d2=delta/2;
    int min=x.indexmin();
    int max=x.indexmax();
    ivector ret(min,max);
    ret=0; 
    min=points.indexmin();
    max=points.indexmax();
    int i1,i2;
    for(int i=min; i<=max; ++i){
      //cout<<i<<" "; 
      i1=binSearch(points(i)-d2,x)+1;
      i2=binSearch(points(i)+d2,x);
      //cout<<i1<<" "<<i2<<endl; 
      ret(i1,i2)=i;        
    }
    return ret;
  }

  ivector genFirstIdx(const dvector& x, int n){
    ivector ret(1,n);
    int count=1;
    int min=x.indexmin();
    int max=x.indexmax();
    ret(1)=min;
    for(int i=(min+1); i<=max; ++i){
      if(fabs(x(i)-x(i-1))>.5){
        count++;
        if(count>n){
          cout<<"Warning in genFirstIdx. The scanning is problematic due to outliers"<<endl; 
        }else{ 
          ret(count)=i;
        }
      }
    }
    return ret;
  }

  ivector genLastIdx(const dvector& x, int n){
    ivector ret(1,n);
    int count=0;
    int min=x.indexmin();
    int max=x.indexmax();
    for(int i=(min+1); i<=max; ++i){
      if(fabs(x(i)-x(i-1))>.5){
        count++;
        ret(count)=i-1;
      }
    }
    ret(n)=max;
    return ret;
  }

DATA_SECTION
  !! cout<<"Reading data in ..."; cout.flush();
  !! ad_comm::change_datafile_name("deltaT.dat");  
  init_int nrowDT 
  init_int ncolDT 
  init_matrix datDT(1,nrowDT,1,ncolDT)
  vector yearDT(1,nrowDT) 
  vector monthDT(1,nrowDT)
  vector dayDT(1,nrowDT)
  vector deltaT(1,nrowDT) 
  vector jdeDT(1,nrowDT)
  !! yearDT=column(datDT,3);
  !! monthDT=column(datDT,2);
  !! dayDT=column(datDT,1);
  !! deltaT=column(datDT,4);
  !! jdeDT=JDE(yearDT,monthDT,dayDT);  

  !! ad_comm::change_datafile_name("input.dat");  
  init_int scanIt;
  init_vector fixFirst(1,8);
  init_matrix fixFirstVar(1,2,1,2);
  init_vector fixLast(1,8);
  init_matrix fixLastVar(1,2,1,2);
  number fixFirstJDU;
  number fixLastJDU; 
  !! dvector fixFirstLastJDE(1,2);  
  !! fixFirstLastJDE(1)=JDE(fixFirst[3],fixFirst[4],fixFirst[5]+fixFirst[6]/24+fixFirst[7]/1440+fixFirst[8]/86400);
  !! fixFirstLastJDE(2)=JDE(fixLast[3],fixLast[4],fixLast[5]+fixLast[6]/24+fixLast[7]/1440+fixLast[8]/86400);
  !! dvector fixFirstLastJDU=JDU(fixFirstLastJDE,jdeDT,deltaT);
  !! fixFirstJDU=fixFirstLastJDU(1);
  !! fixLastJDU=fixFirstLastJDU(2);
  number midLon;
  !! midLon=0.5*fixFirst(1)+0.5*fixLast(1);
  init_vector durationOfSunEvent(1,2); // (fracTowardsDay, fracTowardsNight) Unit is days. For instance 1/24 is an hour
 
  init_int nrowFull 
  init_int ncolFull 
  init_matrix datFull(1,nrowFull,1,ncolFull)

  init_int noBadDays
  init_matrix badDays(1,noBadDays,1,3)

  !! // excludes bad days  
  !! if(noBadDays>=1){
  !!   ivector badjdu=fl(JDU(JDE(column(badDays,3),column(badDays,2),column(badDays,1)),jdeDT,deltaT));
  !!   ivector realjdu=fl(JDU(JDE(column(datFull,1),column(datFull,2),column(datFull,3)+
  !!                              column(datFull,4)/24+column(datFull,5)/1440+column(datFull,6)/86400),jdeDT,deltaT));
  !!   ivector goodIdx(1,nrowFull);
  !!   goodIdx=1;
  !!   for(int i=1; i<=nrowFull; ++i){
  !!     for(int j=1; j<=noBadDays; ++j)if(badjdu(j)==realjdu(i))goodIdx(i)=0;
  !!   }
  !!   int nrowFullTmp=sum(goodIdx);
  !!   dmatrix datFullTmp(1,nrowFullTmp,1,ncolFull);
  !!   int ii=0;   
  !!   for(int i=1; i<=nrowFull; ++i){
  !!     if(goodIdx(i)==1){
  !!       ii++;
  !!       datFullTmp(ii)=datFull(i);
  !!     } 
  !!   }
  !!   datFull.deallocate();
  !!   datFull=datFullTmp;
  !!   nrowFull=nrowFullTmp;
  !! }

  !! ad_comm::change_datafile_name("lunar.dat");  
  init_vector lpCoef(1,5)
  init_vector dCoef(1,5) 
  init_vector mCoef(1,5) 
  init_vector mpCoef(1,5) 
  init_vector fCoef(1,5)
  init_int nrowA
  init_int ncolA
  init_matrix tableA(1,nrowA,1,ncolA)
  init_int nrowB
  init_int ncolB
  init_matrix tableB(1,nrowB,1,ncolB)
  !! cout<<" Done."<<endl;
  
  !! cout<<"Computing JDE and JDU ..."; cout.flush();
  vector year(1,nrowFull) 
  vector month(1,nrowFull)
  vector day(1,nrowFull)
  vector hour(1,nrowFull)
  vector min(1,nrowFull)
  vector sec(1,nrowFull)
  vector depth(1,nrowFull)
  vector light(1,nrowFull)
  vector temp(1,nrowFull)
  vector jde(1,nrowFull)
  vector jdu(1,nrowFull)


  !! year=column(datFull,1);
  !! month=column(datFull,2);
  !! day=column(datFull,3);
  !! hour=column(datFull,4);
  !! min=column(datFull,5);
  !! sec=column(datFull,6);
  !! depth=column(datFull,7);
  !! light=column(datFull,8);
  !! temp=column(datFull,9);

  !! jde=JDE(year,month,day+hour/24+min/1440+sec/86400);
  !! jdu=JDU(jde,jdeDT,deltaT);
  !! cout<<" Done."<<endl;

  int nstep;   
  !! if(scanIt==1){
  !!   dmatrix sunEvent=solarEventTimes(jdu,light);
  !!   //cout<<setprecision(10)<<sunEvent<<endl;  /// HERE  
  !!   //nstep=sunEvent.indexmax()-sunEvent.indexmin()+1;
  !!   // Extending the range 
  !!   double SolarEventDT=durationOfSunEvent(1)+durationOfSunEvent(2);
  !!   double SolarEventAdd=0.5*(durationOfSunEvent(1)-durationOfSunEvent(2));
  !!   for(int i=sunEvent.rowmin(); i<=sunEvent.rowmax(); ++i){
  !!     sunEvent(i,1)+=sunEvent(i,2)*SolarEventAdd;
  !!   }

  !!   //for(int i=sunEvent.rowmin(); i<=sunEvent.rowmax(); ++i){
  !!   //  cout<<i<<setprecision(10)<<" "<<sunEvent(i,1)<<" "<<sunEvent(i,2)<<endl; 
  !!   //}

  !!   ivector nearIdx=isNear(jdu,column(sunEvent,1),SolarEventDT);
  !!   //cout<<"\nnearIdx\n"<<nearIdx<<endl; 
  !!
  !!   if(nearIdx(nearIdx.indexmin())==0){
  !!     nstep=0;
  !!   }else{
  !!     nstep=1; 
  !!   }   
  !!   for(int i=nearIdx.indexmin()+1; i<=nearIdx.indexmax(); ++i){
  !!     if(nearIdx(i)>nearIdx(i-1)){
  !!       nstep++;
  !!     }
  !!   }
  !!
  !!
  !!   int nrow=0; 
  !!   for(int i=1; i<=nrowFull; ++i)if(nearIdx(i)>0.5)++nrow;
  !!   int ncol=12;
  !!   dmatrix dat(1,nrow,0,ncol);
  !!   dvector obs(1,nrow);
  !!   dat.initialize();
  !!   obs.initialize();
  !!   dvector sc(1,2);
  !!   int count=0;  
  !!   for(int i=1; i<=nrowFull; ++i){
  !!     if(nearIdx(i)>0.5){
  !!       count++;
  !!       obs(count)=light(i);
  !!       dat(count,0)=jde(i);        
  !!       dat(count,1)=jdu(i);
  !!       dat(count,2)=siderealUT(jdu(i));
  !!       sc=solarCoordinates(jde(i)); // 1:alpha 2:delta both apparent 
  !!       dat(count,3)=sc(1);
  !!       dat(count,4)=cos(deg2rad(sc(2)));
  !!       dat(count,5)=sin(deg2rad(sc(2)));
  !!       // 6, 7, 8, and 9, reserved for 
  !!       //moonAlpha, cosMoonDelta, sinMoonDelta, and moonFrac 
  !!       // 
  !!       // 
  !!       dat(count,10)=temp(i);
  !!       dat(count,11)=depth(i);        
  !!       dat(count,12)=nearIdx(i);        
  !!     }
  !!   }
  !!   //cout<<"\ndat\n"<<dat<<endl;
  !!   //cout<<"\nobs\n"<<obs<<endl;
  !!   //cout<<"0"<<endl;        
  !!   cout<<nstep<<" solar event times found"<<endl;
  !!   //cout<<"1"<<endl;        
  !!   dmatrix lc=lunarCoordinates(column(dat,0), lpCoef, dCoef, mCoef, mpCoef, fCoef, tableA, tableB);
  !!   dat.colfill(6,lc(1));     
  !!   dat.colfill(7,cos(deg2rad(lc(2))));     
  !!   dat.colfill(8,sin(deg2rad(lc(2))));     
  !!   dat.colfill(9,lc(3));     
  !!   //cout<<"2"<<endl;        
  !!   ofstream red("reduced.dat");
  !!   red<<setprecision(20)<<dat<<endl;
  !!   red.close();
  !!   //cout<<"3"<<endl;          
  !!   ivector idx1(1,nstep);
  !!   //cout<<"3.1"<<endl;          
  !!   //cout<<"\nidx1\n"<<idx1<<endl;
  !!   //cout<<"\ndatCL121\n"<<column(dat,12)<<endl;
  !!   //cout<<"\nnstep\n"<<nstep<<endl;
  !!   idx1=genFirstIdx(column(dat,12),nstep); 
  !!   //cout<<"3.2"<<endl;          
  !!   //cout<<"\nidx1\n"<<idx1<<endl;
  !!   ivector idx2(1,nstep);
  !!   //cout<<"3.3"<<endl;          
  !!   idx2=genLastIdx(column(dat,12),nstep);
  !!   //cout<<idx1<<endl<<endl<<idx2<<endl<<endl;
  !!   //cout<<"4"<<endl;          
  !!   ofstream out("ukf.dat");
  !!   out<<"# midLon"<<endl<<midLon<<endl;
  !!   out<<"# fixFirst"<<endl<<fixFirst(1,2)<<" "<<setprecision(20)<<fixFirstJDU<<endl;
  !!   out<<"# fixFirstVar"<<endl<<fixFirstVar<<endl;
  !!   out<<setprecision(9);
  !!   out<<"# fixLast"<<endl<<fixLast(1,2)<<" "<<setprecision(20)<<fixLastJDU<<endl;
  !!   out<<"# fixLastVar"<<endl<<fixLastVar<<endl;
  !!   out<<"# nrow"<<endl<<nrow<<endl;
  !!   out<<"# ncol"<<endl<<ncol+1<<endl;
  !!   out<<"# nstep"<<endl<<nstep<<endl;
  !!   out<<"# idx1"<<endl<<idx1<<endl;
  !!   out<<"# idx2"<<endl<<idx2<<endl;
  !!   dmatrix obsMat(1,nrow,1,ncol+1);
  !!   obsMat.initialize();
  !!   obsMat.colfill(1,column(dat,0));
  !!   obsMat.colfill(2,column(dat,1));
  !!   obsMat.colfill(3,column(dat,2));
  !!   obsMat.colfill(4,column(dat,11));
  !!   obsMat.colfill(5,column(dat,3));
  //  .........//
  !!   obsMat.colfill(7,lc(1));
  !!   obsMat.colfill(8,lc(2));
  !!   obsMat.colfill(9,lc(3));
  //  .........//
  !!   obsMat.colfill(10,obs);
  !!   obsMat.colfill(11,column(dat,10));
  !!   obsMat.colfill(12,column(dat,12));
  !!   for(int i=1; i<=nrow; ++i){
  !!     obsMat(i,13)=sunEvent((int)dat(i,12),1);
  !!     obsMat(i,6)=(solarCoordinates(obsMat(i,1)))(2);
  !!   }
  !!   out<<"# obsMat"<<endl<<setprecision(20)<<obsMat<<endl;
  !!   out.close();  
  !! }else{
  !!   dmatrix sunEvent=solarEventTimesNoScan(jdu,light);
  !!   nstep=sunEvent.indexmax()-sunEvent.indexmin()+1;
  !!   ivector nearIdx(jdu.indexmin(),jdu.indexmax());
  !!   for(int i=sunEvent.indexmin(); i<=sunEvent.indexmax(); ++i){
  !!     nearIdx((int)sunEvent(i,3),(int)sunEvent(i,4))=i;
  !!   }
  !!   int nrow=0;
  !!   for(int i=1; i<=nrowFull; ++i)if(nearIdx(i)>0.5)++nrow;
  !!   int ncol=12;
  !!   dmatrix dat(1,nrow,0,ncol);
  !!   dvector obs(1,nrow);
  !!   dat.initialize();
  !!   obs.initialize();
  !!   dvector sc(1,2);
  !!   int count=0;  
  !!   for(int i=1; i<=nrowFull; ++i){
  !!     if(nearIdx(i)>0.5){
  !!       count++;
  !!       obs(count)=light(i);
  !!       dat(count,0)=jde(i);        
  !!       dat(count,1)=jdu(i);
  !!       dat(count,2)=siderealUT(jdu(i));
  !!       sc=solarCoordinates(jde(i)); // 1:alpha 2:delta both apparent 
  !!       dat(count,3)=sc(1);
  !!       dat(count,4)=cos(deg2rad(sc(2)));
  !!       dat(count,5)=sin(deg2rad(sc(2)));
  !!       // 6, 7, 8, and 9, reserved for 
  !!       //moonAlpha, cosMoonDelta, sinMoonDelta, and moonFrac 
  !!       // 
  !!       // 
  !!       dat(count,10)=temp(i);
  !!       dat(count,11)=depth(i);        
  !!       dat(count,12)=nearIdx(i);        
  !!     }
  !!   }
  !!  
  !!   cout<<nstep<<" solar event times found"<<endl;
  !!   dmatrix lc=lunarCoordinates(column(dat,0), lpCoef, dCoef, mCoef, mpCoef, fCoef, tableA, tableB);
  !!   dat.colfill(6,lc(1));     
  !!   dat.colfill(7,cos(deg2rad(lc(2))));     
  !!   dat.colfill(8,sin(deg2rad(lc(2))));     
  !!   dat.colfill(9,lc(3));   
  !!   ofstream red("reduced.dat");
  !!   red<<setprecision(20)<<dat<<endl;
  !!   red.close();  
  !!   ivector idx1(1,nstep);
  !!   idx1=genFirstIdx(column(dat,12),nstep);
  !!   ivector idx2(1,nstep);
  !!   idx2=genLastIdx(column(dat,12),nstep);
  !!   //cout<<idx1<<endl<<endl<<idx2<<endl<<endl;
  !!   ofstream out("ukf.dat");
  !!   out<<"# midLon"<<endl<<midLon<<endl;
  !!   out<<"# fixFirst"<<endl<<fixFirst(1,2)<<" "<<setprecision(20)<<fixFirstJDU<<endl;
  !!   out<<"# fixFirstVar"<<endl<<fixFirstVar<<endl;
  !!   out<<setprecision(9);
  !!   out<<"# fixLast"<<endl<<fixLast(1,2)<<" "<<setprecision(20)<<fixLastJDU<<endl;
  !!   out<<"# fixLastVar"<<endl<<fixLastVar<<endl;
  !!   out<<"# nrow"<<endl<<nrow<<endl;
  !!   out<<"# ncol"<<endl<<ncol+1<<endl;
  !!   out<<"# nstep"<<endl<<nstep<<endl;
  !!   out<<"# idx1"<<endl<<idx1<<endl;
  !!   out<<"# idx2"<<endl<<idx2<<endl;
  !!   dmatrix obsMat(1,nrow,1,ncol+1);
  !!   obsMat.initialize();
  !!   obsMat.colfill(1,column(dat,0));
  !!   obsMat.colfill(2,column(dat,1));
  !!   obsMat.colfill(3,column(dat,2));
  !!   obsMat.colfill(4,column(dat,11));
  !!   obsMat.colfill(5,column(dat,3));
  //  .........//
  !!   obsMat.colfill(7,lc(1));
  !!   obsMat.colfill(8,lc(2));
  !!   obsMat.colfill(9,lc(3));
  //  .........//
  !!   obsMat.colfill(10,obs);
  !!   obsMat.colfill(11,column(dat,10));
  !!   obsMat.colfill(12,column(dat,12));
  !!   for(int i=1; i<=nrow; ++i){
  !!     obsMat(i,13)=sunEvent((int)dat(i,12),1);
  !!     obsMat(i,6)=(solarCoordinates(obsMat(i,1)))(2);
  !!   }
  !!   out<<"# obsMat"<<endl<<setprecision(20)<<obsMat<<endl;
  !!   out.close();  
  !! }

PARAMETER_SECTION
  init_number dummy;
  objective_function_value nLogL;

PROCEDURE_SECTION
  exit(0);
  nLogL=square(dummy);

REPORT_SECTION 
  ofstream clog("check.log");
  clog<<"Check the JDE function"<<endl;
  dvector y("{2000,1999,1987,1987,1988,1988,1900,1600,\
              1600,837,-123,-122,-1000,-1000,-1001,-4712}");
  dvector m("{1,1,1,6,1,6,1,1,12,4,12,1,7,2,8,1}");
  dvector d("{1.5,1,27,19.5,27,19.5,1,1,31,10.3,31,1,12.5,29,17.9,1.5}");
  dvector r("{2451545,2451179.5,2446822.5,2446966,2447187.5,2447332,2415020.5,2305447.5,\
              2305812.5,2026871.8,1676496.5,1676497.5,1356001,1355866.5,1355671.4,0}");
  clog<<JDE(y,m,d)-r<<endl<<endl;
 
  clog<<"Check binSearch"<<endl;
  dvector tab("{0,2,3,4,5,7}");
  clog<<binSearch(0,tab)-1<<endl;
  clog<<binSearch(.5,tab)-1<<endl;
  clog<<binSearch(1,tab)-1<<endl;
  clog<<binSearch(2,tab)-2<<endl;
  clog<<binSearch(2.5,tab)-2<<endl;
  clog<<binSearch(6.999,tab)-5<<endl<<endl;
  clog.close();

  

