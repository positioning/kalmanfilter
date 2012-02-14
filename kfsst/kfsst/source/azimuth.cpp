#include <fvar.hpp>
#include <iostream>
#include <cmath>
  const double eps = 1e-8;  //small number to avoid divide by 0
 
  dvariable azimuth(const dvariable& y, const dvariable& x)
  {
    //dvariable z = 180.0*atan(y/(x+eps))/M_PI;
    //dvariable d = 90.0 - z;
    dvariable d = 90.0 - 180.0*atan(y/(x+eps))/M_PI;
    if (x < 0.0)
      d += 180;
    if (d < 0.0)
      d+=360.0;

    return(d);
  }

  double azimuth(const double& y, const double& x)
  {
    //double z = 180.0*atan(y/(x+eps))/M_PI;
    //double d = 90.0 - z;
    double d = 90.0 - 180.0*atan(y/(x+eps))/M_PI;
    if (x < 0.0)
      d += 180;
    if (d < 0.0)
      d+=360.0;
    return(d);
  }

  dvariable gc_dist(const dvector& y1, const dvar_vector y2)
  {
    double G1r = y1(1)/180.0*M_PI+1e-8;
    double L1r = y1(2)/180.0*M_PI+1e-8;

    dvariable G2r = y2(1)/180.0*M_PI+1e-8;
    dvariable L2r = y2(2)/180.0*M_PI+1e-8;

    dvariable DGr = (y2(1)-y1(1))/180.0*M_PI;
  //double DGC1 = 1.852 * 60 * acos(sin(L1r)*sin(L2r) + cos(L1r)*cos(L2r)*cos(DGr))*180/M_PI;
    dvariable DGC1 = 60 * acos(sin(L1r)*sin(L2r) + cos(L1r)*cos(L2r)*cos(DGr))*180/M_PI;
    return(DGC1);
  }

//#define TEST_CODE
#ifdef TEST_CODE
int main(void)
{
  gradient_structure gs;
  double u,v;
  double L1,L2,G1,G2;
  dvariable du, dv;
  dvector v1(1,2);
  dvar_vector v2(1,2);
  while (1)
  {
    /*
    cout << "\nEnter u,v: ";
    cin >> u >> v;
    double chdg = azimuth(v,u);
    cout << "cHeading = " << chdg << endl;

    du = u;
    dv = v;
    dvariable dhdg = azimuth(dv,du);
    cout << "dHeading = " << dhdg << endl;
    */
    cout << "\nEnter L1 G1: ";
    cin  >> L1 >> G1;
    v1(1) = G1;
    v1(2) = L1;
    cout << "Enter L2 G2: ";
    cin  >> L2 >> G2;
    v2(1) = G2;
    v2(2) = L2;
    
    dvariable D = gc_dist(v1,v2);
    cout << "Great circle distance from (" << L1 << "," << G1 << ") to (" << L2 << "," << G2 << ") = " << D << " Nmi" << endl;
    cout << "Great circle distance from (" << L1 << "," << G1 << ") to (" << L2 << "," << G2 << ") = " << (1.852 * D) << " km" << endl;
  }
  exit(0);
}
#endif
#undef TEST_CODE
