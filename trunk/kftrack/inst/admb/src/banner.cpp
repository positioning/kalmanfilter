#include <strstream>
using std::ostrstream;
#include <fvar.hpp>
#include <iomanip>
#include <adstring.hpp>

extern adstring copyright;

adstring make_banner(void)
{
  adstring program_name("State space Kalman filter track estimator\n"\
                        "kftrack version 1.2 ");

  int version = 0;
  adstring banner(1,500);
  banner = program_name + "(";
#if defined(__BCPLUSPLUS__)
  banner += "Borland C++";
  version = __BORLANDC__;
#elif defined (_MSC_VER)
  banner += "Microsoft Visual C++";
  version = _MSC_VER;
#elif defined (__SC__)
  banner += "Symantec C++";
  version = __SC__;
#elif defined (__ZTC__)
  banner += "Zortech C++";
  version = __ZTC__;
#elif defined(__CYGWIN32__)
  banner += "Gnu C++ for Cygwin";
  version = __GNUC__;
#elif defined(__GNUC__)
  banner += "Gnu C++";
  version = __GNUC__;
#elif defined(sun)
  banner += "Sun C++";
  #ifdef __SUNPRO_CC
    version = __SUNPRO_CC;
  #endif
#elif defined(__NDPX__)
  #ifdef __i860
    banner += "(NDP i860 C++ version)";
  #else
    banner += "(NDP C++ version)";
  #endif
#else
  banner += "Unknown Compiler";
#endif
  if (version != 0)
  {
    char s[80];
    ostrstream ss(s,80);
    ss << std::hex << version << '\0';
    banner += ", v" + adstring(s);
  }

#if defined (__ZTC__) || defined (__SC__)
  #ifdef DOS386
    banner += ", protected mode";
  #endif
#endif

  banner += ")";
  banner += copyright; 

  return(banner);
}

/*
int main(void)
{
   cout << make_banner(__FILE__) << endl;
   return(0);
}
*/
