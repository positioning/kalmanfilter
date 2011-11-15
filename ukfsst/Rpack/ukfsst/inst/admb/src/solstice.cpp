#include <iostream.h>
#include "yrmonday.h"

int previous_solstice(const int y0, const int m0, const int d0)
{
   int sda = 21;
   int syr,smo = 0;

   if ( (m0 == 12) && (d0 >= 21) )
   {
     syr = y0;
     smo = 12;
   }
   else if (m0 > 6)
   {
     syr = y0;
     smo = 6;
   }
   else if (m0 == 6)
   {
     if (d0 >= 21)
     {
       syr = y0;
       smo = 6;
     }
     else
     {
       syr = y0-1;
       smo = 12;
     }
   }
   else
   {
     syr = y0-1;
     smo = 12;
   }

   year_month_day solstice(syr, smo, sda);

   int gsolstice = solstice.get_gregorian();

   return(gsolstice);
}

int previous_solstice(const year_month_day ymd)
{
  int s = previous_solstice(ymd.get_year(), ymd.get_month(), ymd.get_day());
  return(s);
}

#undef __TEST_CODE__
#ifdef __TEST_CODE__
#include "yrmonday.cpp"

int main(void)
{
   int y,m,d = 0;
   year_month_day ymd;
   while (1)
   {
     cout << "\nEnter year month day: ";
     cin  >> y >> m >> d;
     ymd.set(y,m,d);
     cout << ymd;

     int s = previous_solstice(ymd);
     year_month_day ss(s);
     cout << " Gregorian date of previous soltice: " << s << ": " << ss << endl;
   }
}
#endif // __TEST_CODE__
