#include "yrmonday.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
//#include <prnstrem.h>
#include <strstream>
using std::ostrstream;
using std::istrstream;
#include <iomanip>
using std::cerr;
using std::cout;
using std::endl;
using std::setw;
using std::setfill;
using std::ostream;
using std::istream;

#ifndef FALSE
  #define FALSE false
#endif
#ifndef TRUE
  #define TRUE true
#endif
//#include "trace.h"

int year_month_day::month_length[14] =

{
  0, 31, 28, 31, 30, 31, 30,
  31, 31, 30, 31, 30, 31, 0
};

char year_month_day::delimiter = '/';

char set_year_month_day_delimiter(const char c)
{
  char old_delimiter = year_month_day::delimiter;
  year_month_day::delimiter=c;
  return(old_delimiter);
}

year_month_day::year_month_day(const int _year, const int _month, const int _day)
{
  set(_year, _month, _day);
}

year_month_day::year_month_day(void)
{
  set(0,1,1);
}

year_month_day::year_month_day(const long _gdn)
{
  m_gdn = _gdn;
  to_date('G');
}

int year_month_day::is_leap_year(const long _year, const char _calendar)
{
  char calendar = (char)toupper((int)_calendar);
  long y = _year;
  if (y < 1900)
    y+= 1900;

  if ( y%4 )
    return ( false );
  else
  {
    if ( calendar == 'J' )
      return ( true );
    else
      return ( ( y%100 != 0L || y%400 == 0L ) ? true : false);
  }
}

int year_month_day::set_feb_length(const long yr, const char calendar)
{
  int t = month_length[2];
  month_length[2] = 28 + is_leap_year(yr,calendar) ;
  return(t);
}

int year_month_day::reset_feb_length(void)
{
  int t = month_length[2];
  month_length[2] = 28;
  return(t);
}

long year_month_day::lfloor(const long a, long b)
{
  return ( a >= 0L ? a/b : ( a%b == 0L ) - 1 - labs(a)/b );
}

void year_month_day::to_gdn(const char _calendar)
{
  int day = m_day;
  int month = m_month;
  long year = m_year;
  long gdn = 0L;
  char calendar = (char)toupper((int)_calendar);
  set_feb_length(year,calendar);
  if ( month < 1  || month > 12 ||
      day < 1  || day > month_length[month] )
  {
    valid = false;
  }

  else
  {
    gdn = (year-1)*365 + lfloor(year-1,4L);
    if ( calendar == 'G' )
      gdn += lfloor(year-1,400L) - lfloor(year-1,100L);

    while (--month)
      gdn += month_length[month];

    gdn += day - 577736L - 2*(calendar=='J');
    m_gdn = gdn;
    valid = true;
  }

  reset_feb_length();
}

void year_month_day::to_date(const char _calendar)
{
  int month, i, exception;
  long year, gdn, y4, y100, y400;
  char calendar = (char)toupper((int)_calendar);
  gdn = m_gdn;
  gdn += 577735L + 2*(calendar=='J');
  y400 = 146100L - 3*(calendar=='G');
  y100 =  36525L -   (calendar=='G');
  y4   =   1461L;
  exception = false;
  year = 400*lfloor(gdn,y400);
  gdn -= y400*lfloor(gdn,y400);
  if ( gdn > 0L )
  {
    year += 100*lfloor(gdn,y100);
    gdn -= y100*lfloor(gdn,y100);
    exception = ( gdn == 0L && calendar == 'G' );
    if ( gdn > 0L )
    {
      year += 4*lfloor(gdn,y4);
      gdn -= y4*lfloor(gdn,y4);
      if ( gdn > 0L )
      {
        i = 0;
        while ( gdn > 365 && ++i < 4 )
        {
          year++;
          gdn -= 365L;
        }
      }
    }
  }
  if ( exception )
    gdn = 366L;
  else
  {
    year++;
    gdn++;
  }
  set_feb_length(year,calendar);
  month = 1;
  while ( month < 13 && gdn > month_length[month] )
    gdn -= month_length[month++];

  if ( month == 13 )
  {
    month = 1;
    year++;
  }
  days_in_month = month_length[month];
  reset_feb_length();
  m_day = (int)gdn;
  m_month = month;
  m_year = year;
  valid = true;
}

void year_month_day::set(const int _year, const int _month, const int _day)
{
  m_year = _year;
  m_month = _month;
  m_day = _day;
  to_gdn();
  if (!valid)
    cout << " " << *this << " not valid" << endl;
}

int year_month_day::operator == (const year_month_day& t)
{
  return (m_gdn==t.m_gdn);
}

int year_month_day::operator <= (const year_month_day& t)
{
  return (m_gdn <= t.m_gdn);
}

int year_month_day::operator < (const year_month_day& t)
{
  return (m_gdn < t.m_gdn);
}

int year_month_day::operator > (const year_month_day& t)
{
  return (m_gdn > t.m_gdn);
}

int year_month_day::operator >= (const year_month_day& t)
{
  return (m_gdn >= t.m_gdn);
}

year_month_day year_month_day::operator = (const year_month_day& t)
{
  m_year = t.m_year;
  m_month = t.m_month;
  m_day = t.m_day;
  m_gdn = t.m_gdn;
  days_in_month= t.days_in_month;
  valid = t.valid;
  return(*this);
}

int year_month_day::add(const int t)
{
  m_gdn += t;
  to_date();
  return(valid);
}

int year_month_day::subtract(const int t)
{
  m_gdn -= t;
  to_date();
  return(valid);
}

year_month_day year_month_day::operator ++ (void)
{
  add(1);
  return(*this);
}

year_month_day year_month_day::operator -- (void)
{
  subtract(1);
  return(*this);
}

year_month_day year_month_day::operator += (const int t)
{
  add(t);
  return(*this);
}

year_month_day year_month_day::operator -= (const int t)
{
  subtract(t);
  return(*this);
}

year_month_day year_month_day::operator + (const int t)
{
  add(t);
  return(*this);
}

year_month_day year_month_day::operator - (int t)
{
  subtract(t);
  return(*this);
}

void year_month_day::month_increment(void)
{
  //clogf << "line " << 308 << ", file " << "yrmonday.cpp" << ", " << "m_gdn" " = " << m_gdn<< ", " << "*this" " = " << *this << endl;

  m_gdn += days_in_month;
  to_gdn();
  //clogf << "line " << 311 << ", file " << "yrmonday.cpp" << ", " << "m_gdn" " = " << m_gdn<< ", " << "*this" " = " << *this << endl;

}

void year_month_day::month_decrement(void)
{
  //clogf << "line " << 316 << ", file " << "yrmonday.cpp" << ", " << "m_gdn" " = " << m_gdn<< ", " << "*this" " = " << *this << endl;

  m_gdn -= days_in_month;

  to_gdn();

  //clogf << "line " << 319 << ", file " << "yrmonday.cpp" << ", " << "m_gdn" " = " << m_gdn<< ", " << "*this" " = " << *this << endl;

}

istream& operator>>(istream& istr, year_month_day& t)
  {
    const int buf_len = 31;
    char buf[buf_len];
    memset(buf,'\0',buf_len);
    istr >> buf;
    if (istr)
    {
      int l = strlen(buf);
      int delimiter_count = 0;
      for (int i = 0; i < l; i++)
      {
        if (buf[i] == t.delimiter)
        {
          delimiter_count ++;
          buf[i] = ' ';
          if (buf[i+1] == '0')
          {
            buf[i+1] = ' ';
          }
        }
      }
      buf[l+1] = '\0';
      istrstream ss(buf,l);
      int yr = 0;
      int mon = 0;
      int da = 0;
      if (delimiter_count == 2)
      {
        ss >> yr >> mon >> da;
      }
      else if (delimiter_count == 1)
      {
        ss >> yr >> mon;
        da = 1;
      }
      else
      {
        cerr << "Illegal number of delimiters in date field." << endl;
        cerr << "Deliminter count = " << delimiter_count << endl;
        cerr << "date field is (delimiters set to ' ') '" << buf << "'" << endl;
      }
      if (!ss)
      {
        cerr << "error in istrstream" << endl;
        cerr << "buf = '" << buf  << "'" << endl;
        cerr << "l = " << l << endl;
      }
      if ((mon < 1) || (mon > 12) )
      {
        cerr << "month value out of range in istream& operator>>(istream& istr, year_month_day& t)\n"
        << "valid values are between 1 and 12; you had " << mon << endl;
        cerr << "buf = '" << buf  << "'" << endl;
        cerr << "l = " << l << endl;
        exit(1);
      }
      t.set(yr,mon,da);
    }
    return(istr);
  }

void year_month_day::to_string(char* buf) const
{
  char tmp[31];
  ostrstream s(tmp,30);
  s << setw(4) << m_year<< delimiter << setw(2) << setfill('0') << m_month
  << delimiter << setw(2) << setfill('0') << m_day
  << '\0';
  strcpy(buf,tmp);
}

ostream& operator << (ostream& ostr, const year_month_day& t)
  {
    char buf[31];
    t.to_string(buf);
    ostr << buf;
    return(ostr);
  }

//prnstream& operator << (prnstream& pstr, const year_month_day& t)
//  {
//    char buf[31];
//    t.to_string(buf);
//    pstr << buf;
//    return(pstr);
//  }

int operator - (const year_month_day& t2, const year_month_day& t1)
  {
    int diff = 0x7fff;
    if ((t1.m_year > 0) && (t2.m_year > 0))
    {
      diff = t2.m_gdn-t1.m_gdn;
    }
    return(diff);
  }

year_month_day_vector::year_month_day_vector(const int n1, const int n2)
{
  v = (year_month_day_vrep*) malloc( (size_t)sizeof(year_month_day_vrep) );
  v->en1 = n1;
  v->en2 = n2;
  v->nsize = n2-n1+1;
  v->refcnt = 1;
  allocate ();
}

void year_month_day_vector::allocate(const int n1, const int n2)
{
  v = (year_month_day_vrep*) malloc( (size_t)sizeof(year_month_day_vrep) );
  v->en1 = n1;
  v->en2 = n2;
  v->nsize = n2-n1+1;
  v->refcnt = 1;
  allocate ();
}
void year_month_day_vector::allocate(void)
{
  v->p = (year_month_day_*) malloc( (size_t)v->nsize*sizeof(year_month_day_) );
  if (!v->p)
  {
    cerr << "New failed in vector allocation.\n";
    exit(1);
  }
}

year_month_day_vector::~year_month_day_vector()
{
  if (--v->refcnt==0)
  {
    free(v->p);
    free(v);
  }
}

#ifndef OPT_LIB
year_month_day_& year_month_day_vector::operator()(const int i)
{
  if ( (i < v->en1) || (i > v->en2) )
  {
    cerr << "subscript out of range in year_month_day_vector::operator()(const int i)" << endl;
    cerr << "i = "<< i << ", n1 = " << v->en1 <<", n2 = "<<v->en2<< endl;
    exit(1);
  } 
  return v->p[i-v->en1];
}

year_month_day_& year_month_day_vector::operator[](const int i)
{
  if ( (i < v->en1) || (i > v->en2) )
  {
    cerr << "subscript out of range in year_month_day_vector::operator[](const int i)" << endl;
    cerr << "i = "<< i << ", n1 = " << v->en1 <<", n2 = "<<v->en2<<"\n";
    exit(1);
  }
  return v->p[i-v->en1];
}
#endif

year_month_day_vector& year_month_day_vector::operator=(year_month_day_vector& x)
{
  if ( (x.v->en1 != v->en1) || (x.v->en2 != v->en2) )
  {
    cerr << "Attempted assignment to vectors of unequal size.\n";
    exit(1);
  }
  x.v->refcnt++;
  v = x.v;
  return *this;
}

#undef _TEST_CODE
#ifdef _TEST_CODE
// stuff for testing classes
int main(void)
{
  year_month_day ym(77,5,1);
  year_month_day test(77,5,1);
  ym -= 1;
  int w = 1;
  for (int i = 1; i <= 20; i++)
  {

     ym += w;
     cout << ym;
     if (ym == test)
       cout << " TRUE " << test;
     else
       cout << " FALSE";

     cout << endl;
  }

  for (int i=1; i <= 22; i++)
  {
     ym -= w;
     cout << ym;
     if (ym == test)
       cout << " TRUE " << test;
     else
       cout << " FALSE";

     cout << endl;
  }

  /*
  year_month_day d;

  while (1)
  {
    cout << "Enter a year_month_day: ";
    cin >> d;

    cout << d.get_year() << "  " << d.get_month_value() << "  " <<d.get_day() << endl;
  }
  */

  /*
  char old_d = set_year_month_day_delimiter(':');
  cout << "previous delimiter was '" << old_d << "'" << endl;

  year_month_day d;
  year_month_day p;

  long_lat x, y;

  double eff, cat;

  ifstream file("test_eff.dat");
  while ( (file) && (d.get_year() < 84) )
  {
    p = d;
    file >> d >> y >> x >> eff >> cat;
    cout << d << "  " << d.get_year() << "  " << d.get_month_value() << "  "
         << y.value() << "  " << x.value() << "  " << eff << "  " << cat << endl;
  }
  old_d = set_year_month_day_delimiter('/');
  cout << "previous delimiter was '" << old_d << "'" << endl;
  cout << d << endl;
  */

  int nday = 368;
  year_month_day_vector ymd(1,nday);
  year_month_day day1(2000,7,1);
  ymd(1) = day1;
  int gday1 = day1.get_gregorian();
  for (int i = 2; i <= nday; i++)
  {
    ymd(i) = ymd(i-1) + 1;
  }

  for (int i = 1; i <= nday+1; i++)
  {
   cout << i << "  " << ymd(i) << "  " << ymd(i).get_gregorian() <<  "  " 
        << (ymd(i).get_gregorian()-day1+1) << endl;
  }
}
#endif

