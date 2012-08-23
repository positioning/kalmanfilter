#ifndef __year_month_day__
#define __year_month_day__
#include <iostream>
#include <stdlib.h>

//class prnstream ;

class year_month_day
{
private:
  long m_gdn;
  int valid;
  int set_feb_length(const long yr, const char calendar='G');
  int reset_feb_length(void);
  long lfloor(const long a, const long b);

protected:
  static char delimiter;
  static int month_length[14];
  int m_day;
  int m_month;
  long m_year;
  int days_in_month;
  int add(const int t);
  int subtract(const int t);
  void to_string(char* s) const;
  void to_gdn(const char calendar = 'G');
  void to_date(const char calendar = 'G');
  int is_leap_year(const long year, const char calendar='G');
  int day_of_week(void);

public:
  year_month_day(void);
  year_month_day(const int y, const int m, const int d);
  year_month_day(const long _gdn);
  ~year_month_day(void) { }

  void set(const int y, const int m, const int d);

  int get_month(void) const { return (m_month); }
  int get_month_value(void) const { return (m_month); }
  int get_day(void) const { return (m_day); }
  int get_day_value(void) const { return (m_day); }
  int get_year(void) const { return (m_year); }
  long get_gregorian(void) const { return(m_gdn); }
  int get_month_length(void) const { return(days_in_month); }

  year_month_day operator = (const year_month_day&);
  year_month_day operator ++ (void);
  year_month_day operator ++ (int) { return(++*this); }
  year_month_day operator -- (void);
  year_month_day operator -- (int) { return(--*this); }
  year_month_day operator += (const int);
  year_month_day operator + (const int);
  year_month_day operator -= (const int);
  year_month_day operator - (const int);
  void month_decrement(void);
  void month_increment(void);
  int operator == (const year_month_day&);
  int operator <= (const year_month_day&);
  int operator >= (const year_month_day&);
  int operator < (const year_month_day&);
  int operator > (const year_month_day&);

  friend std::istream& operator>>(std::istream& istr, year_month_day& t);
  friend std::ostream& operator<<(std::ostream& ostr, const year_month_day& t);

  //friend prnstream& operator<<(prnstream& ostr, const year_month_day& t);

  friend int operator - (const year_month_day& t1, const year_month_day& t2);
  friend char set_year_month_day_delimiter(const char c);
};

typedef year_month_day year_month_day_;
//typedef year_month_day date;

struct year_month_day_vrep
{
  year_month_day_ * p;
  int en1;
  int en2;
  int nsize;
  int refcnt;
};

class year_month_day_vector
{
  year_month_day_vrep * v;
  void allocate();
public: 
  friend class year_month_day_matrix;
  void allocate(const int n1, const int n2);
  year_month_day_vector(const int n1, const int n2);
  year_month_day_vector(){};
  year_month_day_* base()
  {
    return v->p;
  }
#ifdef OPT_LIB
  year_month_day_& operator[](const int i)
    {return v->p[i-v->en1];}
  year_month_day_& operator()(const int i)
    {return v->p[i-v->en1];}
#else
  year_month_day_& operator[](const int i);
  year_month_day_& operator()(const int i);
#endif
  year_month_day_vector& operator=(year_month_day_vector& x);
  ~year_month_day_vector();
  inline year_month_day_vector(year_month_day_vector& x)
  {
    x.v->refcnt++;
    v = x.v;
  }
};

#endif // __year_month_day__
