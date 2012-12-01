#ifndef __quadmap_h__
#define __quadmap_h__

#include <stdlib.h>
#ifdef _MSC_VER
  #define _USE_MATH_DEFINES
#endif
#include <cmath>
#include <iostream>
#include <list>
#include <vector>
#include <algorithm>
#include <stdexcept>

using namespace std;

/**Maximal length of elems*/
const int MAX_OBJECT = 12;
/** Maximal depth of tree*/
const int MAX_DEPTH = 20;
const double G2R=2.0*M_PI/360.0;
const double k1dist=3440.06479*M_PI/(G2R)/180.0; 

/**
 * A located object is an object together with a point (x,y) in the
 * plane at which the value (v) is located.
 * author Peter Sestoft 
 * author Anders Nielsen (anielsen@dina.kvl.dk) (minor modifications only)   
 * version 0.1, 2002-06-03 
 * Modified for C++ by Anders Nielsen (anders.nielsen@hawaii.edu) and  
 * Johnoel Ancheta (johnoel@hawaii.edu) 2006-06-29  
 */
class Located 
{
public:
double x, y, v;

Located();

Located(const Located& loc) {
	x =  loc.x;
	y =  loc.y;
	v =  loc.v;
}

Located(double xx, double yy, double vv) {
	x = xx;
	y = yy;
	v = vv;
}

virtual ~Located() {;}

Located operator=(const Located& loc){
  x=loc.x;
  y=loc.y;
  v=loc.v;
  return *this;
}

bool within(double x0, double y0, double w, double h){
    return within(x, y, x0, y0, w, h);
}

bool within(double x, double y, double x0, double y0, double w, double h){
    return x0 <= x && y0 <= y && x < x0+w && y < y0+h ? true : false;
}

double dist(double x2, double y2){
	return dist(x, y, x2, y2);
}

double dist(double x1, double y1, double x2, double y2){
   double cosd=sin(y1*G2R)*sin(y2*G2R)+cos(y1*G2R)*cos(y2*G2R)*cos((x1-x2)*G2R);
   double s=k1dist*acos(cosd);
   return s;                                                                                                              
   //return sqrt(sqr(x1-x2) + sqr(y1-y2));
}

double sqr(double x){
	return x * x;
}

double getObject(){
	return v;
}
};

/**
 * An abstract class which can be either an undivided
 * rectangle QMR, or a subdivided rectangle QM4.
 * @author Peter Sestoft
 * @author Anders Nielsen (anielsen@dina.kvl.dk) (minor modifications only)
 * @version 0.1, 2002-06-03
 * Modified for C++ by Anders Nielsen (anders.nielsen@hawaii.edu) and  
 * Johnoel Ancheta (johnoel@hawaii.edu) 2006-06-29    
 */
class QMNode {
public:

QMNode(){;}

virtual ~QMNode(){;}

virtual bool empty() const = 0;

virtual QMNode* add(double qx0, double qy0, double qw, double qh, int depth, const Located& loc) = 0;

virtual void app(double qx0, double qy0, double qw, double qh, double x, double y, double d, list<Located>& qmapp) = 0;

virtual void app(double qx0, double qy0, double qw, double qh, double x, double y, double w, double h, list<Located>& qmapp) = 0;

/**Add a located object to qmn, return possibly updated node*/
static QMNode* add(QMNode& qmn, double &qx0, double &qy0, double &qw, double &qh, int depth, const Located& loc);
};

// Qmap tree node representing an undivided rectangle -----------------
/**
 * An object consisting of a linked list of Located objects
 * within this undivided sub--rectangle.
 * author Peter Sestoft
 * author Anders Nielsen (anielsen@dina.kvl.dk) (minor modifications only)
 * version 0.1, 2002-06-03
 * Modified for C++ by Anders Nielsen (anders.nielsen@hawaii.edu) and  
 * Johnoel Ancheta (johnoel@hawaii.edu) 2006-06-29    
 */
class QMR: public QMNode {
public: 

list<Located> elems;

QMR() {
    list<Located> _elems;
    elems=_elems;
}

QMR(const Located& loc) {
    list<Located> _elems;
    _elems.push_back(loc);
    elems=_elems;
}

QMR(const QMR& qmr){
  elems = qmr.elems;
}

virtual ~QMR() {;}

QMR operator=(const QMR& qmr){
  list<Located> el; 
  list<Located>::const_iterator pos;
  for(pos=qmr.elems.begin(); pos!=qmr.elems.end(); ++pos){
    Located loc = *pos;
    el.push_back(loc);    
  }
  elems=el;
  return *this;
} 

QMNode* add(double qx0, double qy0, double qw, double qh, int depth, const Located& loc);

bool empty() const {
    return elems.empty() ? true : false; 
}

void app(double qx0, double qy0, double qw, double qh, double x, double y, double d, list<Located>& qmapp) {
	list<Located>::const_iterator pos;
	for(pos=elems.begin(); pos!=elems.end(); ++pos){
		Located loc = *pos;
		if (loc.dist(x, y) <= d)
			qmapp.push_back(loc);
	}
}

void app(double qx0, double qy0, double qw, double qh, double x0, double y0, double w, double h, list<Located>& qmapp) {
	list<Located>::const_iterator pos;      
	for(pos=elems.begin(); pos!=elems.end(); ++pos){
		Located loc = (Located)*pos;
		if (loc.within(x0, y0, w, h))
			qmapp.push_back(loc);
	}
}

};


// Qmap tree node representing a subdivided rectangle -----------------
/**
 * An object consisting of four references to QMNotes, each
 * representing a quarter of the rectangle. Some of these may be NULL if
 * the corresponding sub--rectangle is empty. The naming conventions within
 * this class can be confusing, as the sub--rectangles are named
 * NW, NE, SE and SW with the convention that SE
 * and SW cover the highest y coordinates. This convention is only
 * used locally in the Quadmap structure.
 * @author Peter Sestoft
 * @author Anders Nielsen (anielsen@dina.kvl.dk) (minor modifications only)
 * @version 0.1, 2002-06-03
 * Modified for C++ by Anders Nielsen (anders.nielsen@hawaii.edu) and  
 * Johnoel Ancheta (johnoel@hawaii.edu) 2006-06-29    
 */
class QM4: public QMNode {
public: 
QMNode* ne;
QMNode* nw;
QMNode* sw;
QMNode* se;

QM4() {
	ne = new QMR();
	nw = new QMR();
	sw = new QMR();
	se = new QMR();
}

QM4(const QM4& qm4){
  ne = qm4.ne;
  nw = qm4.nw;
  sw = qm4.sw;
  se = qm4.se;
}

virtual ~QM4() {
	if (ne != 0) {
		delete ne;
		ne = 0;
	}
	if (nw != 0) {
		delete nw;
		nw = 0;
	}
	if (sw != 0) {
		delete sw;
		sw = 0;
	}
	if (se != 0) {
		delete se;
		se = 0;
	}
}

QM4 operator=(const QM4& qm4){
  ne = qm4.ne;
  nw = qm4.nw;
  sw = qm4.sw;
  se = qm4.se;
  return *this;
}

QMNode* add(double qx0, double qy0, double qw, double qh, int depth, const Located& loc);


bool empty() const {
	return ne->empty() && nw->empty() && sw->empty() && se->empty() ? true : false; 
}

void app(double qx0, double qy0, double qw, double qh, double x, double y, double d, list<Located>& qmapp) {
	double qw2 = qw/2, qh2 = qh/2;
	double qxm = qx0+qw2, qym = qy0 + qh2; // center of rectangle is (qxm,qym)
	// (W, E, N, S) = overlaps with (West, East, North, South)
	bool W = x-d < qxm;
	bool E = x+d >= qxm;
	bool N = y-d < qym;
	bool S = y+d >= qym;
	if ( (!nw->empty()) && N && W)
		nw->app(qx0, qy0, qw2, qh2, x, y, d, qmapp);
	if ( (!ne->empty()) && N && E)
		ne->app(qxm, qy0, qw2, qh2, x, y, d, qmapp);
	if ( (!sw->empty()) && S && W)
		sw->app(qx0, qym, qw2, qh2, x, y, d, qmapp);
	if ( (!se->empty()) && S && E)
		se->app(qxm, qym, qw2, qh2, x, y, d, qmapp);
}

void app(double qx0, double qy0, double qw, double qh, double x0, double y0, double w, double h, list<Located>& qmapp) {
	double qw2 = qw/2, qh2 = qh/2;
	double qxm = qx0+qw2, qym = qy0 + qh2; // center of rectangle is (qxm,qym)
	// (W, E, N, S) = overlaps with (West, East, North, South)
	bool W = x0 < qxm;
	bool E = x0+w >= qxm;
	bool N = y0 < qym;
	bool S = y0+h >= qym;
	if ((!nw->empty()) && N && W)
		nw->app(qx0, qy0, qw2, qh2, x0, y0, w, h, qmapp);
	if ((!ne->empty()) && N && E)
		ne->app(qxm, qy0, qw2, qh2, x0, y0, w, h, qmapp);
	if ((!sw->empty()) && S && W)
		sw->app(qx0, qym, qw2, qh2, x0, y0, w, h, qmapp);
	if ((!se->empty()) && S && E)
		se->app(qxm, qym, qw2, qh2, x0, y0, w, h, qmapp);
}
};

// Quadmaps -----------------------------------------------------------
/**
 * A quadmap is a map from points (x, y) in an axis-parallel plane
 * rectangle to zero or more objects located at or near that point.
 * The time to lookup the objects at or near a given point (x,y) is
 * O(max(log N), R) where N is the total number of objects and R is
 * the number or objects returned by the query.
 * author Peter Sestoft 
 * author Anders Nielsen (anielsen@dina.kvl.dk) (minor modifications only)   
 * version 0.1, 2002-06-03 
 * Modified for C++ by Anders Nielsen (anders.nielsen@hawaii.edu) and  
 * Johnoel Ancheta (johnoel@hawaii.edu) 2006-06-29     
 */
class Quadmap {
public: 
QMNode* qmn;
double qx0, qy0, qw, qh;

Quadmap(double _qx0, double _qy0, double _qw, double _qh, QMNode* _qmn) {
	if (_qw >= 0 && _qh >= 0) {
		qx0 = _qx0; qy0 = _qy0; qw = _qw; qh = _qh; qmn = _qmn;
	} else {
        	throw runtime_error("Quadmap: qw or qh is negative");
	}
}

Quadmap(double _qx0, double _qy0, double _qw, double _qh){
	if (_qw >= 0 && _qh >= 0) {
		qx0 = _qx0; qy0 = _qy0; qw = _qw; qh = _qh; qmn = new QMR();
	} else {
        	throw runtime_error("Quadmap: qw or qh is negative");
	}  
}

Quadmap ();

Quadmap (const Quadmap& qm) {
	qmn =  qm.qmn;
	qx0 =  qm.qx0;
	qy0 =  qm.qy0;
	qw =  qm.qw;
	qh =  qm.qh;
}

virtual ~Quadmap() {
	if (qmn != 0) {
		delete qmn;
		qmn = 0;
	}
}
 
void add(Located& loc);

void add(double x, double y, double o) {
	Located tmp(x, y, o);
	add(tmp);
}

void app(list<Located>& qmapp) {
	app(qx0, qy0, qw, qh, qmapp);
}

void app(double x0, double y0, double w, double h, list<Located>& qmapp) {
	if (qmn->empty() == false)
		qmn->app(qx0, qy0, qw, qh, x0, y0, w, h, qmapp);
}

void app(double x0, double y0, double d, list<Located>& qmapp) {
	if (qmn->empty() == false)
		qmn->app(qx0, qy0, qw, qh, x0, y0, d, qmapp);
}
  
/** Find all objects in the quadmap (could optimize)*/
list<Located> get() {
	list<Located> res;
	app(qx0, qy0, qw, qh, res);
	return res;
}

/** Find all objects whose Euclidean distance from (x, y) is at most d*/
list<Located> get(double x, double y, double d) {
	list<Located> res;
	app(x, y, d, res);
	return res;
}

/** Find objects within rectangle (x0, y0) + (w, h)*/
list<Located> get(double x0, double y0, double w, double h) {
	list<Located> res;
	app(x0, y0, w, h, res);
	return res;
}

};

QMNode* QM4::add(double qx0, double qy0, double qw, double qh, int depth, const Located& loc) {
    double qw2 = qw/2, qh2 = qh/2;
    double qxm = qx0+qw2, qym = qy0 + qh2; // center of rectangle is (qxm,qym)
    if (loc.y < qym){
      if (loc.x < qxm){  // NW
        nw = QMNode::add(*nw, qx0, qy0, qw2, qh2, depth+1, loc);
      }else{             // NE
        ne = QMNode::add(*ne, qxm, qy0, qw2, qh2, depth+1, loc);
      }
    }else{
      if (loc.x < qxm){  // SW
        sw = QMNode::add(*sw, qx0, qym, qw2, qh2, depth+1, loc);
      }else{             // SE
        se = QMNode::add(*se, qxm, qym, qw2, qh2, depth+1, loc);
      }
    }
     
    return this;
}

void Quadmap::add(Located& loc){
    if((loc.x>=qx0)&&(loc.x<=(qx0+qw))&&(loc.y>qy0)&&(loc.y<(qy0+qh))){
      qmn = QMNode::add(*qmn, qx0, qy0, qw, qh, 0, loc);
    }else{
      cout<<"Quadmap: Individual outside map"<<endl;
    }
}

QMNode* QMNode::add(QMNode& qmn, double &qx0, double &qy0, double &qw, double &qh, int depth, const Located& loc) {
	if (qmn.empty()){
		return new QMR(loc);
	} else {
		return qmn.add(qx0, qy0, qw, qh, depth, loc);
	}
}

QMNode* QMR::add(double qx0, double qy0, double qw, double qh, int depth, const Located& loc) {
	elems.push_back(loc);
	if (elems.size() > MAX_OBJECT && depth < MAX_DEPTH) { // Subdivide
		QM4 *qt4 = new QM4();
		for(list<Located>::const_iterator pos = elems.begin(); pos != elems.end(); ++pos) {
			qt4->add(qx0, qy0, qw, qh, depth, *pos);
		}
		return qt4;
	} else {
		return this;
	}
}

//class QMM {
//public:
//  std::vector< Quadmap > qmv; 
//  std::vector< double > timev; 
//  Quadmap qm; 
//
//QMM() {
//    cout<<"ok"<<endl;
//    std::vector< Quadmap > _qmv;
//    std::vector< double > _timev;
//    Quadmap _qm(-.01,-90.01,360.02,180.02); 
//    qmv=_qmv;
//    timev=_timev;
//    qm=_qm; 
//}
//
//
//QMM(const QMM& qmm) {
//	qmv = qmm.qmv;
//	timev = qmm.timev;
//        qm=qmm.qm;
//}
//
//virtual ~QMM() {;}
//
//void newqm(double t){
//  Quadmap *qm= new Quadmap(-.01,-90.01,360.02,180.02);
//  qmv.push_back(*qm);
//  timev.push_back(t);
//  cout<<qmv.size()<<endl;
//}

//void addpoint(double x, double y, double v){
//  qm.add(x,y,v);
//}

//void addqm(double time){
//  timev.push_back(time);
//  qmv.push_back(qm);
//  qm = *new Quadmap(-.01,-90.01,360.02,180.02);
//} 
//};



#endif
