// Anders Nielsen <anders.nielsen@hawaii.edu> Dec 2005 

#ifndef __basics_h__
#define __basics_h__

#include <fvar.hpp>
#include <math.h>

  dvar_matrix asRow(const dvar_vector& x){
    RETURN_ARRAYS_INCREMENT();	
    dvar_matrix ret(1,1,x.indexmin(),x.indexmax());
    ret(1)=x;	
    RETURN_ARRAYS_DECREMENT();
    return ret;
  }

  dvar_matrix asCol(const dvar_vector& x){
    RETURN_ARRAYS_INCREMENT();	
    dvar_matrix ret=trans(asRow(x));
    RETURN_ARRAYS_DECREMENT();
    return ret;
  }

  dmatrix getSubMat(const dmatrix& X, const int i1, const int i2){
    dmatrix ret(i1,i2,X.colmin(),X.colmax());
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }

  dvar_matrix getSubMat(const dvar_matrix& X, const int i1, const int i2){
    RETURN_ARRAYS_INCREMENT();	
    dvar_matrix ret(i1,i2,X.colmin(),X.colmax());
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    RETURN_ARRAYS_DECREMENT();
    return ret;
  }

  dvector getSubVec(const dvector& X, const int i1, const int i2){
    dvector ret(i1,i2);
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }

  dvar_vector getSubVec(const dvar_vector& X, const int i1, const int i2){
    RETURN_ARRAYS_INCREMENT();	
    dvar_vector ret(i1,i2);
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    RETURN_ARRAYS_DECREMENT();
    return ret;
  }

  dvar_vector weightedVectorMean(const dvar_matrix& X, const dvector& W){
    RETURN_ARRAYS_INCREMENT();	
    dvar_vector ret=W(0)*X(0);
    int n=W.indexmax();
    for(int i=1; i<=n; ++i)ret+=W(i)*X(i);
    RETURN_ARRAYS_DECREMENT();
    return ret;
  } 

  dvar_matrix weightedCovEst(const dvar_matrix& Xp, const dvar_vector& mu, const dvector& W){
    RETURN_ARRAYS_INCREMENT(); 
    dvar_matrix diff=asRow(Xp(0)-mu);
    dvar_matrix ret=W(0)*trans(diff)*diff;
    int n=W.indexmax();
    for(int i=1; i<=n; ++i){
      diff=asRow(Xp(i)-mu);
      ret+=W(i)*trans(diff)*diff;
    }
    RETURN_ARRAYS_DECREMENT();
    return ret;
  } 

  dvar_matrix weightedCov2Est(const dvar_matrix& Xp, const dvar_vector& mux, 
                              const dvar_matrix& Yp, const dvar_vector& muy, const dvector& W){
    RETURN_ARRAYS_INCREMENT();	
    dvar_matrix ret=W(0)*asCol(Xp(0)-mux)*asRow(Yp(0)-muy);
    int n=W.indexmax();
    for(int i=1; i<=n; ++i){
      ret+=W(i)*asCol(Xp(i)-mux)*asRow(Yp(i)-muy);
    }
    RETURN_ARRAYS_DECREMENT();
    return ret;
  }

  //dvar_matrix chol(const dvar_matrix& X){
    //ofstream xout("X.out");
    //xout<<X;
    //xout.close();
  //  return choleski_decomp(X);
  //}

  dvar_matrix sigmaPoints(const dvar_vector& mu, const dvar_matrix& V, const double& kappa){
    RETURN_ARRAYS_INCREMENT();	
    int stateDim=mu.indexmax()-mu.indexmin()+1;
    dvar_matrix ret(0,2*stateDim,mu.indexmin(),mu.indexmax());
    dvar_matrix T=trans(chol((stateDim+kappa)*V));
    //dvar_matrix T=trans(choleski_decomp((stateDim+kappa)*V));
    ret(0)=mu;
    for(int i=1; i<=stateDim; ++i){
      ret(i)=mu-T(i);
      ret(stateDim+i)=mu+T(i);
    }	
    RETURN_ARRAYS_DECREMENT();
    return ret;
  }


#endif



