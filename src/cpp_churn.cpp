#include <Rcpp.h>
using namespace Rcpp;

//' The number of periods since x was positive (churn).
//'
//' @param x input numeric vector
//' @return periods since last positive value
// [[Rcpp::export]]
NumericVector cpp_churn(NumericVector x){
  int rows =  x.size();
  NumericVector ts(rows);
  ts[0] = 0 ; 
  
  for(int i = 0; i < rows; ++i){
    if(i > 0){ 
      if(x[i] == 0){
        if(x[i - 1] == 0){  
          ts[i] = 1 + ts[i - 1]; 
        } else {
          ts[i] = 1; 
        }
      } else {
        ts[i] = 0;
      }
    }
  }
  return ts;
}