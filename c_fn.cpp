#include <Rcpp.h>
#include <numeric>      // std::partial_sum
#include <functional>   // std::multiplies
#include <iostream>


using namespace Rcpp;

// [[Rcpp::export]]
void opt_C(NumericMatrix rev, NumericMatrix opt_L, NumericMatrix opt_F, NumericVector sell_L, NumericVector sell_F, NumericVector fare_L, NumericVector fare_F, double lambda_L, double lambda_F){
  int nrow = rev.nrow();
  int ncol = rev.ncol();
  
  for(int prd = 1; prd < ncol; prd++)
  {
    for(int rem = 1; rem < nrow; rem++)
    {
      double tmp = 0;
      int local = 0;
      int flow = 0;
      double tmp_new;
      
      for(int i = opt_L((rem - 1), prd) - 1; i < 10; i++ )
      {
        for(int j = opt_F((rem - 1), prd) - 1; j < 10; j++ )
        {
          tmp_new = lambda_L*((sell_L[i]*(fare_L[i] + rev((rem-1),(prd-1)) ))+(1-sell_L[i]) * rev(rem,(prd-1)) )+
            lambda_F*((sell_F[j]*(fare_F[j] + rev((rem-1),(prd-1)) ))+(1-sell_F[j]) * rev(rem,(prd-1)) )+
            (1-lambda_L-lambda_F) * rev(rem,(prd-1));
          if( tmp_new > tmp)
          {
            tmp = tmp_new;
            local = i;
            flow = j;
          }
        }
      }
      opt_L(rem, prd) = local + 1;
      opt_F(rem, prd) = flow + 1;
      rev(rem, prd) = tmp;
    }
  }
}