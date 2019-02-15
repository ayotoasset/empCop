#include <Rcpp.h>
#include <string>
#include <iostream>
#include <bitset>
using namespace Rcpp;

//How do i document thoose functions ? I dont, really...

// Prerequisite :

double prod(NumericVector vars){
  double multi = 1;
  for(int i = 0; i < vars.size(); i++)
  {
    multi = multi * vars[i];
  }
  return multi;
}

// [[Rcpp::export]]
NumericVector Cpp_pCopula_cbkmCopula(IntegerVector d_moins_J,
                                   IntegerVector J,
                                   NumericMatrix u,
                                   NumericMatrix boxes,
                                   double size_box,
                                   int m,
                                   NumericVector weights,
                                   Function vCopula_wrapper){

  int d = boxes.ncol();
  int p = J.length();
  int n = boxes.nrow();
  NumericVector y_min (d);
  NumericVector min;
  NumericVector max;
  NumericVector cumsum_result = 0;
  List intersect = List::create();
  NumericVector mes_known;
  NumericVector mes_lebesgue;
  J = J-1;
  d_moins_J = d_moins_J -1;
  NumericVector Result (u.nrow());

  for(int n_obs = 0; n_obs<u.nrow();n_obs++){
    cumsum_result[0] = 0.0;
    for(int i=0;i<n;i++){

      // retourne l'intersection es deux rectangles [x_min,x_max] et
      // [y_min,y_max] sous la forme d'un rectangle [rez_min,rez_max]
      min = pmax(boxes.row(i),y_min);
      max = pmin(boxes.row(i)+size_box,u.row(n_obs));

      // Add weights only if the intersection is not empty :
      if (! any(min > max).is_true()){
        // mesure of the known copula on it's margins, per box :
        mes_known = vCopula_wrapper(min[J],max[J]);

        // lebegue copula measure on it's margins, per box :
        mes_lebesgue = prod(max[d_moins_J] - min[d_moins_J])*pow(m,d-p);

        // return the cumulative sum :
        cumsum_result[0] = cumsum_result[0] + mes_known[0] * mes_lebesgue[0] * weights[i];

      }
    }
    Result[n_obs] = cumsum_result[0];
  }
  return Result;
}

// // [[Rcpp::export]]
// StringVector Cpp_number2binary(long int number,const int noBits)
// {
//   StringVector binary = std::bitset<64>(number).to_string();
//   binary = binary.substr(binary.size() - noBits);
//   return(binary);
// }

// // [[Rcpp::export]] //
// List Cpp_intersect(NumericVector x_min,
//                    NumericVector  x_max,
//                    NumericVector  y_min,
//                    NumericVector  y_max) {
// // retourne l'intersection es deux rectangles [x_min,x_max] et
// // [y_min,y_max] sous la forme d'un rectangle [rez_min,rez_max]
//
//   // Create a list :
//   NumericVector min = pmax(x_min,y_min);
//   NumericVector max = pmin(x_max,y_max);
//   List L = List::create(R_NilValue);
//   // List L = List::create(Named("min") = min , _["max"] = max);
//
//   if (! any(min > max).is_true()) {
//     L = List::create(Named("min") = min , _["max"] = max);
//   }
//   return(L);
// }




