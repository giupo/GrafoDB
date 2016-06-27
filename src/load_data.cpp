#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include "utils.h"

using namespace Rcpp;
using namespace std;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

//' Converte un dataframe in timeseries
//'
//' Viene usato nella conversione dei dati dal db
//'
//' @name convert_data_frame
//' @usage convert_data_frame(df)
//' @param df data.frame con le serie storiche 
//' @return a List
//' @export
//' @import Rcpp
//' @useDynLib GrafoDB
// [[Rcpp::export]]

List convert_data_frame(SEXP dataframe0) {
  List z = List::create();
  DataFrame df = as<DataFrame>(dataframe0);
  NumericVector ids = df["id"];
  NumericVector anni = df["anno"];
  NumericVector periodi = df["periodo"];
  NumericVector freqs = df["freq"];
  CharacterVector dati = df["dati"];
  CharacterVector nomi = df["name"];
  NumericVector stocks = df["stock"];
  int size = ids.size();
  double tol = 0.01;
  for(int i = 0; i < size; i++) {
     string name(nomi[i]);
     int anno = anni[i];
     int periodo = periodi[i];
     int freq = freqs[i];
     string data(dati[i]);

     if(anno < tol  || periodo < tol || freq < tol) {
        Json::Value root = parseJSON(data);
        if (root.size() == 0) {
          z[name] = NumericVector::create();
        } else if(root[0].isNumeric()) {
          z[name] = asNumericVector(root);
        } else {
          z[name] = asCharacterVector(root);
        }
      } else {	
         NumericVector ts = createTimeSeries(anno, periodo, freq, data);
         ts.attr("stock") = stocks[i];
         ts.attr("name") = name;
         z[name] = ts;
      }
  }
  return z;
}
