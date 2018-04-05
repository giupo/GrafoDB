
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <Rcpp.h>
#include "json/json.h"
#include <algorithm>
#include "utils.h"
#include <boost/algorithm/string/replace.hpp>

using namespace std;
using namespace Rcpp;


NumericVector asNumericVector(Json::Value root){
  // cout << root << endl;
  Json::Value value;
  unsigned int size = root.size();
  NumericVector z(size);
  for(unsigned int i = 0; i < size; ++i) {
    value = root[i];
    //cout << value << endl;
    if(value.isNull()) {
      z[i] = NA_REAL;
    } else {
      z[i] = value.asDouble();
    }
  }
  return z;
}

CharacterVector asCharacterVector(Json::Value root) {
  Json::Value value;
  unsigned int size = root.size();
  CharacterVector z(size);
  for(unsigned int i = 0; i < size; ++i) {
    value = root[i];
    if(value.isNull()) {
      z[i] = NA_STRING;
    } else {
      z[i] = value.asString();
    }
  }
  return z;
}


Json::Value parseJSON(string json) {
  Json::Value root;
  Json::Reader reader;
  // ReplaceStringInPlace(json, "NaN", "null");
  boost::replace_all(json, "NaN", "null");
  boost::replace_all(json, "Infinity", "null");
  reader.parse(json, root);
  return root;
}

NumericVector createTimeSeries(double anno, double periodo, 
                               double freq, string json_dati) {
  // cout << json_dati << endl;
  NumericVector dati = asNumericVector(parseJSON(json_dati));
  // for tsp
  double start = anno + periodo/freq - 1/freq;
  double end = start + dati.size()/freq - 1/freq; 
  
  dati.attr("tsp") = NumericVector::create(start, end, freq);
  dati.attr("class") = "ts";
  return dati; 
}
