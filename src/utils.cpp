
#include <iostream>
#include <vector>
#include <string>
#include <sstream>

#include <cstdio>
#include <cstdlib>
#include <pwd.h>
#include <unistd.h>
#include <Rcpp.h>
#include <json/json.h>

#include "utils.hpp"

using namespace std;
using namespace Rcpp;

string quote(string s) {
  std::stringstream ss;
  ss << "'" << s << "'";
  return ss.str();
}

vector<string> quote(vector<string> v) {
  vector<string> ret(v.size());
  for(unsigned int i=0; i < v.size(); ++i) {
    ret[i] = quote(v[i]);
  }
  return ret;
}

string join(vector<string> v, char j) {
  std::stringstream ss;
  for(size_t i = 0; i < v.size(); ++i) {
    if(i != 0) {
      ss << j;
    }
    ss << v[i];
  }
  return ss.str();
}

string whoami() {
  register uid_t uid = geteuid();
  register struct passwd *pw = getpwuid(uid);
  if(!pw) {
    return string("");
  } else {
    return string(pw->pw_name);
  }
}


NumericVector parseJSON(string json) {
  Json::Value root;
  Json::Value value;
  Json::Reader reader;
  unsigned int i;
  reader.parse(json, root);
  int size = root.size();
  NumericVector z(size);
  for(i = 0; i < size; ++i) {
    value = root[i];
    if(value.isNull()) {
      z[i] = NA_REAL;
    } else {
      z[i] = value.asDouble();
    }
  }
  return z;
}

NumericVector createTimeSeries(double anno, double periodo, 
                               double freq, string json_dati) {

  NumericVector dati = parseJSON(json_dati);
  // for tsp
  double start = anno + periodo/freq - 1/freq;
  double end = start + dati.size()/freq - 1/freq; 
  
  dati.attr("tsp") = NumericVector::create(start, end, freq);
  dati.attr("class") = "ts";
  return dati; 
}
