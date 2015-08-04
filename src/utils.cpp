
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
#include <algorithm>
#include "utils.hpp"
#include <boost/algorithm/string/replace.hpp>

using namespace std;
using namespace Rcpp;

void ReplaceStringInPlace(std::string& subject, const std::string& search,
                          const std::string& replace) {
  size_t pos = 0;
  while ((pos = subject.find(search, pos)) != std::string::npos) {
    subject.replace(pos, search.length(), replace);
    pos += replace.length();
  }
}

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


NumericVector asNumericVector(Json::Value root){
  Json::Value value;
  unsigned int size = root.size();
  NumericVector z(size);
  for(unsigned int i = 0; i < size; ++i) {
    value = root[i];
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
  reader.parse(json, root);
  return root;
}

NumericVector createTimeSeries(double anno, double periodo, 
                               double freq, string json_dati) {
  NumericVector dati = asNumericVector(parseJSON(json_dati));
  // for tsp
  double start = anno + periodo/freq - 1/freq;
  double end = start + dati.size()/freq - 1/freq; 
  
  dati.attr("tsp") = NumericVector::create(start, end, freq);
  dati.attr("class") = "ts";
  return dati; 
}
