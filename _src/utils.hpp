#ifndef __UTILS_HPP_
#define __UTILS_HPP_

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <json/json.h>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

string quote(string s);
vector<string> quote(vector<string> v);
string join(vector<string> v, char j);
string whoami();
Json::Value parseJSON(string json);
NumericVector asNumericVector(Json::Value root);
CharacterVector asCharacterVector(Json::Value root);
NumericVector createTimeSeries(double anno, double periodo, 
                               double freq, string json_dati);
#endif
