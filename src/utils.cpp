
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include "utils.hpp"

using namespace std;

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
