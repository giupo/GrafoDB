#ifndef __UTILS_HPP_
#define __UTILS_HPP_

#include <iostream>
#include <vector>
#include <string>
#include <sstream>

using namespace std;

string quote(string s);
vector<string> quote(vector<string> v);
string join(vector<string> v, char j);
string whoami();
#endif
