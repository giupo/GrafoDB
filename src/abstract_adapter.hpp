#ifndef __ABSTRACT_ADAPTER_HPP__
#define __ABSTRACT_ADAPTER_HPP__

#include <string>
#include <vector>
#include <Rcpp.h>
#include <json/json.h>

#include "utils.hpp"

using namespace std;
using namespace Rcpp;

class AbstractAdapter {
public:
  virtual CharacterMatrix getArchi() = 0;
  virtual List getData(vector<string> names) = 0;
  
  virtual bool hasHistoricalData() = 0;
  virtual List getHistoricalData(vector<string> names) = 0;
  virtual List getData() = 0;
  virtual vector<string> getNames() = 0;
  virtual void commit() = 0;
  virtual void init() = 0;
  virtual void matchOrdinal() = 0;
  virtual bool hasConflicts(const string name = "") = 0;
  virtual DataFrame getConflicts(const string name = "") = 0;
  virtual void do_history(const vector<string> names) = 0;
};

#endif
