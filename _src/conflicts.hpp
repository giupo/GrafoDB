#ifndef __CONFLICTS_HPP__
#define __CONFLICTS_HPP__

#include <string>
#include <pqxx/pqxx>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;
using namespace pqxx;

bool hasConflicts(string tag, pqxx::work* T);

#endif
