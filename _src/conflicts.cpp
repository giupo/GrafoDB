
#include <sstream>
#include <Rcpp.h>

#include "conflicts.hpp"

using namespace std;
using namespace pqxx;
using namespace Rcpp;

bool hasConflicts(string tag, pqxx::work* T) {
  stringstream sql;
  sql << "select name from conflitti where tag = '";
  sql << tag << "'";
  pqxx::result res = T->exec(sql);
  return res.size() > 0;
}
