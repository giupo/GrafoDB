#ifndef __GRAFODB_HPP__
#define __GRAFODB_HPP__

#include <string>
#include <map>

#include <Rcpp.h>

//#include "boost/graph/edge_list.hpp"
//#include "boost/graph/topological_sort.hpp"
#include "series.hpp"

using namespace std;
using namespace Rcpp;

class GrafoDB {

public:
  GrafoDB(List dati, List functions, CharacterMatrix archi, string tag);
  ~GrafoDB();
  string tag;
  List dati;
  List functions;
  CharacterMatrix archi;
};

#endif
