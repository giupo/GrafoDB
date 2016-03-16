#include <string>
#include <Rcpp.h>

#include "boost/graph/edge_list.hpp"
#include "grafodb.hpp"

using namespace std;
using namespace Rcpp;

GrafoDB::GrafoDB(List dati, List functions, CharacterMatrix archi, string tag) {
  this->tag = tag;
  this->dati = dati;
  this->functions = functions;
  this->archi = archi;
}

GrafoDB::~GrafoDB() {    
}
