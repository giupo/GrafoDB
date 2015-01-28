#include <Rcpp.h>

#include "save_graph.hpp"
#include "tags.hpp"
#include "conflicts.hpp"
#include "grafodb.hpp"
#include "update_graph.hpp"
#include "create_graph.hpp"
#include "copy_graph.hpp"

using namespace Rcpp;

void save_graph(GrafoDB x, std::string tag, pqxx::work* T) {
  if(hasConflicts(x.tag, T)) {
    stop("Il grafo ha conflitti, risolverli prima di salvare");
  }

  if(tagExists(tag, T)) {
    update_graph(x, T);
  } else {
    if(tag.compare(x.tag) == 0) {
      create_graph(x, tag, T);
    } else {
      copy_graph(x.tag, tag, T);
      update_graph(x, tag, T);
    }
  }
}
