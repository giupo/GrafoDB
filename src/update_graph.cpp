
#include <pqxx/pqxx>

#include "update_graph.hpp"

void update_graph(GrafoDB g, pqxx::work* T) {
  update_graph(g, g.tag, T);
}

void update_graph(GrafoDB g, std::string tag, pqxx::work* T) {
  
}

