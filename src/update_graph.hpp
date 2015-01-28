#ifndef __UPDATEGRAPH_HPP__
#define __UPDATEGRAPH_HPP__

#include <pqxx/pqxx>
#include <string>

#include "grafodb.hpp"

void update_graph(GrafoDB g, pqxx::work* T);
void update_graph(GrafoDB g, std::string tag, pqxx::work* T);

#endif
