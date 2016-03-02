#ifndef __COPYGRAPH_HPP__
#define __COPYGRAPH_HPP__

#include <pqxx/pqxx>
#include <string>

void copy_graph(std::string fromTag, std::string toTag, pqxx::work* T);

#endif
