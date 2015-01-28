#include "tags.hpp"

#include <sstream>

bool tagsExists(std::string tag, pqxx::work* T) {
  std::stringstream sql;
  sql << "select * from grafi where tag='";
  sql << tag << "'";
  pqxx::result res = T->exec(sql);
  return res.size() > 0;
}
