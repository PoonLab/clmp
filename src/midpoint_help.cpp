#include <Rcpp.h>
using namespace Rcpp;

// port of phangorn .Call to bipCPP
// file at https://github.com/KlausVigo/phangorn/blob/master/src/phangorn_help.cpp
// replacement for bip maybe more error tolerant slightly slower
// import: edge matrix, number of tips
// export: Descendants(x, 1:max(x$edge), "all")
// [[Rcpp::export]]
std::vector< std::vector<int> > bipCPP(IntegerMatrix orig, int nTips) {
  IntegerVector parent = orig( _, 0);
  IntegerVector children = orig( _, 1);
  int m = max(parent), j=0;
  // create list for results
  std::vector< std::vector<int> > out(m) ;
  std::vector<int> y;
  for(int i = 0; i<nTips; i++){
    out[i].push_back(i + 1L);
  }
  for(int i = 0; i<parent.size(); i++){
    j = parent[i] - 1L;
    if(children[i] > nTips){
      y = out[children[i] - 1L];
      out[j].insert( out[j].end(), y.begin(), y.end() );
    }
    else out[j].push_back(children[i]);
  }
  for(int i=0; i<m; ++i){
    sort(out[i].begin(), out[i].end());
  }
  return out;    // return the list
}





/*** R
library(clmp)
t7 <- read.tree(text="(((A:2.5, B:2.5):2, C:4.5):3, (D:1, E:1):6.5):0;")
res <- clmp(t7)
midpoint(res)
*/