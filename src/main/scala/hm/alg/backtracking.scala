package hm.alg

import java.lang.Math.max

import hm.DS

/**
  * @author Alexandru Stana, alexandru.stana@busymachines.com
  * @since 20/06/2018
  */
object backtracking {
  implicit class B(ds: DS) {

    def backtrack: Int = {

      if (ds.N == 0 || ds.G == 0) 0
      else if (ds.g(ds.N - 1) > ds.G) DS(ds.G, ds.N - 1, ds.g, ds.v).backtrack
      else
        max(
          ds.v(ds.N - 1) + DS(ds.G - ds.g(ds.N - 1), ds.N - 1, ds.g, ds.v).backtrack,
          DS(ds.G,                                   ds.N - 1, ds.g, ds.v).backtrack
        )
    }
  }
}
