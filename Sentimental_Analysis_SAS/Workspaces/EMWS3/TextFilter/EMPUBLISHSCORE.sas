where (restaurant_name ? "istanbul turkish restaurant");
_document_ = _n_;
rc=tgscore(review,"termloc.TextFilter_tmconfig", "termloc.TextFilter_filtterms", "TextFilter_out", "&_multifile", 0);
drop rc;
