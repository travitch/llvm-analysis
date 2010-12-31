 * Convert broken-up struct parameters back into their aggregate
   forms.
 * At the same time, rewrite uses of those broken-out parameters into
   extractvalue constant instructions refering to the reconstructed
   parameter.
 * Convert sret params into return values, as they should be.  This
   also involves rewriting the return sequence (which will look like
   some bitcasts followed by a memcpy).  The memcpy should be removed
   and the ret needs to be converted from void to something sensible.
