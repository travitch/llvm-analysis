 * Convert broken-up struct parameters back into their aggregate
   forms.
 * At the same time, rewrite uses of those broken-out parameters into
   extractvalue constant instructions refering to the reconstructed
   parameter.
 * Convert sret params into return values, as they should be.  This
   also involves rewriting the return sequence (which will look like
   some bitcasts followed by a memcpy).  The memcpy should be removed
   and the ret needs to be converted from void to something sensible.


# Example (Original)
%struct.S = type { i32, i32, double }

@.str = private constant [12 x i8] c"foo, %s/%f\0A\00"
@.str1 = private constant [4 x i8] c"bar\00"
@x = global i32 3, align 4
@y = common global i32 0, align 4
@d = common global double 0.000000e+00, align 8

define void @f(%struct.S* sret %agg.result, i32 %s.0, i32 %s.1, double %s.2) nounwind {
  %s = alloca %struct.S, align 4
  %1 = getelementptr inbounds %struct.S* %s, i32 0, i32 0
  store i32 %s.0, i32* %1, align 4
  %2 = getelementptr inbounds %struct.S* %s, i32 0, i32 1
  store i32 %s.1, i32* %2, align 4
  %3 = getelementptr inbounds %struct.S* %s, i32 0, i32 2
  store double %s.2, double* %3, align 4
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), double %s.2)
  %5 = bitcast %struct.S* %agg.result to i8*
  %6 = bitcast %struct.S* %s to i8*
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %5, i8* %6, i32 16, i32 4, i1 false)
  ret void
}


# Example (Rewritten parameters - not return type)
%struct.S = type { i32, i32, double }

@.str = private constant [12 x i8] c"foo, %s/%f\0A\00"
@.str1 = private constant [4 x i8] c"bar\00"
@x = global i32 3, align 4
@y = common global i32 0, align 4
@d = common global double 0.000000e+00, align 8

define void @f(%struct.S* sret %agg.result, %struct.S %s) nounwind {
define void @f(%struct.S* sret %agg.result, i32 %s.0, i32 %s.1, double %s.2) nounwind {
  %s.0 = getelementptr inbounds %struct.S* %s, i32 0, i32 0
  %s.1 = getelementptr inbounds %struct.S* %s, i32 0, i32 1
  %s.2 = getelementptr inbounds %struct.S* %s, i32 0, i32 2
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8]* @.str1, i32 0, i32 0), double %s.2)
  %5 = bitcast %struct.S* %agg.result to i8*
  %6 = bitcast %struct.S* %s to i8*
  call void @llvm.memcpy.p0i8.p0i8.i32(i8* %5, i8* %6, i32 16, i32 4, i1 false)
  ret void
}
