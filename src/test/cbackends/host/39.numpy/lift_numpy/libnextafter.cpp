
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEXTAFTER_UF_H
#define NEXTAFTER_UF_H
; 
float nextafter_uf(float x, float y){
    return x<y? x+ std::numeric_limits<float>::epsilon() : (x>y? x - std::numeric_limits<float>::epsilon() : x)   ;; 
}

#endif
 ; 
void nextafter(float * v_initial_param_518_240, float * v_initial_param_519_241, float * & v_user_func_525_243, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_525_243 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_239 = 0;(v_i_239 <= (-1 + v_N_0)); (++v_i_239)){
        v_user_func_525_243[v_i_239] = nextafter_uf(v_initial_param_518_240[v_i_239], v_initial_param_519_241[v_i_239]); 
    }
}
}; 