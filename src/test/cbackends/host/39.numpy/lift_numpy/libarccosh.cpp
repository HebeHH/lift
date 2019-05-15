
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
float arccos_uf(float x){
    { return acos(x); }; 
}

#endif; 
void arccosh(float * v_initial_param_107_91, float * & v_user_func_109_92, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_109_92 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_90 = 0;(v_i_90 <= (-1 + v_N_0)); (++v_i_90)){
        v_user_func_109_92[v_i_90] = arccos_uf(v_initial_param_107_91[v_i_90]); 
    }
}
}; 