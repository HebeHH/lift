
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void cumprod(float * v_initial_param_280_133, float * & v_user_func_283_134, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_283_134 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_290 = 1.0f;
    for (int v_i_132 = 0;(v_i_132 <= (-1 + v_N_0)); (++v_i_132)){
        scan_acc_290 = prod2_uf(scan_acc_290, v_initial_param_280_133[v_i_132]); 
        v_user_func_283_134[v_i_132] = scan_acc_290; 
    }
}
}; 