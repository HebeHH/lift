
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif; 
void nancumsum(float * v_initial_param_292_139, float * & v_user_func_295_140, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_295_140 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_302 = 0.0f;
    for (int v_i_138 = 0;(v_i_138 <= (-1 + v_N_0)); (++v_i_138)){
        scan_acc_302 = add(scan_acc_302, v_initial_param_292_139[v_i_138]); 
        v_user_func_295_140[v_i_138] = scan_acc_302; 
    }
}
}; 