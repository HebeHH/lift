
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void diff(float * v_initial_param_319_162, float * & v_user_func_322_163, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_322_163 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_160 = 0;(v_i_160 <= (-2 + v_N_0)); (++v_i_160)){
        // For each element reduced sequentially
        v_user_func_322_163[v_i_160] = 0.0f; 
        for (int v_i_161 = 0;(v_i_161 <= 1); (++v_i_161)){
            v_user_func_322_163[v_i_160] = diff2(v_user_func_322_163[v_i_160], v_initial_param_319_162[(v_i_160 + v_i_161)]); 
        }
    }
}
}; 