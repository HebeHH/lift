
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINC_UF_H
#define SINC_UF_H
; 
float sinc_uf(float x){
    return sin(M_PI*x)/(M_PI*x) ;; 
}

#endif
 ; 
void sinc(float * v_initial_param_530_219, float * & v_user_func_532_220, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_532_220 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_218 = 0;(v_i_218 <= (-1 + v_N_0)); (++v_i_218)){
        v_user_func_532_220[v_i_218] = sinc_uf(v_initial_param_530_219[v_i_218]); 
    }
}
}; 