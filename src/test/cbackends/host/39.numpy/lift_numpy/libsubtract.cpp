
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SUBTRACT_H
#define SUBTRACT_H
; 
float subtract(float l, float r){
    { return l - r; }; 
}

#endif
 ; 
void subtract(float * v_initial_param_669_285, float * v_initial_param_670_286, float * & v_user_func_676_288, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_676_288 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_284 = 0;(v_i_284 <= (-1 + v_N_0)); (++v_i_284)){
        v_user_func_676_288[v_i_284] = subtract(v_initial_param_669_285[v_i_284], v_initial_param_670_286[v_i_284]); 
    }
}
}; 