
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef R2D_UF_H
#define R2D_UF_H
; 
float r2d_uf(float x){
    { return x*180/M_PI; }; 
}

#endif
 ; 
void degrees(float * v_initial_param_161_73, float * & v_user_func_163_74, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_163_74 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_72 = 0;(v_i_72 <= (-1 + v_N_0)); (++v_i_72)){
        v_user_func_163_74[v_i_72] = r2d_uf(v_initial_param_161_73[v_i_72]); 
    }
}
}; 