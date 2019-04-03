#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <stdlib.h>
#include "libconv_gpu_integrate_first_half.cpp"

float mult(float l, float r){
    {
        { return l * r; }; 
    }
}
float add(float x, float y){
    {
        { return x+y; }; 
    }
}
float id(float x){
    {
        { return x; }; 
    }
}


int main( int argc, char** argv ) {

	lift_init();

    const int kernel_xdim_SV = 3;
    const int kernel_ydim_SV = 3;
    const int input_xdim_SV = 8;
    const int input_ydim_SV = 8;
    const int in_channels_SV = 2;
    const int out_channels_SV = 3;
    const int n_inputs_SV = 2;
    const unsigned int platform_id = 0, device_id = 0;

    float input_X[n_inputs_SV][input_ydim_SV][input_xdim_SV][in_channels_SV] = {
        {
        {{0.0, 0.0},   {1.0, 1.0},   {2.0, 2.0},   {3.0, 3.0},
         {4.0, 4.0},   {5.0, 5.0},   {6.0, 6.0},  {7.0, 7.0}},
        {{8.0, 8.0},   {9.0, 9.0},   {10.0, 10.0}, {11.0, 11.0},
         {12.0, 12.0}, {13.0, 13.0}, {14.0, 14.0}, {15.0, 15.0}},
        {{16.0, 16.0}, {17.0, 17.0}, {18.0, 18.0}, {19.0, 19.0},
         {20.0, 20.0}, {21.0, 21.0}, {22.0, 22.0}, {23.0, 23.0}},
        {{24.0, 24.0}, {25.0, 25.0}, {26.0, 26.0}, {27.0, 27.0},
         {28.0, 28.0}, {29.0, 29.0}, {30.0, 30.0}, {31.0, 31.0}},
        {{32.0, 32.0}, {33.0, 33.0}, {34.0, 34.0}, {35.0, 35.0},
         {36.0, 36.0}, {37.0, 37.0}, {38.0, 38.0}, {39.0, 39.0}},
        {{40.0, 40.0}, {41.0, 41.0}, {42.0, 42.0}, {43.0, 43.0},
         {44.0, 44.0}, {45.0, 45.0}, {46.0, 46.0}, {47.0, 47.0}},
        {{48.0, 48.0}, {49.0, 49.0}, {50.0, 50.0}, {51.0, 51.0},
         {52.0, 52.0}, {53.0, 53.0}, {54.0, 54.0}, {55.0, 55.0}},
        {{56.0, 56.0}, {57.0, 57.0}, {58.0, 58.0}, {59.0, 59.0},
         {60.0, 60.0}, {61.0, 61.0}, {62.0, 62.0}, {63.0, 63.0}}},
        {
        {{0.0, 0.0},   {1.0, 1.0},   {2.0, 2.0},   {3.0, 3.0},
         {4.0, 4.0},   {5.0, 5.0},   {6.0, 6.0},  {7.0, 7.0}},
        {{8.0, 8.0},   {9.0, 9.0},   {10.0, 10.0}, {11.0, 11.0},
         {12.0, 12.0}, {13.0, 13.0}, {14.0, 14.0}, {15.0, 15.0}},
        {{16.0, 16.0}, {17.0, 17.0}, {18.0, 18.0}, {19.0, 19.0},
         {20.0, 20.0}, {21.0, 21.0}, {22.0, 22.0}, {23.0, 23.0}},
        {{24.0, 24.0}, {25.0, 25.0}, {26.0, 26.0}, {27.0, 27.0},
         {28.0, 28.0}, {29.0, 29.0}, {30.0, 30.0}, {31.0, 31.0}},
        {{32.0, 32.0}, {33.0, 33.0}, {34.0, 34.0}, {35.0, 35.0},
         {36.0, 36.0}, {37.0, 37.0}, {38.0, 38.0}, {39.0, 39.0}},
        {{40.0, 40.0}, {41.0, 41.0}, {42.0, 42.0}, {43.0, 43.0},
         {44.0, 44.0}, {45.0, 45.0}, {46.0, 46.0}, {47.0, 47.0}},
        {{48.0, 48.0}, {49.0, 49.0}, {50.0, 50.0}, {51.0, 51.0},
         {52.0, 52.0}, {53.0, 53.0}, {54.0, 54.0}, {55.0, 55.0}},
        {{56.0, 56.0}, {57.0, 57.0}, {58.0, 58.0}, {59.0, 59.0},
         {60.0, 60.0}, {61.0, 61.0}, {62.0, 62.0}, {63.0, 63.0}}}};

     float input_B[out_channels_SV] = {0.0, 1.0, 2.0};

     float input_K[out_channels_SV][kernel_ydim_SV][kernel_xdim_SV][in_channels_SV] = {
      {
        {{1, 0}, {3, 0}, {5, 0}},
        {{7, 0}, {9, 0}, {11, 0}},
        {{13, 0}, {15, 0}, {17, 0}}},
      {
        {{0, 1}, {0, 3}, {0, 5}},
        {{0, 7}, {0, 9}, {0, 11}},
        {{0, 13}, {0, 15}, {0, 17}}},
      {
        {{1, 0}, {3, 0}, {5, 0}},
        {{7, 0}, {9, 0}, {11, 0}},
        {{13, 0}, {15, 0}, {17, 0}}}};

    /* float out[n_inputs_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)][out_channels_SV] = {0}; */

    float *out1 = nullptr;


    execute(reinterpret_cast<float *>(input_K), reinterpret_cast<float *>(input_B), reinterpret_cast<float *>(input_X), out1);
    


	float (&out)[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)] = *reinterpret_cast<float (*)[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)]>(out1);


#include "./golden.c"
	/*
    float gold[n_inputs_SV][out_channels_SV][input_ydim_SV - (kernel_ydim_SV - 1)][input_xdim_SV - (kernel_xdim_SV - 1)] =
      {
        {
          {
            {1029, 1110, 1191, 1272, 1353, 1434},
            {1677, 1758, 1839, 1920, 2001, 2082},
            {2325, 2406, 2487, 2568, 2649, 2730},
            {2973, 3054, 3135, 3216, 3297, 3378},
            {3621, 3702, 3783, 3864, 3945, 4026},
            {4269, 4350, 4431, 4512, 4593, 4674}},
          {
            {1030, 1111, 1192, 1273, 1354, 1435},
            {1678, 1759, 1840, 1921, 2002, 2083},
            {2326, 2407, 2488, 2569, 2650, 2731},
            {2974, 3055, 3136, 3217, 3298, 3379},
            {3622, 3703, 3784, 3865, 3946, 4027},
            {4270, 4351, 4432, 4513, 4594, 4675}},
          {
            {1031, 1112, 1193, 1274, 1355, 1436},
            {1679, 1760, 1841, 1922, 2003, 2084},
            {2327, 2408, 2489, 2570, 2651, 2732},
            {2975, 3056, 3137, 3218, 3299, 3380},
            {3623, 3704, 3785, 3866, 3947, 4028},
            {4271, 4352, 4433, 4514, 4595, 4676}}},
        {
          {
            {1029, 1110, 1191, 1272, 1353, 1434},
            {1677, 1758, 1839, 1920, 2001, 2082},
            {2325, 2406, 2487, 2568, 2649, 2730},
            {2973, 3054, 3135, 3216, 3297, 3378},
            {3621, 3702, 3783, 3864, 3945, 4026},
            {4269, 4350, 4431, 4512, 4593, 4674}},
          {
            {1030, 1111, 1192, 1273, 1354, 1435},
            {1678, 1759, 1840, 1921, 2002, 2083},
            {2326, 2407, 2488, 2569, 2650, 2731},
            {2974, 3055, 3136, 3217, 3298, 3379},
            {3622, 3703, 3784, 3865, 3946, 4027},
            {4270, 4351, 4432, 4513, 4594, 4675}},
          {
            {1031, 1112, 1193, 1274, 1355, 1436},
            {1679, 1760, 1841, 1922, 2003, 2084},
            {2327, 2408, 2489, 2570, 2651, 2732},
            {2975, 3056, 3137, 3218, 3299, 3380},
            {3623, 3704, 3785, 3866, 3947, 4028},
            {4271, 4352, 4433, 4514, 4595, 4676}}}};
*/

    // Verify the result
    bool result=true;

    for (int i=0; i < n_inputs_SV; i++) {
        for (int j=0; j < out_channels_SV; j++) {
            printf("[");
            for (int k=0; k < input_ydim_SV - (kernel_ydim_SV - 1); k++) {
                for (int l=0; l < input_xdim_SV - (kernel_xdim_SV - 1); l++) {
                    printf(" %d", (int)out[i][j][k][l]); 
                }
                printf("\n ");
            }
            printf("]\n");
        }
    }


    for (int i=0; i < n_inputs_SV; i++) {
        for (int j=0; j < out_channels_SV; j++) {
            for (int k=0; k < input_ydim_SV - (kernel_ydim_SV - 1); k++) {
                for (int l=0; l < input_xdim_SV - (kernel_xdim_SV - 1); l++) {
                    if (out[i][j][k][l] != gold[i][j][k][l]) {
                        result=false;
                        printf("out[%d][%d][%d][%d] = ", i, j, k, l);
                        printf("%f != %f\n", out[i][j][k][l], gold[i][j][k][l]);
                        printf("Fail.\n");
                        return( EXIT_SUCCESS );
                    }
                }
            }
        }
    }

    std::cout<< "Success!\n";

    std::cout << "Done.\n";
    return( EXIT_SUCCESS );
}
