#include <Rcpp.h>
#include "FastNoise.h"

using namespace Rcpp;

FastNoise perlin_c(int seed, double freq, int interp, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  FastNoise noise_gen;
  noise_gen.SetSeed(seed);
  noise_gen.SetFrequency(freq);
  noise_gen.SetInterp((FastNoise::Interp) interp);
  if (pertube != 0) noise_gen.SetGradientPerturbAmp(pertube_amp);
  if (fractal != 0) {
    noise_gen.SetFractalType((FastNoise::FractalType) (fractal - 1));
    noise_gen.SetFractalOctaves(octaves);
    noise_gen.SetFractalLacunarity(lacunarity);
    noise_gen.SetFractalGain(gain);
  }

  return noise_gen;
}


//[[Rcpp::export]]
NumericMatrix perlin_2d_c(int height, int width, int seed, double freq, int interp, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width);
  int i,j;
  double new_i, new_j;

  FastNoise noise_gen = perlin_c(seed, freq, interp, fractal, octaves, lacunarity, gain, pertube, pertube_amp);

  for (i = 0; i < height; ++i) {
    for (j = 0; j < width; ++j) {
      new_i = (double) i;
      new_j = (double) j;

      if (pertube == 1) {
        noise_gen.GradientPerturb(new_j, new_i);
      } else if (pertube == 2) {
        noise_gen.GradientPerturbFractal(new_j, new_i);
      }
      if (fractal == 0) {
        noise(i, j) = noise_gen.GetPerlin(new_j, new_i);
      } else {
        noise(i, j) = noise_gen.GetPerlinFractal(new_j, new_i);
      }
    }
  }

  return noise;
}

//[[Rcpp::export]]
NumericMatrix perlin_3d_c(int height, int width, int depth, int seed, double freq, int interp, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width * depth);
  int i,j,k;
  double new_i, new_j, new_k;

  FastNoise noise_gen = perlin_c(seed, freq, interp, fractal, octaves, lacunarity, gain, pertube, pertube_amp);

  for (k = 0; k < depth; ++k) {
    for (i = 0; i < height; ++i) {
      for (j = 0; j < width; ++j) {
        new_i = (double) i;
        new_j = (double) j;
        new_k = (double) k;

        if (pertube == 1) {
          noise_gen.GradientPerturb(new_j, new_i, new_k);
        } else if (pertube == 2) {
          noise_gen.GradientPerturbFractal(new_j, new_i, new_k);
        }
        if (fractal == 0) {
          noise(i, j + k * width) = noise_gen.GetPerlin(new_j, new_i, new_k);
        } else {
          noise(i, j + k * width) = noise_gen.GetPerlinFractal(new_j, new_i, new_k);
        }
      }
    }
  }

  return noise;
}
