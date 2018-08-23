#include <Rcpp.h>
#include "FastNoise.h"

using namespace Rcpp;

FastNoise simplex_c(int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  FastNoise noise_gen;
  noise_gen.SetSeed(seed);
  noise_gen.SetFrequency(freq);
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
NumericMatrix simplex_2d_c(int height, int width, int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width);
  int i,j;
  double new_i, new_j;
  FastNoise noise_gen = simplex_c(seed, freq, fractal, octaves, lacunarity, gain, pertube, pertube_amp);

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
        noise(i, j) = noise_gen.GetSimplex(new_j, new_i);
      } else {
        noise(i, j) = noise_gen.GetSimplexFractal(new_j, new_i);
      }
    }
  }

  return noise;
}

//[[Rcpp::export]]
NumericMatrix simplex_3d_c(int height, int width, int depth, int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width * depth);
  int i,j,k;
  double new_i, new_j, new_k;

  FastNoise noise_gen = simplex_c(seed, freq, fractal, octaves, lacunarity, gain, pertube, pertube_amp);

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

//[[Rcpp::export]]
NumericMatrix simplex_4d_c(int height, int width, int depth, int time, int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width * depth * time);
  int i,j,k,l;
  double new_i, new_j, new_k, new_l;

  FastNoise noise_gen = simplex_c(seed, freq, fractal, octaves, lacunarity, gain, pertube, pertube_amp);
  for (l = 0; l < time; ++l) {
    for (k = 0; k < depth; ++k) {
      for (i = 0; i < height; ++i) {
        for (j = 0; j < width; ++j) {
          new_i = (double) i;
          new_j = (double) j;
          new_k = (double) k;
          new_l = (double) l;

          noise(i, j + k * width + l * width * depth) = noise_gen.GetSimplex(new_j, new_i, new_k, new_l);
        }
      }
    }
  }

  return noise;
}
