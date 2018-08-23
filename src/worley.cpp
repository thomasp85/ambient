#include <Rcpp.h>
#include "FastNoise.h"

using namespace Rcpp;

FastNoise worley_c(int seed, double freq, int dist, int value, IntegerVector dist2ind, double jitter, int pertube, double pertube_amp) {
  FastNoise noise_gen;
  noise_gen.SetSeed(seed);
  noise_gen.SetFrequency(freq);

  if (value == 1) stop("NoiseLookup is not supported");
  noise_gen.SetCellularDistanceFunction((FastNoise::CellularDistanceFunction) dist);
  noise_gen.SetCellularReturnType((FastNoise::CellularReturnType) value);
  noise_gen.SetCellularDistance2Indices(dist2ind[0], dist2ind[1]);
  noise_gen.SetCellularJitter(jitter);
  if (pertube != 0) noise_gen.SetGradientPerturbAmp(pertube_amp);

  return noise_gen;
}

//[[Rcpp::export]]
NumericMatrix worley_2d_c(int height, int width, int seed, double freq, int dist, int value, IntegerVector dist2ind, double jitter, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width);
  int i,j;
  double new_i, new_j;
  FastNoise noise_gen = worley_c(seed, freq, dist, value, dist2ind, jitter, pertube, pertube_amp);


  for (i = 0; i < height; ++i) {
    for (j = 0; j < width; ++j) {
      new_i = (double) i;
      new_j = (double) j;

      if (pertube == 1) {
        noise_gen.GradientPerturb(new_j, new_i);
      } else if (pertube == 2) {
        noise_gen.GradientPerturbFractal(new_j, new_i);
      }
      noise(i, j) = noise_gen.GetCellular(new_j, new_i);
    }
  }

  return noise;
}

//[[Rcpp::export]]
NumericMatrix worley_3d_c(int height, int width, int depth, int seed, double freq, int dist, int value, IntegerVector dist2ind, double jitter, int pertube, double pertube_amp) {
  NumericMatrix noise(height, width * depth);
  int i,j,k;
  double new_i, new_j, new_k;

  FastNoise noise_gen = worley_c(seed, freq, dist, value, dist2ind, jitter, pertube, pertube_amp);

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

        noise(i, j + k * width) = noise_gen.GetCellular(new_j, new_i, new_k);
      }
    }
  }

  return noise;
}
