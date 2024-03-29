#include <cpp11/matrix.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include "FastNoise.h"

FastNoise worley_c(int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int dist, int value, cpp11::integers dist2ind, double jitter, int pertube, double pertube_amp) {
  FastNoise noise_gen;
  noise_gen.SetSeed(seed);
  noise_gen.SetFrequency(freq);

  if (value == 1) cpp11::stop("NoiseLookup is not supported");
  noise_gen.SetCellularDistanceFunction((FastNoise::CellularDistanceFunction) dist);
  noise_gen.SetCellularReturnType((FastNoise::CellularReturnType) value);
  noise_gen.SetCellularDistance2Indices(dist2ind[0], dist2ind[1]);
  noise_gen.SetCellularJitter(jitter);
  if (pertube != 0) noise_gen.SetGradientPerturbAmp(pertube_amp);
  if (fractal != 0) {
    noise_gen.SetFractalType((FastNoise::FractalType) (fractal - 1));
    noise_gen.SetFractalOctaves(octaves);
    noise_gen.SetFractalLacunarity(lacunarity);
    noise_gen.SetFractalGain(gain);
  }

  return noise_gen;
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> worley_2d_c(int height, int width, int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int dist, int value, cpp11::integers dist2ind, double jitter, int pertube, double pertube_amp) {
  cpp11::writable::doubles_matrix<> noise(height, width);
  int i,j;
  double new_i, new_j;
  FastNoise noise_gen = worley_c(seed, freq, fractal, octaves, lacunarity, gain, dist, value, dist2ind, jitter, pertube, pertube_amp);


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
        noise(i, j) = noise_gen.GetCellular(new_j, new_i);
      } else {
        noise(i, j) = noise_gen.GetCellularFractal(new_j, new_i);
      }
    }
  }

  return noise;
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> worley_3d_c(int height, int width, int depth, int seed, double freq, int fractal, int octaves, double lacunarity, double gain, int dist, int value, cpp11::integers dist2ind, double jitter, int pertube, double pertube_amp) {
  cpp11::writable::doubles_matrix<> noise(height, width * depth);
  int i,j,k;
  double new_i, new_j, new_k;

  FastNoise noise_gen = worley_c(seed, freq, fractal, octaves, lacunarity, gain, dist, value, dist2ind, jitter, pertube, pertube_amp);

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
          noise(i, j + k * width) = noise_gen.GetCellular(new_j, new_i, new_k);
        } else {
          noise(i, j + k * width) = noise_gen.GetCellularFractal(new_j, new_i, new_k);
        }
      }
    }
  }

  return noise;
}

[[cpp11::register]]
cpp11::writable::doubles gen_worley2d_c(cpp11::doubles x, cpp11::doubles y, double freq, int seed, int dist, int value, cpp11::integers dist2ind, double jitter) {
  cpp11::writable::doubles noise(x.size());
  FastNoise generator = worley_c(seed, freq, 0, 0, 0.0, 0.0, dist, value, dist2ind, jitter, 0, 0.0);
  for (int i = 0; i < x.size(); i++) {
    noise[i] = generator.GetCellular(x[i], y[i]);
  }
  return noise;
}

[[cpp11::register]]
cpp11::writable::doubles gen_worley3d_c(cpp11::doubles x, cpp11::doubles y, cpp11::doubles z, double freq, int seed, int dist, int value, cpp11::integers dist2ind, double jitter) {
  cpp11::writable::doubles noise(x.size());
  FastNoise generator = worley_c(seed, freq, 0, 0, 0.0, 0.0, dist, value, dist2ind, jitter, 0, 0.0);
  for (int i = 0; i < x.size(); i++) {
    noise[i] = generator.GetCellular(x[i], y[i], z[i]);
  }
  return noise;
}
