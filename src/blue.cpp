#include <cpp11/matrix.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/function.hpp>

#include <fftw3.h>
#include <vector>

int find_cluster(int size, fftw_complex* input, fftw_complex* output, fftw_complex* final, fftw_plan& input_plan, fftw_plan& output_plan, cpp11::doubles& kernel, bool find_void) {
  int n_ones = 0;
  for (int i = 0; i < size; ++i) {
    if (input[i][0] == 1.0) ++n_ones;
  }

  if (n_ones * 2 >= size) {
    for (int i = 0; i < size; ++i) {
      input[i][0] = input[i][0] == 1.0 ? 0.0 : 1.0;
    }
  }

  fftw_execute(input_plan);

  for (int i = 0; i < size; ++i) {
    output[i][0] *= kernel[i];
    output[i][1] *= kernel[i];
  }

  fftw_execute(output_plan);

  int index = 0;
  double val = final[index][0];

  if (find_void) {
    for (int i = 1; i < size; ++i) {
      if (input[i][0] != 1.0 && final[i][0] < val) {
        val = final[i][0];
        index = i;
      }
    }
  } else {
    for (int i = 1; i < size; ++i) {
      if (input[i][0] == 1.0 && final[i][0] > val) {
        val = final[i][0];
        index = i;
      }
    }
  }

  return index;
}

int find_tightest(int size, fftw_complex* input, fftw_complex* output, fftw_complex* final, fftw_plan& input_plan, fftw_plan& output_plan, cpp11::doubles& kernel) {
  return find_cluster(size, input, output, final, input_plan, output_plan, kernel, false);
}
int find_voidest(int size, fftw_complex* input, fftw_complex* output, fftw_complex* final, fftw_plan& input_plan, fftw_plan& output_plan, cpp11::doubles& kernel) {
  return find_cluster(size, input, output, final, input_plan, output_plan, kernel, true);
}

[[cpp11::register]]
cpp11::writable::doubles blue_c(cpp11::integers dim, int n_seeds, SEXP seed, cpp11::doubles kernel) {
  int n_pixels = 1;
  std::vector<int> dim_rev;
  for (R_xlen_t i = dim.size() - 1; i >= 0; --i) {
    n_pixels *= dim[i];
    dim_rev.push_back(dim[i]);
  }

  fftw_complex* input = fftw_alloc_complex(n_pixels);
  fftw_complex* input_backup = fftw_alloc_complex(n_pixels);
  fftw_complex* output = fftw_alloc_complex(n_pixels);
  fftw_complex* final = fftw_alloc_complex(n_pixels);
  fftw_plan plan_forward = fftw_plan_dft(dim_rev.size(), dim_rev.data(), input, output, FFTW_FORWARD, FFTW_MEASURE);
  fftw_plan plan_backward = fftw_plan_dft(dim_rev.size(), dim_rev.data(), output, final, FFTW_BACKWARD, FFTW_MEASURE);

  cpp11::writable::doubles noise(n_pixels);

  double* seed_p = REAL(seed);
  for (R_len_t i = 0; i < Rf_length(seed); ++i) {
    input[i][0] = seed_p[i];
    input[i][1] = 0.0;
  }

  while (true) {
    int tightest = find_tightest(n_pixels, input, output, final, plan_forward, plan_backward, kernel);
    input[tightest][0] = 0.0;
    int voidest = find_voidest(n_pixels, input, output, final, plan_forward, plan_backward, kernel);
    if (tightest == voidest) {
      input[tightest][0] = 1.0;
      break;
    } else {
      input[voidest][0] = 1.0;
    }
  }

  for (int i = 0; i < n_pixels; ++i) {
    input_backup[i][0] = input[i][0];
  }

  for (int i = n_seeds; i > 0; --i) {
    int tightest = find_tightest(n_pixels, input, output, final, plan_forward, plan_backward, kernel);
    input[tightest][0] = 0.0;
    noise[tightest] = i - 1;
  }

  for (int i = 0; i < n_pixels; ++i) {
    input[i][0] = input_backup[i][0];
  }

  for (int i = n_seeds; i < n_pixels/2; ++i) {
    int voidest = find_voidest(n_pixels, input, output, final, plan_forward, plan_backward, kernel);
    input[voidest][0] = 1.0;
    noise[voidest] = i;
  }
  for (int i = n_pixels/2; i < n_pixels; ++i) {
    int tightest = find_tightest(n_pixels, input, output, final, plan_forward, plan_backward, kernel);
    input[tightest][0] = 0.0;
    noise[tightest] = i;
  }

  for (R_xlen_t i = 0; i < noise.size(); ++i) {
    noise[i] /= (n_pixels - 1);
  }

  fftw_destroy_plan(plan_forward);
  fftw_destroy_plan(plan_backward);
  fftw_free(input);
  fftw_free(input_backup);
  fftw_free(final);
  fftw_free(output);

  if (dim.size() == 2) {
    noise.attr("class") = {"matrix", "array"};
    noise.attr("dim") = dim;
  } else if (dim.size() > 2) {
    noise.attr("class") = "array";
    noise.attr("dim") = dim;
  }
  return noise;
}
