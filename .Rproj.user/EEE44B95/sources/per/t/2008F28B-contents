# Benchmarking qs() Package

Here, we test the speed at which the `qsave()` and `qread()` functions from the `qs` package operate relative to the functions `read_rds()`/`readRDS()` and `write_rds()`/`saveRDS()`. There are some benchmarks on the `qs()` package that are available on the `qs` package github repository. However, this is mostly an illustration of the parameterization of the `qs` package functions and how different options change the file size and the speed for a single object. In the following tests, we look at the speed and efficiency of the mentioned packages by looking at the performance of functions across objects of different sizes. 

We first look at the speed of saving objects. We look at the performance of the saving functions relative to the size of the file in a csv form. The graph below shows that the `qsave()` function operates much quicker than `saveRDS()` or `save_rds()` for saving files that range in size from ~2 mb to ~800 mb. The x-axis has been converted to log scale for better visualization. 

![image](/Users/noahforougi/benchmark_qs/results/figures/saving_time.png)

The other part of the story is looking at the size of the saved .rds or .qs object. The 
