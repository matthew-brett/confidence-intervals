# # An R tutorial on confidence intervals with plots
#
# Here is a *population* of values.  In fact, in our case, it is a complete
# collection of the birth weights for all children born in North Carolina in
# 2008.  See <https://github.com/odsti/datasets/tree/main/birth_weights> for more
# on the source of the data.

# You will also need 'data.table' and 'animation'.
library(dplyr)
library(glue)
library(ggplot2)
library(cowplot)

# Script directory.
# https://stackoverflow.com/a/1816487 (Hadley Wickham).
# "Don't ask me how it works though, because I've forgotten :/"
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
script_dir <- dirname(frame_files[[length(frame_files)]])

# Birth weights for every baby born in North Carolina.
bw_pop <- read.csv(glue('{script_dir}/data/nc_birth_weights.csv'))
bw_pop_vals <- bw_pop[['birth_weight']]
pop_mean <- mean(bw_pop_vals)

# A random sample of birthweights that someone has taken for us.
bw_sample <- read.csv(glue('{script_dir}/data/nc_birth_weights_sample.csv'))
bw_sample_vals <- bw_sample[['birth_weight']]
sample_mean <- mean(bw_sample_vals)

# The size of our sample(s).
n <- 50

# Use theme_bw by default.
tbw <- theme_set(theme_bw())

# Default bins for samples.
breaks <- seq(0.2, 5.5, by=0.2)

plot_bw <- function(df) {
    if (nrow(df) > 1000) {  # Population
        fill_col <- 'light grey'
        mean_col <- 'blue'
    } else {  # Sample
        fill_col <- 'dark grey'
        mean_col <- 'red'
    }
    mean_val <- mean(df[['birth_weight']])
    label <- glue('Mean: {round(mean_val, 2)}')
    p <- (
        ggplot(df, aes(birth_weight))
        + geom_histogram(fill = fill_col, breaks = breaks)
        + geom_vline(aes(xintercept=mean_val, colour=label),
                     linewidth=1,
                     linetype='dashed')
        + labs(x = "Birth weight", colour="")  # Reset colour for later.
        + scale_colour_manual(values=mean_col)
        + theme(legend.position.inside=c(0.25, 0.8),
                legend.text = element_text(size=15),
                aspect.ratio=0.7)
    )
}


samp_ylims = ylim(0, 15)

# Make some random samples.
set.seed(1939)
n_samples <- 5
samples <- list()
for (i in 2:(n_samples + 1)) {
    sample <- sample_n(bw_pop, n)
    name <- sprintf('%d', i)
    p <- (plot_bw(sample)
          + labs(title = glue('Sample {name}'))
          + samp_ylims)
    samples[[name]] = list(sample=sample, plot=p, mean=mean(sample[['birth_weight']]))
}

# Prepare for use in plot_grid.
plots = list()
for (name in names(samples)) {
    v <- samples[[name]]
    plots[[name]] <- (v[['plot']]
                    + theme_half_open(12)
                    + theme(legend.position = 'none')
                    + labs(title=NULL)
                    + xlab(glue('Mean: {round(v[["mean"]], 2)}'))
                    )
}
plots[[1]] <- plots[[1]] + ylab('Counts')

# Estimated SD of sampling distribution of mean.
sem <- sd(bw_pop_vals) / sqrt(n)
samp_lims <- pop_mean + sem * c(-4, 4)

# Prepare growing histograms of sample means.
samp_dists = list()
means = data.frame(sample_mean=numeric())
samp_breaks = seq(2.9, 3.6, 0.1)
mean_col = 'green'
for (name in names(samples)) {
    means[nrow(means) + 1,] = samples[[name]][['mean']]
    p <- (
        ggplot(means, aes(sample_mean))
        + geom_histogram(breaks = samp_breaks)
        + xlab('Mean birth weight')
        + ylim(0, 5)
        + theme_half_open(12)
        + labs(title=NULL)
    )
    samp_dists[[name]] <- p
}

# Compile sampling distribution of mean.
n_iters <- 10000
samp_dist_vals <- numeric(n_iters)
for (i in 1:n_iters) {
    sample <- sample(bw_pop_vals, n)
    samp_dist_vals[i] <- mean(sample)
}
samp_dist <- data.frame(sample_mean=samp_dist_vals)

m_samp_dist <- mean(samp_dist_vals)

samp_bin_width <- 0.01
kde <- density(samp_dist_vals, bw=0.02)
kde_df <- data.frame(x = kde$x, y = kde$y * n_iters * samp_bin_width)

plot_ssamp_dist <- function(df, offset=0) {
    df <- data.table::copy(df)
    df$x <- df$x + offset
    return (
        ggplot(df, aes(df))
        + geom_area(aes(x = x, y = y), linewidth=2, alpha=0.5)
        + labs(x = 'Sample mean', y = 'Count',
               title = 'Sampling distribution')
        + geom_vline(aes(xintercept = m_samp_dist + offset),
                     linetype='dashed')
    )
}

kde_cdf <- cumsum(kde$y / sum(kde$y))
le_025 <- kde_cdf <= 0.025
ge_975 <- kde_cdf >= 0.975
lower_tail <- subset(kde_df, le_025)
upper_tail <- subset(kde_df, ge_975)
lo_025_th <- max(lower_tail$x)
hi_975_th <- min(upper_tail$x)

lo_025_th <- max(lower_tail$x)
hi_975_th <- min(upper_tail$x)
upper_diff <- m_samp_dist - lo_025_th
lower_diff <- m_samp_dist - hi_975_th

# Function to generate animation.
library(animation)

build_moving <- function(vid_len = 6, video_name = 'moving_dist.mp4') {
    offsets <- sin(seq(0, 2 * pi, 0.05))

    min_x <- min(kde$x) - 0.7
    max_x <- max(kde$x) + 0.7

    # vid_len in seconds.
    fps <- length(offsets) / vid_len

    saveVideo(
        {
            ani.options(interval = 1 / fps)
            for (i in 1:length(offsets)) {
                offset <- offsets[i]
                p <- plot_ssamp_dist(kde_df, offset)
                p <- p + xlim(min_x, max_x)
                plot(p)
            }
        },
        video.name = video_name
    )
}

# Write animation with:
# build_moving()
