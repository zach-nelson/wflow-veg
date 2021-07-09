# wflow-veg

A [workflowr][] project.

[workflowr]: https://github.com/jdblischak/workflowr

## Quick start workflowr

```
library("workflowr")

# Configure Git (only need to do once per computer)
wflow_git_config(user.name = "Full Name", user.email = "email@domain")

# Start a new workflowr project
From local directory e.g. C:/Dropbox/Rprojects

wflow_start("wflow-veg")

# Build the site
wflow_build()

# Publish the site, i.e. version the source code and HTML results
wflow_publish("analysis/*", "Start my new project")

```

## Quick start targets
In the root directory of the workflowr project, create a `_targets.R` script.

```
tar_make()
```
creates a `_targets` directory that will store the targets from the data pipeline. The .rmd files from the analysis folder now have access to targets through tar_load() or tar_read().

