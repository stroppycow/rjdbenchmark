# rjdbenchmark - JDemetra+ Workspace Comparison Tool

`rjdbenchmark` is an R package designed to compare and analyze JDemetra+ workspaces. It provides tools for statistical benchmarking, data transformation, and structured reporting.

## Key Techniques Used

- **Data Handling with `data.table`** ([MDN](https://rdatatable.gitlab.io/data.table/)): Efficient manipulation of large datasets.
- **Parsing and Serialization with `arrow`** ([Apache Arrow](https://arrow.apache.org/)): Enables fast columnar data access.
- **Date Handling with `lubridate`** ([lubridate Reference](https://lubridate.tidyverse.org/)): Simplifies working with dates and times.
- **Pattern Matching with `stringr`** ([stringr Reference](https://stringr.tidyverse.org/)): Streamlines string manipulation.
- **YAML Configuration Loading with `yaml`** ([yaml Reference](https://cran.r-project.org/web/packages/yaml/yaml.pdf)): Reads and writes YAML configuration files.
- **Java Integration with `rJava`** ([rJava Reference](https://www.rforge.net/rJava/)): Connects R with Java for extended functionality.
- **JDemetra+ API with `RJDemetra`** ([RJDemetra GitHub](https://github.com/InseeFr/RJDemetra)): Interfaces with the JDemetra+ seasonal adjustment library.

## Notable Dependencies

- **JDemetra+ (`RJDemetra`)**: An advanced tool for time series analysis.
- **rjd3workspace** ([GitHub](https://github.com/rjdverse/rjd3workspace)): Provides workspace manipulation features.
- **Quarto (`quarto`)** ([Quarto](https://quarto.org/)): Used for generating documentation.
- **Docker** ([Docker](https://www.docker.com/)): Supports containerized execution of the project.
