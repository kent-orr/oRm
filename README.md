# oRm: An Object-Relational Mapping (ORM) Framework for R

oRm is an open-source Object-Relational Mapping (ORM) framework for R, designed to simplify the process of interacting with databases in R. It provides a flexible and intuitive way to define models, perform CRUD operations, and build complex queries.

## Installation

You can install the latest version of oRm from GitHub using the `remotes` package:

```r
# Install remotes if not already installed
if (!require(remotes)) {
  install.packages("remotes")
}

# Install oRm from GitHub
remotes::install_github("kent-orr/oRm")

## Getting Started
To get started with oRm, follow these steps:

1. Load the package:  

```r
library(oRm)
```  
2. Define an engine

```r
engine = Engine$new()
```

3. Apply  
Create a connection to the database:
```r
# Connect to a SQLite database
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Initialize the "users" table in the database
User$new()$create_table(con)
```

Apply
4.
Perform CRUD operations:
# Create a new user
user <- User$new(name = "Alice", email = "alice@example.com")
user$save(con)

# Read a user by ID
user_id <- 1
found_user <- User$new()$read(con, id == user_id)

# Update a user
found_user$name <- "Alice Updated"
found_user$save(con)

# Delete a user
found_user$delete(con)
Apply
Documentation
For more detailed information about oRm, check out the package documentation:
Package documentation
Vignettes
Function references
Contributing
Contributions to oRm are welcome! If you have any bug reports, feature requests, or code contributions, please feel free to open an issue or submit a pull request on GitHub.
License
oRm is distributed under the MIT License.
Acknowledgments
oRm was inspired by the popular Python ORM library, SQLAlchemy.