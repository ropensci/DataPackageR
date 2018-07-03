
# Configuring and controlling DataPackageR builds.

Data package builds are controlled using the `config.yml` file.

This file is created in the package source tree when the user creates a
package using `datapackage_skeleton()`.

It is automatically populated with the names of the `code_files` and
`data_objects` the passed in to datapackage\_skeleton.

## The `config.yml` file.

The structure of a correctly formatted `config.yml` file is shown below:

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
      objects: cars_over_20
      render_root:
        tmp: '584762'

## YAML config file properties.

The main section of the file is the `configuration:` section.

It has three properties:

  - `files:`
    
    The files (`R` or `Rmd`) to be processed by DataPackageR. They are
    processed in the order shown. Users running multi-script workflows
    with dependencies between the scripts need to ensure the files are
    processed in the correct order.
    
    Here `subsetCars.Rmd` is the only file to process.
    
    Each file itself has several properties:
    
      - `name:` The name of the file. This is transformed to an absolute
        path within the package.
    
      - `enabled:` A logical `yes`, `no` flag indicating whether the
        file should be rendered during the build, or whether it should
        be skipped. This is useful for ‘turning off’ long running
        processing tasks if they have not changed. Disabling processing
        of a file will not overwrite existing documentation or data
        objecs created during previous builds.

  - `objects:`
    
    The names of the data objects created by the processing files, to be
    stored in the package. These names are compared against the objects
    created in the render environment by each file. They names must
    match.

  - `render_root:`
    
    The directory where the `Rmd` or `R` files will be rendered.
    Defaults to a randomly named subdirectory of `tempdir()`. Allows
    workflows that use multiple scripts and create file system artifacts
    to function correctly by simply writing to and reading from the
    working directory.

## Editing the YAML config file.

The structure of the YAML is simple enough to understand but complex
enough that it can be a pain to edit by hand.

DataPackageR provides a number of API calls to construct, read, modify,
and write the yaml config file.

### API calls

#### `construct_yml_config`

Make an r object representing a YAML config file.

##### Example

The YAML config shown above was created by:

``` r
# Note this is done by the datapackage_skeleton. 
# The user doesn't usually need to call 
# construct_yml_config()
yml <- DataPackageR::construct_yml_config(
  code = "subsetCars.Rmd",
  data = "cars_over_20"
  )
```

#### `yml_find`

Read a yaml config file from a package path into an r object. \#\#\#\#\#
Example Read the YAML config file from the `mtcars20` example.

``` r
# returns an r object representation of
# the config file.
mtcars20_config <- yml_find(
  file.path(tempdir(),"mtcars20")
  )
```

#### `yml_list_objects`

List the `objects` in a config read by `yml_find`.

##### Example

``` r
  yml_list_objects(yml)
```

    cars_over_20

#### `yml_list_files`

List the `files` in a config read by `yml_find`.

##### Example

``` r
  yml_list_files(yml)
```

    subsetCars.Rmd

#### `yml_disable_compile`

Disable compilation of named files in a config read by `yml_find`.

##### Example

``` r
yml_disabled <- yml_disable_compile(
    yml,
    filenames = "subsetCars.Rmd")
```

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: no
      objects: cars_over_20
      render_root:
        tmp: '799346'

#### `yml_enable_compile`

Enable compilation of named files in a config read by `yml_find`.

##### Example

``` r
yml_enabled <- yml_enable_compile(
    yml,
    filenames = "subsetCars.Rmd")
```

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
      objects: cars_over_20
      render_root:
        tmp: '799346'

#### `yml_add_files`

Add named files to a config read by `yml_find`.

##### Example

``` r
yml_twofiles <- yml_add_files(
    yml,
    filenames = "anotherFile.Rmd")
```

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
        anotherFile.Rmd:
          name: anotherFile.Rmd
          enabled: yes
      objects: cars_over_20
      render_root:
        tmp: '799346'

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
        anotherFile.Rmd:
          name: anotherFile.Rmd
          enabled: yes
      objects: cars_over_20
      render_root:
        tmp: '799346'

#### `yml_add_objects`

Add named objects to a config read by `yml_find`.

##### Example

``` r
yml_twoobj <- yml_add_objects(
    yml_twofiles,
    objects = "another_object")
```

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
        anotherFile.Rmd:
          name: anotherFile.Rmd
          enabled: yes
      objects:
      - cars_over_20
      - another_object
      render_root:
        tmp: '799346'

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
        anotherFile.Rmd:
          name: anotherFile.Rmd
          enabled: yes
      objects:
      - cars_over_20
      - another_object
      render_root:
        tmp: '799346'

#### `yml_remove_files`

Remove named files from a config read by `yml_find`.

##### Example

``` r
yml_twoobj <- yml_remove_files(
    yml_twoobj,
    filenames = "anotherFile.Rmd")
```

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
      objects:
      - cars_over_20
      - another_object
      render_root:
        tmp: '799346'

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
      objects:
      - cars_over_20
      - another_object
      render_root:
        tmp: '799346'

#### `yml_remove_objects`

Remove named objects from a config read by `yml_find`.

##### Example

``` r
yml_oneobj <- yml_remove_objects(
    yml_twoobj,
    objects = "another_object")
```

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
      objects: cars_over_20
      render_root:
        tmp: '799346'

    configuration:
      files:
        subsetCars.Rmd:
          name: subsetCars.Rmd
          enabled: yes
      objects: cars_over_20
      render_root:
        tmp: '799346'

#### `yml_write`

Write a modified config to its package path.

##### Example

``` r
yml_write(yml_oneobj, path = "path_to_package")
```

The `yml_oneobj` read by `yml_find()` carries an attribute that is the
path to the package. The user doesn’t need to pass a `path` to
`yml_write` if the config has been read by `yml_find`.
