# R6 class for a plant protection product with at least one active ingredient

Contains basic information about the active ingredients in the product

## Format

An [R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) generator
object.

## Public fields

- `name`:

  The name of the product

- `ais`:

  A list of active ingredients

- `concentrations`:

  The concentration of the ais

- `concentration_units`:

  Defaults to g/L

- `density`:

  The density of the product

- `density_units`:

  Defaults to g/L

## Methods

### Public methods

- [`ppp$new()`](#method-ppp-new)

- [`ppp$clone()`](#method-ppp-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ppp$new(
      name,
      ...,
      concentrations,
      concentration_units = "g/L",
      density = 1000,
      density_units = "g/L"
    )

#### Arguments

- `name`:

  The name of the product

- `...`:

  Identifiers of the active ingredients

- `concentrations`:

  Concentrations of the active ingredients

- `concentration_units`:

  Defaults to g/L

- `density`:

  The density

- `density_units`:

  Defaults to g/L

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ppp$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
