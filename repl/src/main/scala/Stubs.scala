// Dummy packages just to populate the existence of these top-level synthetic
// packages in the Scala compiler.

/**
  * Package that gets filled with any script files that the user imports
  */
package $file{
  private object `_`
}

/**
  * Package that gets filled with ivy artifacts the user loads
  */
package $ivy{
  private object `_`
}
