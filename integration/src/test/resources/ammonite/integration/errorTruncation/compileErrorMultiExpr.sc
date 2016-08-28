// Make sure the line numbers work properly in cases where we have statement
// breaks, both implicitly due to newlines, explicitly due to semi-colons, or
// both!
123;456

;implicitly{
  math.max(1 + 2 + 3, 4)
}

identity("Hello");
doesntexist
