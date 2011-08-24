data Parser a = P {
  parse :: String -> Maybe (String, a)
}