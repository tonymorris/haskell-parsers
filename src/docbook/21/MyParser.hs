phoneParser :: Parser String
phoneParser = bindParser digit (\d -> bindParser phoneBodyParser (\z -> mapParser (is '#') (\_ -> d : z)))