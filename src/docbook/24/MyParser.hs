personParser2 :: Parser Person
personParser2 = do age <- ageParser
                   spaces
                   firstName <- firstNameParser
                   spaces
                   surname <- surnameParser
                   spaces
                   gender <- genderParser
                   spaces
                   phone <- phoneParser
                   return (Person age firstName surname gender phone)