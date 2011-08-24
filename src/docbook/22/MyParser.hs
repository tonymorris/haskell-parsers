personParser1 :: Parser Person
personParser1 = bindParser ageParser (\age ->
                spaces >>>
                bindParser firstNameParser (\firstName ->
                spaces >>>
                bindParser surnameParser (\surname ->
                spaces >>>
                bindParser genderParser (\gender ->
                spaces >>>
                bindParser phoneParser (\phone ->
                value (Person age firstName surname gender phone))))))