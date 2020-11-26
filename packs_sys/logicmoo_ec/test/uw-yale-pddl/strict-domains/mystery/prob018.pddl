(DEFINE (PROBLEM MYSTY-18)
   (:DOMAIN MYSTERY-TYPED)
   (:OBJECTS PORK ONION FLOUNDER MUFFIN TUNA GRAPEFRUIT BROCCOLI
             YOGURT LEMON TOFU PAPAYA HOTDOG PISTACHIO SWEETROLL CHOCOLATE
             LOBSTER ENDIVE RICE CHICKEN SNICKERS SHRIMP SCALLOP HAM TOMATO
             KALE ORANGE CHERRY MARZIPAN PEPPER PEAR SCALLION TURKEY
             POTATO - FOOD
             STIMULATION LOVE LUBRICITY AESTHETICS EXPECTATION LEARNING
             CURIOSITY TRIUMPH ACHIEVEMENT UNDERSTANDING SATISFACTION
             ENTERTAINMENT INTOXICATION SATIETY EMPATHY REST EXCITEMENT
             - PLEASURE
             LACERATION DREAD ANGINA PROSTATITIS JEALOUSY ABRASION HANGOVER
             DEPRESSION BOILS ANXIETY ANGER LONELINESS SCIATICA GRIEF
             LONELINESS-1 ANXIETY-2 GRIEF-4 ABRASION-8 DEPRESSION-3
             PROSTATITIS-7 DREAD-6 BOILS-16 SCIATICA-5 ANGER-15 - PAIN
             BOSNIA PENNSYLVANIA GOIAS OREGON - PROVINCE
             URANUS JUPITER MARS VULCAN - PLANET)
   (:INIT (EATS PEPPER TOMATO)
          (EATS BROCCOLI TURKEY)
          (LOCALE CHOCOLATE BOSNIA)
          (LOCALE CHICKEN GOIAS)
          (EATS POTATO ORANGE)
          (EATS HAM PEPPER)
          (HARMONY CURIOSITY VULCAN)
          (EATS ENDIVE ORANGE)
          (HARMONY LOVE JUPITER)
          (EATS GRAPEFRUIT TOMATO)
          (HARMONY EXCITEMENT MARS)
          (EATS MARZIPAN PEAR)
          (EATS SHRIMP HOTDOG)
          (EATS CHERRY TOMATO)
          (LOCALE RICE PENNSYLVANIA)
          (HARMONY EXPECTATION VULCAN)
          (EATS ONION LOBSTER)
          (EATS SWEETROLL SNICKERS)
          (HARMONY LEARNING MARS)
          (EATS PAPAYA SNICKERS)
          (EATS CHICKEN FLOUNDER)
          (EATS ONION CHICKEN)
          (CRAVES UNDERSTANDING RICE)
          (LOCALE HAM BOSNIA)
          (CRAVES DREAD-6 SCALLION)
          (EATS PISTACHIO CHERRY)
          (CRAVES LOVE ONION)
          (EATS PAPAYA SWEETROLL)
          (CRAVES LONELINESS HOTDOG)
          (CRAVES PROSTATITIS TUNA)
          (EATS TOFU CHOCOLATE)
          (CRAVES TRIUMPH PISTACHIO)
          (EATS SCALLOP CHICKEN)
          (CRAVES LONELINESS-1 SHRIMP)
          (LOCALE HOTDOG OREGON)
          (CRAVES ABRASION-8 KALE)
          (CRAVES SCIATICA LOBSTER)
          (EATS PEPPER HAM)
          (HARMONY UNDERSTANDING JUPITER)
          (ORBITS URANUS JUPITER)
          (EATS SNICKERS PEAR)
          (HARMONY INTOXICATION JUPITER)
          (EATS TOMATO PEPPER)
          (HARMONY SATIETY MARS)
          (EATS KALE MARZIPAN)
          (CRAVES BOILS LEMON)
          (EATS SNICKERS PAPAYA)
          (CRAVES STIMULATION PORK)
          (CRAVES GRIEF-4 SCALLOP)
          (EATS SCALLOP MUFFIN)
          (EATS SCALLION GRAPEFRUIT)
          (EATS SWEETROLL PAPAYA)
          (EATS PORK PEAR)
          (CRAVES EXPECTATION LEMON)
          (EATS CHERRY PEPPER)
          (EATS SCALLOP SHRIMP)
          (LOCALE TOMATO BOSNIA)
          (HARMONY REST VULCAN)
          (HARMONY STIMULATION VULCAN)
          (LOCALE TOFU GOIAS)
          (CRAVES EXCITEMENT TURKEY)
          (CRAVES HANGOVER YOGURT)
          (EATS PISTACHIO TOFU)
          (LOCALE PISTACHIO PENNSYLVANIA)
          (CRAVES DEPRESSION-3 KALE)
          (EATS PAPAYA YOGURT)
          (EATS PEAR SNICKERS)
          (EATS CHICKEN ONION)
          (EATS TUNA CHOCOLATE)
          (CRAVES ANGINA TUNA)
          (EATS LEMON PAPAYA)
          (LOCALE MARZIPAN PENNSYLVANIA)
          (EATS SNICKERS CHERRY)
          (EATS POTATO SNICKERS)
          (EATS YOGURT FLOUNDER)
          (CRAVES BOILS-16 TURKEY)
          (EATS YOGURT PAPAYA)
          (HARMONY TRIUMPH VULCAN)
          (LOCALE ORANGE GOIAS)
          (HARMONY LUBRICITY JUPITER)
          (EATS FLOUNDER CHICKEN)
          (EATS GRAPEFRUIT SCALLION)
          (EATS LEMON POTATO)
          (EATS TOMATO GRAPEFRUIT)
          (EATS MUFFIN YOGURT)
          (EATS SNICKERS POTATO)
          (LOCALE FLOUNDER BOSNIA)
          (EATS SCALLION TUNA)
          (EATS CHICKEN SNICKERS)
          (EATS SCALLOP RICE)
          (EATS SCALLION SCALLOP)
          (CRAVES JEALOUSY BROCCOLI)
          (LOCALE SCALLION BOSNIA)
          (LOCALE POTATO BOSNIA)
          (EATS CHOCOLATE TOFU)
          (HARMONY SATISFACTION VULCAN)
          (LOCALE PEAR BOSNIA)
          (EATS RICE SCALLOP)
          (EATS ORANGE ENDIVE)
          (CRAVES ANGER-15 POTATO)
          (EATS SHRIMP SCALLOP)
          (LOCALE ENDIVE OREGON)
          (EATS YOGURT MUFFIN)
          (EATS LOBSTER ONION)
          (CRAVES ANGER PAPAYA)
          (CRAVES LACERATION ONION)
          (EATS BROCCOLI ORANGE)
          (EATS TURKEY BROCCOLI)
          (LOCALE SNICKERS PENNSYLVANIA)
          (CRAVES SATIETY HAM)
          (LOCALE LEMON PENNSYLVANIA)
          (EATS CHERRY SNICKERS)
          (ORBITS JUPITER MARS)
          (CRAVES GRIEF ENDIVE)
          (LOCALE MUFFIN GOIAS)
          (EATS FLOUNDER YOGURT)
          (EATS MUFFIN SCALLOP)
          (CRAVES ACHIEVEMENT CHOCOLATE)
          (EATS KALE POTATO)
          (CRAVES ANXIETY-2 SHRIMP)
          (EATS SNICKERS SWEETROLL)
          (LOCALE TURKEY PENNSYLVANIA)
          (LOCALE TUNA BOSNIA)
          (EATS CHERRY PISTACHIO)
          (CRAVES LUBRICITY FLOUNDER)
          (EATS ENDIVE RICE)
          (LOCALE CHERRY BOSNIA)
          (EATS ORANGE POTATO)
          (HARMONY AESTHETICS VULCAN)
          (LOCALE GRAPEFRUIT GOIAS)
          (EATS TOFU PISTACHIO)
          (EATS POTATO KALE)
          (CRAVES LEARNING PAPAYA)
          (LOCALE PEPPER OREGON)
          (EATS SNICKERS CHICKEN)
          (EATS SWEETROLL LOBSTER)
          (CRAVES EMPATHY ORANGE)
          (HARMONY ACHIEVEMENT JUPITER)
          (CRAVES PROSTATITIS-7 MARZIPAN)
          (EATS TURKEY HOTDOG)
          (EATS POTATO LEMON)
          (CRAVES SATISFACTION CHICKEN)
          (EATS ORANGE BROCCOLI)
          (EATS CHICKEN SCALLION)
          (EATS CHOCOLATE TUNA)
          (ATTACKS GOIAS OREGON)
          (EATS PISTACHIO TOMATO)
          (CRAVES AESTHETICS TUNA)
          (EATS PEPPER CHERRY)
          (CRAVES DEPRESSION YOGURT)
          (CRAVES DREAD FLOUNDER)
          (HARMONY ENTERTAINMENT MARS)
          (HARMONY EMPATHY JUPITER)
          (EATS PAPAYA LEMON)
          (EATS TOMATO CHERRY)
          (CRAVES SCIATICA-5 TURKEY)
          (CRAVES CURIOSITY HOTDOG)
          (LOCALE ONION BOSNIA)
          (ORBITS MARS VULCAN)
          (CRAVES ABRASION BROCCOLI)
          (EATS PORK HAM)
          (CRAVES REST PEAR)
          (EATS PEAR PORK)
          (EATS HOTDOG TURKEY)
          (LOCALE SHRIMP GOIAS)
          (LOCALE SCALLOP PENNSYLVANIA)
          (EATS SCALLOP SCALLION)
          (CRAVES INTOXICATION SCALLOP)
          (LOCALE LOBSTER PENNSYLVANIA)
          (ATTACKS BOSNIA PENNSYLVANIA)
          (EATS HAM PORK)
          (EATS HOTDOG SHRIMP)
          (CRAVES ANXIETY LEMON)
          (EATS TUNA SCALLION)
          (ATTACKS PENNSYLVANIA GOIAS)
          (CRAVES ENTERTAINMENT SHRIMP)
          (LOCALE PAPAYA BOSNIA)
          (EATS MARZIPAN KALE)
          (EATS TOMATO PISTACHIO)
          (LOCALE YOGURT PENNSYLVANIA)
          (LOCALE PORK GOIAS)
          (EATS SCALLION CHICKEN)
          (LOCALE BROCCOLI BOSNIA)
          (EATS RICE ENDIVE)
          (EATS CHICKEN SCALLOP)
          (EATS PEAR MARZIPAN)
          (LOCALE SWEETROLL BOSNIA)
          (LOCALE KALE BOSNIA)
          (EATS LOBSTER SWEETROLL))
   (:GOAL (AND (CRAVES LACERATION CHICKEN)
               (CRAVES ANGINA CHICKEN)
               (CRAVES ANGER-15 CHICKEN)
               (CRAVES DREAD CHICKEN))))